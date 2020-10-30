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

#define _C_FDatabas
#define _SMARTBASE 
#if 0
FDatabas.c

Methods for the Object Database class which supports the runtime file management 
interface between the Smartbase High Speed Engine and the host file management system.
The data structures, macros, and coding conventions used in this source file are designed
to isolate and protect the Smartbase High Speed Engine from the low level differences
between host file management systems. 


AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fdatabas.h"
#include "tdatabas.h"
#include "tobject.h"
#include "tdirect.h"
#include "futil1.h"
#include "futil2.h"
#include "futil3.h"
#include "fconio.h"
#include "fproc.h"
#include "tbytevec.h"
#include "fmake.h"
#include "fmath1.h"
#include "fdatefnc.h"
#include "terror.h"
#include "tstruct.h"

/*  Define all static variables for this Class. */

static unsigned char	FDatabas_OrMasks[8]   = {0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x01};
static unsigned char	FDatabas_AndMasks[8]  = {0x7F,0xBF,0xDF,0xEF,0xF7,0xFB,0xFD,0xFE};


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Init

Initialize the Smartbase Object Database File management system.

Note:   This function should only be called once at the beginning of the application.

#endif

TVAL    FDatabas_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
/*  Don't initialize more than once. */
if (gCP->FDatabas_Initialized) 
    FrameExit(gCP->TObject_OK);
gCP->FDatabas_Initialized = TRUE;

/* Initialize Global Error Messages */
gTP->FDatabas_IOerr			= TPERROR("!databaseOpen:Error Reading Database root page Table!");
gTP->FDatabas_OutOfMemory	= TPERROR("!Out of Repository Disk Space!");
gTP->FDatabas_PageSize		= TPERROR("!databaseResize: Resize Page failed!");

/*  Register Database Procedures. */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"compress",(LpFUNC)&FDatabas_Compress);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"uncompress",(LpFUNC)&FDatabas_Uncompress);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"encode",(LpFUNC)&FDatabas_Encode);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"decode",(LpFUNC)&FDatabas_Decode);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"loadMetaData",(LpFUNC)&FDatabas_LoadMetaData);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"saveMetaData",(LpFUNC)&FDatabas_SaveMetaData);
ExitOnError(*ec);

/*  Set up pointers to global symbols. */

gCP->FDatabas_pCompress =	FSmartbase_GetSymbolValuePtr(gCP,gTP,"compress",TRUE);
gCP->FDatabas_pUncompress = FSmartbase_GetSymbolValuePtr(gCP,gTP,"uncompress",TRUE);
gCP->FDatabas_pEncode =		FSmartbase_GetSymbolValuePtr(gCP,gTP,"encode",TRUE);
gCP->FDatabas_pDecode =		FSmartbase_GetSymbolValuePtr(gCP,gTP,"decode",TRUE);

FrameExit(*ec);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_GetPageTable

The  GetPageTable  function returns a pointer to the Page Table information.

#endif

PAGETABLE* FDatabas_GetPageTable(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
PAGETABLE*      pageTable;
 
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/*  Make sure the arguments are correct */

if (selfTval.Tag != TYBYTEVECTOR)
    {
    return(NULL);
    }

pageTable = (PAGETABLE*)*ByteVector(selfTval)->itsByteArray;
return(pageTable);
}


/*--------------------------------------------------------------------------------------- */
/*
FDatabas_MakeFrameID

The  FDatabas_MakeFrameID  function returns a frameID from the frame count and the
frame's page bit map index.

*/

REAL FDatabas_MakeFrameID(LpXCONTEXT gCP,LpTHREAD gTP,NUM frameCount, NUM frameIndex)
{
REAL			frameID;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
frameID = ((REAL)frameIndex * (REAL)_FRAMEINDEXSHIFT) + (REAL)frameCount;
#if _REPOTESTMODE	/* Database validity check */     
		if (FDatabas_FrameIDToBIndex(gCP,gTP,frameID) != frameIndex)
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
			}
		else
		if (FDatabas_FrameIDToBCount(gCP,gTP,frameID) != frameCount)
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
			}
#endif				/* Database validity check */
return(frameID);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_FrameIDToFPos

The  FDatabas_FrameIDToFPos  function returns an absolute file position from a frameID.

#endif

NUM FDatabas_FrameIDToFPos(LpXCONTEXT gCP,LpTHREAD gTP,REAL frameID)
{
NUM				frameFPos;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
if (frameID < 0) return(NILPAGEINDEX);
frameFPos = (frameID / (REAL)_FRAMEINDEXSHIFT) + .5;
frameFPos = sizeof(FILEROOT) + (frameFPos * _PAGESIZE);
return(frameFPos);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_FrameIDToFSize

The  FDatabas_FrameIDToFSize  function returns an absolute file size from a frameID.

#endif

NUM FDatabas_FrameIDToFSize(LpXCONTEXT gCP,LpTHREAD gTP,REAL frameID)
{
NUM				frameFPos;
NUM				frameSize;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
if (frameID < 0) return(NILPAGEINDEX);
frameFPos = (frameID / (REAL)_FRAMEINDEXSHIFT) + .5;
frameSize = (REAL)(frameID - ((REAL)frameFPos * (REAL)_FRAMEINDEXSHIFT));
frameSize = frameSize * _PAGESIZE;
return(frameSize);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_FrameIDToBIndex

The  FDatabas_FrameIDToBIndex  function returns a bit map index from a frameID.

#endif

NUM FDatabas_FrameIDToBIndex(LpXCONTEXT gCP,LpTHREAD gTP,REAL frameID)
{
NUM				frameBIndex;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
if (frameID < 0) return(NILPAGEINDEX);
frameBIndex = (frameID / (REAL)_FRAMEINDEXSHIFT) + .5;
return(frameBIndex);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_FrameIDToBCount

The  FDatabas_FrameIDToBCount  function returns a bit map page count from a frameID.

#endif

NUM FDatabas_FrameIDToBCount(LpXCONTEXT gCP,LpTHREAD gTP,REAL frameID)
{
NUM				frameBCount;
NUM				frameFPos;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
if (frameID < 0) return(NILPAGEINDEX);
frameFPos = (frameID / (REAL)_FRAMEINDEXSHIFT) + .5;
frameBCount = (REAL)(frameID - ((REAL)frameFPos * (REAL)_FRAMEINDEXSHIFT));
return(frameBCount);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Open

The  databaseOpen  Procedure opens the specified Database file. Database files are a 
primary local database storage tool for the SmartBase engine. Database files support 
the basic file paging services necessary for extremely high speed storage and retrieval 
of arbitrarily complex objects which maybe indexed by arbitrarily complex keys.

The arguments are as follows.

baseFilePos     The base file position of the Database in the host file system.
				Note: All parent repositories have a base file position of zero (0). 
				      All child repositories have a base file position of greater than zero. 

filename        The file name of the Database on the host file system. 
                The name may include the full path name of the file.

mode            share:  A value of (share:) or zero (0) requests that only an existing file be 
                        opened for read access only. 
                new:    A value of (new:) or one (1) indicates that a new file should be created,
                        or an existing file overwritten for read & write access. 
                update: A value of (update:) or two (2) indicates that an existing file be 
                        opened, but, if one does not exist, a new file should be created, and
                        used for both read & write access.
                tclear: A value of (tclear:) creates a new file with the Tree indexing strategy turned on.
                tree:   A value of (tree:) indicates that the Tree indexing strategy should be turned on.

For example
    
	(databaseOpen  0 "myfolder\myodbfile"  new:)
            =>  initializes the specified Database file and returns an integer file 
                identifier to be used in all future references to this file.
                
Note:   After the above, "myfolder\myodbfile" will be initialized as a Database file.

#endif

TVAL FDatabas_Open(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
NUM				baseFilePos = 0;
NUM				baseFileSize = 0;
NUM             pageTableFrameSize;
NUM             pageTableByteLen;
NUM				pageBitCount;
NUM             i;
NUM             ioerr;
BOLE            rollback = TRUE;
FILEROOT        fileRoot;
PAGETABLE*      pageTable;
TVAL            openCmd = TFUNCTION(FConio_fopen);
TVAL            makeCmd = TFUNCTION(FMake_Vector);
BOLE            newFile = FALSE;
BOLE            newSetTreeOn = FALSE;
BOLE            checkTreeOn = FALSE;
BOLE            readWriteOn;
StartFrame
DeclareTVAL(fileName);
DeclareTVAL(fileID);
DeclareTVAL(fileVEC);
DeclareTVAL(tmp);
EndFrame
 
/*  Make sure the arguments are correct */

if ((argc >= 1) && (isNumIndex(&argv[0])))
    {
    baseFilePos = asNumIndex(&argv[0]);
    }
else
if ((argc >= 1) && (argv[0].Tag == TYREPOINSERT))
    {
    baseFilePos = FDatabas_FrameIDToFPos(gCP,gTP,argv[0].u.Real);
    baseFileSize = FDatabas_FrameIDToFSize(gCP,gTP,argv[0].u.Real);
    }
else
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
   
/*  Open the specified Database file, return if there is an error. */

*fileName = argv[1];
if (((argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"update") == 0)) ||
    ((argv[2].Tag == TYNUM) && (argv[2].u.Int == 2)))
    {
    /*  If the mode is update (2), find out if the Database file exists. */
    /*  Note:   This code opens the Database file for both read & write. */
    
    readWriteOn = TRUE;

	if (baseFilePos == 0)
		{
		/*  If the mode is update (2), find out if the Database file exists. */
		/*  Note:   This code opens the Database file for both read & write. */

		*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(0),TINT(5));

		/* Does the Database file not exist? */

		if (fileID->Tag == TYERROR)
			{
			/*  Create the specified Database file, return if there is an error. */
			/*  Note:   This code opens the Database file for both read & write. */
        
			newFile = TRUE;
			*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(1),TINT(5));
			ExitOnError(*fileID);
			}
		else
			{
			newFile = FALSE;
			}
		}
	else
		{
		/*  If this is an inserted repository, then open the inserted repository. */
		/*  Note:   This code opens the Database file for both read & write. */

		*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(3),TINT(5));
		ExitOnError(*fileID);
		newFile = FALSE;
		}
    }
else
/*  If the mode is share (0), open an existing Database file. */
/*  Note:   This code opens the Database file for read only. */

if (((argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"share") == 0)) ||
    ((argv[2].Tag == TYNUM) && (argv[2].u.Int == 0)))
    {
    readWriteOn = FALSE;
    newFile = FALSE;
	if (baseFilePos == 0)
		{
		/*  Open an existing Database file for a standalone repository. */
		*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(2),TINT(5));
		}
	else
		{
		/*  Open an existing Database file for an inserted repository. */
		*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(3),TINT(5));
		}
    ExitOnError(*fileID);
    }
else
/*  If the mode is new (1), create a new Database overwriting the old. */
/*  Note:   This code opens the Database file for both read & write. */

if (((argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"new") == 0)) ||
    ((argv[2].Tag == TYNUM) && (argv[2].u.Int == 1)))
    {
    readWriteOn = TRUE;
    newFile = TRUE;
	if (baseFilePos == 0)
		{
		/*  Open an existing Database file for a standalone repository. */
		*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(1),TINT(5));
		}
	else
		{
		/*  Open an existing Database file for an inserted repository. */
		*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(3),TINT(5));
		}
    ExitOnError(*fileID);
    }
else
/*  If the mode is tree, open the Database for Tree indexing.  */
/*  Note:   This code opens the Database file for both read & write. */
/*  Note2:  This code sets the Tree indexing stragey on.			 */

if ((argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"tree") == 0))
    {
    /*  If the mode is tree, find out if the Database file exists.		 */
    /*  Note:   This code opens the Database file for both read & write. */
    
    readWriteOn = TRUE;
	newSetTreeOn = TRUE;
	checkTreeOn = TRUE;

	if (baseFilePos == 0)
		{
		/*  If the mode is tree, find out if the Database file exists.      */
		/*  Note:   This code opens the Database file for both read & write. */

		*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(0),TINT(5));

		/* Does the Database file not exist? */

		if (fileID->Tag == TYERROR)
			{
			/*  Create the specified Database file, return if there is an error. */
			/*  Note:   This code opens the Database file for both read & write. */
        
			newFile = TRUE;
			*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(1),TINT(5));
			ExitOnError(*fileID);
			}
		else
			{
			newFile = FALSE;
			}
		}
	else
		{
		FrameExit(TERROR("!ObjRepository.open: cannot open an embedded repository for Tree indexing!"));
		}
    }
else
/*  If the mode is bnew, clear and open the Database for Tree indexing.  */
/*  Note:   This code opens the Database file for both read & write. */
/*  Note2:  This code sets the Tree indexing stragey on.			 */

if ((argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"bnew") == 0))
    {
    readWriteOn = TRUE;
    newFile = TRUE;
	newSetTreeOn = TRUE;
	checkTreeOn = TRUE;

	if (baseFilePos == 0)
		{
		/*  Open an existing Database file for a standalone repository. */
		*fileID = FSmartbase_Eval(gCP,gTP,openCmd,3,*fileName,TINT(1),TINT(5));
		}
	else
		{
		/*  Open an existing Database file for an inserted repository. */
		FrameExit(TERROR("!ObjRepository.open: cannot open an embedded repository for Tree indexing!"));
		}
    ExitOnError(*fileID);
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }


/*  If the file exists, read the root and page table from the Database file. */

if (!newFile)
    {
    /*  Read the Database file's root page. */
    
	ioerr = FDatabas_readf(gCP,gTP,*fileID,baseFilePos,0,sizeof(FILEROOT),(char*)&fileRoot);
    if (ioerr != 0)
        {
        FrameExit(gTP->FDatabas_IOerr);
        }
    else
    if (fileRoot.recordType != _ROOTRECORDTYPE)
        {
        FrameExit(gTP->FDatabas_IOerr);
        }
    else
    if (fileRoot.recordVersion != _VERSION)
        {
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_WRONG_VERSION);
        }
        
    /*  Make byte vector to hold the Database file's page table. */
    *tmp = TSYMBOL("byte");
    *fileVEC = FSmartbase_Eval(gCP,gTP,makeCmd,2,
                                     *tmp,
                                     TINT(fileRoot.PageTableSize));
    ExitOnError(*fileVEC);
    ByteVector(*fileVEC)->itsCdr = *fileID;
    pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;

    /*  Read the Database file's page table. */
    
	ioerr = FDatabas_readf(gCP,gTP,*fileID,baseFilePos,fileRoot.PageTableFPos,fileRoot.PageTableSize,(char*)pageTable);
    if ((ioerr != 0) || (pageTable->recordType != _PAGERECORDTYPE))
        {
        FrameExit(gTP->FDatabas_IOerr);
        }
    else
    if (fileRoot.recordVersion != _VERSION)
        {
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_WRONG_VERSION);
        }

    /*  Copy the main page bit map to the used page bit map. */
    /*  Note: This allows unlimited rollback of transaction */
    /*        disk operations because new pages are always */
    /*        allocated from the used page bit map. Pages are */
    /*        freed only from the main page bit map. Therefore, */
    /*        a newly freed page is never reused until after */
    /*        this transaction is committed. */

	_FMemory_memcpy(&pageTable->PageBitMaps[pageTable->PageBitMapByteLen],	/* to	  */
							pageTable->PageBitMaps,							/* from	  */
							pageTable->PageBitMapByteLen);					/* length */

	if ((checkTreeOn == TRUE) && (pageTable->TreeIndexOn != TRUE))
		{
		FrameExit(TERROR("!ObjRepository.open: expected repository with Tree indexing turned on!"));
		}

    }
else
/*  If the file is new, initialize the new Database file. */

    {
    /*  Resize the new Database page table for the number of some initial pages. */
    
	if (baseFileSize == 0)
		/*  Resize the parent repository page table at the initial frame size. */
		{
		pageBitCount			= PAGETABLEMAXPAGESFROMPAGES(_INITPAGECOUNT);
		pageTableByteLen        = PAGETABLESIZEFROMPAGES(_INITPAGECOUNT);
		pageTableFrameSize      = PAGETABLEFRAMESIZEFROMSIZE(pageTableByteLen);
		}
	else
		/*  Resize the parent repository page table at the maximum file size. */
		{
		pageBitCount			= PAGETABLEMAXPAGESFROMFILELEN(baseFileSize);
		pageTableByteLen        = PAGETABLESIZEFROMFILELEN(baseFileSize);
		pageTableFrameSize      = PAGETABLEFRAMESIZEFROMSIZE(pageTableByteLen);
		}

    /*  Make byte vector to hold Database page table. */

    *tmp = TSYMBOL("byte");
    *fileVEC = FSmartbase_Eval(gCP,gTP,makeCmd,2,
                                     *tmp,
                                     TINT(pageTableFrameSize));
    ExitOnError(*fileVEC);
    ByteVector(*fileVEC)->itsCdr = *fileID;
    pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;

    /*  Initialize the new Database file with a root page. */
    
    strcpy(fileRoot.AisIdentifier,"AIS");
    sprintf(fileRoot.VersionIdentifier,"%d",_VERSION);
    fileRoot.recordType     = _ROOTRECORDTYPE;
    fileRoot.recordVersion  = _VERSION;
    fileRoot.recordLength   = sizeof(FILEROOT) - sizeof(DBRECORDHEADER);
    fileRoot.SaveTimeStamp  = FDateFnc_now(gCP,gTP,0,NULL).u.Real;
    fileRoot.PageTableFPos  = sizeof(FILEROOT);
    fileRoot.PageTableSize  = pageTableFrameSize;
    
	ioerr = FDatabas_writef(gCP, gTP,*fileID,baseFilePos,0,sizeof(FILEROOT),(char*)&fileRoot);
    if (ioerr != 0) 
        FrameExit(gTP->FDatabas_IOerr);
    
    /*  Initialize the new Database file with a page table. */
    
    strcpy(pageTable->AisIdentifier,"AIS");
    sprintf(pageTable->VersionIdentifier,"%d",_VERSION);
    pageTable->recordType       = _PAGERECORDTYPE;
    pageTable->recordVersion    = _VERSION;
	pageTable->recordLength     = pageTableFrameSize - sizeof(DBRECORDHEADER);
    pageTable->SaveTimeStamp    = FDateFnc_now(gCP,gTP,0,NIL).u.Real;
    pageTable->baseFilePos		= baseFilePos;
    pageTable->PageTableFPos    = sizeof(FILEROOT);
    pageTable->PageTableSize    = pageTableFrameSize;
    pageTable->PageTableFrame   = FDatabas_MakeFrameID(gCP, gTP,(pageTable->PageTableSize/_PAGESIZE),((pageTable->PageTableFPos-sizeof(FILEROOT))/_PAGESIZE));
	if (baseFileSize == 0)
		pageTable->FileSize = sizeof(FILEROOT);					/* The SetFrame call will increase this size.	*/
	else
		pageTable->FileSize = FILESIZEFROMFILELEN(baseFileSize);/* Child repository sizes never grow.			*/
    pageTable->MaxPageCount     = pageBitCount;
    pageTable->UsedPageCount    = 0;							/* The SetFrame call will increase this count.	*/
    pageTable->UsedFileBytes    = sizeof(FILEROOT);				/* The SetFrame call will increase this size.	*/
    pageTable->MetaDataObjectFrame	= NILPAGEINDEX;
    pageTable->IndexObjectFrame	= NILPAGEINDEX;
    pageTable->IAmDirty         = FALSE;						/* The SetFrame call will set this item.		*/
    pageTable->TreeIndexOn     = newSetTreeOn;					/* Set Tree indexing strategy on/off.			*/
    pageTable->FirstPhysicalPage = 0;
    pageTable->LastPhysicalPage = 0;							/* The SetFrame call will set this item.		*/
    pageTable->SavedObjectCount	= 0;							/* The SetFrame call will increase this count.	*/
    pageTable->PreviousVacantPage = 0;
    pageTable->PageMapsByteLen	= pageBitCount / 4;
    pageTable->PageBitMapByteLen = pageBitCount / 8;

    /*  Set all pages in both the main and used page bit maps to vacant. */
    
    for (i = 1; i < pageTable->PageMapsByteLen; ++i)
		{
		pageTable->PageBitMaps[i] = 0;
		}

    /*  Set the first frame (for the page table) to occupied. */
    /*  Note: The first Frame will contain the Page Table. */
	FDatabas_SetFrame(gCP, gTP, pageTable,0,pageTableFrameSize / _PAGESIZE);
        
    /*  Write the page table to the Database file. */

	ioerr = FDatabas_writef(gCP, gTP,*fileID,baseFilePos,sizeof(FILEROOT),pageTable->PageTableSize,(char*)pageTable);
    if (ioerr != 0) 
        FrameExit(gTP->FDatabas_IOerr);
    }
    
/*  Set the automatic rollback switch. */

pageTable->RollBackOn = rollback;
    
/*  Set the time stamp for all transactions */
/*  which occur during this Open session. */

pageTable->OpenTimeStamp = FDateFnc_now(gCP,gTP,0,NIL).u.Real;

/*  Set the read-write mode for all transactions */
/*  which occur during this Open session. */

pageTable->ReadWriteOn = readWriteOn;

/*  Return the file ID of the newly open Database file. */

FrameExit(*fileVEC);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_NewMF

The  NewMF  function creates a new Database virtual memory file. 

The arguments are as follows.

baseFileSize    The base file size of the virtual memory file. 


#endif

TVAL FDatabas_NewMF(LpXCONTEXT gCP,LpTHREAD gTP,NUM baseFileSize)
{
NUM             baseFilePos = 0;
NUM             pageTableFrameSize;
NUM             pageTableByteLen;
NUM				pageBitCount;
NUM             i;
NUM             ioerr;
FILEROOT        fileRoot;
PAGETABLE*      pageTable;
TVAL            makeCmd = TFUNCTION(FMake_Vector);
BOLE            readWriteOn = TRUE;
BOLE            rollback = TRUE;
StartFrame
DeclareTVAL(fileID);
DeclareTVAL(fileVEC);
DeclareTVAL(byte);
EndFrame
 
/*  Open the specified Database virtual file, return if there is an error. */
/*  Open a virtual Database file for a standalone repository. */

*byte = TSYMBOL("byte");
*fileID = FSmartbase_Eval(gCP,gTP,makeCmd,2,*byte,TINT(baseFileSize));
ExitOnError(*fileID);

/*  Resize the new Database page table for the number of some initial pages. */

if (baseFileSize == 0)
	/*  Resize the parent repository page table at the initial frame size. */
	{
	pageBitCount			= PAGETABLEMAXPAGESFROMPAGES(_INITPAGECOUNT);
	pageTableByteLen        = PAGETABLESIZEFROMPAGES(_INITPAGECOUNT);
	pageTableFrameSize      = PAGETABLEFRAMESIZEFROMSIZE(pageTableByteLen);
	}
else
	/*  Resize the parent repository page table at the maximum file size. */
	{
	pageBitCount			= PAGETABLEMAXPAGESFROMFILELEN(baseFileSize);
	pageTableByteLen        = PAGETABLESIZEFROMFILELEN(baseFileSize);
	pageTableFrameSize      = PAGETABLEFRAMESIZEFROMSIZE(pageTableByteLen);
	}

/*  The file is new, initialize the new Database virtual file. */
/*  Make byte vector to hold Database page table. */

*fileVEC = FSmartbase_Eval(gCP,gTP,makeCmd,2,*byte,TINT(pageTableFrameSize));
ExitOnError(*fileVEC);
ByteVector(*fileVEC)->itsCdr = *fileID;
pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;

/*  Initialize the new Database file with a root page. */

strcpy(fileRoot.AisIdentifier,"AIS");
sprintf(fileRoot.VersionIdentifier,"%d",_VERSION);
fileRoot.recordType     = _ROOTRECORDTYPE;
fileRoot.recordVersion  = _VERSION;
fileRoot.recordLength   = sizeof(FILEROOT) - sizeof(DBRECORDHEADER);
fileRoot.SaveTimeStamp  = FDateFnc_now(gCP,gTP,0,NULL).u.Real;
fileRoot.PageTableFPos  = sizeof(FILEROOT);
fileRoot.PageTableSize  = pageTableFrameSize;

ioerr = FDatabas_writef(gCP, gTP,*fileID,baseFilePos,0,sizeof(FILEROOT),(char*)&fileRoot);
if (ioerr != 0) 
    FrameExit(gTP->FDatabas_IOerr);

/*  Initialize the new Database file with a page table. */

strcpy(pageTable->AisIdentifier,"AIS");
sprintf(pageTable->VersionIdentifier,"%d",_VERSION);
pageTable->recordType       = _PAGERECORDTYPE;
pageTable->recordVersion    = _VERSION;
pageTable->recordLength     = pageTableFrameSize - sizeof(DBRECORDHEADER);
pageTable->SaveTimeStamp    = FDateFnc_now(gCP,gTP,0,NIL).u.Real;
pageTable->baseFilePos		= baseFilePos;
pageTable->PageTableFPos    = sizeof(FILEROOT);
pageTable->PageTableSize    = pageTableFrameSize;
pageTable->PageTableFrame   = FDatabas_MakeFrameID(gCP, gTP,(pageTable->PageTableSize/_PAGESIZE),((pageTable->PageTableFPos-sizeof(FILEROOT))/_PAGESIZE));
if (baseFileSize == 0)
	pageTable->FileSize = sizeof(FILEROOT);					/* The SetFrame call will increase this size.	*/
else
	pageTable->FileSize = FILESIZEFROMFILELEN(baseFileSize);/* Child repository sizes never grow.			*/
pageTable->MaxPageCount     = pageBitCount;
pageTable->UsedPageCount    = 0;							/* The SetFrame call will increase this count.	*/
pageTable->UsedFileBytes    = sizeof(FILEROOT);				/* The SetFrame call will increase this size.	*/
pageTable->MetaDataObjectFrame	= NILPAGEINDEX;
pageTable->IndexObjectFrame	= NILPAGEINDEX;
pageTable->IAmDirty         = FALSE;						/* The SetFrame call will set this item.		*/
pageTable->FirstPhysicalPage = 0;
pageTable->LastPhysicalPage = 0;							/* The SetFrame call will set this item.		*/
pageTable->SavedObjectCount	= 0;							/* The SetFrame call will increase this count.	*/
pageTable->PreviousVacantPage = 0;
pageTable->PageMapsByteLen	= pageBitCount / 4;
pageTable->PageBitMapByteLen = pageBitCount / 8;

/*  Set all pages in both the main and used page bit maps to vacant. */

for (i = 1; i < pageTable->PageMapsByteLen; ++i)
	{
	pageTable->PageBitMaps[i] = 0;
	}

/*  Set the first frame (for the page table) to occupied. */
/*  Note: The first Frame will contain the Page Table. */
FDatabas_SetFrame(gCP, gTP,pageTable,0,pageTableFrameSize / _PAGESIZE);
            
/*  Write the page table to the Database file. */

ioerr = FDatabas_writef(gCP, gTP,*fileID,baseFilePos,sizeof(FILEROOT),pageTable->PageTableSize,(char*)pageTable);
if (ioerr != 0) 
    FrameExit(gTP->FDatabas_IOerr);
    
/*  Set the automatic rollback switch. */

pageTable->RollBackOn = rollback;
    
/*  Set the time stamp for all transactions */
/*  which occur during this Open session. */

pageTable->OpenTimeStamp = FDateFnc_now(gCP,gTP,0,NULL).u.Real;

/*  Set the read-write mode for all transactions */
/*  which occur during this Open session. */

pageTable->ReadWriteOn = readWriteOn;

/*  Return the file ID of the newly open Database file. */

FrameExit(*fileVEC);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Close

The  databaseClose  Procedure closes the specified Database file object (odbID). 
Database files are a primary local database storage tool for the SmartBase engine. 
Database files support the basic file paging services necessary for extremely high 
speed storage and retrieval of arbitrarily complex objects which may be indexed by 
arbitrarily complex keys.

The arguments are as follows.

odbID       The database file object returned by a previous databaseOpen procedure.

option      The file close option 
            erase:      (0) = reject all transactions and erase file after closing. 
            commit:     (1) = accept all transactions and do not erase file after closing.
            refuse:     (2) = reject all transactions and do not erase file after closing.

    
    (setq  odbID  (databaseOpen  "\myfolder\myodbfile"  1))

    (databaseClose  odbID  1)

Note:   After the above, "\myfolder\myodbfile" will be initialized as a Database 
        file and then closed.

#endif

TVAL FDatabas_Close(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL            frameID;
NUM             ioerr;
NUM				pageTableSize;
NUM				maxPageCount;
NUM				pageTableBIndex;
NUM				pageTableBCount;
FILEROOT        fileRoot;
PAGETABLE*      pageTable;
TVAL            freeCmd = TFUNCTION(FDatabas_FreeFrameID);
TVAL            newCmd = TFUNCTION(FDatabas_NewFrameID);
StartFrame
DeclareTVAL(err);
DeclareTVAL(newFrame);
DeclareTVAL(fileID);
DeclareTVAL(fileVEC);
EndFrame
 
/*  Make sure the arguments are correct */

if ((argc != 2) || (argv[0].Tag != TYBYTEVECTOR))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

*fileVEC = argv[0];
*fileID = ByteVector(argv[0])->itsCdr;
if (fileID->Tag == TYNUM)
	{
	/* If this is a disk file, we delete the file ID */
	/* so that no one will try to close the file twice. */

	ByteVector(*fileVEC)->itsCdr = gCP->TObject_VOID;
	}
	pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;

/*  If the option is zero (0) destroy the Database file. */

if (((argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"erase") == 0)) ||
    ((argv[1].Tag == TYNUM) && (argv[1].u.Int == 0)))
    {
    /*  Destroy the specified Database file, return if there is an error. */
    
	if (fileID->Tag == TYNUM)
		{
		(*gCP->_Host_Closef)((POINTER)gCP,gTP,fileID->u.Int, 0);
		}
    }
else
/*  If the option is two (2) with automatic rollback, reject all transactions, */
/*  or If the Database was opened for read only, then reject all transactions. */

if ((!pageTable->ReadWriteOn) ||
    ((pageTable->RollBackOn) &&
    ((argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"refuse") == 0))) ||
    ((argv[1].Tag == TYNUM) && (argv[1].u.Int == 2)))
    {
    /*  Close the specified Database file, reject all transactions. */
    
	if (fileID->Tag == TYNUM)
		{
		(*gCP->_Host_Closef)((POINTER)gCP,gTP,fileID->u.Int, 1);
		}
    }
else
/*  If the option is one (1) close and preserve the Database file. */

if (((argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"commit") == 0)) ||
    ((argv[1].Tag == TYNUM) && (argv[1].u.Int == 1)))
    {
    /*  Do we need to find another frame to store the page table? */
    
    if (pageTable->IAmDirty)
        {

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

		/* Always free the old page table frame to assure unlimited roll back. */

		*err = FSmartbase_Eval(gCP,gTP,freeCmd,2,*fileVEC,TFRAME(pageTable->PageTableFrame));
		ExitOnError(*err);

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

		/* Compute the new proper length of the page table adjusted to a frame boundary. */

TryAgain:
		maxPageCount = pageTable->MaxPageCount;
		pageTableSize = PAGETABLESIZEFROMPAGES(pageTable->MaxPageCount);
		pageTable->PageTableSize = PAGETABLESIZEFROMPAGES(pageTable->MaxPageCount);


#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

		/* Now secure a new page table frame for writing the altered page table. */

		*newFrame = FSmartbase_Eval(gCP,gTP,newCmd,2,*fileVEC,TINT(pageTable->PageTableSize));
		ExitOnError(*newFrame);
		pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;


		/* Make sure the page table did not grow trying to allocate space for itself. */

		if (pageTableSize != pageTable->PageTableSize)
			{
			pageTableBIndex = FDatabas_FrameIDToBIndex(gCP,gTP,newFrame->u.Real);
			pageTableBCount = FDatabas_FrameIDToBCount(gCP,gTP,newFrame->u.Real);
			FDatabas_FreeUsedFrame(gCP,gTP,pageTable, pageTableBIndex, pageTableBCount);

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

			goto TryAgain;
			}

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

#if _REPOTESTMODE	/* Database validity check */
		frameID = newFrame->u.Real;
		newPageTableSize = FDatabas_FrameIDToFSize(gCP,gTP,frameID);
		if (pageTableSize < newPageTableSize)
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
			}
#endif				/* Database validity check */

		pageTable->PageTableFrame = frameID = newFrame->u.Real;
		pageTable->PageTableFPos = FDatabas_FrameIDToFPos(gCP,gTP,frameID);
		pageTable->PageTableSize = FDatabas_FrameIDToFSize(gCP,gTP,frameID);

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

		/*  While an empty page is at the end of the Database file,  */
		/*  we will wish to release the empty space. This technique is  */
		/*  called resizing and it keeps the Database from always growing  */
		/*  and never shrinking after repeated file use. */

		while (REFBIT(pageTable->PageBitMaps,pageTable->LastPhysicalPage) == 0)
			{
			/*  Update the page table to reflect the newly released page bytes. */
    
			--pageTable->LastPhysicalPage;
			}
		}
        
#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

	/*  Resize the files space to reflect any released page bytes. */
	/*  Note: Do not resize if we are a child repository. */

	if ((pageTable->baseFilePos == 0) && (fileID->Tag == TYNUM))
		{
		pageTable->FileSize = sizeof(FILEROOT) + ((pageTable->LastPhysicalPage + 1) * _PAGESIZE);
		}

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

    /*  Save the Database file's page table (if necessary). */
    
    if (pageTable->IAmDirty)
        {
        /*  Write the Database file's page table. */
        
		strcpy(pageTable->AisIdentifier,"AIS");
		sprintf(pageTable->VersionIdentifier,"%d",_VERSION);
        pageTable->recordType	 = _PAGERECORDTYPE;
        pageTable->recordVersion = _VERSION;
        pageTable->recordLength  = pageTable->PageTableSize - sizeof(DBRECORDHEADER);
		pageTable->SaveTimeStamp = FDateFnc_now(gCP,gTP,0,NULL).u.Real;
        pageTable->IAmDirty      = FALSE;

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

		ioerr = FDatabas_writef(gCP, gTP,*fileID,pageTable->baseFilePos,pageTable->PageTableFPos,pageTable->PageTableSize,(char*)pageTable);
        if (ioerr != 0) 
            FrameExit(gTP->FDatabas_IOerr);

        /*  Write the Database file's root record. */
        
		strcpy(fileRoot.AisIdentifier,"AIS");
		sprintf(fileRoot.VersionIdentifier,"%d",_VERSION);
		fileRoot.recordType     = _ROOTRECORDTYPE;
		fileRoot.recordVersion  = _VERSION;
        fileRoot.recordLength   = sizeof(FILEROOT) - sizeof(DBRECORDHEADER);
        fileRoot.SaveTimeStamp  = FDateFnc_now(gCP,gTP,0,NULL).u.Real;
        fileRoot.PageTableFPos  = pageTable->PageTableFPos;
        fileRoot.PageTableSize  = pageTable->PageTableSize;
        
		ioerr = FDatabas_writef(gCP, gTP,*fileID,pageTable->baseFilePos,0,sizeof(FILEROOT),(char*)&fileRoot);
        if (ioerr != 0) 
            FrameExit(gTP->FDatabas_IOerr);
        }


    /*  Resize the files space to reflect any released page bytes. */
	/*  Note: Do not resize if we are an inserted repository. */
    
	if ((pageTable->baseFilePos == 0) && (fileID->Tag == TYNUM))
		{
		ioerr = (*gCP->_Host_Resizef)((POINTER)gCP,gTP,fileID->u.Int, pageTable->baseFilePos + pageTable->FileSize);
		}
    
#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
		FSmartbase_Eval(gCP,gTP,TGVALUE("inspect"),3,argv[0],TFRAME(pageTable->IndexObjectFrame),TSYMBOL("check"));
#endif				/* Database validity check */

		/*  Close the specified Database file, return if there is an error. */
    
	if (fileID->Tag == TYNUM)
		{
		(*gCP->_Host_Closef)((POINTER)gCP,gTP,fileID->u.Int, 1);
		}
    }
else
/*  If the mode is anything else, then we have an error. */
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
    
/*  Return to the calling procedure. */

FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_NewFrameID

The FDatabas_NewFrameID Procedure allocates a disk frame in the specified
Database file. A disk frame equal to the specified requested size will
be returned. If there is no error, the page number will be returned; 
otherwise, an error will be returned.

The arguments are as follows.

odbID		The database file object returned by a previous database-open procedure.

size		The size of the disk page to be returned.

returns:	A the new frame ID as a real number which contains the page count and the
			starting page index of the frame.

Note:   Initializes the specified Database file and returns a database 
        page number to be used in all future references to this page.
        
#endif

TVAL FDatabas_NewFrameID(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM             newSize;
NUM             extraFrameSize;
NUM             newFrameIndex;
NUM             newFrameSize;
NUM             newBitMapByteLen;
NUM             oldBitMapByteLen;
REAL	        newFrameID;
NUM				newFileSize;
NUM				newPageTableSize;
NUM				newMaxPageCount;
unsigned char*	sourcePtr;
unsigned char*	targetPtr;
unsigned char*	stopPtr;
BOLE            addToEndOfFile;
PAGETABLE*      pageTable;
TVAL            resizeCmd = TFUNCTION(FMake_Resize);
StartFrame
DeclareTVAL(err);
DeclareTVAL(fileID);
DeclareTVAL(fileVEC);
EndFrame
 
/*  Make sure the arguments are correct */

if ((argc != 2) || (argv[0].Tag != TYBYTEVECTOR) || !isNumIndex(&argv[1]))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Initialize the Database root and page table, etc. */

*fileVEC = argv[0];
*fileID = ByteVector(*fileVEC)->itsCdr;
pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;
newSize = asNumIndex(&argv[1]);
    
#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/*  Do we have an empty interior Database file page, or do we add to the end? */
/*  Note:   The empty pages are chained together, by frame, so we can quickly */
/*          access the first free page of each frame size. */

newFrameSize =	FRAMECOUNTFROMSIZE(newSize);
newFrameIndex = FDatabas_FindFrame(gCP, gTP, pageTable, newFrameSize);
addToEndOfFile = (newFrameIndex == NILPAGEINDEX) ? TRUE : FALSE;
    
/*  Do we add to the end of the Database file? */
/*  Note:	We must extend the page table bit maps */
/*			to include the new pages required. */

if (addToEndOfFile)
    {
    /*  Make sure we are not adding pages beyond the maximum. */
	/*  Note:  Parent repositories are initialized with an expanding page table. */
    /*         Furthermore, all of these algorithms are sensitive to byte boundries, */
	/*		   so we take special care to assure that added pages are in byte increments. */

	extraFrameSize = max(newFrameSize,_INITPAGECOUNT);
	extraFrameSize = MAXPAGESFROMPAGES(extraFrameSize);
	newMaxPageCount	= pageTable->MaxPageCount + extraFrameSize;

	if (newMaxPageCount > _MAXDISKPAGECOUNT)
		{
		FrameExit(gTP->FDatabas_OutOfMemory);		
		}

	/*  Make sure child repositories do not extend beyond their allocated size */
	/*  Note:  Child repositories are allocated with a fixed maximum file size. */

	newFileSize = FILESIZEFROMPAGES(newMaxPageCount);

	/*  Check to make sure we didn't go over the fixed file length (child repositories). */

	if ((pageTable->baseFilePos != 0) && (newFileSize > pageTable->FileSize))
		{
		extraFrameSize = newFrameSize;
		extraFrameSize = MAXPAGESFROMPAGES(extraFrameSize);
		newFileSize = FILESIZEFROMPAGES(newMaxPageCount);
		if ((pageTable->baseFilePos != 0) && (newFileSize > pageTable->FileSize))
			{
			FrameExit(gTP->FDatabas_OutOfMemory);		
			}
		}

    /*  Extend both page table bit maps by the required amount. */
    /*  Note:   We must add bits to both the main and used */
    /*          page table bit maps. This may involve */
    /*          growing and repositioning the page table. */

    /*  Resize the byte vector to hold the new page table bit maps. */

	newPageTableSize = PAGETABLESIZEFROMPAGES(newMaxPageCount);
    *fileVEC = FSmartbase_Eval(gCP,gTP,resizeCmd,2,argv[0],TINT(newPageTableSize));
    pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;

    /*  Move the used page table bit map up to make room for the extra pages in the main bit map. */
    
	oldBitMapByteLen = pageTable->PageBitMapByteLen;
	newBitMapByteLen = newMaxPageCount / 8;
	targetPtr = &pageTable->PageBitMaps[newBitMapByteLen + oldBitMapByteLen];
	sourcePtr = &pageTable->PageBitMaps[oldBitMapByteLen + oldBitMapByteLen];
	stopPtr = &pageTable->PageBitMaps[newBitMapByteLen];
	while (targetPtr > stopPtr)
		{
		*(--targetPtr) = *(--sourcePtr);
		}

    /*  Set the main page table bit map extension pages to vacant. */
    
	targetPtr = &pageTable->PageBitMaps[newBitMapByteLen];
	stopPtr = &pageTable->PageBitMaps[oldBitMapByteLen];
	while (targetPtr > stopPtr)
		{
		*(--targetPtr) = 0;
		}

    /*  Fix up the resize page table. */
    
    strcpy(pageTable->AisIdentifier,"AIS");
    sprintf(pageTable->VersionIdentifier,"%d",_VERSION);
    pageTable->recordType       = _PAGERECORDTYPE;
    pageTable->recordVersion    = _VERSION;
    pageTable->recordLength     = newPageTableSize - sizeof(DBRECORDHEADER);
    pageTable->SaveTimeStamp    = FDateFnc_now(gCP,gTP,0,NIL).u.Real;
    pageTable->PageTableSize    = newPageTableSize;
    pageTable->MaxPageCount     = newMaxPageCount;
    pageTable->IAmDirty         = TRUE;
    pageTable->PageMapsByteLen	= newMaxPageCount / 4;
    pageTable->PageBitMapByteLen = newMaxPageCount / 8;

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

    /*  Now we are in a completely different state with a new */
    /*  resized page table bit map. Therefore we call ourselves */ 
	/*  recursively to allocoate the originally requested page */
	/*	and then we return. */

    *err = FDatabas_NewFrameID(gCP,gTP,argc,argv);
    FrameExit(*err);
    }


#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/*  Return the new frame bit map index. */

newFrameID = FDatabas_MakeFrameID(gCP, gTP,newFrameSize,newFrameIndex);
FrameExit(TFRAME(newFrameID));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_AdjustPageTable

The FDatabas_AdjustPageTable Procedure adjusts the page table so that there
there are the exact number of bit map pages needed for the specified Database 
file size. If there is no error, true will be returned.

The arguments are as follows.

fileVector	The database file vector whose page table is to be adjusted.

newFileSize	The new file size for the adjustment.

returns:	An error or true.

#endif

TVAL FDatabas_AdjustPageTable(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileVector,NUM newFileSize)     
{
NUM				newMaxPageCount;
NUM             extraFrameSize;
PAGETABLE*		pageTable;
TVAL            freeCmd = TFUNCTION(FDatabas_FreeFrameID);
TVAL            newCmd = TFUNCTION(FDatabas_NewFrameID);
StartFrame
DeclareTVAL(err);
DeclareTVAL(fileID);
DeclareTVAL(newFrame);
DeclareTVAL(fileVEC);
EndFrame
 
/*  Initialize the Database root and page table, etc. */

*fileVEC = fileVector;
*fileID = ByteVector(*fileVEC)->itsCdr;
pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;
   
/*  Make sure the new file size is an eight page boundary. */

newFileSize = FILESIZEFROMFILELEN(newFileSize);
newMaxPageCount = MAXPAGESFROMSIZE(newFileSize - sizeof(FILEROOT));
 
/*  Do we have an exact match and require no adjustment, */
/*  or, is the existing page bit map larger than required? */

if (newMaxPageCount <= pageTable->MaxPageCount)
	{
	FrameExit(gCP->Tval_TRUE);
	}
    
/*  Do we add to the end of the Database file? */
/*  Note:	We must extend the page table bit maps */
/*			to include the new pages required. */

if (newMaxPageCount >= pageTable->MaxPageCount)
    {
    /*  Compute the extra pages required for the new file size. */
    /*  Note:   We must make sure the extra pages end on an		*/
    /*          8 page boundary so the page bit map will be		*/
    /*          an integral of bytes in length.					*/

	extraFrameSize = (newMaxPageCount - pageTable->MaxPageCount) * _PAGESIZE;
	
    /*  Allocate a frame of this extra size to extend the page table.	*/
    /*  Note:   Allocating and freeing a frame will force all normal	*/
    /*          page table bit map growing algorithms to perform as		*/
    /*          they normally would.									*/
	
	*newFrame = FSmartbase_Eval(gCP,gTP,newCmd,2,fileVector,TINT(extraFrameSize));
	ExitOnError(*newFrame);
	*err = FSmartbase_Eval(gCP,gTP,freeCmd,2,fileVector,*newFrame);
	ExitOnError(*err);
	FrameExit(gCP->Tval_TRUE);

    }

/*  If we get here, something has gone wrong. */

FrameExit(gCP->Tval_FALSE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_FreeFrameID

The FDatabas_FreeFrameID Procedure frees the specified disk frame in the 
specified Database file. The specified disk frame will be returned to the 
Database files page table. If there is no error, true will be returned; 
otherwise, an error will be returned.

The arguments are as follows.

odbID   The database file object returned by a previous database-open procedure.

frameID The disk frame (obtained from a previous FDatabas_NewFrameID call).

Note:   The new frame ID is a real number which contains the page count and the
		starting page index of the frame.
     
#endif

TVAL FDatabas_FreeFrameID(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
REAL			frameID;
NUM             frameIndex;
NUM             frameCount;
PAGETABLE*      pageTable;
StartFrame
DeclareTVAL(fileID);
DeclareTVAL(fileVEC);
EndFrame
 
/*  Make sure the arguments are correct */

if ((argc != 2) || (argv[0].Tag != TYBYTEVECTOR) || (!isRealIndex(&argv[1])))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Initialize the Database root and page table, etc. */

*fileVEC = argv[0];
*fileID = ByteVector(*fileVEC)->itsCdr;
pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;

/*  We connot free a nipotent page index */

frameID = argv[1].u.Real;
if (frameID == NILPAGEINDEX)
	{
	FrameExit(TBOOL(TRUE));
	}

/*  Isolate the frameID and the frameSize from the page index. */

frameCount = FDatabas_FrameIDToBCount(gCP,gTP,frameID);
frameIndex = FDatabas_FrameIDToBIndex(gCP,gTP,frameID);

/*  Free the specified repository frame. */

FDatabas_FreeFrame(gCP,gTP,pageTable, frameIndex, frameCount);
FrameExit(TFRAME(NILPAGEINDEX));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_LoadIndexStretegy

The  FDatabas_LoadIndexStretegy  Procedure object loads the index stretegy for the specified 
Database file (see the loadObject procedure). 

The arguments are as follows.

self		The database repository.

#endif

TVAL FDatabas_LoadIndexStrategy(LpXCONTEXT gCP,LpTHREAD gTP,TDatabase* self)       
{
PAGETABLE*      pageTable;
StartFrame
DeclareTVAL(odbID);
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);
EndFrame
 
if (self->itsOdbID == NIL)
	{
	prmv[0] = TINT(self->itsBaseFilePos);
	prmv[1] = TOBJ(self->itsFileName);
	prmv[2] = TSYMBOL("update");
	*odbID = FDatabas_Open(gCP,gTP,3,prmv);
	ExitOnError(*odbID);
	if (odbID->Tag != TYBYTEVECTOR) {FrameExit(gCP->TObject_ERROR_INVALID);} else {self->itsOdbID = odbID->u.ByteVector;}
	pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
	self->itsTreeOn = pageTable->TreeIndexOn;
	prmv[0] = self->itsOdbID->itsCdr;
	prmv[1] = TINT(1);
	*ret = FConio_fclose(gCP,gTP,2,prmv);
	self->itsOdbID = NIL;
	ExitOnError(*ret);
	}
else
	{
	pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
	self->itsTreeOn = pageTable->TreeIndexOn;
	}

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_LoadIndex

The  FDatabas_LoadIndex  Procedure object loads the index object for the specified 
Database file (see the loadObject procedure). If there is no error, the index object 
for the specified Database file will be returned; otherwise, an error will be returned.

The arguments are as follows.

self		The database repository.
itsCodeKey  The encryption key. If present and true, automatic record decompression only is turned on. 
				If not present or false, automatic record decryption/decompression is turned off. If present 
				and a Number, automatic record decryption is turned on. Note: The Database always 
				uses the  decode  and  uncompress  procedure; therefore, the user may override these with 
				his own procedures if desired (optional).
#endif

TVAL FDatabas_LoadIndex(LpXCONTEXT gCP,LpTHREAD gTP,TDatabase* self,TVAL itsCodeKey)       
{
REAL            frameID;
NUM             frameIndex;
NUM             frameSize;
NUM             frameCount;
NUM             framePos;
PAGETABLE*      pageTable;
StartFrame
DeclareTVAL(fileID);
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);
EndFrame
 
/*  Manage the case where decryption is requested. */

if (isNumIndex(&itsCodeKey))
	 {
	/*  Read the index object frame stored in the Database page table. */
	*fileID = self->itsOdbID->itsCdr;
	pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
	frameID =		pageTable->IndexObjectFrame;
	frameCount =	FDatabas_FrameIDToBCount(gCP,gTP,frameID);
	frameSize =		FDatabas_FrameIDToFSize(gCP,gTP,frameID);
	framePos =		FDatabas_FrameIDToFPos(gCP,gTP,frameID);
	frameIndex =	FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
	if ((frameIndex < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
		{
		self->itsIndex = TDirectory_New(gCP,gTP);
		FrameExit(TOBJ(self->itsIndex));
		}
	*ret = FDatabas_readR(gCP,gTP,*fileID,pageTable->baseFilePos,framePos,frameSize,FALSE);
	ExitOnError(*ret);
	if (ret->Tag != TYDIRECTORY)
		{
		self->itsIndex = TDirectory_New(gCP,gTP);
		FrameExit(TOBJ(self->itsIndex));
		}
	 ExitOnError(*ret);
	 prmv[0] = *ret;
	 prmv[1] = itsCodeKey;
	 *ret = FDatabas_Decode(gCP,gTP,2,&prmv[0]);
	 ExitOnError(*ret);
	 *ret = FConio_loadObject(gCP,gTP,1,ret);
	 if (ret->Tag != TYDIRECTORY)
		{
		self->itsIndex = TDirectory_New(gCP,gTP);
		FrameExit(TOBJ(self->itsIndex));
		}
	 self->itsIndex = ret->u.Directory;
	 FrameExit(*ret);
	 }

/*  Manage the case where uncompression is requested. */

if ((itsCodeKey.Tag == TYBOLE) && (itsCodeKey.u.Bool == TRUE))
	 {
	 /*  Read the index object frame stored in the Database page table. */
	 *fileID = self->itsOdbID->itsCdr;
	 pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
	 frameID =		pageTable->IndexObjectFrame;
	 frameCount =	FDatabas_FrameIDToBCount(gCP,gTP,frameID);
	 frameSize =		FDatabas_FrameIDToFSize(gCP,gTP,frameID);
	 framePos =		FDatabas_FrameIDToFPos(gCP,gTP,frameID);
	 frameIndex =	FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
	 if ((frameIndex < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
		{
		self->itsIndex = TDirectory_New(gCP,gTP);
		FrameExit(TOBJ(self->itsIndex));
		}
	 *ret = FDatabas_readR(gCP,gTP,*fileID,pageTable->baseFilePos,framePos,frameSize,FALSE);
	 ExitOnError(*ret);
	 *ret = FDatabas_Uncompress(gCP,gTP,1,ret);
	 ExitOnError(*ret);
	 *ret = FConio_loadObject(gCP,gTP,1,ret);
	 ExitOnError(*ret);
	 if (ret->Tag != TYDIRECTORY)
		{
		self->itsIndex = TDirectory_New(gCP,gTP);
		FrameExit(TOBJ(self->itsIndex));
		}
	 self->itsIndex = ret->u.Directory;
	 FrameExit(*ret);
	 }

/*  Read the index object frame stored in the Database page table. */

*fileID = self->itsOdbID->itsCdr;
pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
frameID =		pageTable->IndexObjectFrame;
frameCount =	FDatabas_FrameIDToBCount(gCP,gTP,frameID);
frameSize =		FDatabas_FrameIDToFSize(gCP,gTP,frameID);
framePos =		FDatabas_FrameIDToFPos(gCP,gTP,frameID);
frameIndex =	FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
if ((frameIndex < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
	{
	self->itsIndex = TDirectory_New(gCP,gTP);
    FrameExit(TOBJ(self->itsIndex));
	}
*ret = FDatabas_readR(gCP,gTP,*fileID,pageTable->baseFilePos,framePos,frameSize,FALSE);
ExitOnError(*ret);
if (ret->Tag != TYDIRECTORY)
	{
	self->itsIndex = TDirectory_New(gCP,gTP);
	FrameExit(TOBJ(self->itsIndex));
	}
self->itsIndex = ret->u.Directory;   
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_SaveIndex

The  FDatabas_SaveIndex  Procedure saves the specified object as the index of the 
specified Database file (see the saveObject procedure). Only values which respond 
to the isObject predicate can be saved. If there is no error, the page identifier, 
where the object was saved, will be returned; otherwise, an error will be returned.

The arguments are as follows.

myOdbID		The database page table vector of the object repository.

newValue	The object to be saved (only values which respond to the isObject predicate 
				can be saved) as the index of the Database file.

itsCodeKey  The encryption key. If present and true, automatic record compression only is turned on. 
				If not present or false, automatic record encryption/compression is turned off. If present 
				and a Number, automatic record encryption is turned on. Note: The Database always 
				uses the  compress  and  encode  procedure; therefore, the user may override these with 
				his own procedures if desired (optional).

Note:   Saves the object as the index of the specified Database file.

#endif

TVAL FDatabas_SaveIndex(LpXCONTEXT gCP,LpTHREAD gTP,TByteVector* myOdbID,TVAL newValue,TVAL itsCodeKey)       
{
REAL            frameID;
NUM             framePos;
PAGETABLE*      pageTable;
StartFrame
DeclareTVAL(fileID);
DeclareTVAL(saveMe);
DeclareTVAL(spaceNeeded);
DeclareTVAL(newPage);
DeclareTVAL(ret);
DeclareTVAL(err);
DeclareTVALArray(prmv,4);
EndFrame
 
/*  Manage the case where encryption is requested. */

if (isNumIndex(&itsCodeKey))
 {
 *saveMe = FConio_saveObject(gCP,gTP,1,&newValue);
 ExitOnError(*saveMe);
 prmv[0] = *saveMe;
 prmv[1] = itsCodeKey;
 *saveMe = FDatabas_Encode(gCP,gTP,2,&prmv[0]);
 ExitOnError(*saveMe);
 *ret = FDatabas_SaveIndex(gCP,gTP,myOdbID,*saveMe,gCP->TObject_VOID);
 FrameExit(*ret);
 }

/*  Manage the case where compression is requested. */

if ((itsCodeKey.Tag == TYBOLE) && (itsCodeKey.u.Bool == TRUE))
 {
 *saveMe = FConio_saveObject(gCP,gTP,1,&newValue);
 ExitOnError(*saveMe);
 *saveMe = FDatabas_Compress(gCP,gTP,1,saveMe);
 ExitOnError(*saveMe);
 *ret = FDatabas_SaveIndex(gCP,gTP,myOdbID,*saveMe,gCP->TObject_VOID);
 FrameExit(*ret);
 }

/*  Initialize the Database root and page table. */

*saveMe = newValue;
*fileID = myOdbID->itsCdr;
pageTable = (PAGETABLE*)*myOdbID->itsByteArray;
if (pageTable->ReadWriteOn == FALSE)
    {
	prmv[0] = TOBJ(myOdbID);
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!databaseSaveIndex: Invalid Write Permission on odbID: %a",&prmv[0]);
	FrameExit(*ret);
    }

/*  Compute the space required for the saved object. */

*spaceNeeded = TDatabase_SizeOf(gCP,gTP,1,saveMe);
spaceNeeded->u.Int += sizeof(OBRECORDHEADER);

/*  Free the old index page and find a Database frame which  */
/*  is a good fit for the space required for the saved object. */

frameID = pageTable->IndexObjectFrame;
prmv[0] = TOBJ(myOdbID);
prmv[1] = TFRAME(frameID);
*err = FDatabas_FreeFrameID(gCP,gTP,2,&prmv[0]);
ExitOnError(*err);
prmv[0] = TOBJ(myOdbID);
prmv[1] = *spaceNeeded;
*newPage = FDatabas_NewFrameID(gCP,gTP,2,&prmv[0]);
ExitOnError(*newPage);
pageTable = (PAGETABLE*)*myOdbID->itsByteArray;
pageTable->IndexObjectFrame = frameID = newPage->u.Real;
framePos = FDatabas_FrameIDToFPos(gCP,gTP,frameID);
    
/*  Save the object into the Database page. */

*err = FDatabas_writeR(gCP,gTP,*fileID,pageTable->baseFilePos,framePos,*saveMe,FALSE);
ExitOnError(*err);
    
#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
		FSmartbase_Eval(gCP,gTP,TGVALUE("inspect"),3,argv[0],TFRAME(pageTable->IndexObjectFrame),TSYMBOL("check"));
#endif				/* Database validity check */

/*  Return the new frame bit map index. */

FrameExit(TFRAME(frameID));
}

    
/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_LoadMetaData

The  FDatabas_LoadMetaData  Procedure object loads the MetaData object for the specified 
Database file (see the loadObject procedure). If there is no error, the MetaData object 
for the specified Database file will be returned; otherwise, an error will be returned.

The arguments are as follows.

repository   The database file object returned by a previous databaseOpen procedure.

#endif

TVAL FDatabas_LoadMetaData(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
TVAL            databaseOpen = TFUNCTION(FDatabas_Open);
TVAL            databaseClose = TFUNCTION(FDatabas_Close);
TVAL            databaseLoad = TFUNCTION(FDatabas_Load);
TVAL			selfTval;
PAGETABLE*      pageTable;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareOBJ(TDatabase,insertRepo);
DeclareTVAL(savePath);
DeclareTVAL(theItem);
DeclareTVAL(theDiskPage);
DeclareTVAL(ec);
DeclareTVAL(tmp);
EndFrame


/*  Check if there is one argument which is an object repository. */

if ((argc != 1) || (argv[0].Tag != TYOBJREPOSITORY))
    {
	*ec = TERROR("!loadMetaData: expecting an object repository as the single argument!");
    FrameExit(*ec);
    }
	
/*  Retrieve the database archive index (if necessary). */

selfTval = argv[0];
self = selfTval.u.Repository;
if (self->itsOdbID == NIL)
    {
   /* Open the database archive disk file. */

    *savePath = gCP->TSymbol_Path->itsGlobalValue;
    gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
    gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
    *tmp = TSYMBOL("update");
    *ec = FSmartbase_Eval(gCP,gTP,databaseOpen,3,TINT(self->itsBaseFilePos),TOBJ(self->itsFileName),*tmp);
    gCP->TSymbol_Path->itsGlobalValue = *savePath;
    ExitOnError(*ec);
	if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;}
    *ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
    ExitOnError(*ec);

	/*  Read the database disk page number from the ObjectRepository meta data frame. */

	pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
	theDiskPage->u.Real = pageTable->MetaDataObjectFrame;
	theDiskPage->Tag = TYFRAME;
    *theItem = FSmartbase_Eval(gCP,gTP,databaseLoad,3,TOBJ(self->itsOdbID),TFRAME(theDiskPage->u.Real),self->itsCodeKey);
    ExitOnError(*theItem);

    /* Close the database archive disk file. */

    *tmp = TSYMBOL("refuse");
    FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
    self->itsOdbID = NIL;
    self->itsIndex = NIL;
    self->itsTransactionOn = FALSE;
    }
else
    {
	/*  Read the database disk page number from the ObjectRepository meta data frame. */

	pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
	theDiskPage->u.Real = pageTable->MetaDataObjectFrame;
	theDiskPage->Tag = TYFRAME;
    *theItem = FSmartbase_Eval(gCP,gTP,databaseLoad,3,TOBJ(self->itsOdbID),TFRAME(theDiskPage->u.Real),self->itsCodeKey);
    ExitOnError(*theItem);
    }

FrameExit(*theItem);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_SaveMetaData

The  FDatabas_SaveMetaData  Procedure saves the specified object as the MetaData of the 
specified Database file (see the saveObject procedure). Only values which respond 
to the isObject predicate can be saved. If there is no error, the page identifier, 
where the object was saved, will be returned; otherwise, an error will be returned.

The arguments are as follows.

repository   The database file object returned by a previous databaseOpen procedure.

object		 The object to be saved (only values which respond to the isObject predicate 
				can be saved) as the MetaData of the Database file.

Note:   Saves the object as the meta data of the specified Database file.
                
#endif

TVAL FDatabas_SaveMetaData(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
TVAL			databaseOpen = TFUNCTION(FDatabas_Open);
TVAL			databaseClose = TFUNCTION(FDatabas_Close);
TVAL			databaseSave = TFUNCTION(FDatabas_Save);
TVAL			databaseFree = TFUNCTION(FDatabas_Free);
TVAL			selfTval;
TVAL			newValue;
PAGETABLE*      pageTable;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theDiskPage);
DeclareTVAL(savePath);
DeclareTVAL(ec);
DeclareTVAL(tmp);
EndFrame


/*  Check if there is one argument which is an object repository. */

if ((argc != 2) || (argv[0].Tag != TYOBJREPOSITORY) || ((argv[1].Tag != TYVOID) && (_TObject_TypeFlag(argv[1].Tag) != _TObject_TfTOBJECT)))
    {
	*ec = TERROR("!saveMetaData: expecting an object repository and an object data type as the two arguments!");
    FrameExit(*ec);
    }
	
/*  Retrieve the database archive index (if necessary). */

selfTval = argv[0];
self = selfTval.u.Repository;
newValue = argv[1];

/*  Retrieve the database archive index (if necessary). */

if (self->itsOdbID == NIL)
	{
	/* Open the database archive disk file. */
 
	*savePath = gCP->TSymbol_Path->itsGlobalValue;
	gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
	gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	*tmp = TSYMBOL("update");
	*ec = FSmartbase_Eval(gCP,gTP,databaseOpen,3,TINT(self->itsBaseFilePos),TOBJ(self->itsFileName),*tmp);
	gCP->TSymbol_Path->itsGlobalValue = *savePath;
	ExitOnError(*ec);
	if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
	*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
	ExitOnError(*ec);

	/*  Read the database disk page number from the ObjectRepository meta data frame. */

	pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
	theDiskPage->u.Real = pageTable->MetaDataObjectFrame;
	if (theDiskPage->u.Real == NILPAGEINDEX) theDiskPage->Tag = TYVOID; else theDiskPage->Tag = TYFRAME;

	/* Save the new value in the database archive disk file associated with the key. */

	if (newValue.Tag != TYVOID)
		{
		if (theDiskPage->Tag == TYFRAME)
			{
			*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,TFRAME(theDiskPage->u.Real),self->itsCodeKey);
			if (isERROR(theDiskPage))
				{
				TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
				FrameExit(*theDiskPage);
				}
			pageTable->MetaDataObjectFrame = theDiskPage->u.Real;
			}
		else
			{
			*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,gCP->Tval_VOID,self->itsCodeKey);
			if (isERROR(theDiskPage))
				{
				TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
				FrameExit(*theDiskPage);
				}
			pageTable->MetaDataObjectFrame = theDiskPage->u.Real;
			}
		}
	else
		/* Erase the new value from the database archive disk file removing the key. */

		{
		if (theDiskPage->Tag == TYFRAME)
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TFRAME(theDiskPage->u.Real));
			ExitOnError(*ec);
			pageTable->MetaDataObjectFrame = NILPAGEINDEX;
			}
		}
    pageTable->IAmDirty = TRUE;

	/* Close the database archive disk file. */
 
	*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
	if (isERROR(ec))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
		FrameExit(*ec);
		}
	*tmp = TSYMBOL("commit");
	*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
	ExitOnError(*ec);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
	}
else
	{
	/*  Read the database disk page number from the ObjectRepository meta data frame. */

	pageTable = (PAGETABLE*)*self->itsOdbID->itsByteArray;
	theDiskPage->u.Real = pageTable->MetaDataObjectFrame;
	if (theDiskPage->u.Real == NILPAGEINDEX) theDiskPage->Tag = TYVOID; else theDiskPage->Tag = TYFRAME;

	/* Save the new value in the database archive disk file associated with the key. */

	if (newValue.Tag != TYVOID)
		{
		if (theDiskPage->Tag == TYFRAME)
			{
			*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,TFRAME(theDiskPage->u.Real),self->itsCodeKey);
			if (isERROR(theDiskPage))
				{
				TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
				FrameExit(*theDiskPage);
				}
			pageTable->MetaDataObjectFrame = theDiskPage->u.Real;
			}
		else
			{
			*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,gCP->Tval_VOID,self->itsCodeKey);
			if (isERROR(theDiskPage))
				{
				TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
				FrameExit(*theDiskPage);
				}
			pageTable->MetaDataObjectFrame = theDiskPage->u.Real;
			}
		}
	else
		/* Erase the new value from the database archive disk file removing the key. */

		{
		if (theDiskPage->Tag == TYFRAME)
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TFRAME(theDiskPage->u.Real));
			ExitOnError(*ec);
			pageTable->MetaDataObjectFrame = NILPAGEINDEX;
			}
		}
 
	}

FrameExit(selfTval);
}

    
/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Load

The  FDatabas_Load  Procedure loads the contents of the specified object
index from the specified Database file (see the loadObject procedure). 
If there is no error, the object stored in the specified Database file 
(object index) will be returned; otherwise, an error will be returned.

The arguments are as follows.

odbID		The database file object returned by a previous databaseOpen procedure.

frameID		The frameID whose contents are to be object loaded (see FDatabas_NewFrameID).

encrypt		The encryption key. If present and true, automatic record decompression only is turned on. 
			If not present or false, automatic record decryption/decompression is turned off. If present 
			and a Number, automatic record decryption is turned on. Note: The Database always 
			uses the  decode  and  uncompress  procedure; therefore, the user may override these with 
			his own procedures if desired (optional).


Note:   Loads the object saved in the specified Database object id.
                
#endif

TVAL FDatabas_Load(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
REAL            frameID;
NUM             frameIndex;
NUM             frameCount;
NUM             frameSize;
NUM             framePos;
PAGETABLE*      pageTable;
TVAL            loadCmd = TFUNCTION(FConio_loadObject);
StartFrame
DeclareTVAL(fileID);
DeclareTVAL(fileVEC);
DeclareTVAL(loadMe);
DeclareTVAL(ret);
EndFrame
 
/*  Make sure the arguments are correct */

if ((argc < 2) || (argv[0].Tag != TYBYTEVECTOR) || (!isRealIndex(&argv[1])))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Manage the case where decryption is requested. */

if ((argc == 3) && (isNumIndex(&argv[2])))
    {
    *loadMe = FDatabas_Load(gCP,gTP,2,argv);
    ExitOnError(*loadMe);
    *loadMe = FSmartbase_Eval(gCP,gTP,*gCP->FDatabas_pDecode,2,*loadMe,argv[2]);
    ExitOnError(*loadMe);
	*ret = FSmartbase_Eval(gCP,gTP,loadCmd,1,*loadMe);
    FrameExit(*ret);
    }

/*  Manage the case where uncompression is requested. */

if ((argc == 3) && (argv[2].Tag == TYBOLE) && (argv[2].u.Bool == TRUE))
    {
    *loadMe = FDatabas_Load(gCP,gTP,2,argv);
    ExitOnError(*loadMe);
    *loadMe = FSmartbase_Eval(gCP,gTP,*gCP->FDatabas_pUncompress,1,*loadMe);
    ExitOnError(*loadMe);
	*ret = FSmartbase_Eval(gCP,gTP,loadCmd,1,*loadMe);
    FrameExit(*ret);
    }

/*  Initialize the Database root and page table. */

*fileVEC = argv[0];
*fileID = ByteVector(*fileVEC)->itsCdr;
pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;
frameID = argv[1].u.Real;

/*  Validate the database frame */

frameSize	=	FDatabas_FrameIDToFSize(gCP,gTP,frameID);
frameCount	=	FDatabas_FrameIDToBCount(gCP,gTP,frameID);
frameIndex	=	FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
framePos	=	FDatabas_FrameIDToFPos(gCP,gTP,frameID);
if ((frameIndex < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
	{
    FrameExit(gCP->Tval_VOID);
	}

/*  Load the object from the Database frame. */

*loadMe = FDatabas_readR(gCP,gTP,*fileID,pageTable->baseFilePos,framePos,frameSize,FALSE);
ExitOnError(*loadMe);	
    
FrameExit(*loadMe);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Save

The  FDatabas_Save  Procedure saves the specified object into the specified 
Database file (see the saveObject procedure). Only values which respond to 
the isObject predicate can be saved. If the optional object index (objID) 
is present, the databaseSave procedure will overwrite the object previously 
saved at that index. If there is no error, the object index (objID) where 
the object was saved, will be returned; otherwise, an error will be returned. 

The arguments are as follows.

odbID		The database file object returned by a previous databaseOpen procedure.

object		The object to be saved (only values which respond to the isObject predicate can be saved).

frameID		The frameID whose contents are to be object loaded, see FDatabas_NewFrameID (gCP,gTP,optional).

encrypt		The encryption key. If present and true, automatic record compression only is turned on. 
			If not present or false, automatic record encryption/compression is turned off. If present 
			and a Number, automatic record encryption is turned on. Note: The Database always 
			uses the  compress  and  encode  procedure; therefore, the user may override these with 
			his own procedures if desired (optional).

Note:   Saves the object in the specified Database file indexed by the 
        value of frameID.
                
#endif

TVAL FDatabas_Save(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
REAL            frameID;
NUM             framePos;
PAGETABLE*      pageTable;
TVAL            saveCmd = TFUNCTION(FConio_saveObject);
TVAL            freeCmd = TFUNCTION(FDatabas_FreeFrameID);
TVAL            newCmd = TFUNCTION(FDatabas_NewFrameID);
TVAL            sizeofCmd = TFUNCTION(TDatabase_SizeOf);
StartFrame
DeclareTVAL(fileID);
DeclareTVAL(fileVEC);
DeclareTVAL(saveMe);
DeclareTVAL(spaceNeeded);
DeclareTVAL(newPage);
DeclareTVAL(ret);
DeclareTVAL(err);	
EndFrame
 
/*  Make sure the basic arguments are correct */

if ((argc < 2)  || (argc > 4)  || (argv[0].Tag != TYBYTEVECTOR))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
else
if ((argc >= 3) && (isRealIndex(&argv[2])))
    frameID = argv[2].u.Real;
else
    frameID = NILPAGEINDEX;

/*  Manage the case where encryption is requested. */

if ((argc == 4) && (isNumIndex(&argv[3])))
    {
    *saveMe = FSmartbase_Eval(gCP,gTP,saveCmd,1,argv[1]);
    ExitOnError(*saveMe);
    *saveMe = FSmartbase_Eval(gCP,gTP,*gCP->FDatabas_pEncode,2,*saveMe,argv[3]);
    ExitOnError(*saveMe);
    *ret = FSmartbase_Eval(gCP,gTP,TFUNCTION(FDatabas_Save),3,argv[0],*saveMe,argv[2]);
	FrameExit(*ret)
    }

/*  Manage the case where compression is requested. */

if ((argc == 4) && (argv[3].Tag == TYBOLE) && (argv[3].u.Bool == TRUE))
    {
    *saveMe = FSmartbase_Eval(gCP,gTP,saveCmd,1,argv[1]);
    ExitOnError(*saveMe);
    *saveMe = FSmartbase_Eval(gCP,gTP,*gCP->FDatabas_pCompress,1,*saveMe);
    ExitOnError(*saveMe);
	*ret = FSmartbase_Eval(gCP,gTP,TFUNCTION(FDatabas_Save),3,argv[0],*saveMe,argv[2]); 
    FrameExit(*ret);
    }

/*  Initialize the Database root and page table. */

*fileVEC = argv[0];
*fileID = ByteVector(*fileVEC)->itsCdr;
*saveMe = argv[1];
pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;
if (pageTable->ReadWriteOn == FALSE)
    {
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!databaseSave: Invalid Write Permission on odbID: %a",&argv[0]);
	FrameExit(*ret);
    }

/*  Compute the space required for the saved object. */

*spaceNeeded = FSmartbase_Eval(gCP,gTP,sizeofCmd,1,*saveMe);

/*  Free the old object page and find a Database frame which  */
/*  is a good fit for the space required for the saved object. */

FSmartbase_Eval(gCP,gTP,freeCmd,2,argv[0],TFRAME(frameID));
*newPage = FSmartbase_Eval(gCP,gTP,newCmd,2,argv[0],*spaceNeeded);
ExitOnError(*newPage);
pageTable	= (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;
frameID		= newPage->u.Real;
framePos	= FDatabas_FrameIDToFPos(gCP,gTP,frameID);
    
/*  Save the object into the Database page. */

*err = FDatabas_writeR(gCP,gTP,*fileID,pageTable->baseFilePos,framePos,*saveMe,FALSE);
ExitOnError(*err);
       
#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
		FSmartbase_Eval(gCP,gTP,TGVALUE("inspect"),3,argv[0],TFRAME(pageTable->IndexObjectFrame),TSYMBOL("check"));
#endif				/* Database validity check */

/*  Return the object index associated with the saved object. */

FrameExit(TFRAME(frameID));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Free

The  FDatabas_Free  Procedure frees the specified object index in the 
specified Database file. If there is no error, true will be returned; 
otherwise, an error will be returned.

The arguments are as follows.

odbID		The database file object returned by a previous databaseOpen procedure.

frameID		The frameID whose contents are to be object loaded, see FDatabas_NewFrameID.

#endif

TVAL FDatabas_Free(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
REAL            frameID;
PAGETABLE*      pageTable;
TVAL            freeCmd = TFUNCTION(FDatabas_FreeFrameID);
StartFrame
DeclareTVAL(ret);
DeclareTVAL(fileID);
DeclareTVAL(fileVEC);
EndFrame
 
/*  Make sure the arguments are correct */

if ((argc != 2) || (argv[0].Tag != TYBYTEVECTOR) || (!isRealIndex(&argv[1])))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Initialize the Database root and page table, etc. */

*fileVEC = argv[0];
*fileID = ByteVector(*fileVEC)->itsCdr;
pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;
if (pageTable->ReadWriteOn == FALSE)
    {
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!databaseFree: Invalid Write Permission on odbID: %a",&argv[0]);
	FrameExit(*ret);
    }

/* Free the Database disk object id, then free its Database page. */

frameID = argv[1].u.Real;
*ret = FSmartbase_Eval(gCP,gTP,freeCmd,2,argv[0],TFRAME(frameID));
FrameExit(*ret);
}

    
/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Encode

The  encode  Procedure calls the compress procedure, to convert a Byte Vector, String, 
or Symbol into a much smaller Byte Vector. Then the encode procedure uses the specified 
key to encode the compressed byte vector. The same key must also be passed to the decode 
procedure to reconstruct the data.

The arguments are as follows.

input   The Byte Vector, String, or Symbol to be compressed and then encoded.

key     The Number key to use for encoding (must also be used for later decoding).

output  The much smaller Byte Vector after compression.

For example
    
    (setq secret (encode  "Shall we make this string a secret?"  193830574818473))
    
    (setq output  (decode  secret  193830574818473))
    
    (= output "Shall we make this string a secret?")    =>  true


#endif

TVAL FDatabas_Encode(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
REAL            key;
LpCHAR          pKey;
NUM             KeyLen;
NUM             i;
NUM             j;
LpCHAR          pOut;
NUM             OutLen;
StartFrame
DeclareTVAL(out);
DeclareTVAL(compressCmd);
EndFrame
 

/*  Make sure the input arguments are correct. */

if (argc != 2) 
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
else
if (isRealIndex(&argv[1]))
    {
    key = argv[1].u.Real;
    }
else
if (argv[1].Tag == TYNUM)
    {
    key = argv[1].u.Int;
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Miscellaneous Initialization. */
*compressCmd = TGVALUE("compress");


/*  Compress the input into an output byte vector. */
*out = FSmartbase_Eval(gCP,gTP,*compressCmd,1,argv[0]);
ExitOnError(*out);
pOut = (LpCHAR)ByteArray(*out);
OutLen = ByteVector(*out)->itsMaxItemIndex;
pKey = (LpCHAR)&key;
KeyLen = sizeof(REAL);

/*  Encode by xoring the key with the compressed data bytes. */

for (i = 0, j = 0; i < OutLen; ++i)
    {
    pOut[i] ^= pKey[j];
    if ((j = ((j+1) % KeyLen)) == 0)
        {
        key += 1.0/3.0;
        }
    }

/*  Return the compressed/encoded Byte Vector. */

FrameExit(*out);
}

    
/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Decode

The  decode  Procedure uses the specified key to decode the encoded byte vector. Then 
decode calls the uncompress procedure, to convert a Byte Vector into the original Byte 
Vector before decoding. The same key was also be passed to the encode procedure to 
encode the original data. 


The arguments are as follows.

input   The Byte Vector to be decoded and then uncompressed.

key     The Number key to use for decoding (must also have be used for encoding).

output  The original Byte Vector before encoding.

For example
    
    (setq secret (encode  "Shall we make this string a secret?"  193830574818473))
    
    (setq output  (decode  secret  193830574818473))
    
    (= output "Shall we make this string a secret?")    =>  true


#endif

TVAL FDatabas_Decode(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
REAL            key;
LpCHAR          pKey;
NUM             KeyLen;
NUM             i;
NUM             j;
LpCHAR          pOut;
NUM             OutLen;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(out);
DeclareTVAL(uncompressCmd);
EndFrame



/*  Miscellaneous Initialization. */
*uncompressCmd = TGVALUE("uncompress");
 
/*  Make sure the input arguments are correct. */

if (argc != 2) 
    {
    *ret = TERROR("!decode: Invalid number of arguments!");
	FrameExit(*ret);
    }
else
if (argv[0].Tag != TYBYTEVECTOR) 
    {
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,
		                     "!decode: Expecting a Byte Vector for argument 1, received = %a!",
							 &argv[0]);
 	FrameExit(*ret);
   }
else
if (isRealIndex(&argv[1]))
    {
    key = argv[1].u.Real;
    }
else
if (argv[1].Tag == TYNUM)
    {
    key = argv[1].u.Int;
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Decode by xoring the key with the compressed data bytes. */

pOut = (LpCHAR)ByteArray(argv[0]);
OutLen = ByteVector(argv[0])->itsMaxItemIndex;
pKey = (LpCHAR)&key;
KeyLen = sizeof(REAL);
for (i = 0, j = 0; i < OutLen; ++i)
    {
    pOut[i] ^= pKey[j];
    if ((j = ((j+1) % KeyLen)) == 0)
        {
        key += 1.0/3.0;
        }
    }

/*  Uncompress the input into an output byte vector. */

*out = FSmartbase_Eval(gCP,gTP,*uncompressCmd,1,argv[0]);
ExitOnError(*out);

/*  Return the compressed/encoded Byte Vector. */

FrameExit(*out);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Compress

The  compress  Procedure uses the extremely fast LZRW1 compression algorithm, 
by Ross Williams, to convert a Byte Vector, String, or Symbol into a much
smaller Byte Vector.

Note:   We include the original length as the first sizeof(NUM) bytes
        of the smaller compressed vector, so we can know how much space
        to allocate on decompression.

The arguments are as follows.

input   The Byte Vector, String, or Symbol to be compressed.

output  The much smaller Byte Vector after compression.

For example

    (setq smaller (compress  "Shall we make this string smaller?"))

    (setq output  (uncompress  smaller))

    (= output "Shall we make this string smaller?") =>  true

#endif

TVAL FDatabas_Compress(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
#define         FUDGE       300
LpCHAR          pIn;
NUM             InLen;
LpCHAR          pOut;
NUM             OutLen;
TVAL            makeCmd = TFUNCTION(FMake_Vector);
TVAL            resizeCmd = TFUNCTION(FMake_Resize);
StartFrame
DeclareTVAL(out);
DeclareTVAL(tmp);
EndFrame

/*  Make sure the input argument is correct. */

if (argc != 1)
    {
	*tmp = gCP->TObject_ERROR_INVALID_ARGLIST;
    FrameExit(*tmp);
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    pIn = (LpCHAR)ByteArray(argv[0]);
    InLen = ByteVector(argv[0])->itsMaxItemIndex;
    }
else
if (argv[0].Tag == TYSTRING)
    {
    pIn = (LpCHAR)CharArray(argv[0]);
    InLen = strlen(CharArray(argv[0]));
    }
else
if (argv[0].Tag == TYSYMBOL)
    {
    pIn = (LpCHAR)SymbolArray(argv[0]);
    InLen = strlen(SymbolArray(argv[0]));
    }
else
if (argv[0].Tag == TYTEXT)
    {
    pIn = (LpCHAR)&argv[0].u.Text[0];
    InLen = strlen(argv[0].u.Text);
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    pIn = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    InLen = SubLen(argv[0]);
    }
else
    {
	*tmp = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!compress: Invalid Argument type  %a!",&argv[0]);
	FrameExit(*tmp);
    }

/*  Create the output byte vector for the maximum length. */

*tmp = TSYMBOL("byte");
*out = FSmartbase_Eval(gCP,gTP,makeCmd,2,*tmp,TINT(InLen + FUDGE + sizeof(NUM)));
ExitOnError(*out);
pOut = (LpCHAR)ByteArray(*out);
OutLen = ByteVector(*out)->itsMaxItemIndex - sizeof(NUM);

/*  Run the LZRW1 compression algorithm and resize the Byte Vector. */
/*  Note:   Save the original length as the first four bytes of the */
/*          output vector, so we can know how much space to allocate */
/*          during the decompression phase. */

FDatabas_lzrw1_compress((unsigned char*)pIn,InLen,(unsigned char*)pOut + sizeof(NUM),&OutLen);
*((LpNUM)pOut) = InLen;
*out = FSmartbase_Eval(gCP,gTP,resizeCmd,2,*out,TINT(OutLen + sizeof(NUM)));
ExitOnError(*out);

/*  Return the compressed Byte Vector. */

FrameExit(*out);
}

    
/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Uncompress

The  uncompress  Procedure uses the extremely fast LZRW1 decompression algorithm, 
by Ross Williams, to convert previously compressed Byte Vector into the original
much larger Byte Vector.

Note:   We include the original length as the first sizeof(NUM) bytes
        of the smaller compressed vector, so we can know how much space
        to allocate on decompression.

The arguments are as follows.

input   The much smaller Byte Vector after compression.

output  A Byte Vector with the original contents before compression.

For example

    (setq smaller (compress  "Shall we make this string smaller?"))

    (setq output  (uncompress  smaller))

    (= output "Shall we make this string smaller?") =>  true

#endif

TVAL FDatabas_Uncompress(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
LpCHAR          pIn;
NUM             InLen;
LpCHAR          pOut;
NUM             OutLen;
NUM             OriginLen;
TVAL            makeCmd = TFUNCTION(FMake_Vector);
TVAL            resizeCmd = TFUNCTION(FMake_Resize);
StartFrame
DeclareTVAL(out);
DeclareTVAL(tmp);
EndFrame

/*  Make sure the input argument is correct, and get the address and length. */
/*  Note:   The original length was saved as the first four bytes of the */
/*          input vector, so we could know how much space to allocate */
/*          during this decompression phase. */

if (argc != 1)
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    pIn = (LpCHAR)ByteArray(argv[0]);
    InLen = ByteVector(argv[0])->itsMaxItemIndex;
    OriginLen = *((LpNUM)pIn);
    }
else
if (argv[0].Tag == TYVOID)
    {
	*out = gCP->Tval_VOID;
 	FrameExit(*out);
    }
else
    {
	*tmp = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!uncompress: Invalid argument type: %a!",&argv[0]);
 	FrameExit(*tmp);
   }

/*  Create the output byte vector for the original length. */

*tmp = TSYMBOL("byte");                                                             
*out = FSmartbase_Eval(gCP,gTP,makeCmd,2,*tmp,TINT(OriginLen + FUDGE));
ExitOnError(*out);
pOut = (LpCHAR)ByteArray(*out);
OutLen = ByteVector(*out)->itsMaxItemIndex;

/*  Run the LZRW1 decompression algorithm into the output Byte Vector. */

FDatabas_lzrw1_decompress((unsigned char*)pIn + sizeof(NUM),InLen - sizeof(NUM),(unsigned char*)pOut,&OutLen);

/*  Check for invalid uncompressed Byte Vector. */

if (OriginLen != OutLen)
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
*out = FSmartbase_Eval(gCP,gTP,resizeCmd,2,*out,TINT(OriginLen));
ExitOnError(*out);

/*  Return the compressed Byte Vector. */

FrameExit(*out);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_fast_copy

The FDatabas_fast_copy function transfers bytes from the source memory location
to the destination memory location.

#endif

void FDatabas_fast_copy(unsigned char *pSrc,unsigned char *pDst,NUM len)
{
while (len--) *pDst++=*pSrc++;
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_lzrw1_compress

The  FDatabas_lzrw1_compress  Procedure uses the extremely fast LZRW1 compression 
algorithm, by Ross Williams.

The original notes are as follows:

    Input  : Specify input block using p_src_first and src_len.
    Input  : Point p_dst_first to the start of the output zone (OZ).
    Input  : Point p_dst_len to a unsigned long to receive the output length.
    Input  : Input block and output zone must not overlap.
    Output : Length of output block written to *p_dst_len.
    Output : Output block in Mem[p_dst_first..p_dst_first+*p_dst_len-1].
    Output : May write in OZ=Mem[p_dst_first..p_dst_first+src_len+256-1].
    Output : Upon completion guaranteed *p_dst_len<=src_len+FLAG_BYTES.

#endif

#define FLAG_BYTES    4      /* Number of bytes used by copy flag.	*/
#define FLAG_COMPRESS 0      /* Signals that compression occurred.	*/
#define FLAG_COPY     1      /* Signals that a copyover occurred.	*/

void FDatabas_lzrw1_compress(p_src_first,src_len,p_dst_first,p_dst_len)
/* Input  : Specify input block using p_src_first and src_len.          */
/* Input  : Point p_dst_first to the start of the output zone (OZ).     */
/* Input  : Point p_dst_len to a unsigned long to receive the output length.    */
/* Input  : Input block and output zone must not overlap.               */
/* Output : Length of output block written to *p_dst_len.               */
/* Output : Output block in Mem[p_dst_first..p_dst_first+*p_dst_len-1]. */
/* Output : May write in OZ=Mem[p_dst_first..p_dst_first+src_len+256-1].*/
/* Output : Upon completion guaranteed *p_dst_len<=src_len+FLAG_BYTES.  */
unsigned char *p_src_first,*p_dst_first; NUM src_len,*p_dst_len;
#define PS *p++!=*s++  /* Body of inner unrolled matching loop.         */
#define ITEMMAX 16     /* Maximum number of bytes in an expanded item.  */
{unsigned char *p_src=p_src_first,*p_dst=p_dst_first;
 unsigned char *p_src_post=p_src_first+src_len,*p_dst_post=p_dst_first+src_len;
 unsigned char *p_src_max1=p_src_post-ITEMMAX,*p_src_max16=p_src_post-16*ITEMMAX;
 unsigned char *hash[4096],*p_control; UNUM control=0,control_bits=0;
 *p_dst=FLAG_COMPRESS; p_dst+=FLAG_BYTES; p_control=p_dst; p_dst+=2;
 while (TRUE)
   {unsigned char *p,*s; UNUM unroll=16,len,index; UNUM offset;
    if (p_dst>p_dst_post) goto overrun;
    if (p_src>p_src_max16)
      {unroll=1;
       if (p_src>p_src_max1)
         {if (p_src==p_src_post) break; goto literal;}}
    begin_unrolled_loop:
       index=((40543*((((p_src[0]<<4)^p_src[1])<<4)^p_src[2]))>>4) & 0xFFF;
       p=hash[index]; hash[index]=s=p_src; offset=s-p;
       if (offset>4095 || p<p_src_first || offset==0 || PS || PS || PS)
         {literal: *p_dst++=*p_src++; control>>=1; control_bits++;}
       else
         {PS || PS || PS || PS || PS || PS || PS ||
          PS || PS || PS || PS || PS || PS || s++; len=s-p_src-1;
          *p_dst++=((offset&0xF00)>>4)+(len-1); *p_dst++=offset&0xFF;
          p_src+=len; control=(control>>1)|0x8000; control_bits++;}
    if (--unroll) goto begin_unrolled_loop;
    if (control_bits==16)
      {*p_control=control&0xFF; *(p_control+1)=control>>8;
       p_control=p_dst; p_dst+=2; control=control_bits=0;}
   }
 control>>=16-control_bits;
 *p_control++=control&0xFF; *p_control++=control>>8;
 if (p_control==p_dst) p_dst-=2;
 *p_dst_len=p_dst-p_dst_first;
 return;
 overrun: FDatabas_fast_copy(p_src_first,p_dst_first+FLAG_BYTES,src_len);
          *p_dst_first=FLAG_COPY; *p_dst_len=src_len+FLAG_BYTES;
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_lzrw1_decompress

The  FDatabas_lzrw1_decompress  Procedure uses the extremely fast LZRW1 decompression 
algorithm, by Ross Williams.

The original notes are as follows:

    Input  : Specify input block using p_src_first and src_len.
    Input  : Point p_dst_first to the start of the output zone.
    Input  : Point p_dst_len to a unsigned long to receive the output length.
    Input  : Input block and output zone must not overlap. User knows
    Input  : upperbound on output block length from earlier compression.
    Input  : In any case, maximum expansion possible is eight times.
    Output : Length of output block written to *p_dst_len.
    Output : Output block in Mem[p_dst_first..p_dst_first+*p_dst_len-1].
    Output : Writes only  in Mem[p_dst_first..p_dst_first+*p_dst_len-1].

#endif

void FDatabas_lzrw1_decompress(p_src_first,src_len,p_dst_first,p_dst_len)
/* Input  : Specify input block using p_src_first and src_len.          */
/* Input  : Point p_dst_first to the start of the output zone.          */
/* Input  : Point p_dst_len to a unsigned long to receive the output length.    */
/* Input  : Input block and output zone must not overlap. User knows    */
/* Input  : upperbound on output block length from earlier compression. */
/* Input  : In any case, maximum expansion possible is eight times.     */
/* Output : Length of output block written to *p_dst_len.               */
/* Output : Output block in Mem[p_dst_first..p_dst_first+*p_dst_len-1]. */
/* Output : Writes only  in Mem[p_dst_first..p_dst_first+*p_dst_len-1]. */
unsigned char *p_src_first, *p_dst_first; NUM src_len, *p_dst_len;
{UNUM controlbits=0, control=0;
 unsigned char *p_src=p_src_first+FLAG_BYTES, *p_dst=p_dst_first,
       *p_src_post=p_src_first+src_len;
 if (*p_src_first==FLAG_COPY)
   {FDatabas_fast_copy(p_src_first+FLAG_BYTES,p_dst_first,src_len-FLAG_BYTES);
    *p_dst_len=src_len-FLAG_BYTES; return;}
 while (p_src!=p_src_post)
   {if (controlbits==0)
      {control=*p_src++; control|=(*p_src++)<<8; controlbits=16;}
    if (control&1)
      {unsigned int offset,len; unsigned char *p;
       offset=(*p_src&0xF0)<<4; len=1+(*p_src++&0xF);
       offset+=*p_src++&0xFF; p=p_dst-offset;
       while (len--) *p_dst++=*p++;}
    else
       *p_dst++=*p_src++;
    control>>=1; controlbits--;
   }
 *p_dst_len=p_dst-p_dst_first;
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_readf

The  FDatabas_readf  function performs low level file read operations. Operations can
be switched from disk file to virtual memory file depending upon the contents of the
fileID argument.

The arguments are as follows:

    fileID		The disk file integer ID or the virtual memory file ByteVector.
    base 		The file base displacement.
    disp 		The record displacement within the file.
    size 		The size (in bytes) of the record to be read.
    data 		The record pointer where the data is to be copied.

	Return		Zero if there are no errors; otherwise, -1 if an error occurs.

#endif

NUM	 FDatabas_readf(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileID,NUM base,NUM disp,NUM size,char* data)
{
NUM		ioerr;
StartFrame
EndFrame

/* Use the contents of the fileID argument to determine */
/* whether this is a read from a disk file. */

if (fileID.Tag == TYNUM)
	{
	/* Seek to the proper displacement from the front of the disk file. */

    ioerr = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID.u.Int, base + disp, 1);
    if (ioerr < 0)
		{
		FrameExit(-1);
		}

	/* Read the specified number of bytes from the disk file. */

    ioerr = (*gCP->_Host_Readf)((POINTER)gCP,gTP,fileID.u.Int, size, (char*)data);
    if (ioerr != 0)
        {
        FrameExit(-1);
        }

	FrameExit(0);
	}
else
/* Use the contents of the fileID argument to determine */
/* whether this is a read from a virtual memory file. */

if (fileID.Tag == TYBYTEVECTOR)
	{
	/* Make sure the Byte Vector has enough room. */

	if (fileID.u.ByteVector->itsMaxItemIndex < (disp + size))
		{
		FrameExit(-1);
		}

	/* Copy the bytes from the Byte Vector to the data area. */

	FMemory_memcpy(gCP, gTP, data,&ByteArray(fileID)[disp],size);
	FrameExit(0);
	}


/* We cannot recognize the fileID argument, therefore return an error. */

FrameExit(-1);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_writef

The  FDatabas_writef  function performs low level file write operations. Operations can
be switched from disk file to virtual memory file depending upon the contents of the
fileID argument.

The arguments are as follows:

    fileID		The disk file integer ID or the virtual memory file ByteVector.
    base 		The file base displacement.
    disp 		The record displacement within the file.
    size 		The size (in bytes) of the record to be written.
    data 		The record pointer from where the data is to be copied.

	Return		Zero if there are no errors; otherwise, -1 if an error occurs.

#endif

NUM	 FDatabas_writef(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileID,NUM base,NUM disp,NUM size,char* data)
{
NUM		ioerr;
StartFrame
DeclareTVAL(ec);
EndFrame

/* Use the contents of the fileID argument to determine */
/* whether this is a write to a disk file. */

if (fileID.Tag == TYNUM)
	{
	/* Seek to the proper displacement from the front of the disk file. */

    ioerr = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID.u.Int, base + disp, 1);
    if (ioerr < 0)
		{
		FrameExit(-1);
		}

	/* Write the specified number of bytes to the disk file. */

    ioerr = (*gCP->_Host_Writef)((POINTER)gCP,gTP,fileID.u.Int, size, (char*)data);
    if (ioerr != 0)
        {
        FrameExit(-1);
        }

	FrameExit(0);
	}
else
/* Use the contents of the fileID argument to determine */
/* whether this is a write to a virtual memory file. */

if (fileID.Tag == TYBYTEVECTOR)
	{
	/* Make sure the Byte Vector has enough room. */

	if (fileID.u.ByteVector->itsMaxItemIndex < (disp + size))
		{
		*ec = TByteVector_SetMaxIndex(gCP,gTP,fileID,disp + size);
		if (ec->Tag == TYERROR)
			{
			FrameExit(-1);
			}
		}

	/* Copy the bytes to the Byte Vector from the data area. */

	FMemory_memcpy(gCP, gTP, &ByteArray(fileID)[disp],data,size);
	FrameExit(0);
	}


/* We cannot recognize the fileID argument, therefore return an error. */

FrameExit(-1);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_readR

The  FDatabas_readR  function performs low level file read operations returning a record. 
Operations can be switched from disk file to virtual memory file depending upon the contents 
of the fileID argument.

The arguments are as follows:

    fileID		The disk file integer ID or the virtual memory file ByteVector.
    base 		The file base displacement.
    disp 		The record displacement within the file.
    size 		The size (in bytes) of the record to be read.
    bvector 	TRUE if the record is to be read as a Byte Vector,
				FALSE if the record is to be read as an object.

	Return		The object record read from the database file.

#endif

TVAL FDatabas_readR(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileID,NUM base,NUM disp,NUM size,BOLE bvector)
{
NUM		ioerr;
TVAL    readCmd = TFUNCTION(FConio_fread);
TVAL	loadCmd = TFUNCTION(FConio_loadObject);
StartFrame
DeclareTVAL(ec);
DeclareTVAL(rec);
EndFrame

/* Read all records as objects. Do not read records as ByteVectors. */
/* Note: We used to read ByteVectors, but no longer allow this.		*/

bvector = FALSE;

/* Use the contents of the fileID argument to determine */
/* whether this is a read from a disk file. */

if (fileID.Tag == TYNUM)
	{
	/* Seek to the proper displacement from the front of the disk file. */

    ioerr = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID.u.Int, base + disp, 1);
    if (ioerr < 0)
		{
		FrameExit(gTP->FDatabas_IOerr);
		}

	if (bvector)
		{	
		/* Read the specified number of bytes from the disk file. */

		*rec = FSmartbase_Eval(gCP,gTP,readCmd,2,fileID,TINT(size));
		ExitOnError(*rec);
		}
	else
		{	
		/* Load the specified object from the disk file. */
		
		*rec = FSmartbase_Eval(gCP,gTP,loadCmd,1,fileID);
		ExitOnError(*rec);
		}

	FrameExit(*rec);
	}

/* Use the contents of the fileID argument to determine */
/* whether this is a read from a virtual memory file. */

if ((fileID.Tag == TYBYTEVECTOR) && (bvector == TRUE))
	{
	/* Make sure the Byte Vector has enough room. */

	if (fileID.u.ByteVector->itsMaxItemIndex < (disp + size))
		{
		FrameExit(gTP->FDatabas_IOerr);
		}

	/* Create a record object large enough for the data area. */

	rec->u.ByteVector = TByteVector_New(gCP,gTP);
	rec->Tag = TYBYTEVECTOR;
	*rec = TByteVector_SetMaxIndex(gCP,gTP,*rec,size);
	ExitOnError(*rec);

	/* Copy the bytes from the Byte Vector to the record data area. */

	FMemory_memcpy(gCP, gTP, &ByteArray(*ec)[0],&ByteArray(fileID)[disp],size);
	FrameExit(*rec);
	}

/* Use the contents of the fileID argument to determine */
/* whether this is a load from a virtual memory file. */

if ((fileID.Tag == TYBYTEVECTOR) && (bvector == FALSE))
	{	
	/* Make sure the Byte Vector has enough room. */

	if (fileID.u.ByteVector->itsMaxItemIndex < (disp + size))
		{
		FrameExit(gTP->FDatabas_IOerr);
		}

	/* Create a record object large enough for the data area. */

	rec->u.ByteVector = TByteVector_New(gCP,gTP);
	rec->Tag = TYBYTEVECTOR;
	*rec = TByteVector_SetMaxIndex(gCP,gTP,*rec,size);
	ExitOnError(*rec);

	/* Copy the bytes from the Byte Vector to the record data area. */

	FMemory_memcpy(gCP, gTP, &ByteArray(*ec)[0],&ByteArray(fileID)[disp],size);	
	FrameExit(*rec);	

	/* Load the object from the Byte Vector record. */	
	*rec = FSmartbase_Eval(gCP,gTP,loadCmd,1,*rec);
	FrameExit(*rec);	
	}

/* We cannot recognize the fileID argument, therefore return an error. */

FrameExit(gTP->FDatabas_IOerr);}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_writeR

The  FDatabas_writeR  function performs low level file write operations using a record. 
Operations can be switched from disk file to virtual memory file depending upon the contents 
of the fileID argument.

The arguments are as follows:

    fileID		The disk file integer ID or the virtual memory file ByteVector.
    base 		The file base displacement.
    disp 		The record displacement within the file.
    record 		The record (a Byte Vector) to be written.
    bvector 	TRUE if the record is to be written as a Byte Vector,
				FALSE if the record is to be written as an object.

	Return		True if no error occurs.

#endif

TVAL FDatabas_writeR(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileID,NUM base,NUM disp,TVAL record,BOLE bvector)
{
NUM		size;
NUM		ioerr;
TVAL    writeCmd = TFUNCTION(FConio_fwrite);
TVAL	saveCmd = TFUNCTION(FConio_saveObject);	
StartFrame
DeclareTVAL(ec);
DeclareTVAL(rec);
EndFrame

/* Save all records as objects. Do not save records as ByteVectors. */
/* Note: We used to save ByteVectors, but no longer allow this.		*/

bvector = FALSE;

/* Use the contents of the fileID argument to determine */
/* whether this is a write to a disk file. */

if (fileID.Tag == TYNUM)
	{
	/* Seek to the proper displacement from the front of the disk file. */

    ioerr = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID.u.Int, base + disp, 1);
    if (ioerr < 0)
		{
		FrameExit(gTP->FDatabas_IOerr);
		}

	if ((bvector) && (record.Tag == TYBYTEVECTOR))
		{
		/* Write the specified number of bytes to the disk file. */	
		*rec = FSmartbase_Eval(gCP,gTP,writeCmd,2,fileID,record);	
		ExitOnError(*rec);
		}
	else
		{
		/* Save the specified object to the disk file. */

		*rec = FSmartbase_Eval(gCP,gTP,saveCmd,2,fileID,record);
		ExitOnError(*rec);
		}

	FrameExit(gCP->Tval_TRUE);
	}
	
/* Use the contents of the fileID argument to determine */
/* whether this is a write to a virtual memory file. */

if ((fileID.Tag == TYBYTEVECTOR) && (record.Tag == TYBYTEVECTOR) && bvector)
	{
	/* Make sure the Byte Vector has enough room. */

	size = ByteVector(record)->itsMaxItemIndex;
	if (fileID.u.ByteVector->itsMaxItemIndex < (disp + size))
		{
		*ec = TByteVector_SetMaxIndex(gCP,gTP,fileID,disp + size);
		ExitOnError(*ec);
		}

	/* Copy the bytes from the record data area to the Byte Vector. */

	FMemory_memcpy(gCP, gTP, &ByteArray(fileID)[disp],&ByteArray(record)[0],size);
	FrameExit(gCP->Tval_TRUE);
	}

/* Use the contents of the fileID argument to determine */
/* whether this is a save to a virtual memory file. */

if ((fileID.Tag == TYBYTEVECTOR) && (bvector == FALSE))	
	{
	/* Save the object record to a Byte Vector record. */
	*rec = FSmartbase_Eval(gCP,gTP,saveCmd,1,record);	
	ExitOnError(*rec);

	/* Make sure the Byte Vector has enough room. */

	size = ByteVector(*rec)->itsMaxItemIndex;	
	if (fileID.u.ByteVector->itsMaxItemIndex < (disp + size))
		{	
		*ec = TByteVector_SetMaxIndex(gCP,gTP,fileID,disp + size);
		ExitOnError(*ec);	
		}

	/* Copy the bytes from the record data area to the Byte Vector. */

	FMemory_memcpy(gCP, gTP, &ByteArray(fileID)[disp],&ByteArray(*rec)[0],size);
	FrameExit(gCP->Tval_TRUE);	
	}


/* We cannot recognize the fileID argument, therefore return an error. */

FrameExit(gTP->FDatabas_IOerr);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FindFrame

Return the bit index value for the first vacant frame of the specified length in Pages. Vacant
pages are represented by 0 bits, while occupied pages are represented by 1 bits. A Frame is
a consecutive run of Pages of the specified length.

For example:

	frameIndex = FindFrame(pageTable,3);

The above FindFrame invocation returns the bit index of the first vacant page (which is
immediately followed by two other vacant pages). If there do not exist three vacant pages
immediately adjacent anywhere within the page table, then -1 is returned.

Note:	This function uses the PreviousVacantPage element of the page table to store 
        the previous vacant frame found. The previous vacant frame found, is used as 
        a memoed hint, showing where to start looking, the next time FindFrame is called.

Note:	The FindFrame function sets pages occupied in BOTH the main and used page bit maps.
		This allows unlimited rollback of disk operations during a transaction because
		new pages are allocated in both page bit maps, but pages are only freed from the
		main page bit map. Therefore, newly freed pages are never reused prior to 
		this transaction being committed.			

#endif

/* Here follows the table of largest consecutive bit runs */
/* for each of the 256 bit values of a byte. */

typedef struct {NUM largestRun;		/* Largest run of vacant blocks in byte */
                NUM startIndex;		/* Starting bit index of largest run within byte */
                NUM begRunSize;		/* Largest starting run of vacant blocks at first of byte */
                NUM endRunSize;		/* Largest trailing run of vacant blocks at end of byte */
               } runElement;

static runElement runTable[256] = {
								 {8,  0,  8,  8},		/* 0000 0000  */
								 {7,  0,	 7,  0},		/* 0000 0001  */
								 {6,  0,	 6,  1},		/* 0000 0010  */
								 {6,  0,	 6,  0},		/* 0000 0011  */
								 {5,  0,	 5,  2},		/* 0000 0100  */
								 {5,  0,	 5,  0},		/* 0000 0101  */
								 {5,  0,	 5,  1},		/* 0000 0110  */
								 {5,  0,	 5,  0},		/* 0000 0111  */

								 {4,  0,	 4,  3},		/* 0000 1000  */
								 {4,  0,	 4,  0},		/* 0000 1001  */
								 {4,  0,	 4,  1},		/* 0000 1010  */
								 {4,  0,	 4,  0},		/* 0000 1011  */
								 {4,  0,	 4,  2},		/* 0000 1100  */
								 {4,  0,	 4,  0},		/* 0000 1101  */
								 {4,  0,	 4,  1},		/* 0000 1110  */
								 {4,  0,	 4,  0},		/* 0000 1111  */

								 {4,  4,	 3,  4},		/* 0001 0000  */
								 {3,  0,	 3,  0},		/* 0001 0001  */
								 {3,  0,	 3,  1},		/* 0001 0010  */
								 {3,  0,	 3,  0},		/* 0001 0011  */
								 {3,  0,	 3,  2},		/* 0001 0100  */
								 {3,  0,	 3,  0},		/* 0001 0101  */
								 {3,  0,	 3,  1},		/* 0001 0110  */
								 {3,  0,	 3,  0},		/* 0001 0111  */

								 {3,  0,	 3,  3},		/* 0001 1000  */
								 {3,  0,	 3,  0},		/* 0001 1001  */
								 {3,  0,	 3,  1},		/* 0001 1010  */
								 {3,  0,	 3,  0},		/* 0001 1011  */
								 {3,  0,	 3,  2},		/* 0001 1100  */
								 {3,  0,	 3,  0},		/* 0001 1101  */
								 {3,  0,	 3,  1},		/* 0001 1110  */
								 {3,  0,	 3,  0},		/* 0001 1111  */

								 {5,  3,	 2,  5},		/* 0010 0000  */
								 {4,  3,	 2,  0},		/* 0010 0001  */
								 {3,  3,	 2,  1},		/* 0010 0010  */
								 {3,  3,	 2,  0},		/* 0010 0011  */
								 {2,  0,	 2,  2},		/* 0010 0100  */
								 {2,  0,	 2,  0},		/* 0010 0101  */
								 {2,  0,	 2,  1},		/* 0010 0110  */
								 {2,  0,	 2,  0},		/* 0010 0111  */

								 {3,  5,	 2,  3},		/* 0010 1000  */
								 {2,  0,	 2,  0},		/* 0010 1001  */
								 {2,  0,	 2,  1},		/* 0010 1010  */
								 {2,  0,	 2,  0},		/* 0010 1011  */
								 {2,  0,	 2,  2},		/* 0010 1100  */
								 {2,  0,	 2,  0},		/* 0010 1101  */
								 {2,  0,	 2,  1},		/* 0010 1110  */
								 {2,  0,	 2,  0},		/* 0010 1111  */

								 {4,  4,	 2,  4},		/* 0011 0000  */
								 {3,  4,	 2,  0},		/* 0011 0001  */
								 {2,  0,	 2,  1},		/* 0011 0010  */
								 {2,  0,	 2,  0},		/* 0011 0011  */
								 {2,  0,	 2,  2},		/* 0011 0100  */
								 {2,  0,	 2,  0},		/* 0011 0101  */
								 {2,  0,	 2,  1},		/* 0011 0110  */
								 {2,  0,	 2,  0},		/* 0011 0111  */

								 {3,  5,	 2,  3},		/* 0011 1000  */
								 {2,  0,	 2,  0},		/* 0011 1001  */
								 {2,  0,	 2,  1},		/* 0011 1010  */
								 {2,  0,	 2,  0},		/* 0011 1011  */
								 {2,  0,	 2,  2},		/* 0011 1100  */
								 {2,  0,	 2,  0},		/* 0011 1101  */
								 {2,  0,	 2,  1},		/* 0011 1110  */
								 {2,  0,	 2,  0},		/* 0011 1111  */

								 {6,  2,	 1,  6},		/* 0100 0000  */
								 {5,  2,	 1,  0},		/* 0100 0001  */
								 {4,  2,	 1,  1},		/* 0100 0010  */
								 {4,  2,	 1,  0},		/* 0100 0011  */
								 {3,  2,	 1,  2},		/* 0100 0100  */
								 {3,  2,	 1,  0},		/* 0100 0101  */
								 {3,  2,	 1,  1},		/* 0100 0110  */
								 {3,  2,	 1,  0},		/* 0100 0111  */

								 {3,  5,	 1,  3},		/* 0100 1000  */
								 {2,  2,	 1,  0},		/* 0100 1001  */
								 {2,  2,	 1,  1},		/* 0100 1010  */
								 {2,  2,	 1,  0},		/* 0100 1011  */
								 {2,  2,	 1,  2},		/* 0100 1100  */
								 {2,  2,	 1,  0},		/* 0100 1101  */
								 {2,  2,	 1,  1},		/* 0100 1110  */
								 {2,  2,	 1,  0},		/* 0100 1111  */

								 {4,  4,	 1,  4},		/* 0101 0000  */
								 {3,  4,	 1,  0},		/* 0101 0001  */
								 {2,  4,	 1,  1},		/* 0101 0010  */
								 {2,  4,	 1,  0},		/* 0101 0011  */
								 {2,  6,	 1,  2},		/* 0101 0100  */
								 {1,  0,	 1,  0},		/* 0101 0101  */
								 {1,  0,	 1,  1},		/* 0101 0110  */
								 {1,  0,	 1,  0},		/* 0101 0111  */

								 {3,  5,	 1,  3},		/* 0101 1000  */
								 {2,  5,	 1,  0},		/* 0101 1001  */
								 {1,  0,	 1,  1},		/* 0101 1010  */
								 {1,  0,	 1,  0},		/* 0101 1011  */
								 {2,  6,	 1,  2},		/* 0101 1100  */
								 {1,  0,	 1,  0},		/* 0101 1101  */
								 {1,  0,	 1,  1},		/* 0101 1110  */
								 {1,  0,	 1,  0},		/* 0101 1111  */

								 {5,  3,	 1,  5},		/* 0110 0000  */
								 {4,  3,	 1,  0},		/* 0110 0001  */
								 {3,  3,	 1,  1},		/* 0110 0010  */
								 {3,  3,	 1,  0},		/* 0110 0011  */
								 {2,  3,	 1,  2},		/* 0110 0100  */
								 {2,  3,	 1,  0},		/* 0110 0101  */
								 {2,  3,	 1,  1},		/* 0110 0110  */
								 {2,  3,	 1,  0},		/* 0110 0111  */

								 {3,  5,	 1,  3},		/* 0110 1000  */
								 {2,  5,	 1,  0},		/* 0110 1001  */
								 {1,  0,	 1,  1},		/* 0110 1010  */
								 {1,  0,	 1,  0},		/* 0110 1011  */
								 {2,  6,	 1,  2},		/* 0110 1100  */
								 {1,  0,	 1,  0},		/* 0110 1101  */
								 {1,  0,	 1,  1},		/* 0110 1110  */
								 {1,  0,	 1,  0},		/* 0110 1111  */

								 {4,  4,	 1,  4},		/* 0111 0000  */
								 {3,  4,	 1,  0},		/* 0111 0001  */
								 {2,  4,	 1,  1},		/* 0111 0010  */
								 {2,  4,	 1,  0},		/* 0111 0011  */
								 {2,  6,	 1,  2},		/* 0111 0100  */
								 {1,  0,	 1,  0},		/* 0111 0101  */
								 {1,  0,	 1,  1},		/* 0111 0110  */
								 {1,  0,	 1,  0},		/* 0111 0111  */

								 {3,  5,	 1,  3},		/* 0111 1000  */
								 {2,  5,	 1,  0},		/* 0111 1001  */
								 {1,  0,	 1,  1},		/* 0111 1010  */
								 {1,  0,	 1,  0},		/* 0111 1011  */
								 {2,  6,	 1,  2},		/* 0111 1100  */
								 {1,  0,	 1,  0},		/* 0111 1101  */
								 {1,  0,	 1,  1},		/* 0111 1110  */
								 {1,  0,	 1,  0},		/* 0111 1111  */

								 {7,  1,  0,	 7},		/* 1000 0000  */
								 {6,  1,  0,	 0},		/* 1000 0001  */
								 {5,  1,  0,	 1},		/* 1000 0010  */
								 {5,  1,  0,	 0},		/* 1000 0011  */
								 {4,  1,  0,	 2},		/* 1000 0100  */
								 {4,  1,  0,	 0},		/* 1000 0101  */
								 {4,  1,  0,	 1},		/* 1000 0110  */
								 {4,  1,  0,	 0},		/* 1000 0111  */

								 {3,  1,  0,	 3},		/* 1000 1000  */
								 {3,  1,  0,	 0},		/* 1000 1001  */
								 {3,  1,  0,	 1},		/* 1000 1010  */
								 {3,  1,  0,	 0},		/* 1000 1011  */
								 {3,  1,  0,	 2},		/* 1000 1100  */
								 {3,  1,  0,	 0},		/* 1000 1101  */
								 {3,  1,  0,	 1},		/* 1000 1110  */
								 {3,  1,  0,	 0},		/* 1000 1111  */

								 {4,  4,  0,	 4},		/* 1001 0000  */
								 {3,  4,  0,	 0},		/* 1001 0001  */
								 {2,  1,  0,	 1},		/* 1001 0010  */
								 {2,  1,  0,	 0},		/* 1001 0011  */
								 {2,  1,  0,	 2},		/* 1001 0100  */
								 {2,  1,  0,	 0},		/* 1001 0101  */
								 {2,  1,  0,	 1},		/* 1001 0110  */
								 {2,  1,  0,	 0},		/* 1001 0111  */

								 {3,  5,  0,	 3},		/* 1001 1000  */
								 {2,  1,  0,	 0},		/* 1001 1001  */
								 {2,  1,  0,	 1},		/* 1001 1010  */
								 {2,  1,  0,  0},		/* 1001 1011  */
								 {2,  1,  0,  2},		/* 1001 1100  */
								 {2,  1,  0,  0},		/* 1001 1101  */
								 {2,  1,  0,  1},		/* 1001 1110  */
								 {2,  1,  0,  0},		/* 1001 1111  */

								 {5,  3,  0,  5},		/* 1010 0000  */
								 {4,  3,  0,  0},		/* 1010 0001  */
								 {3,  3,  0,  1},		/* 1010 0010  */
								 {3,  3,  0,  0},		/* 1010 0011  */
								 {2,  3,  0,  2},		/* 1010 0100  */
								 {2,  3,  0,  0},		/* 1010 0101  */
								 {2,  3,  0,  1},		/* 1010 0110  */
								 {2,  3,  0,  0},		/* 1010 0111  */

								 {3,  5,  0,  3},		/* 1010 1000  */
								 {2,  5,  0,  0},		/* 1010 1001  */
								 {1,  1,  0,  1},		/* 1010 1010  */
								 {1,  1,  0,  0},		/* 1010 1011  */
								 {2,  6,  0,  2},		/* 1010 1100  */
								 {1,  1,  0,  0},		/* 1010 1101  */
								 {1,  1,  0,  1},		/* 1010 1110  */
								 {1,  1,  0,  0},		/* 1010 1111  */

								 {4,  4,  0,  4},		/* 1011 0000  */
								 {3,  4,  0,  0},		/* 1011 0001  */
								 {2,  4,  0,  1},		/* 1011 0010  */
								 {2,  4,  0,  0},		/* 1011 0011  */
								 {2,  6,  0,  2},		/* 1011 0100  */
								 {1,  1,  0,  0},		/* 1011 0101  */
								 {1,  1,  0,  1},		/* 1011 0110  */
								 {1,  1,  0,  0},		/* 1011 0111  */

								 {3,  5,  0,  3},		/* 1011 1000  */
								 {2,  5,  0,  0},		/* 1011 1001  */
								 {1,  1,  0,  1},		/* 1011 1010  */
								 {1,  1,  0,  0},		/* 1011 1011  */
								 {2,  6,  0,  2},		/* 1011 1100  */
								 {1,  1,  0,  0},		/* 1011 1101  */
								 {1,  1,  0,  1},		/* 1011 1110  */
								 {1,  1,  0,  0},		/* 1011 1111  */

								 {6,  2,  0,  6},		/* 1100 0000  */
								 {5,  2,  0,  0},		/* 1100 0001  */
								 {4,  2,  0,  1},		/* 1100 0010  */
								 {4,  2,  0,  0},		/* 1100 0011  */
								 {3,  2,  0,  2},		/* 1100 0100  */
								 {3,  2,  0,  0},		/* 1100 0101  */
								 {3,  2,  0,  1},		/* 1100 0110  */
								 {3,  2,  0,  0},		/* 1100 0111  */

								 {3,  5,  0,  3},		/* 1100 1000  */
								 {2,  2,  0,  0},		/* 1100 1001  */
								 {2,  2,  0,  1},		/* 1100 1010  */
								 {2,  2,  0,  0},		/* 1100 1011  */
								 {2,  2,  0,  2},		/* 1100 1100  */
								 {2,  2,  0,  0},		/* 1100 1101  */
								 {2,  2,  0,  1},		/* 1100 1110  */
								 {2,  2,  0,  0},		/* 1100 1111  */

								 {4,  4,  0,  4},		/* 1101 0000  */
								 {3,  4,  0,  0},		/* 1101 0001  */
								 {2,  4,  0,  1},		/* 1101 0010  */
								 {2,  4,  0,  0},		/* 1101 0011  */
								 {2,  6,  0,  2},		/* 1101 0100  */
								 {1,  2,  0,  0},		/* 1101 0101  */
								 {1,  2,  0,  1},		/* 1101 0110  */
								 {1,  2,  0,  0},		/* 1101 0111  */

								 {3,  5,  0,  3},		/* 1101 1000  */
								 {2,  5,  0,  0},		/* 1101 1001  */
								 {1,  2,  0,  1},		/* 1101 1010  */
								 {1,  2,  0,  0},		/* 1101 1011  */
								 {2,  6,  0,  2},		/* 1101 1100  */
								 {1,  2,  0,  0},		/* 1101 1101  */
								 {1,  2,  0,  1},		/* 1101 1110  */
								 {1,  2,  0,  0},		/* 1101 1111  */

								 {5,  3,  0,  5},		/* 1110 0000  */
								 {4,  3,  0,  0},		/* 1110 0001  */
								 {3,  3,  0,  1},		/* 1110 0010  */
								 {3,  3,  0,  0},		/* 1110 0011  */
								 {2,  3,  0,  2},		/* 1110 0100  */
								 {2,  3,  0,  0},		/* 1110 0101  */
								 {2,  3,  0,  1},		/* 1110 0110  */
								 {2,  3,  0,  0},		/* 1110 0111  */

								 {3,  5,  0,  3},		/* 1110 1000  */
								 {2,  5,  0,  0},		/* 1110 1001  */
								 {1,  3,  0,  1},		/* 1110 1010  */
								 {1,  3,  0,  0},		/* 1110 1011  */
								 {2,  6,  0,  2},		/* 1110 1100  */
								 {1,  3,  0,  0},		/* 1110 1101  */
								 {1,  3,  0,  1},		/* 1110 1110  */
								 {1,  3,  0,  0},		/* 1110 1111  */

								 {4,  4,  0,  4},		/* 1111 0000  */
								 {3,  4,  0,  0},		/* 1111 0001  */
								 {2,  4,  0,  1},		/* 1111 0010  */
								 {2,  4,  0,  0},		/* 1111 0011  */
								 {2,  6,  0,  2},		/* 1111 0100  */
								 {1,  4,  0,  0},		/* 1111 0101  */
								 {1,  4,  0,  1},		/* 1111 0110  */
								 {1,  4,  0,  0},		/* 1111 0111  */

								 {3,  5,  0,  3},		/* 1111 1000  */
								 {2,  5,  0,  0},		/* 1111 1001  */
								 {1,  5,  0,  1},		/* 1111 1010  */
								 {1,  5,  0,  0},		/* 1111 1011  */
								 {2,  6,  0,  2},		/* 1111 1100  */
								 {1,  6,  0,  0},		/* 1111 1101  */
								 {1,  7,  0,  1},		/* 1111 1110  */
								 {0, -1,  0,  0},		/* 1111 1111  */

							   }; /* end firstRun table */

NUM FDatabas_FindFrame(LpXCONTEXT gCP,LpTHREAD gTP, PAGETABLE* pageTable, NUM requestedSize)
{
/* Register allocations for speed */
register	runElement*		re;
register	unsigned char*	pageMainMap;
register	unsigned char*	pageUsedMap;
register	NUM				foundBitAt = -1;
register	NUM             byteIndex = 0;
register	NUM				bitIndex = 0;
register	NUM             pageMapByteLen;

/* Automatic variables normally allocated */
NUM             pageMapBitLen;
NUM				remainingSize;
NUM				memoStartByte;
NUM				lastFramePage;
NUM				fileSize;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/* Load the page map information from the page table. */
pageMapByteLen = pageTable->PageBitMapByteLen;
pageMapBitLen = pageMapByteLen * 8;
pageMainMap = &pageTable->PageBitMaps[0];
pageUsedMap = &pageTable->PageBitMaps[pageTable->PageBitMapByteLen];

/* Retrieve the memoed start bit (if any). */
memoStartByte = pageTable->PreviousVacantPage;

/* Search the bit array for 'N' consecutive vacant blocks. */
/* Note:  C will not allow us to address each bit, so we use */
/*        a series of byte masks and a state algorithm to */
/*        get at the individual bits in each byte of the bit array. */
/* Note2: Here we search from the memoed byte location to the end */
/*        of the bit vector. This speeds up the search because */
/*        we usually find empty spaces at the memoed locations. */
remainingSize = requestedSize;
foundBitAt = -1;
for (byteIndex = memoStartByte, bitIndex = (byteIndex * 8); byteIndex < pageMapByteLen; ++byteIndex, bitIndex += 8)
	{
	/* We use the next eight bits in the bit array */
	/* to address the proper run table element. */
	re = &runTable[pageUsedMap[byteIndex]];

	/* Are we looking for the first vacant block? */
	if (foundBitAt < 0)
		/* We are looking for the first vacant block. */
		{
		/* Have we found a run large enough? */
		HighFirstRun:
		if (re->largestRun >= requestedSize)
			/* We found a run large enough. */
			{
			foundBitAt = bitIndex + re->startIndex; 

			/*  Check to make sure we didn't go over bit length. */
			if ((foundBitAt + requestedSize) >= pageMapBitLen)
				{
				goto NaiveRun;
				}
			else
				{
				fileSize = sizeof(FILEROOT) + ((foundBitAt + requestedSize) * _PAGESIZE);

				/*  Check to make sure we didn't go over fixed file length (child repositories). */

				if ((pageTable->baseFilePos != 0) && (fileSize > pageTable->FileSize)) 
					goto NaiveRun;
				else
					goto Found;
				}
			}

		/* Do we have an end run to use as a beginning? */
		if (re->endRunSize > 0)
			/* We have an end run large enough to use as a beginning. */
			{
			foundBitAt = bitIndex + 8 - re->endRunSize; 
			remainingSize = requestedSize - re->endRunSize; 
			}
		}
	else
	/* We have already found the first vacant block. */
	/* We are looking for consecutive vacant blocks. */
		{
		/* Have we found a consecutive run large enough? */
		if (re->begRunSize >= remainingSize)
			/* We found a consecutive run large enough. */
			{
			/*  Check to make sure we didn't go over bit length. */
			if ((foundBitAt + requestedSize) >= pageMapBitLen)
				{
				goto NaiveRun;
				}
			else
				{
				fileSize = sizeof(FILEROOT) + ((foundBitAt + requestedSize) * _PAGESIZE);

				/*  Check to make sure we didn't go over fixed file length (child repositories). */

				if ((pageTable->baseFilePos != 0) && (fileSize > pageTable->FileSize)) 
					goto NaiveRun;
				else
					goto Found;
				}
			}

		/* Do we have a completely vacant eight blocks? */
		if (re->begRunSize == 8)
			/* We have a completely vacant eight blocks. */
			{
			remainingSize -= re->begRunSize; 
			}
		else
			/* We do not have a consecutive run large enough. */
			{
			foundBitAt = -1; 
			remainingSize = requestedSize;
			goto HighFirstRun;
			}
		}
	} /* end of byte loop */


/* Search the bit array for 'N' consecutive vacant blocks. */
/* Note:  C will not allow us to address each bit, so we use */
/*        a series of byte masks and a state algorithm to */
/*        get at the individual bits in each byte of the bit array. */
/* Note2: Here we search from the zero location to the memoed */
/*        location because we did not find a vacant block  */
/*        sequence at the memoed locations. */
NaiveRun:
memoStartByte += ((requestedSize / 8 ) + 1);
if (pageMapByteLen < memoStartByte) memoStartByte = pageMapByteLen;
remainingSize = requestedSize;
foundBitAt = -1;
for (byteIndex = 0, bitIndex = 0; byteIndex < memoStartByte; ++byteIndex, bitIndex += 8)
	{
	/* We use the next eight bits in the bit array */
	/* to address the proper run table element. */
	re = &runTable[pageUsedMap[byteIndex]];

	/* Are we looking for the first vacant block? */
	if (foundBitAt < 0)
		/* We are looking for the first vacant block. */
		{
		/* Have we found a run large enough? */
		LowFirstRun:
		if (re->largestRun >= requestedSize)
			/* We found a run large enough. */
			{
			foundBitAt = bitIndex + re->startIndex; 
			goto Found;
			}

		/* Do we have an end run to use as a beginning? */
		if (re->endRunSize > 0)
			/* We have an end run large enough to use as a beginning. */
			{
			foundBitAt = bitIndex + 8 - re->endRunSize; 
			remainingSize = requestedSize - re->endRunSize; 
			}
		}
	else
	/* We have already found the first vacant block. */
	/* We are looking for consecutive vacant blocks. */
		{
		/* Have we found a consecutive run large enough? */
		if (re->begRunSize >= remainingSize)
			/* We found a consecutive run large enough. */
			{
			goto Found;
			}

		/* Do we have a completely vacant eight blocks? */
		if (re->begRunSize == 8)
			/* We have a completely vacant eight blocks. */
			{
			remainingSize -= re->begRunSize; 
			}
		else
			/* We do not have a consecutive run large enough. */
			{
			foundBitAt = -1; 
			remainingSize = requestedSize;
			goto LowFirstRun;
			}
		}
	} /* end of byte loop */


/*  If we cannot find the requested vacant blocks, return false. */
NotFound:
return(-1);


/*  We have found a consecutive block of the requested size. */
Found:

/*  Check to make sure we didn't go over bit length. */

if ((foundBitAt + requestedSize) >= pageMapBitLen) goto NotFound;

/*  Compute the temporary last frame page and the file size. */

lastFramePage = foundBitAt + requestedSize - 1;
fileSize = sizeof(FILEROOT) + ((foundBitAt + requestedSize) * _PAGESIZE);

/*  Check to make sure we didn't go over fixed file length (child repositories). */

if ((pageTable->baseFilePos != 0) && (fileSize > pageTable->FileSize)) goto NotFound;

/*  Mark the found block and its immediate neighbors as occupied. */

for (remainingSize = 0; remainingSize < requestedSize; ++remainingSize)
	{
	bitIndex = foundBitAt + remainingSize;
	byteIndex = bitIndex/8;
#if _REPOTESTMODE	/* Database validity check */     
	if ((pageUsedMap[byteIndex] & FDatabas_OrMasks[(bitIndex%8)]) != 0)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
		}
	if ((pageMainMap[byteIndex] & FDatabas_OrMasks[(bitIndex%8)]) != 0)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
		}
#endif				/* Database validity check */
	pageUsedMap[byteIndex] |= FDatabas_OrMasks[(bitIndex%8)];
	pageMainMap[byteIndex] |= FDatabas_OrMasks[(bitIndex%8)];
	} /* end set bits occupied loop */

/*  Update the page table to reflect the newly occupied page bytes,  */
/*  and set the page table dirty so that it will be written out upon close. */

if (pageTable->baseFilePos == 0) pageTable->FileSize = max(pageTable->FileSize,fileSize);
pageTable->UsedPageCount += requestedSize;
pageTable->UsedFileBytes += (requestedSize * _PAGESIZE);
pageTable->LastPhysicalPage = max(pageTable->LastPhysicalPage,lastFramePage);
++pageTable->SavedObjectCount;
pageTable->IAmDirty = TRUE;

/*  Memo the current byte index in the BitVector cdr for the next search. */

pageTable->PreviousVacantPage = foundBitAt / 8;

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/*  Convert the bit index of the available block into a tval. */
return(foundBitAt);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FreeUsedFrame

Sets the specified pages to vacant. Vacant pages are represented by 0 bits, 
while occupied pagess are represented by 1 bits. A Frame is a consecutive run of
pages. 

For example:

		FreeUsedFrame(pageTable, frameBitIndex, 3);

The above FreeUsedFrame invocation sets the 3 pages (starting at page index i) to be vacant.				

Note:	The FreeUsedFrame function sets pages vacant in both the used page bit map and
		the main bit map.			

#endif

NUM FDatabas_FreeUsedFrame(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable, NUM frameBitIndex, NUM requestedSize)
{
/* Register allocations for speed */
register	unsigned char*	pageMainMap;
register	unsigned char*	pageUsedMap;
register	NUM				foundBitAt = -1;
register	NUM             byteIndex = 0;
register	NUM				bitIndex = 0;
register	NUM             pageMapByteLen;

/* Automatic variables normally allocated */
NUM             pageMapBitLen;
NUM				remainingSize;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/* Load the page map information from the page table. */
pageMapByteLen = pageTable->PageBitMapByteLen;
pageMapBitLen = pageMapByteLen * 8;
pageMainMap = &pageTable->PageBitMaps[0];
pageUsedMap = &pageTable->PageBitMaps[pageTable->PageBitMapByteLen];
foundBitAt = frameBitIndex;

/* The frame must not be out of range. */
/* Note: A Frame is a consecutive run of pages. */
if ((foundBitAt < 0) || ((foundBitAt + requestedSize) >= pageMapBitLen))
	{
	return(-1);
	}

/*  Mark the specified frame as vacant. */
for (remainingSize = 0; remainingSize < requestedSize; ++remainingSize)
	{
	bitIndex = foundBitAt + remainingSize;
	byteIndex = bitIndex/8;
#if _REPOTESTMODE	/* Database validity check */     
	if ((pageUsedMap[byteIndex] & FDatabas_OrMasks[(bitIndex%8)]) == 0)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
		}
	if ((pageMainMap[byteIndex] & FDatabas_OrMasks[(bitIndex%8)]) == 0)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
		}
#endif				/* Database validity check */
	pageMainMap[byteIndex] &= FDatabas_AndMasks[(bitIndex%8)];
	pageUsedMap[byteIndex] &= FDatabas_AndMasks[(bitIndex%8)];
	} /* end set bits vacant loop */

/*  Update the page table to reflect the newly vacant page bytes,  */
/*  and set the page table dirty so that it will be written out upon close. */
/*  Note: The close function is the only function which reduces the */
/*        file size and the last physical page. */

pageTable->UsedPageCount -= requestedSize;
pageTable->UsedFileBytes -= (requestedSize * _PAGESIZE);
--pageTable->SavedObjectCount;
pageTable->IAmDirty = TRUE;

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/*  Convert the bit index of the available block into a tval. */
return(foundBitAt + requestedSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FreeFrame

Sets the specified pages to vacant. Vacant pages are represented by 0 bits, 
while occupied pagess are represented by 1 bits. A Frame is a consecutive run of
pages. 

For example:

		FreeFrame(pageTable, frameBitIndex, 3);

The above FreeFrame invocation sets the 3 pages (starting at page index i) to be vacant.				

Note:	The FreeFrame function sets pages vacant in only the used page bit map.
		This allows unlimited rollback of disk operations during a transaction because
		new pages are allocated in both page bit maps, but pages are only freed from the
		main page bit map. Therefore, newly freed pages are never reused prior to 
		this transaction being committed.			

#endif

NUM FDatabas_FreeFrame(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable, NUM frameBitIndex, NUM requestedSize)
{
/* Register allocations for speed */
register	unsigned char*	pageMainMap;
register	NUM				foundBitAt = -1;
register	NUM             byteIndex = 0;
register	NUM				bitIndex = 0;
register	NUM             pageMapByteLen;

/* Automatic variables normally allocated */
NUM             pageMapBitLen;
NUM				remainingSize;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/* Load the page map information from the page table. */
pageMapByteLen = pageTable->PageBitMapByteLen;
pageMapBitLen = pageMapByteLen * 8;
pageMainMap = &pageTable->PageBitMaps[0];
foundBitAt = frameBitIndex;

/* The frame must not be out of range. */
/* Note: A Frame is a consecutive run of pages. */
if ((foundBitAt < 0) || ((foundBitAt + requestedSize) >= pageMapBitLen))
	{
	return(-1);
	}

/*  Mark the specified frame as vacant. */
for (remainingSize = 0; remainingSize < requestedSize; ++remainingSize)
	{
	bitIndex = foundBitAt + remainingSize;
	byteIndex = bitIndex/8;
#if _REPOTESTMODE	/* Database validity check */     
	if ((pageMainMap[byteIndex] & FDatabas_OrMasks[(bitIndex%8)]) == 0)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
		}
#endif				/* Database validity check */
	pageMainMap[byteIndex] &= FDatabas_AndMasks[(bitIndex%8)];
	} /* end set bits vacant loop */

/*  Update the page table to reflect the newly vacant page bytes,  */
/*  and set the page table dirty so that it will be written out upon close. */
/*  Note: The close function is the only function which reduces the */
/*        file size and the last physical page. */

pageTable->UsedPageCount -= requestedSize;
pageTable->UsedFileBytes -= (requestedSize * _PAGESIZE);
--pageTable->SavedObjectCount;
pageTable->IAmDirty = TRUE;

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/*  Convert the bit index of the available block into a tval. */
return(foundBitAt + requestedSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetFrame

Sets the specified pages to occupied. Vacant pages are represented by 0 bits, 
while occupied pagess are represented by 1 bits. A Frame is a consecutive run of
pages. 

For example:

		SetFrame(pageTable, frameBitIndex, 3);

The above SetFrame invocation sets the 3 pages (starting at page index i) to be occupied.

Note:	The SetFrame function sets pages occupied in BOTH the main and used page bit maps.
		This allows unlimited rollback of	disk operations during a transaction because
		new pages are allocated in both page bit maps, but pages are only freed from the
		main page bit map. Therefore, newly freed pages are never reused prior to 
		this transaction being committed.			

#endif

NUM FDatabas_SetFrame(LpXCONTEXT gCP, LpTHREAD gTP, PAGETABLE* pageTable, NUM frameBitIndex, NUM requestedSize)
{
/* Register allocations for speed */
register	unsigned char*	pageUsedMap;
register	unsigned char*	pageMainMap;
register	NUM				foundBitAt = -1;
register	NUM             byteIndex = 0;
register	NUM				bitIndex = 0;
register	NUM             pageMapByteLen;

/* Automatic variables normally allocated */
NUM             pageMapBitLen;
NUM				remainingSize;
NUM				lastFramePage;
NUM				fileSize;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/* Load the page map information from the page table. */
pageMapByteLen = pageTable->PageBitMapByteLen;
pageMapBitLen = pageMapByteLen * 8;
pageMainMap = &pageTable->PageBitMaps[0];
pageUsedMap = &pageTable->PageBitMaps[pageTable->PageBitMapByteLen];
foundBitAt = frameBitIndex;

/* The frame must not be out of range. */
/* Note: A Frame is a consecutive run of pages. */
if ((foundBitAt < 0) || ((foundBitAt + requestedSize) >= pageMapBitLen))
	{
	return(-1);
	}

/*  Mark the specified frame as occupied. */
/*  Note: Frame is marked as occupied in both bit maps. */
for (remainingSize = 0; remainingSize < requestedSize; ++remainingSize)
	{
	bitIndex = foundBitAt + remainingSize;
	byteIndex = bitIndex/8;
	pageMainMap[byteIndex] |= FDatabas_OrMasks[(bitIndex%8)];
	pageUsedMap[byteIndex] |= FDatabas_OrMasks[(bitIndex%8)];
	} /* end set bits occupied loop */

/*  Update the page table to reflect the newly occupied page bytes,  */
/*  and set the page table dirty so that it will be written out upon close. */

lastFramePage = foundBitAt + requestedSize - 1;
fileSize = sizeof(FILEROOT) + ((foundBitAt + requestedSize) * _PAGESIZE);
if (pageTable->baseFilePos == 0) pageTable->FileSize = max(pageTable->FileSize,fileSize);
pageTable->UsedPageCount += requestedSize;
pageTable->UsedFileBytes += (requestedSize * _PAGESIZE);
pageTable->LastPhysicalPage = max(pageTable->LastPhysicalPage,lastFramePage);
++pageTable->SavedObjectCount;
pageTable->IAmDirty = TRUE;

#if _REPOTESTMODE	/* Database validity check */     
		FDatabas_Validate(gCP,gTP,pageTable);
#endif				/* Database validity check */

/*  Convert the bit index of the available frame into a tval. */
return(foundBitAt + requestedSize);
}


/*--------------------------------------------------------------------------------------- */
#if 0
CheckFrame

Checks the specified pages to verify that they are occupied. Vacant pages are represented 
by 0 bits, while occupied pages are represented by 1 bits. A Frame is a consecutive run of
pages. 

For example:

		CheckFrame(pageTable, frameBitIndex, 3);

The above CheckFrame invocation verifies the 3 pages (starting at page index i) to be occupied.

Note:	The CheckFrame function checks pages occupied in ONLY the main page bit map.
		This allows unlimited rollback of disk operations during a transaction because
		new pages are allocated in both page bit maps, but pages are only freed from the
		main page bit map. Therefore, newly freed pages are never reused prior to 
		this transaction being committed.			

#endif

NUM FDatabas_CheckFrame(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable, NUM frameBitIndex, NUM requestedSize)
{
/* Register allocations for speed */
register	unsigned char*	pageMainMap;
register	NUM				foundBitAt = -1;
register	NUM             byteIndex = 0;
register	NUM				bitIndex = 0;
register	NUM             pageMapByteLen;

/* Automatic variables normally allocated */
NUM             pageMapBitLen;
NUM				remainingSize;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/* Load the page map information from the page table. */
pageMapByteLen = pageTable->PageBitMapByteLen;
pageMapBitLen = pageMapByteLen * 8;
pageMainMap = &pageTable->PageBitMaps[0];
foundBitAt = frameBitIndex;

/* The frame must not be out of range. */
/* Note: A Frame is a consecutive run of pages. */
if ((foundBitAt < 0) || ((foundBitAt + requestedSize) >= pageMapBitLen))
	{
	return(-1);
	}

/*  Mark the specified frame as occupied. */
/*  Note: Frame is marked as occupied in both bit maps. */
for (remainingSize = 0; remainingSize < requestedSize; ++remainingSize)
	{
	bitIndex = foundBitAt + remainingSize;
	byteIndex = bitIndex/8;
	if ((pageMainMap[byteIndex] & FDatabas_OrMasks[(bitIndex%8)]) == 0)
		{
		/*  Return failure if any of the frame was vacant. */
		return(0);
		}
	} /* end set bits occupied loop */

/*  Return success if the whole frame was occupied. */
return(1);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Inspect

The  databaseInspect Procedure returns statistical and status information
concernin the specified Database file. The type of information to be returned
is dependent upon the optional second argument.

The mandatory argument is as follows.

odbID   The database file object returned by a previous databaseOpen procedure.

The optional second arguments are as follows. For example

    (inspect repo)					Returns the size of the Database file in bytes.

    (inspect repo check:)			Performs a complete validation inspection of the Database file.
        
    (inspect repo frame: key)		Returns a frameID containing the position and size of the record.

    (inspect repo frameID length:)	Returns the size of the specified object in the Database file.

    (inspect repo frameID date:)	Returns the time stamp of the specified object in the Database file.

    (inspect repo frameID check:)	Performs a validation inspection of the specified object in the Database file.

	(inspect repo frameID filepos:)	Returns the file position of the specified object in the Database file.

    (inspect repo free:)			Returns the free bytes in the Database file.

    (inspect repo directory:)		Returns a copy of the index directory for the Database file.

    (inspect repo index:)			Displays a complete set of index allocation statistics for the Database file.

    (inspect repo pages:)			Displays page fragmentation statistics for the Database file.

    (inspect repo show:)			Displays a complete set of inspection statistics for the Database file.

    (inspect repo stats:)			Returns a complete structure of inspection statistics for the Database file.

#endif

TVAL FDatabas_Inspect(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
REAL            frameID;
NUM             indexItem;
NUM             frameIndex;
NUM             frameSize;
NUM             frameCount;
NUM             framePos;
NUM             ioerr;
BOLE			pageFree;
BOLE			usedPageFree;
NUM				indexLength;
DBRECORDHEADER	recordHeader;
PAGETABLE*      pageTable;
TVAL            showCmd = TFUNCTION(FConio_Writeln);
StartFrame
DeclareTVAL(fileID);
DeclareTVAL(theIndex);
DeclareTVAL(fileVEC);
DeclareTVAL(result);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareTVAL(key);
DeclareTVAL(value);
DeclareTVAL(tmp1);
DeclareTVAL(tmp2);
DeclareTVAL(tmp3);
EndFrame
 
/*  Make sure the mandatory argument is correct. */

if ((argc < 1) || (argv[0].Tag != TYOBJREPOSITORY))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Initialize the Database root and page table, etc. */

*theIndex = TOBJ(Database(argv[0])->itsIndex);
*fileVEC = TOBJ(Database(argv[0])->itsOdbID);
*fileID = ByteVector(*fileVEC)->itsCdr;
pageTable = (PAGETABLE*)*ByteVector(*fileVEC)->itsByteArray;

/*  Return the size of the Database file in bytes. */

if (argc == 1)
    {
    FrameExit(TINT(pageTable->FileSize));
    }
else

/*  Return the free bytes in the Database file. */

if ((argc == 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"free") == 0))
    {
    FrameExit(TINT(pageTable->FileSize - pageTable->UsedFileBytes));
    }
else

/*  Return a copy of the index directory for the Database file. */

if ((argc == 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"directory") == 0))
    {
    FrameExit(FUtil2_Copy(gCP,gTP,1,theIndex));
    }
else

/*  Return the size of this object in the Database file. */

if ((argc == 3) && 
    ((argv[1].Tag == TYFRAME) || (argv[1].Tag == TYREPOINSERT)) && 
    (argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"length") == 0))
    {
    /*  Locate a Database page from the object index. */

	frameID		= argv[1].u.Real;
	frameSize	= FDatabas_FrameIDToFSize(gCP,gTP,frameID);
	frameCount	= FDatabas_FrameIDToBCount(gCP,gTP,frameID);
	frameIndex	= FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
	framePos	= FDatabas_FrameIDToFPos(gCP,gTP,frameID);

    if ((frameID < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
        {
        FrameExit(TINT(0));
        }
    else
        {
        FrameExit(TINT(frameSize));
        }
    }
else

/*  Return the time stamp of this object in the Database file. */

if ((argc == 3) && 
    ((argv[1].Tag == TYFRAME) || (argv[1].Tag == TYREPOINSERT)) && 
    (argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"date") == 0))
    {
    /*  Locate a Database page from the frameID. */

	frameID		= argv[1].u.Real;
	frameCount	= FDatabas_FrameIDToBCount(gCP,gTP,frameID);
	frameSize	= FDatabas_FrameIDToFSize(gCP,gTP,frameID);
	frameIndex	= FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
	framePos	= FDatabas_FrameIDToFPos(gCP,gTP,frameID);

    if ((frameID < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
        {
        FrameExit(TFRAME(0));
        }
    else
        {
		/*  Read the Database object's reacord header. */
    
		ioerr = FDatabas_readf(gCP,gTP,*fileID,pageTable->baseFilePos,framePos,sizeof(recordHeader),(char*)&recordHeader);
		if (ioerr != 0)
			{
			FrameExit(gTP->FDatabas_IOerr);
			}
		else
		if (recordHeader.recordVersion != _VERSION)
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_WRONG_VERSION);
			}

        ret->u.Real = recordHeader.SaveTimeStamp;
        ret->Tag = TYDATE;
        FrameExit(*ret);
        }
    }
else
/*  Check the validity of the specified object in the Database file. */

if ((argc == 3) && 
    ((argv[1].Tag == TYFRAME) || (argv[1].Tag == TYREPOINSERT)) && 
    (argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"check") == 0))
    {
    /*  Locate a Database page from the frameID. */

	frameID		= argv[1].u.Real;
	frameCount	= FDatabas_FrameIDToBCount(gCP,gTP,frameID);
	frameSize	= FDatabas_FrameIDToFSize(gCP,gTP,frameID);
	frameIndex	= FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
	framePos	= FDatabas_FrameIDToFPos(gCP,gTP,frameID);

	/* Check the frameID for validity. */

    if ((frameID < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
        {
		FrameExit(TERROR("!ObjectRepository: frameID out or range!"));
        }
    else
	/* Check the frameID for allocation and content validity. */

        {
		/*  Read the Database object's record header. */    
		ioerr = FDatabas_readf(gCP,gTP,*fileID,pageTable->baseFilePos,framePos,sizeof(recordHeader),(char*)&recordHeader);
		if (ioerr != 0)
			{
			FrameExit(gTP->FDatabas_IOerr);
			}
		else
		/* Check record version validity. */
		if (recordHeader.recordVersion != _VERSION)
			{
			FrameExit(TERROR("!ObjectRepository: invalid record version!"));
			}
		else
		/* Check record type validity. */
		if ((recordHeader.recordType != _PAGERECORDTYPE) && 
			(recordHeader.recordType != _ROOTRECORDTYPE) &&
			(recordHeader.recordType != _OBJECTRECORDTYPE))
			{
			FrameExit(TERROR("!ObjectRepository: invalid record type!"));
			}
		else
		/* Check record length validity. */
		if (recordHeader.recordLength > frameSize)
			{
			FrameExit(TERROR("!ObjectRepository: invalid record length!"));
			}
		else
		/* Check frame bitmap allocation. */
		if (FDatabas_CheckFrame(gCP,gTP,pageTable, frameIndex, frameCount) == 0)
			{
			FrameExit(TERROR("!ObjectRepository: invalid frame allocation!"));
			}

		/* Frame inspection was successful. */
        ret->u.Int = recordHeader.recordLength;
        ret->Tag = TYNUM;
        FrameExit(*ret);
        }
    }
else
/*  Return the file position of this object in the Database file. */

if ((argc == 3) && 
    ((argv[1].Tag == TYFRAME) || (argv[1].Tag == TYREPOINSERT)) && 
    (argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"filepos") == 0))
    {
    /*  Locate a Database page from the object index. */

	frameID		= argv[1].u.Real;
	frameCount	= FDatabas_FrameIDToBCount(gCP,gTP,frameID);
	frameIndex	= FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
	framePos	= FDatabas_FrameIDToFPos(gCP,gTP,frameID);

    if ((frameID < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
        {
        FrameExit(TFRAME(0));
        }
    else
        {
        FrameExit(TINT(framePos));
        }
    }
else

if ((argc == 3) && 
    ((argv[1].Tag == TYFRAME) || (argv[1].Tag == TYREPOINSERT)) && 
    (argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"frame") == 0))
    {
    /*  Return the diskobject of the record (file position and size of the record) */

	frameID		= argv[1].u.Real;
	frameCount	= FDatabas_FrameIDToBCount(gCP,gTP,frameID);
	frameIndex	= FDatabas_FrameIDToBIndex(gCP,gTP,frameID);
	framePos	= FDatabas_FrameIDToFPos(gCP,gTP,frameID);

    if ((frameID < 0) || ((frameIndex + frameCount) >= pageTable->MaxPageCount))
        {
        FrameExit(TFRAME(NILPAGEINDEX));
        }
    else
        {
        FrameExit(TFRAME(frameID));
        }
    }
else



/*  Displays a complete set of inspection statistics for debugging the Database file. */

if ((argc == 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"show") == 0))
    {
	/* make sure that string args are not garbage collected */
	*tmp = TSTRING("*** Page Table Inspection Statistics ***"); 
    FSmartbase_Eval(gCP,gTP,showCmd,2,TGVALUE("_eol"),*tmp);
    *tmp = TSTRING("Starting File Position = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->baseFilePos));
    *tmp = TSTRING("Page Table File Position = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->PageTableFPos));
	*tmp = TSTRING("Page Table Size = ");
    FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->PageTableSize));
    *tmp = TSTRING("File Size = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->FileSize));
    *tmp = TSTRING("Max Page Count = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->MaxPageCount));
    *tmp = TSTRING("Used Page Count = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->UsedPageCount));
    *tmp = TSTRING("Used File Bytes = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->UsedFileBytes));
    *tmp = TSTRING("Page Table Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->PageTableFrame));
    *tmp = TSTRING("MetaData Object Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->MetaDataObjectFrame));
    *tmp = TSTRING("Index Object Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->IndexObjectFrame));
    *tmp = TSTRING("First Physical Page = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->FirstPhysicalPage));
    *tmp = TSTRING("Last Physical Page = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->LastPhysicalPage));
    *tmp = TSTRING("Open Time Stamp = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->OpenTimeStamp));
    *tmp = TSTRING("Page Table is Dirty = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TBOOL(pageTable->IAmDirty));
    *tmp = TSTRING("Automatic Rollback On = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TBOOL(pageTable->RollBackOn));

    goto ValidationInspection;
    }
else

/*  Displays page index allocation statistics for debugging the Database file. */

if ((argc == 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"index") == 0))
    {
	*tmp = TSTRING("*** Index Allocation Statistics ***");
    FSmartbase_Eval(gCP,gTP,showCmd,2,TGVALUE("_eol"),*tmp);
    *tmp = TSTRING("Starting File Position = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->baseFilePos));
    *tmp = TSTRING("Page Table File Position = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->PageTableFPos));
	*tmp = TSTRING("Page Table Size = ");
    FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->PageTableSize));
    *tmp = TSTRING("File Size = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->FileSize));
    *tmp = TSTRING("Max Page Count = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->MaxPageCount));
    *tmp = TSTRING("Used Page Count = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->UsedPageCount));
    *tmp = TSTRING("Used File Bytes = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->UsedFileBytes));
    *tmp = TSTRING("Page Table Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->PageTableFrame));
    *tmp = TSTRING("MetaData Object Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->MetaDataObjectFrame));
    *tmp = TSTRING("Index Object Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->IndexObjectFrame));
    *tmp = TSTRING("First Physical Page = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->FirstPhysicalPage));
    *tmp = TSTRING("Last Physical Page = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->LastPhysicalPage));
    *tmp = TSTRING("Open Time Stamp = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->OpenTimeStamp));
    *tmp = TSTRING("Page Table is Dirty = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TBOOL(pageTable->IAmDirty));
    *tmp = TSTRING("Automatic Rollback On = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TBOOL(pageTable->RollBackOn));

	*tmp = TSTRING("Index Entries (if any)...");
    FSmartbase_Eval(gCP,gTP,showCmd,1,*tmp);
    indexItem = 0;
	indexLength = Directory(*theIndex)->itsMaxItemIndex;
    while (indexItem < indexLength)
        {
		*key = PBindArray(*theIndex)[indexItem].Key;
		*value = PBindArray(*theIndex)[indexItem].Value;
		if (value->Tag == TYFRAME)
			{
			*tmp1 = TSTRING("[");
			*tmp2 = TSTRING("] FRAME = ");
			FSmartbase_Eval(gCP,gTP,showCmd,4,
								*tmp1,
								*key,
								*tmp2,
								TREAL(value->u.Real));
			}
		else
		if (value->Tag == TYREPOINSERT)
			{
			*tmp1 = TSTRING("[");
			*tmp2 = TSTRING("] CHILD = ");
			FSmartbase_Eval(gCP,gTP,showCmd,4,
								*tmp1,
								*key,
								*tmp2,
								TREAL(value->u.Real));
			}
		else
			{
			*tmp1 = TSTRING("[");
			*tmp2 = TSTRING("] IMMEDIATE = ");
			FSmartbase_Eval(gCP,gTP,showCmd,4,
								*tmp1,
								*key,
								*tmp2,
								*value);
			}

        ++indexItem;
        }

    goto ValidationInspection;
    }
else

/*  Returns a structure of allocation statistics for debugging the Database file. */

if ((argc == 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"stats") == 0))
    {
	result->u.Object = (TObject *)TStructure_New(gCP,gTP);
	result->Tag = result->u.Object->itsObjectType;

	*tmp1 = TSTRING("StartingFilePosition");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->baseFilePos));
	*tmp1 = TSTRING("PageTableFilePosition");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->PageTableFPos));
	*tmp1 = TSTRING("PageTableSize");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->PageTableSize));
	*tmp1 = TSTRING("FileSize");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->FileSize));
	*tmp1 = TSTRING("MaxPageCount");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->MaxPageCount));
	*tmp1 = TSTRING("UsedPageCount");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->UsedPageCount));
	*tmp1 = TSTRING("UsedFileBytes");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->UsedFileBytes));
	*tmp1 = TSTRING("PageTableFrame");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TREAL(pageTable->PageTableFrame));
	*tmp1 = TSTRING("MetaDataObjectFrame");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TREAL(pageTable->MetaDataObjectFrame));
	*tmp1 = TSTRING("IndexObjectFrame");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TREAL(pageTable->IndexObjectFrame));
	*tmp1 = TSTRING("FirstPhysicalPage");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->FirstPhysicalPage));
	*tmp1 = TSTRING("LastPhysicalPage");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TINT(pageTable->LastPhysicalPage));
	*tmp1 = TSTRING("OpenTimeStamp");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TREAL(pageTable->OpenTimeStamp));
	*tmp1 = TSTRING("PageTableDirty");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TBOOL(pageTable->IAmDirty));
	*tmp1 = TSTRING("AutomaticRollbackOn");
	TStructure_AddNewValue(gCP,gTP,*result,*tmp1,TBOOL(pageTable->RollBackOn));

	FrameExit(*result);
    }
else

/*  Displays page fragmentation statistics for debugging the Database file. */

if ((argc == 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"pages") == 0))
    {
	*tmp = TSTRING("*** Page Table Fragmentation Statistics ***");
    FSmartbase_Eval(gCP,gTP,showCmd,2,TGVALUE("_eol"),*tmp);
    *tmp = TSTRING("Starting File Position = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->baseFilePos));
    *tmp = TSTRING("Page Table File Position = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->PageTableFPos));
	*tmp = TSTRING("Page Table Size = ");
    FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->PageTableSize));
    *tmp = TSTRING("File Size = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->FileSize));
    *tmp = TSTRING("Max Page Count = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->MaxPageCount));
    *tmp = TSTRING("Used Page Count = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->UsedPageCount));
    *tmp = TSTRING("Used File Bytes = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->UsedFileBytes));
    *tmp = TSTRING("Page Table Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->PageTableFrame));
    *tmp = TSTRING("MetaData Object Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->MetaDataObjectFrame));
    *tmp = TSTRING("Index Object Frame = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->IndexObjectFrame));
    *tmp = TSTRING("First Physical Page = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->FirstPhysicalPage));
    *tmp = TSTRING("Last Physical Page = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TINT(pageTable->LastPhysicalPage));
    *tmp = TSTRING("Open Time Stamp = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TREAL(pageTable->OpenTimeStamp));
    *tmp = TSTRING("Page Table is Dirty = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TBOOL(pageTable->IAmDirty));
    *tmp = TSTRING("Automatic Rollback On = ");
	FSmartbase_Eval(gCP,gTP,showCmd,2,*tmp,TBOOL(pageTable->RollBackOn));

	*tmp = TSTRING("Index Entries (if any)...");
    FSmartbase_Eval(gCP,gTP,showCmd,1,*tmp);
    indexItem = 0;
	indexLength = Directory(*theIndex)->itsMaxItemIndex;
    while (indexItem < indexLength)
        {
		*key = PBindArray(*theIndex)[indexItem].Key;
		*value = PBindArray(*theIndex)[indexItem].Value;
		if (value->Tag == TYFRAME)
			{
			*tmp1 = TSTRING("[");
			*tmp2 = TSTRING("] FRAME = ");
			FSmartbase_Eval(gCP,gTP,showCmd,4,
								*tmp1,
								*key,
								*tmp2,
								TREAL(value->u.Real));
			}
		else
		if (value->Tag == TYREPOINSERT)
			{
			*tmp1 = TSTRING("[");
			*tmp2 = TSTRING("] CHILD = ");
			FSmartbase_Eval(gCP,gTP,showCmd,4,
								*tmp1,
								*key,
								*tmp2,
								TREAL(value->u.Real));
			}
		else
			{
			*tmp1 = TSTRING("[");
			*tmp2 = TSTRING("] IMMEDIATE = ");
			FSmartbase_Eval(gCP,gTP,showCmd,4,
								*tmp1,
								*key,
								*tmp2,
								*value);
			}

        ++indexItem;
        }


	*tmp = TSTRING("Frame Entries...");
    FSmartbase_Eval(gCP,gTP,showCmd,1,*tmp);
    frameIndex = pageTable->FirstPhysicalPage;
    while (frameIndex <= pageTable->LastPhysicalPage)
        {
        pageFree = REFBIT(&pageTable->PageBitMaps[0],frameIndex);
        usedPageFree = REFBIT(&pageTable->PageBitMaps[pageTable->PageBitMapByteLen],frameIndex);
		*tmp1 = TSTRING("[");
		*tmp2 = TSTRING("] MAIN FREE = ");
		*tmp3 = TSTRING(", USED FREE = ");
        FSmartbase_Eval(gCP,gTP,showCmd,6,
                            *tmp1,
                            TINT(frameIndex),
                            *tmp2,
                            TBOOL((BOLE)!pageFree),
                            *tmp3,
                            TBOOL((BOLE)!usedPageFree));
        ++frameIndex;
        }

    goto ValidationInspection;
    }
else

/*  Performs a complete validation inspection of the Database file. */

if ((argc == 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"check") == 0))
    {
    ValidationInspection:
    
	FDatabas_Validate(gCP,gTP,pageTable);
                        
    FrameExit(gCP->Tval_TRUE);
    }

FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FDatabas_Validate

The  FDatabas_Validate Function checks the page table for validity.
        
#endif

NUM FDatabas_Validate(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable)        
{
NUM             frameIndex;
BOLE			pageFree;
NUM             checkFileSize;
NUM             checkPageTableSize;
NUM             checkMaxPageCount;
NUM             checkUsedPageCount;
NUM             checkUsedObjectCount;
NUM             checkUsedFileBytes;
 
    
/*  Are any of the sensitive 8 page byte boundry conditions violated? */

checkMaxPageCount = MAXPAGESFROMPAGES(pageTable->MaxPageCount);
if (pageTable->MaxPageCount != checkMaxPageCount)
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
	}

checkFileSize = FILESIZEFROMFILELEN(pageTable->FileSize - sizeof(FILEROOT));
if (pageTable->FileSize > checkFileSize)
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
	}

checkPageTableSize = PAGETABLEFRAMESIZEFROMSIZE(pageTable->PageTableSize);
if (pageTable->PageTableSize != checkPageTableSize)
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
	}

/*  Examine each page entry for any invalid conditions. */

checkFileSize = 0;
checkMaxPageCount = 0;
checkUsedPageCount = 0;
checkUsedObjectCount = 0;
checkUsedFileBytes = 0;
for (frameIndex = 0; frameIndex < pageTable->MaxPageCount; ++frameIndex)
    {
    pageFree = !REFBIT(pageTable->PageBitMaps,frameIndex);

    /* Computer the total database file size. */

    if (frameIndex <= pageTable->LastPhysicalPage)
        {
		checkFileSize += _PAGESIZE;
        }

    /* Is the frame empty or is it allocated and nonempty? */

    if (pageFree == FALSE)
        {
        /* Make sure the frame index is valid. */

        if ((frameIndex < pageTable->FirstPhysicalPage) || (frameIndex > pageTable->LastPhysicalPage))
            {
            FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
            }
        
        /* Compute the valid used and free file bytes plus. */
        /* the valid nonempty (as opposed to empty) page count. */

        checkUsedFileBytes += _PAGESIZE;
        checkUsedPageCount++;
        }           
    }
    
/*  Check the collected Database statistics versus the */
/*  represented statisics from the Database page table. */ 

if (((pageTable->baseFilePos == 0) && (checkFileSize > (NUM)(pageTable->FileSize - sizeof(FILEROOT)))) ||
    (checkUsedPageCount != pageTable->UsedPageCount) ||
    (checkUsedFileBytes != (NUM)(pageTable->UsedFileBytes - sizeof(FILEROOT))))
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
    }
    
/* If we get to here, the page table has passed all checks. */
                
return(0);
}
