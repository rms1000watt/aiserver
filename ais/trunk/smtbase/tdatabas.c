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

#define _SMARTBASE
#if 0
TDatabase.c

Declaration of the Database CLASS which manages the General Object Repository. 
The General Object Repository supports the association of persistent objects
of an arbitrary nature with a key value of an arbitrary nature..


PARENT:             TObject 

AUTHORS:            Michael F. Korns

Version	Date		Who		Change
		12/15/2006	TMay	modified TDatabase_SizeOf to return record size including 
							record header for TVALs. 
#endif

#include "tdatabas.h"
#include "tdirect.h"
#include "fdatabas.h"
#include "tlambda.h"
#include "fproc.h"
#include "fconio.h"
#include "fconvert.h"
#include "futil3.h"
#include "futil2.h"
#include "futil1.h"
#include "fobject.h"
#include "fmake.h"
#include "tpair.h"
#include "fdatefnc.h"
#include "tbytevec.h"

							
/*--------------------------------------------------------------------------------------- */
#if 0
TDatabase_Init

Initialize the TDatabase class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TDatabase_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
EndFrame

/*  Don't initialize more than once. */

if (gCP->TDatabase_Initialized) return;
gCP->TDatabase_Initialized    = TRUE;
TObject_Init(gCP,gTP);

/*  Initialize the new type for this class. */
FSmartbase_NewType  (gCP,
					 gTP,
					 TYOBJREPOSITORY,
					(LpCHAR)"ObjectRepository",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TDatabase_MakeObjectRepository,
					&TDatabase_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TDatabase_SetIV1,
					&TDatabase_SetIV2,
					&TDatabase_SetIV3,
					&TDatabase_GetIV1,
					&TDatabase_GetIV2,
					&TDatabase_GetIV3,
					&TObject_MapDefault,
					&TObject_Mapc,
					&TObject_PrintDefault,
					&TDatabase_Load,
					&TDatabase_Save,
					&TDatabase_ComputeSize,
					&FObject_CopyNever,
					&TDatabase_Doomed);

FSmartbase_NewType  (gCP,
					 gTP,
					 TYREPOINSERT,
					(LpCHAR)"RepoInsert",
					_TObject_TfNATIVE,
					sizeof(REAL),
					(LpFNEW)&FConvert_ToInteger,
					&TObject_MarkNever,
					&TObject_GlobalMarkNever,
					&FObject_RealAnyCnv,
					&TObject_RealAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapObject,
					&TObject_MapcObject,
					&TObject_PrintDefault,
					&TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType  (gCP,
					 gTP,
				    TYFRAME,
					(LpCHAR)"DiskFrame",
					_TObject_TfNATIVE,
					sizeof(REAL),
					(LpFNEW)&FConvert_ToNumber,
					&TObject_MarkNever,
					&TObject_GlobalMarkNever,
					&FObject_RealAnyCnv,
					&TObject_RealAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapObject,
					&TObject_MapcObject,
					&TObject_PrintDefault,
					&TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

/* Note: In addition to the global functions registered below, see also "clear" and "length" */

FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"abortTransaction",(LpFUNC)&TDatabase_AbortTransaction);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isTransaction",(LpFUNC)&TDatabase_IsTransaction);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"beginTransaction",(LpFUNC)&TDatabase_BeginTransaction);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"commitTransaction",(LpFUNC)&TDatabase_CommitTransaction);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"checkPointTransaction",(LpFUNC)&TDatabase_CheckPointTransaction);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isImmediate",(LpFUNC)&TDatabase_isImmediate);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"saveImmediate",(LpFUNC)&TDatabase_SaveImmediate);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"rename",(LpFUNC)&TDatabase_Rename);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sizeof",(LpFUNC)&TDatabase_SizeOf);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"saveRepository",(LpFUNC)&TDatabase_SaveRepository);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"loadRepository",(LpFUNC)&TDatabase_LoadRepository);

gCP->TDatabase_MinBlockSize = (_MINPAGECOUNT * _PAGESIZE) + sizeof(FILEROOT);

FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
TDatabase_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TDatabase_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
StartFrame
DeclareOBJ(TDatabase,it);
DeclareOBJ(TStructure,Pv);
DeclareTVAL(retTval);
DeclareTVAL(tmp);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    /*  We must make sure the Load does not create two identical object repositories. */
    /*  If there already is a matching object repository, then we shall merge them here. */
    
	it = TDatabase_New(gCP,gTP);
	it->itsFileName = TString_MakeUnique(gCP,gTP,TDatabaseOnDiskPtr(aHMemory,0)->itsFileName);
	it->itsBaseFilePos = TObject_LoadTval(gCP,gTP,TDatabaseOnDiskPtr(aHMemory,0)->itsBaseFilePos).u.Int;
	it = TDatabase_UniqueObjectRepository(gCP,gTP,it);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TDatabase*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asObject(tmp) = (TObject*)it;
        asTag(tmp) = it->itsObjectType;

        it->itsFileName		= TString_MakeUnique(gCP,gTP,TDatabaseOnDiskPtr(aHMemory,0)->itsFileName);
		it->itsBufferCount	= TDatabaseOnDiskPtr(aHMemory,0)->itsBufferCount;
        it->itsCodeKey		= TObject_LoadTval(gCP,gTP,TDatabaseOnDiskPtr(aHMemory,0)->itsCodeKey);
        it->itsBaseFilePos	= TObject_LoadTval(gCP,gTP,TDatabaseOnDiskPtr(aHMemory,0)->itsBaseFilePos).u.Int;
        it->itsBufferIndex	= 0;
        it->itsBufferKeys	= NIL;
        it->itsBufferValues = NIL;
        it->itsIndex = NIL;
        it->itsOdbID = NIL;
        it->itsTransactionOn = FALSE;
            
        asTag(retTval) = it->itsObjectType;
        asObject(retTval) = (TObject*)it;
        }
    else
        *retTval = gCP->TObject_ERROR_INVALID;
    }

FrameExit(*retTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TDatabase_MakeObjectRepository

The new ObjectRepository: Procedure creates a new ObjecrRepository object.  

The arguments are as follows:

 fileName  The name of the database archive file to be associated with the ObjectRepository. 
      If no such file exists, a new database archive file will be created.

 tree:    [optional] If the key word tree: is present, the database archive file will be set
	  Tree indexing strategy on for all further processing.  If no such file exists, a new 
      database archive file will be created. After clearing, the database archive file 
	  will be set to Tree indexing strategy on for all further processing.

 tclear:  [optional] If the key word clear: is present, the database archive file will be 
      cleared immediately before any further processing.  If no such file exists, 
      a new database archive file will be created.

 clear:   [optional] If the key word clear: is present, the database archive file will be 
      cleared immediately before any further processing.  If no such file exists, 
      a new database archive file will be created.

 key: code  [optional] If the key word key: is present, a numeric encryption code to use in 
      record encrypting the database archive file must follow. This same encryption 
      key code must be used in all future references to the ObjectRepository. If the code
	  is the boolean value false, then no encryption or compression are used with this
	  Object Repository, and the code value false must always be used  in all future 
	  references to the ObjectRepository.

 buffer: count [optional] If the key word buffer: is present, the numeric buffered object count 
      must follow. The ObjectRepository will remember the last {count} objects retrieved 
      to minimize disk access when required. As same objects are retrieved, the buffered 
      object is returned and no disk access takes place. When a new object is retrieved, 
      the oldest buffered object is thrown away to make room for the newly retrieved object.

For example

 (new ObjectRepository: fileName  clear:  key:  code  buffer: count)   =>  makes the specified ObjectRepository .

#endif

TVAL TDatabase_MakeObjectRepository(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM             argIndex;
TVAL            databaseClose = TFUNCTION(FDatabas_Close);
TVAL            makeVector = TFUNCTION(FMake_Vector);
TVAL            appendCmd = TFUNCTION(FUtil2_Append);
CHAR			name[500];
PAGETABLE*		pageTable;
StartFrame
DeclareTVAL(savePath);
DeclareTVAL(ec);
DeclareTVAL(odbID);
DeclareTVAL(tmp1);
DeclareOBJ(TDatabase,self);
DeclareOBJ(TDatabase,anObjRepo);
DeclareTVAL(ret);
DeclareTVAL(pathName);
DeclareTVAL(db);
DeclareTVALArray(prmv,4);
EndFrame

/*  There must be at least one argument and no more than six. */

if ((argc < 1) || (argc > 6))
 {
 FrameExit(gCP->TObject_ERROR_INVALID);
 }

/* Create the new ObjectRepository object. */
/* Note: If the object is not properly created, */
/*       we will never return to this location. */
/*       We will be thrown back to the main loop. */

self = TDatabase_New(gCP,gTP);
ret->u.Object = (TObject*)self;
ret->Tag = self->itsObjectType;
*db = *ret;

/*  Set up the database archive disk file name. */
/*  Note: The file name is appended to the system */
/*        path (_path) to give the full file name */

if ((argv[0].Tag == TYSTRING) || (argv[0].Tag == TYTEXT) || (argv[0].Tag == TYSTRINGSUBSTR))
    {
	*pathName = FConio_pathName(gCP, gTP, sizeof(name)-1, name, gCP->TSymbol_Path->itsGlobalValue, argv[0]);
    ExitOnError(*pathName);
    *pathName = FSmartbase_Eval(gCP,gTP,appendCmd,2,*pathName,argv[0]);
    ExitOnError(*pathName);
	switch (pathName->Tag)
		{
		case TYTEXT:
			self->itsFileName = TString_MakeUnique(gCP,gTP,&pathName->u.Text[0]);
			break;

		case TYSTRING:
			self->itsFileName = pathName->u.String;
			break;

		default:
			FrameExit(gCP->TObject_ERROR_INVALID);
			break;
		}
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID);
    }

/*  Check for any other ObjectRepository objects linked to */
/*  the same full database archive full path and file name. */
/*  Note: There can only be one ObjectRepository object linked */
/*        to any database archive disk file. If we find a */
/*        duplicate, we return the duplicate ObjectRepository, */
/*        but only after we abort any transactions which may be */
/*        currently in progress on the duplicate ObjectRepository, */
/*        and reset the buffering settings to no buffering. */

self = TDatabase_UniqueObjectRepository(gCP,gTP,self);  
self->itsBufferCount = 0;
self->itsBufferKeys = NIL;
self->itsBufferValues = NIL;

ret->u.Object = (TObject*)self;
ret->Tag = self->itsObjectType;
*db = *ret;

/*  There may be 0, to 6 remaining arguments. */

argIndex = 1;

while (argIndex < argc)
 {
 switch(argv[argIndex].Tag)
     {
     case TYSYMBOL:
     case TYQUOTEDSYMBOL:
     /* Manage the tree: option, which opens a database archive disk file and sets Tree indexing on if no database file exists. */
     if (strcmp(SymbolArray(argv[argIndex]),"tree") == 0)
      {
       *savePath = gCP->TSymbol_Path->itsGlobalValue;
       gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
       gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	   prmv[0] = TINT(self->itsBaseFilePos);
	   prmv[1] = TOBJ(self->itsFileName);
	   prmv[2] = TSYMBOL("tree");
       *odbID = FDatabas_Open(gCP,gTP,3,prmv);
       gCP->TSymbol_Path->itsGlobalValue = *savePath;
       ExitOnError(*odbID);
	   if (odbID->Tag != TYBYTEVECTOR) {FrameExit(gCP->TObject_ERROR_INVALID);} else {self->itsOdbID = odbID->u.ByteVector;}
	   self->itsIndex = TDirectory_MakeNew(gCP,gTP,0,NIL).u.Directory;
       self->itsTransactionOn = TRUE;
	   pageTable = (PAGETABLE*)self->itsOdbID->itsByteArray;
	   pageTable->TreeIndexOn = TRUE;
	   pageTable->IAmDirty = TRUE;
       argIndex++;
      }
     else
     /* Manage the tclear: option, which creates a new database archive disk file and sets Tree indexing on. */
     if (strcmp(SymbolArray(argv[argIndex]),"tclear") == 0)
      {
       *savePath = gCP->TSymbol_Path->itsGlobalValue;
       gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
       gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	   prmv[0] = TINT(self->itsBaseFilePos);
	   prmv[1] = TOBJ(self->itsFileName);
	   prmv[2] = TSYMBOL("bnew");
       *odbID = FDatabas_Open(gCP,gTP,3,prmv);
       gCP->TSymbol_Path->itsGlobalValue = *savePath;
       ExitOnError(*odbID);
	   if (odbID->Tag != TYBYTEVECTOR) {FrameExit(gCP->TObject_ERROR_INVALID);} else {self->itsOdbID = odbID->u.ByteVector;}
	   self->itsIndex = TDirectory_MakeNew(gCP,gTP,0,NIL).u.Directory;
       self->itsTransactionOn = TRUE;
	   pageTable = (PAGETABLE*)self->itsOdbID->itsByteArray;
	   pageTable->TreeIndexOn = TRUE;
	   pageTable->IAmDirty = TRUE;
       argIndex++;
      }
     else
     /* Manage the clear: option, which creates a new database archive disk file. */
     if (strcmp(SymbolArray(argv[argIndex]),"clear") == 0)
      {
       *savePath = gCP->TSymbol_Path->itsGlobalValue;
       gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
       gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	   prmv[0] = TINT(self->itsBaseFilePos);
	   prmv[1] = TOBJ(self->itsFileName);
	   prmv[2] = TSYMBOL("new");
       *odbID = FDatabas_Open(gCP,gTP,3,prmv);
       gCP->TSymbol_Path->itsGlobalValue = *savePath;
       ExitOnError(*odbID);
	   if (odbID->Tag != TYBYTEVECTOR) {FrameExit(gCP->TObject_ERROR_INVALID);} else {self->itsOdbID = odbID->u.ByteVector;}
	   self->itsIndex = TDirectory_MakeNew(gCP,gTP,0,NIL).u.Directory;
       self->itsTransactionOn = TRUE;
	   pageTable = (PAGETABLE*)self->itsOdbID->itsByteArray;
       argIndex++;
      }
     else
     /* Manage the key: option, which specifies record encryption with a numeric key. */
     if (strcmp(SymbolArray(argv[argIndex]),"key") == 0)
      {
      if (argc <= (argIndex++))
        {
        FrameExit(gCP->TObject_ERROR_INVALID);
        }
      if (isNumIndex(&argv[argIndex]))
        self->itsCodeKey = argv[argIndex];
      else
      if (argv[argIndex].Tag == TYBOLE)
        self->itsCodeKey = argv[argIndex];
      else
        self->itsCodeKey = TBOOL(TRUE);
      argIndex++;
      }
     else
     /* Manage the buffer: option, which specifies the count of items to buffer. */
     if (strcmp(SymbolArray(argv[argIndex]),"buffer") == 0)
      {
      if (argc <= (argIndex++))
        {
        FrameExit(gCP->TObject_ERROR_INVALID);
        }
      if (argv[argIndex].Tag == TYNUM)
        self->itsBufferCount = argv[argIndex].u.Int;
      else
      if ((argv[argIndex].Tag == TYREAL) || (argv[argIndex].Tag == TYFRAME))
        self->itsBufferCount = argv[argIndex].u.Real;
      else
        {
        FrameExit(gCP->TObject_ERROR_INVALID);
        }
      argIndex++;
      if (self->itsBufferCount < 0)
        self->itsBufferCount = 0;
      }
     break;
     
     default:
         FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
     }
    }

/* Record the indexing strategy used */

*ec = FDatabas_LoadIndexStrategy(gCP,gTP,self);

/* Create the buffer object vector for buffering retrieved items. */

if (self->itsBufferCount > 0)
 {
 *ec = FSmartbase_Eval(gCP,gTP,makeVector,1,TINT(self->itsBufferCount));
 ExitOnError(*ec);
 self->itsBufferKeys = ec->u.Vector;
 *ec = FSmartbase_Eval(gCP,gTP,makeVector,1,TINT(self->itsBufferCount));
 ExitOnError(*ec);
 self->itsBufferValues = ec->u.Vector;
 }


/* Close the archive database disk file (if necesary). */

if (self->itsTransactionOn == TRUE)
 {
 self->itsTransactionOn = FALSE;
 *ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
 if (isERROR(ec))
	{
	TDatabase_AbortTransaction(gCP,gTP,1,db);
	FrameExit(*ec);
	}
 *tmp1 = TSYMBOL("commit");
 *ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp1);
 ExitOnError(*ec);
 self->itsOdbID = NIL;
 self->itsIndex = NIL;
 }


FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SaveRepository

Overview				

The saveRepository function saves a new child repository into the specified parent
ObjectRepository and associates the child respository with the specified retrieval key. 
The saveRepository function allows multiple levels of nested indices within a parent 
ObjectRepository, because even child repositories may contain further nested child repositories. 
Furthermore, each nested child repository may be accessed independent of either the parent 
repository or any of the other child repositories.

Type:	 	Function

Syntax:		(saveRepository  aGor  aDirectory)
			(saveRepository  aGor  key  size)
			(saveRepository  aGor  key  aDirectory)
			(saveRepository  aGor  key  aDirectory overflow)

When To Use		

__________________________________________________________________
Arguments		Explanation								                           
aGor			The ObjectRepository into which a new child ObjectRepository is to be saved.
key				The	key of the new child ObjectRepository.
size			The number of bytes reserved for the new child ObjectRepository.
Returns			This function returns the ObjectRepository object for the newly inserted child ObjectRepository.
__________________________________________________________________
Arguments		Explanation								                           
aGor			The ObjectRepository into which a new child ObjectRepository is to be saved.
key				The key of the new child ObjectRepository.
aDirectory		A Directory, of objects, which is to be saved in the new child respository.
Returns			This function returns the ObjectRepository object for the newly inserted child ObjectRepository.
__________________________________________________________________
Arguments		Explanation								                           
aGor			The ObjectRepository into which a new child ObjectRepository is to be saved.
key				The key of the new child ObjectRepository.
aDirectory		A Directory, of objects, which is to be saved in the new child respository.
overflow		The additional percentage to be reserved for overflow in the new child ObjectRepository.
Returns			This function returns the ObjectRepository object for the newly inserted child ObjectRepository.
__________________________________________________________________
  
  
Example1		

	(setq  repo  (new ObjectRepository:  "phrases.db"))
	
	(setq  repo["one"] "Hello world")
	
	repo["one"]			Returns "Hello world"
	
	(saveRepository  repo  Terminators:  10000)
	
	(setq  repo.Terminators["one"]  "Goodbye world")
	
	repo["one"]			Returns "Hello world"
	
	repo.Terminators["one"]	Returns "Goodbye world"

After the saveRepository, the we have a nested index of conversation terminator phrases.
This is a separate nested child repository from the parent repository.

Note:	Child repositories can be imbedded only when the parent repository is using compression.

#endif

TVAL TDatabase_SaveRepository(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BOLE		mustCloseOnExit = FALSE;
BOLE		childRepository = FALSE;
BOLE		blockSave = FALSE;
TDatabase*	self;
TVAL		databaseOpen = TFUNCTION(FDatabas_Open);
TVAL		databaseClose = TFUNCTION(FDatabas_Close);
TVAL		databaseFree = TFUNCTION(FDatabas_Free);
TVAL		databaseNewFrameID = TFUNCTION(FDatabas_NewFrameID);
TVAL		member = TFUNCTION(FUtil3_Member);
NUM			vmPageTablePos;
REAL		frameID;
NUM			newBlockSize;
NUM			newPageSize;
NUM			ioerr;
REAL		newOverFlow;
PAGETABLE*	pageInfo;
PAGETABLE*	newInfo;
StartFrame
DeclareOBJ(TDatabase,insertRepo);
DeclareOBJ(TDirectory,theDirectory);
DeclareTVAL(theRepoInsert);
DeclareTVAL(theDiskPage);
DeclareTVAL(newValue);
DeclareTVAL(index1);
DeclareTVAL(odbID);
DeclareTVAL(fileID);
DeclareTVAL(selfTval);
DeclareTVAL(newRepo);
DeclareTVAL(theInsertFileID);
DeclareTVAL(savePath);
DeclareTVAL(vmfile);
DeclareTVAL(ec);
DeclareTVAL(tmp);
DeclareTVAL(ret);
EndFrame


/* Check for correct number of arguments */
if (argc < 2)
	{
	*ret = TERROR("!saveRepository: Not enough arguments!");
	FrameExit(*ret);
	}

if (argc > 4)
	{
	*ret = TERROR("!saveRepository: Too many arguments!");
	FrameExit(*ret);
	}

/* Check if first argument is an object repository */
if (argv[0].Tag != TYOBJREPOSITORY)
	{
	*ret = TERROR("!saveRepository: Expecting argument 1 to be an Object Repository!");
	FrameExit(*ret);
	}
*selfTval = argv[0];

/* Check if the argument types are valid */
if (argc == 2)
	{
	if (argv[1].Tag == TYDIRECTORY)
		{
		*selfTval = argv[0];
		*index1 = gCP->Tval_VOID;
		*newValue = argv[1];
		self = (TDatabase*)asObject(selfTval);
		newOverFlow = 1;
		childRepository = FALSE;
		}
	else
		{
		*ret = TERROR("!saveRepository: Expecting argument 2 to be a Directory object!");
		FrameExit(*ret);
		}
	}

if (argc == 3)
	{
	*selfTval = argv[0];
	*index1 = argv[1];
	*newValue = argv[2];
	self = (TDatabase*)asObject(selfTval);
	newOverFlow = 1;
	childRepository = TRUE;
	}
else

if (argc == 4)
	{ 
	if ((argv[2].Tag == TYDIRECTORY) && (isNumIndex(&argv[3])))
		{
		*selfTval = argv[0];
		*index1 = argv[1];
		*newValue = argv[2];
		self = (TDatabase*)asObject(selfTval);
		newOverFlow = asNumIndex(&argv[3]);
		childRepository = TRUE;
		}

	else
		if (argv[2].Tag != TYDIRECTORY)
			{
			*ret = TERROR("!saveRepository: saveRepository: Expecting argument 3 to be a Directory object!");
			FrameExit(*ret);
			}
		else
			{
			*ret = TERROR("!saveRepository: Expecting argument 4 to be a number!");
			FrameExit(*ret);
			}
	}

/* This operation invalid when Tree indexing is on */

if (self->itsTreeOn == TRUE)
	{
	FrameExit(TERROR("!saveRepository: invalid operation when Tree indexing on!"));
	}

*ret = gCP->Tval_VOID;

/*  Check for the key in the buffer (if it can be found).		*/
/*  Note:	If the key is found, then we return an error		*/
/*          message, because we cannot insert the repository.	*/

if (self->itsBufferCount > 0)
	{
	*ret = FSmartbase_Eval(gCP,gTP,member,2,*index1,TOBJ(self->itsBufferKeys));
	if (ret->Tag == TYNUM)
		{
		*ret = TERROR("!saveRepository: Cannot save a Repository with buffering in effect!");
		FrameExit(*ret);
		}
	}

/*  Retrieve the database archive index (if necessary). */

if (self->itsOdbID == NIL)
	{
	/* Open the database archive disk file. */
 
	*savePath = gCP->TSymbol_Path->itsGlobalValue;
	gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
	gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	*tmp = TSYMBOL("update");
	*odbID = FSmartbase_Eval(gCP,gTP,databaseOpen,3,TINT(self->itsBaseFilePos),TOBJ(self->itsFileName),*tmp);
	gCP->TSymbol_Path->itsGlobalValue = *savePath;
	ExitOnError(*odbID);
	if (odbID->Tag != TYBYTEVECTOR) {FrameExit(gCP->TObject_ERROR_INVALID);} else {self->itsOdbID = odbID->u.ByteVector;}
	*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
	ExitOnError(*ec);
  
	/* Make sure we close the object repository on exit. */

	mustCloseOnExit = TRUE;
	}

/* Calculate the numeric size (in bytes) of the soon to be saved repository. */

if isNumIndex(newValue)
	{
	/* Make sure the requested size (in bytes) of the saved */
	/* repository is large enough to hold a repository. */
	/* Note: Make sure the page size falls on an 8 bit page map boundary. */
	
	newPageSize = asNumIndex(newValue);
	newPageSize *= newOverFlow;
	if (newPageSize < gCP->TDatabase_MinBlockSize) newPageSize = gCP->TDatabase_MinBlockSize;
    newPageSize = (newPageSize + _PAGEROUND) / _PAGESIZE;
    newPageSize = (newPageSize + 7) / 8;
    newPageSize = newPageSize * 8 * _PAGESIZE;
	}
else
if (newValue->Tag == TYDIRECTORY)
	{
	/* Make sure the requested size (in bytes) of the saved */
	/* repository is large enough to hold a repository. */
	/* Note: Make sure the page size falls on an 8 bit page map boundary. */

	blockSave = TRUE;
	*newRepo = TDatabase_SaveBlock(gCP,gTP,*newValue,newOverFlow,&newBlockSize);
	ExitOnError(*newRepo);
	insertRepo = newRepo->u.Repository;
	insertRepo->itsCodeKey = self->itsCodeKey;
	newInfo = FDatabas_GetPageTable(gCP,gTP,TOBJ(insertRepo->itsOdbID));
	newPageSize = newInfo->FileSize;
	}
else
	{
	*ret = TERROR("!saveRepository: Expecting argument 3 to be a Number or Directory!");
	FrameExit(*ret);
	}

/* Reserve space for and clear the repository before saving. */

if (childRepository)
	{
	/*  Read the database disk page number from the ObjectRepository Directory, */
	/*  and save the persistent object to the database archive index immediate. */

	*theDiskPage = TDatabase_RefIndex(gCP,gTP,*selfTval,*index1);

	/* Free the old repository page (if necessary). */

	if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
		{
		*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
		if (isERROR(ec))
			{
			TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
			FrameExit(*ec);
			}
		}

	/*	We now must reserve a disk page large enough to hold the inserted repository.	*/
	/*	Note:	This only reserves a disk page, it does not initialize the repository.	*/

	*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseNewFrameID,2,TOBJ(self->itsOdbID),TINT(newPageSize));
	if (isERROR(ec))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
		FrameExit(*ec);
		}

	/*	Get the disk frame information. */

	frameID = theDiskPage->u.Real;
	pageInfo = FDatabas_GetPageTable(gCP,gTP,TOBJ(self->itsOdbID));
	if (pageInfo == NULL)
		{
		*ret = TERROR("!saveRepository: Unable to reserve a disk page to hold the inserted repository!");
		FrameExit(*ret);
		}
	*theRepoInsert = *theDiskPage;
	theRepoInsert->Tag = TYREPOINSERT;

	/*	We now must initialize disk page as an inserted repository. */
	/*	Note:	This stamps the disk page so it is initialized as a repository. */

	if (!blockSave)
		{
		*tmp = TSYMBOL("new");
		*theInsertFileID = FSmartbase_Eval(gCP,gTP,databaseOpen,3,*theRepoInsert,TOBJ(self->itsFileName),*tmp);
		ExitOnError(*theInsertFileID);
		theDirectory = TDirectory_New(gCP,gTP);
		*ec = FDatabas_SaveIndex(gCP,gTP,theInsertFileID->u.ByteVector,TOBJ(theDirectory),self->itsCodeKey);
		if (isERROR(ec))
			{
			TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
			FrameExit(*ec);
			}
		*tmp = TSYMBOL("commit");
		*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,*theInsertFileID,*tmp);
		if (isERROR(ec))
			{
			TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
			FrameExit(*ec);
			}
		}
	else
	/* We must write the block virtual memory file to the repository disk page. */
	/* Note: First we must reset the baseFilePos and the FileSize to reflect */
	/*       the settings as they would normally be set for a child repository. */
		{
		/*	Adjust the virtual memory file settings for a child repository. */

		*fileID = self->itsOdbID->itsCdr;
		*vmfile = insertRepo->itsOdbID->itsCdr;
		newInfo = FDatabas_GetPageTable(gCP,gTP,TOBJ(insertRepo->itsOdbID));
		if (newInfo == NULL) 
			{
			*ret = TERROR("!saveRepository: Unable to reserve a disk page of the requested size!");
			FrameExit(*ret);
			}

		vmPageTablePos = newInfo->PageTableFPos;
		newInfo = (PAGETABLE*)&ByteArray(*vmfile)[vmPageTablePos];
		newInfo->baseFilePos = FDatabas_FrameIDToFPos(gCP,gTP,frameID);
		newInfo->FileSize = FDatabas_FrameIDToFSize(gCP,gTP,frameID);

		/*	Save the virtual memory file into the repository disk file. */

		*fileID = self->itsOdbID->itsCdr;
		*vmfile = insertRepo->itsOdbID->itsCdr;
		ioerr = FDatabas_writef(gCP, gTP,*fileID,newInfo->baseFilePos,0,newBlockSize,&ByteArray(*vmfile)[0]);
		if (ioerr != 0)
			{
			FrameExit(gTP->FDatabas_IOerr);
			}
		}

	/*	We now must mark the parent index as holding the inserted repository. */

	theDiskPage->Tag = TYREPOINSERT;
	*ec = TDatabase_SetInIndexOnly(gCP,gTP,*selfTval,*index1,*theDiskPage);
	if (isERROR(ec))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
		FrameExit(*ec);
		}
	}
else
/* Are we saving a standalone repository as a block? */

if (blockSave)
	{
	/* Clear the existing repository */

	TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
	*ec = TDatabase_Clear(gCP,gTP,1,selfTval);
	ExitOnError(*ec);
	TDatabase_BeginTransaction(gCP,gTP,1,selfTval);

	/* Save the virtual memory file into the repository disk file. */

	*fileID = self->itsOdbID->itsCdr;
	*vmfile = insertRepo->itsOdbID->itsCdr;
	ioerr = FDatabas_writef(gCP, gTP,*fileID,0,0,newBlockSize,&ByteArray(*vmfile)[0]);
	if (ioerr != 0)
		{
		FrameExit(gTP->FDatabas_IOerr);
		}
	
	/* We abort the transaction here, because we do not wish to overlay the file image, */
	/* we just wrote directly, with an empty respository index from the previous clear. */
	
	TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
	FrameExit(*selfTval);
	}
else
	/* Make sure the requested size (in bytes) of the saved */
	/* repository is large enough to hold a repository. */
	{
	*ret = TERROR("!saveRepository: Unable to reserve a disk page of the requested size!");
	FrameExit(*ret);
	}
/*  Close the database archive index (if necessary). */

if (mustCloseOnExit == TRUE)
	{
	/* Close the database archive disk file. */
 
	*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
	if (isERROR(ec))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
		FrameExit(*ec);
		}
	*tmp = TSYMBOL("commit");
	*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
	ExitOnError(*ec);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
 	}


FrameExit(*selfTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SaveBlock

The SaveBlock function saves the Directory object in a virtual memory Database. 
Think of the SaveBlock function as a block write of all repository objects at once.
The completed virtual memory Database (as an ObjectRepository) is returned ready for
writing to the disk.

#endif

TVAL TDatabase_SaveBlock(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theBlock,REAL overFlow,LpNUM blockSize)
{
NUM			baseFileSize = _INITPAGECOUNT * _PAGESIZE;
TVAL		databaseClose = TFUNCTION(FDatabas_Close);
TVAL		databaseSave = TFUNCTION(FDatabas_Save);
NUM			anIndex;
PAGETABLE*	pageInfo;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareOBJ(TDirectory,theDirectory);
DeclareTVAL(theDiskPage);
DeclareTVAL(selfTval);
DeclareTVAL(ec);
DeclareTVAL(tmp);
EndFrame

/*  Check and retrieve all of the insertRepository arguments. */

if (theBlock.Tag != TYDIRECTORY)
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
	{
	self = TDatabase_New(gCP,gTP);
	selfTval->u.Repository = self;
	selfTval->Tag = TYOBJREPOSITORY;
	*blockSize = 0;
	}

/*  Create a virtual memory Object Repository of the proper size. */
/*  Retrieve the database archive index (if necessary). */
/*  Open the database archive virtual memory file. */

*ec = FDatabas_NewMF(gCP,gTP,baseFileSize);
ExitOnError(*ec);
if (ec->Tag != TYBYTEVECTOR) {FrameExit(gCP->TObject_ERROR_INVALID);} else {self->itsOdbID = ec->u.ByteVector;}
self->itsTransactionOn = TRUE;

/*  Copy the block Directory, so the block can be written with all its values */
/*  bound with their proper retrieval keys in the ObjectRepository Directory. */

self->itsIndex = (TDirectory*)TDirectory_Copy(gCP,gTP,theBlock);

/*  Save each value object in the ObjectRepository Directory, */

for (anIndex = 0; anIndex < ((TDirectory*)self->itsIndex)->itsMaxItemIndex; anIndex++)
	{
	*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),PBindArray(TOBJ(self->itsIndex))[anIndex].Value,gCP->Tval_VOID,self->itsCodeKey);
	if (isERROR(theDiskPage))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,(TVAL*)&selfTval);
		FrameExit(*theDiskPage);
		}
	theDiskPage->Tag = TYFRAME;
	PBindArray(TOBJ(self->itsIndex))[anIndex].Value = *theDiskPage;
	}


/* Adjust the page table size to reflect an even 8 bit page boundary. */

pageInfo = FDatabas_GetPageTable(gCP,gTP,TOBJ(self->itsOdbID));
if (pageInfo == NULL) FrameExit(TERROR("!saveRepository: Unable to save the Directory Object!"));
FDatabas_AdjustPageTable(gCP, gTP, TOBJ(self->itsOdbID),(NUM)((REAL)pageInfo->UsedFileBytes * 1.25 * overFlow));

/* Close the database archive virtual memory file. */
/* Note: We must do this so that we grow the file properly. */

*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
if (isERROR(ec))
	{
	TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
	FrameExit(*ec);
	}
*tmp = TSYMBOL("commit");
*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
ExitOnError(*ec);
self->itsTransactionOn = FALSE;

/* Return the final virtual file size and the resulting repository. */
/* Note: This virtual file is closed as a standalone repository, */
/*       If we wish to save it as a child repository, we will have */
/*       to reset the baseFilePos and the FileSize. */

pageInfo = FDatabas_GetPageTable(gCP,gTP,TOBJ(self->itsOdbID));
if (pageInfo == NULL) FrameExit(TERROR("!saveRepository: Unable to save the Directory Object!"));
*blockSize = pageInfo->FileSize;
FrameExit(*selfTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
LoadRepository

Overview
				
The loadRepository function loads every object in the specified ObjectRepository. 
The repository is returned as a Directory with each loaded object associated with its
retrieval key. The loadRepository function allows an entire ObjectRepository to be read 
into memory at once. Think of the loadRepository function as a block read of all repository 
objects at once.

Type:	 	Function

Syntax:		(loadRepository  aGor)

When To Use		

__________________________________________________________________
Arguments		Explanation								                           
aGor			The ObjectRepository which is to be loaded into 
				RAM memory.
Returns			This function returns a Directory with each loaded 
				object associated with its retrieval key.
__________________________________________________________________

  
Example1		

(setq  repo  (new ObjectRepository:  "phrases.db"))

(setq  repo["one"] "Hello world")

repo["one"]			Returns "Hello world"

repo["two"]			Returns "Goodbye world"

(setq memGor (loadRepository  repo))

mem["one"]			Returns "Hello world"

mem["two"]			Returns "Goodbye world"

After the loadRepository, the entire contents of the repository have been loaded 
off the disk and stored in memory. The Directory in mem contains each retrieval 
key and its associated loaded object.

#endif

TVAL TDatabase_LoadRepository(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BOLE		mustCloseOnExit = FALSE;
TDatabase*	self;
TVAL		databaseOpen = TFUNCTION(FDatabas_Open);
TVAL		databaseClose = TFUNCTION(FDatabas_Close);
NUM			anIndex;
StartFrame
DeclareOBJ(TDirectory,theDirectory);
DeclareTVAL(selfTval);
DeclareTVAL(savePath);
DeclareTVAL(ec);
DeclareTVAL(tmp);
DeclareTVAL(ret);

EndFrame

/*  Check and retrieve all of the insertRepository arguments. */

if ((argc == 1) && (argv[0].Tag == TYVOID))
	{
	FrameExit(argv[0]);
	}
else
if ((argc != 1) || (argv[0].Tag != TYOBJREPOSITORY))
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
	{
	*selfTval = argv[0];
	self = (TDatabase*)asObject(selfTval);
	}

/* This operation invalid when Tree indexing is on */

if (self->itsTreeOn == TRUE)
	{
	FrameExit(TERROR("!loadRepository: invalid operation when Tree indexing on!"));
	}

*ret = gCP->Tval_VOID;

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
  
	/* Make sure we close the object repository on exit. */

	mustCloseOnExit = TRUE;
	}

/*  Copy the ObjectRepository Directory, so the copy can be returned with all */
/*  saved objects bound with their retrieval keys in the copy Directory. */

ret->u.Object = TDirectory_Copy(gCP,gTP,TOBJ(self->itsIndex));
ret->Tag = TYDIRECTORY;

/*  Read each saved object in the ObjectRepository Directory, */
/*  and return the loaded objects in the copy Directory. */

for (anIndex = 0; anIndex < ((TDirectory*)self->itsIndex)->itsMaxItemIndex; anIndex++)
	{
	PBindArray(*ret)[anIndex].Value = FSmartbase_Ref(gCP,gTP,2,*selfTval,PBindArray(TOBJ(self->itsIndex))[anIndex].Key);
	}


/*  Close the database archive index (if necessary). */

if (mustCloseOnExit == TRUE)
	{
	/* Close the database archive disk file. */
 
	*tmp = TSYMBOL("refuse");
	*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
	ExitOnError(*ec);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
	}


FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TDatabase_UniqueObjectRepository

The UniqueObjectRepository function returns a unique ObjectRepository object.  

#endif

TDatabase* TDatabase_UniqueObjectRepository(LpXCONTEXT gCP,LpTHREAD gTP,TDatabase* self)        
{
NUM             anIndex;
StartFrame
DeclareTVAL(ec);
DeclareOBJ(TDatabase,anObjRepo);
DeclareTVAL(ret);
DeclareTVAL(fileName1);
DeclareTVAL(fileName2);
DeclareTVAL(db);

EndFrame

/*  Check for any other ObjectRepository objects linked to */
/*  the same full database archive full path and file name. */
/*  Note: There can only be one ObjectRepository object linked */
/*        to any database archive disk file. If we find a */
/*        duplicate, we return the duplicate ObjectRepository, */
/*        but only after we abort any transactions which may be */
/*        currently in progress on the duplicate ObjectRepository. */

for (anIndex = 0;anIndex < gCP->TObject_MaxObjectCount; ++anIndex)
    {
    if ((_TObject_ObjectFlag(anIndex) != _TObject_OfVOID) &&
        ((_TObject_ObjectByIndex(anIndex))->itsObjectType == TYOBJREPOSITORY))
        {
        anObjRepo = (TDatabase*)_TObject_ObjectByIndex(anIndex);
		*fileName1 = TOBJ(self->itsFileName);
		*fileName2 = TOBJ(anObjRepo->itsFileName);
        if ((self != anObjRepo) &&
            (FPredicate2_QuickCompare(gCP,gTP,fileName1, fileName2) == 0) &&
			(self->itsBaseFilePos == anObjRepo->itsBaseFilePos))
            {
            self = anObjRepo;
            ret->u.Object = (TObject*)self;
            ret->Tag = self->itsObjectType;
			*db = *ret;
            /*  Abort the old transaction if an old transaction is in progress. */
            *ec = TDatabase_AbortTransaction(gCP,gTP,1,ret);
			FrameExit(anObjRepo);
            }
        }
    }


FrameExit(self);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Clear

The  clear  Procedure erases the specified ObjectRepository database archive file (aGor) 
and forgets all objects saved in the ObjectRepository. An ObjectRepository which has been 
cleared is now empty, and may immediately be used to store new objects if desired.

Example

 (clear aGor)
 
#endif

TVAL    TDatabase_Clear(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL     databaseOpen = TFUNCTION(FDatabas_Open);
TVAL     databaseClose = TFUNCTION(FDatabas_Close);
TVAL     makeVector = TFUNCTION(FMake_Vector);
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(ec);
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVAL(savePath);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,4);

EndFrame

/* Invoke the doClear member function from the Lambda argument (if any). */

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
    {
    /* Invoke the doClear subfunction of the Lambda. */
    *indexProc = TLambda_GetPvMember(gCP,gTP,argv[0],"doClear");
    ExitOnError(*indexProc);
    *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,0,&prmv[0]);
    FrameExit(*ret);
    }
else
/*  There must be at least one argument and it must be an ObjectRepository. */

if ((argc != 1) || (argv[0].Tag != TYOBJREPOSITORY))
    {
    FrameExit(gCP->TObject_ERROR_INVALID);
    }

self = (TDatabase*)argv[0].u.Object;
ret->u.Object = (TObject*)self;
ret->Tag = self->itsObjectType;

/*  Can't clear while a transaction is in progress. */

if (self->itsOdbID != NIL)
    {
	*ret = TERROR("!clear: cannot clear Object Repository while transaction is in progress!");
    FrameExit(*ret);
    }

/* Create a new database archive disk file (which clears out all old data). */

*savePath = gCP->TSymbol_Path->itsGlobalValue;
gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
*tmp = TSYMBOL("new");
*ec = FSmartbase_Eval(gCP,gTP,databaseOpen,3,TINT(self->itsBaseFilePos),TOBJ(self->itsFileName),*tmp);
gCP->TSymbol_Path->itsGlobalValue = *savePath;
ExitOnError(*ec);
if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;};
self->itsIndex = TDirectory_MakeNew(gCP,gTP,0,NIL).u.Directory;
self->itsTransactionOn = TRUE;
    
/* Create the buffer object vector for buffering retrieved items. */

if (self->itsBufferCount > 0)
    {
    *ec = FSmartbase_Eval(gCP,gTP,makeVector,1,TINT(self->itsBufferCount));
    ExitOnError(*ec);
	if (ec->Tag != TYVECTOR) {self->itsBufferKeys = NIL;} else {self->itsBufferKeys = ec->u.Vector;};
    *ec = FSmartbase_Eval(gCP,gTP,makeVector,1,TINT(self->itsBufferCount));
    ExitOnError(*ec);
	if (ec->Tag != TYVECTOR) {self->itsBufferValues = NIL;} else {self->itsBufferValues = ec->u.Vector;};
    self->itsBufferIndex = 0;
    }

/* Close the archive database disk file (if necesary). */

if (self->itsOdbID != NIL)
    {
    /* Close the archive database disk file. */
    self->itsTransactionOn = FALSE;
    *ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
	if (isERROR(ec))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,ret);
		FrameExit(*ec);
		}
    *tmp = TSYMBOL("commit");
    *ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
    ExitOnError(*ec);
    self->itsOdbID = NIL;
    self->itsIndex = NIL;
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TDatabase_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval )
{
TDatabase*    self = (TDatabase*)asObject(&selfTval);

/*  Mark the TDatabase's items so they won't be garbage collected. */

TObject_MarkObj(gCP,gTP,(TObject*)self->itsFileName);
TObject_MarkObj(gCP,gTP,(TObject*)self->itsOdbID);
TObject_MarkObj(gCP,gTP,(TObject*)self->itsIndex);
TObject_MarkObj(gCP,gTP,(TObject*)self->itsBufferKeys);
TObject_MarkObj(gCP,gTP,(TObject*)self->itsBufferValues);
TObject_MarkTval(gCP,gTP,self->itsCodeKey);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Doomed

Garbage collection is about to delete this object. Dispose of the array data.

Note:   This method should only be called by mark and sweep garbage collection!
        This method warns the object that it is about to be deleted. Garbage
        collection first warns all the doomed objects, then it deletes all doomed
        objects.
        
        Do close any files and clean up anything necessary here.
        Do free any resources which you have allocated of which you are the sole owner.

        Do not send delete or dispose messages to any referenced objects,
        Let garbage collection do this.

        Do not delete or release any of your own storage here.
        Let garbage collection do this.

#endif

void    TDatabase_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval )
{
TDatabase*      self = (TDatabase*)asObject(&selfTval);
StartFrame

EndFrame

/* Close the archive database disk file (if necesary). */

if (self->itsOdbID != NIL)
 {
 /* Note: An Object Repository is about to be garbage collected, */
 /*       which has an active transaction in progress. This is */
 /*       clearly an error which we must let the user know about. */
 /*       Most often this error occurs when a child repository */
 /*       has been referenced, but not saved in a variable. */
 if (self->itsOdbID->itsCdr.Tag == TYNUM)
	 {
	 FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_REPOSITORY_GC);
	 }
 
 /* Reset the Object Repository before destroying. */

 self->itsTransactionOn = FALSE;
 self->itsOdbID = NIL;
 self->itsIndex = NIL;
 }

FrameReturn
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TDatabase_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

*aSize += SIZEOF_TDatabaseOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TDatabase_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
TDatabase*     self = (TDatabase*)asObject(&selfTval);
long           theOffset;
LpCHAR		   pName;

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TDatabaseOnDiskPtr(aHMemory,theOffset)->itsBufferCount  = self->itsBufferCount;
TDatabaseOnDiskPtr(aHMemory,theOffset)->itsCodeKey      = TObject_RegisterTval(gCP,gTP,self->itsCodeKey);
TDatabaseOnDiskPtr(aHMemory,theOffset)->itsBaseFilePos  = TObject_RegisterTval(gCP,gTP,TINT(self->itsBaseFilePos));

if (self->itsFileName != NIL)
	{
	pName = CharArray(TOBJ(self->itsFileName));
	}
else
	{
	pName = "";
	}
strcpy(TDatabaseOnDiskPtr(aHMemory,theOffset)->itsFileName,pName);

return(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TDatabase.

Note: We can only have one copy of the same Database object.

#endif

TObject*    TDatabase_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TDatabase,self);
EndFrame

self = (TDatabase*)asObject(&selfTval);
FrameExit((TObject*)self);
}


/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Associative Retrieval:
 
	Any object, previously stored in the specified ObjectRepository, may be retrieved, 
	using its index value. Both the index value and the stored value may be of arbitrary 
	complexity. If no object has been stored under the specified index value, then #void 
	is returned.

	Note:	If the index argument is a frame id (TYFRAME), then the object is loaded from
			the direct ObjectRepository frame reference specified.  

Arguments:

	aGor	The ObjectRepository which is to be referenced.
	index	The index value whose associated object is to be returned.

Return:

	result	The object previously associated with the index value (see the set procedure).

Example:

	Note:	SmartLisp compiles the lexical forms repo["two"] and repo.two both into the 
			following invocations of ref, (ref repo "two").

	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	(setq repo["one"] "Hello world")
	repo["one"]       ==> "Hello world"
	(setq repo.two "Goodbye world")
	repo.two        ==> "Goodbye world"
	(setq repo[34] "Not my world")
	repo[34]        ==> "Not my world"

#endif

TVAL TDatabase_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
TVAL            member = TFUNCTION(FUtil3_Member);
BOLE			mustClose = FALSE;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareOBJ(TDatabase,insertRepo);
DeclareTVAL(savePath);
DeclareTVAL(theItem);
DeclareTVAL(theIndex);
DeclareTVAL(ec);
DeclareTVALArray(prmv,4);

EndFrame

self = (TDatabase*)asObject(&selfTval);

/*  Check if the database archive index is in memory  */

if (self->itsBufferCount > 0)
    {
	/* Check if the key is in the archive index */
    *theIndex = FSmartbase_Eval(gCP,gTP,member,2,index1,TOBJ(self->itsBufferKeys));
    if (theIndex->Tag == TYNUM)
        {
        *theItem = FSmartbase_Ref(gCP,gTP,2,TOBJ(self->itsBufferValues),*theIndex);
        FrameExit(*theItem);
        }
    }
	
/*  Retrieve the database archive index (if necessary). */

if (self->itsOdbID == NIL)
    {
	/* Open the database archive disk file. */
 
	*savePath = gCP->TSymbol_Path->itsGlobalValue;
	gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
	gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	prmv[0] = TINT(self->itsBaseFilePos);
	prmv[1] = TOBJ(self->itsFileName);
	prmv[2] = TSYMBOL("update");
	*ec = FDatabas_Open(gCP,gTP,3,prmv);
	gCP->TSymbol_Path->itsGlobalValue = *savePath;
	ExitOnError(*ec);
	if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
	*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
	ExitOnError(*ec);
	mustClose = TRUE;
	}

*theItem = TDatabase_IndexedDiskLoad(gCP,gTP,selfTval,index1);

if (mustClose)
	{
	/* Close the database archive disk file. */

	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = TSYMBOL("refuse");
    FDatabas_Close(gCP,gTP,2,prmv);
    self->itsOdbID = NIL;
    self->itsIndex = NIL;
    self->itsTransactionOn = FALSE;
    }

FrameExit(*theItem);
}

/*--------------------------------------------------------------------------------------- */
/*
GetIV2

-----------------------
Paired Index Retrieval:
-----------------------
 
	Any object, previously stored in the specified ObjectRepository, may be retrieved, 
	using its numeric index. The index value must be an integer between 0 and the number 
	of objects currently stored in the ObjectRepository (see the length procedure). 
	The ObjectRepository always stores objects sorted by the order of their associated 
	index values (see the compareEQ procedure).

Arguments:

	repo	The ObjectRepository which is to be referenced.
	index	The numeric index value whose associated object is to be returned.
	switch	A numeric index switch which determines whether the key value, 
			or object value is to be referenced.

Return:

	result	If switch is 0, the key value will be returned. 
			If switch is 1, the associated object will be returned (see the set procedure).

Example:

	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	(setq repo["one"] "Hello world")
	(setq repo.two "Goodbye world")
	(setq repo[34] "Not my world")
	repo[0 0]       ==> 34
	repo[0 1]       ==> "Not my world"
	repo[1 0]       ==> "one"
	repo[1 1]       ==> "Hello world"
	repo[2 0]       ==> "two"
	repo[2 1]       ==> "Goodbye world"

Note:	The ObjectRepository stores all saved objects in ascending order by their 
		index key values (see the compareEQ procedure).
  
----------------------------
Archive major key Retrieval:
----------------------------
 
	At any time, the fully loaded Directory for the specified ObjectRepository's major index key value may be retrieved.

	Note: This form of retrieval is legal only when the repository has tree indexing mode set on.

Arguments:

	repo	The ObjectRepository which is to be referenced.
	tree	A symbolic index which must be set to the symbol, |tree|.
	index	An arbitrary major index key value which has previously been used to store
				a Directory object in the ObjectRepository (see the set procedure).

Return:

	result	The fully loaded Directory for the specified ObjectRepository's major index key value.

Example:

	(setq  repo (new ObjectRepository:  "myarchive.odb"  tree:))
		(setq repo[major: A:] #(dir| A "This is the first record" B "This is the second record"))
	repo[major: A:]  ==> #(dir| A "This is the first record" B "This is the second record"))
 
	Note: The ObjectRepository returns the fileID of its currently opened archive database file.
 
------------------------------------
Archive repository length Retrieval:
------------------------------------
 
	At any time, count of the ObjectRepository's database major index elements may be retrieved.

Arguments:

	repo	The ObjectRepository which is to be referenced.
	query	A symbolic index which must be set to the symbol, |query|.
	length	A symbolic index which must be set to the symbol, |length|.

Return:

	result	The count of the ObjectRepository's database major index elements.

Example:

	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	(setq  repo.A "This is a test roecrd")
	(setq  repo.B "This is another test roecrd")
	repo[query: length:]  ==> 2
 
	Note: The ObjectRepository returns the count of the ObjectRepository's database major index elements.
 
----------------------------
Archive file name Retrieval:
----------------------------
 
	At any time, the file name for the specified ObjectRepository's database archive file may be retrieved.

Arguments:

	repo	The ObjectRepository which is to be referenced.
	query	A symbolic index which must be set to the symbol, |query|.
	name	A symbolic index which must be set to the symbol, |name|.

Return:

	result	The file name of the ObjectRepository's database archive file.

Example:

	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	repo[query: name:]  ==> "myarchive.odb"
 
	Note: The ObjectRepository returns the fileID of its currently opened archive database file.
 
-----------------------
Archive odbID Retrieval:
-----------------------
 
	During the process of an active transaction, between a beginTransaction and before 
	a terminating abortTransaction or commitTransaction, the odbID for the currently 
	opened database archive file may be retrieved. This odbID may subsequently be used 
	with the low level databaseInspect procedures in the following chapter.
 
Arguments:
 
	repo	The ObjectRepository which is to be referenced.
	query	A symbolic index which must be set to the symbol, |query|.
	odbID	A symbolic index which must be set to the symbol, |odbid|.

Return:

	result	The odbID of the currently opened database archive file, 
			or #void if no transaction is in progress.

Example:

	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	(beginTransaction repo)
	repo[query: odbid:]     ==> 562946

Note: The ObjectRepository returns the odbID of its currently opened archive database file.

------------------------------------
Archive file Tree Status Retrieval:
------------------------------------
 
	At any time, the file name for the specified ObjectRepository's database archive file may be retrieved.

Arguments:

	repo	The ObjectRepository which is to be referenced.
	query	A symbolic index which must be set to the symbol, |query|.
	tree	A symbolic index which must be set to the symbol, |tree|.

Return:

	result	The Tree indexing status of the ObjectRepository's database archive file (true or false).

Example:

	(setq  repo  (new ObjectRepository:  "myarchive.odb" tree:))
	repo[query: tree:]  ==> true
 
	Note: The ObjectRepository returns the Tree indexing status of the ObjectRepository's database archive file (true or false).
 
-----------------------
Index Value Conversion:
-----------------------
 
	Any index value associated with any object, previously stored in the specified 
	ObjectRepository, may be converted into the numeric position of the stored object 
	in the ObjectRepository. Both the index value and the stored value may be of 
	arbitrary complexity. If no object has been stored under the specified index value, 
	then false is returned.

Arguments:

	aGor		The ObjectRepository which is to be referenced.
	position	A symbolic index which must be set to the symbol, |position|.
	index		A arbitrary index value which has previously been used to store an 
					object in the ObjectRepository (see the set procedure).

Return:

	result	Either the numeric position of specified index value in the ObjectRepository's 
			database archive file, or the boolean false, if no such index exists.

	(setq repo  (new ObjectRepository:  "myarchive.odb"))
	(setq repo["one"] "Hello world")
	(setq repo.two "Goodbye world")
	(setq repo[34] "Not my world")
	(setq repo[34] "Not my world")
	repo[0 0]       ==> 34
	repo[0 1]       ==> "Not my world"
	repo[1 0]       ==> "one"
	repo[1 0]       ==> "Hello world"
	repo[2 0]       ==> "two"
	repo[2 0]       ==> "Goodbye world"
	repo[position: 34]     ==> 0
	repo[position: "two"]    ==> 2
	repo[position: "three"]    ==> false

	Note:	The ObjectRepository stores all saved objects in ascending order by their 
			index key values (see the compareEQ procedure).

------------------
Direct Frame Read:
------------------
 
	Any object, previously stored in the specified ObjectRepository, may be directly read 
	from the frame id of the stored object in the ObjectRepository (thus bypassing the 
	repository index). The specified frame id must have been obtained from a previous
	repository save, see (setq aGor[frame: #void] record). 

	Warning:	If no object has been stored under the specified frame id value, or if the 
				specified frame id is invalid, then an error is returned.	

Arguments:

	aGor	The ObjectRepository which is to be referenced.
	member	A symbolic index which must be set to the symbol, |frame|.
	frame 	Either #void or a valid frame id obtained from a previous
			repository save.

Return:

	result	The fully loaded object, previously saved in the ObjectRepository, 
			at the specified frame location.

	(setq repo  (new ObjectRepository:  "myarchive.odb"))
	(setq frame1 (setq repo[frame: #void] "Hello world"))
	(setq frame2 (setq repo[frame: #void] "Goodbye world"))
	(setq repo[1] "Not my world")
	(setq repo[2] "Not your world")
	(setq frame3 (inspect repo frame: 2))
	
	repo[frame: frame1]    ==> "Hello world"
	repo[frame: frame2]    ==> "Goodbye world"
	repo[frame: frame3]    ==> "Not your world"

	(setq frame2 (setq repo[frame: frame2] "Goodnight world"))

	repo[frame: frame2]    ==> "Goodnight world"

	(setq frame2 (setq repo[frame: frame2] #void))

	;; frame2 is no longer valid.

	repo[frame: frame2]    ==> ...unpredictable results or an error message...


	Note:	Saving #void into the ObjectRepository frees the specified frame id 
			deletes the object previously saved at that frame location in the
			ObjectRepository.

*/

TVAL TDatabase_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2)
{
NUM		aSwitch;
BOLE	mustClose = FALSE;
NUM		numIndex;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(savePath);
DeclareTVAL(theItem);
DeclareTVAL(theIndex);
DeclareTVAL(theKey);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVALArray(prmv,4);
EndFrame

self = (TDatabase*)asObject(&selfTval);

/* **************************************************** */
/* Manage all major index requests.                     */
/* repo[major: index1]                                  */
/* **************************************************** */

if ((index1.Tag == TYSYMBOL) && (strcmp(SymbolArray(index1),"tree") == 0))
	{
	/* Manage a request for the major index key of a repository. */

	*ret = TDatabase_GetIV1(gCP,gTP,selfTval,index2);
	FrameExit(*ret);
	}


/* **************************************************** */
/* Manage all query requests.                           */
/* repo[query: length:]                                 */
/* repo[query: name:]                                   */
/* repo[query: odbid:]                                  */
/* repo[query: tree:]                                   */
/* **************************************************** */

if ((index1.Tag == TYSYMBOL) && ((strcmp(SymbolArray(index1),"query") == 0) || (strcmp(SymbolArray(index1),"member") == 0)))
	{
	/* Manage a request for the database archive file name. */

	if ((index2.Tag == TYSYMBOL) && (strcmp(SymbolArray(index2),"length") == 0))
		{
		*ret = FUtil2_Length(gCP,gTP,1,&selfTval);
		FrameExit(*ret);
		}
	else

	/* Manage a request for the database archive file name. */

	if ((index2.Tag == TYSYMBOL) && (strcmp(SymbolArray(index2),"name") == 0))
		{
		FrameExit(TOBJ(self->itsFileName));
		}
	else

	/* Manage a request for the database archive file object. */

	if ((index2.Tag == TYSYMBOL) && (strcmp(SymbolArray(index2),"odbid") == 0))
		{
		FrameExit(TOBJ(self->itsOdbID));
		}
	else

	/* Manage a request for the database archive file tree indexing strategy. */

	if ((index2.Tag == TYSYMBOL) && (strcmp(SymbolArray(index2),"tree") == 0))
		{
		FrameExit(TBOOL(self->itsTreeOn));
		}
	else

	/* All other requests are errors. */

		{
		FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
		}
	}


/* ******************************************************************* */
/* Manage a request for the position of a key in the ObjectRepository. */
/* repo[position: key:]                                                */
/* ******************************************************************* */

if ((index1.Tag == TYSYMBOL) && (strcmp(SymbolArray(index1),"position") == 0))
	{
	/*  Retrieve the database archive index (if necessary). */

	if (self->itsOdbID == NIL)
		{
		/* Open the database archive disk file. */

		*savePath = gCP->TSymbol_Path->itsGlobalValue;
		gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
		gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
		prmv[0] = TINT(self->itsBaseFilePos);
		prmv[1] = TOBJ(self->itsFileName);
		prmv[2] = TSYMBOL("update");
		*ec = FDatabas_Open(gCP,gTP,3,prmv);
		gCP->TSymbol_Path->itsGlobalValue = *savePath;
		ExitOnError(*ec);
		if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
		*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
		ExitOnError(*ec);

		/*  Read the position of the key in the ObjectRepository Directory. */

		*theIndex = TDatabase_PositionIndex(gCP,gTP,selfTval,index2);

		/* Close the database archive disk file. */

		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = TSYMBOL("refuse");
		FDatabas_Close(gCP,gTP,2,prmv);
		self->itsOdbID = NIL;
		self->itsIndex = NIL;
		self->itsTransactionOn = FALSE;
		FrameExit(*theIndex);
		}
	else
		{
		/*  Read the database disk page number from the ObjectRepository Directory. */

		*theIndex = TDatabase_PositionIndex(gCP,gTP,selfTval,index2);
		FrameExit(*theIndex);
		}
	}

/* ******************************************************************* */
/* Manage a read from the ObjectRepository by direct frame reference.  */
/* repo[frame: index:]                                                 */
/* ******************************************************************* */

if ((index1.Tag == TYSYMBOL) && (strcmp(SymbolArray(index1),"frame") == 0))
	{
	/*  Retrieve the database archive index (if necessary). */
	if (self->itsOdbID == NIL)
		{
		/* Open the database archive disk file. */

		*savePath = gCP->TSymbol_Path->itsGlobalValue;
		gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
		gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
		prmv[0] = TINT(self->itsBaseFilePos);
		prmv[1] = TOBJ(self->itsFileName);
		prmv[2] = TSYMBOL("update");
		*ec = FDatabas_Open(gCP,gTP,3,prmv);
		gCP->TSymbol_Path->itsGlobalValue = *savePath;
		ExitOnError(*ec);
		if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
		*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
		ExitOnError(*ec);

		/* Make sure we will close ObjectRepository after reading. */
		mustClose = TRUE;
		}

	/*  Make sure the second argument is a frame id. */
	if (index2.Tag == TYVOID) 
		{
		index2.Tag = TYFRAME;
		index2.u.Real = NILPAGEINDEX;
		}
	if ((index2.Tag != TYREAL) && (index2.Tag != TYFRAME))
		{
		FrameExit(TERROR("!ObjectRepository: expected frame id!"));
		}

	/*  Read the object from the frame id. */
	index2.Tag = TYFRAME;
	*ret = TDatabase_GetIV1(gCP,gTP,selfTval,index2);

	/* Do we need to close the database archive disk file? */
	if (mustClose)
		{
		/* Close the database archive disk file. */

		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = TSYMBOL("refuse");
		FDatabas_Close(gCP,gTP,2,prmv);
		self->itsOdbID = NIL;
		self->itsIndex = NIL;
		self->itsTransactionOn = FALSE;
		}

	/* Return the object read from the ObjectRepository frame. */
	FrameExit(*ret);
	}

/* ******************************************************************* */
/* Manage a read from the ObjectRepository by paired numeric indices.  */
/* repo[index1 0]		; retrieve the key value                       */
/* repo[index1 1]		; retrieve the data value                      */
/* ******************************************************************* */

if (!isNumIndex(&index1) || !isNumIndex(&index2))
	{
	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
 
aSwitch = asNumIndex(&index2);
if ((aSwitch < 0) || (aSwitch > 1))
	{
	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}

/*  Retrieve the database archive index (if necessary). */

if (self->itsOdbID == NIL)
	{
	/* Open the database archive disk file. */

	*savePath = gCP->TSymbol_Path->itsGlobalValue;
	gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
	gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	prmv[0] = TINT(self->itsBaseFilePos);
	prmv[1] = TOBJ(self->itsFileName);
	prmv[2] = TSYMBOL("update");
	*ec = FDatabas_Open(gCP,gTP,3,prmv);
	gCP->TSymbol_Path->itsGlobalValue = *savePath;
	ExitOnError(*ec);
	if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
	*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
	ExitOnError(*ec);

	/* Make sure we will close ObjectRepository after reading. */
	mustClose = TRUE;
	}

/*  Check the item key from the ObjectRepository Directory. */

numIndex = asNumIndex(&index1);
if ((numIndex < 0) || (numIndex >= ((TDirectory*)self->itsIndex)->itsMaxItemIndex))
	{
	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}

/*  Read the persistent object from the database archive disk file. */

if (aSwitch == 1)
	{
	*theKey = TDatabase_RefIndexKey(gCP,gTP,TOBJ(self),index1);
	*theItem = TDatabase_GetIV1(gCP,gTP,selfTval,*theKey);
	ExitOnError(*theItem);
	}
else
/*  Return the item key from the ObjectRepository Directory. */

	{
	*theKey = TDatabase_RefIndexKey(gCP,gTP,TOBJ(self),index1);
	*theItem = *theKey;
	}


/* Do we need to close the database archive disk file? */
if (mustClose)
	{
	/* Close the database archive disk file. */

	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = TSYMBOL("refuse");
	FDatabas_Close(gCP,gTP,2,prmv);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
	}

FrameExit(*theItem);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV3

The GetIV3 function only has meaning if the repository has tree indexing turned on.


#endif

TVAL TDatabase_GetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2, TVAL index3)
{
BOLE	mustClose = FALSE;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(savePath);
DeclareTVAL(treeMainIndex);
DeclareTVAL(treeNodeIndex);
DeclareTVAL(theNodeIndexDiskPage);
DeclareTVAL(theTerminalDiskPage);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVALArray(prmv,4);
EndFrame

self = (TDatabase*)asObject(&selfTval);

/* **************************************************** */
/* Manage all major index requests.                     */
/* repo[minor: index2 index3]                           */
/* **************************************************** */

if ((index1.Tag == TYSYMBOL) && (strcmp(SymbolArray(index1),"tree") == 0) && (self->itsTreeOn == TRUE))
	{
	/*  Retrieve the database archive index (if necessary). */

	if (self->itsOdbID == NIL)
		{
		/* Open the database archive disk file. */

		*savePath = gCP->TSymbol_Path->itsGlobalValue;
		gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
		gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
		prmv[0] = TINT(self->itsBaseFilePos);
		prmv[1] = TOBJ(self->itsFileName);
		prmv[2] = TSYMBOL("update");
		*ec = FDatabas_Open(gCP,gTP,3,prmv);
		gCP->TSymbol_Path->itsGlobalValue = *savePath;
		ExitOnError(*ec);
		if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
		*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
		ExitOnError(*ec);
		mustClose = TRUE;
		}

	/*  Read the database disk page number from the ObjectRepository Directory. */

	*treeMainIndex = TOBJ(self->itsIndex);
	*theNodeIndexDiskPage = TDirectory_GetIV1(gCP,gTP,*treeMainIndex,index2);
	if (theNodeIndexDiskPage->Tag != TYFRAME)
		{
		FrameExit(*theNodeIndexDiskPage);
		}

	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = TREAL(theNodeIndexDiskPage->u.Real);
	prmv[2] = self->itsCodeKey;
	*treeNodeIndex = FDatabas_Load(gCP,gTP,3,prmv);
	ExitOnError(*treeNodeIndex);
	if (treeNodeIndex->Tag != TYDIRECTORY)
		{
		FrameExit(TERROR("!repository.IndexedDiskRead: invalid node Tree repository index!"));
		}

	*theTerminalDiskPage = TDirectory_GetIV1(gCP,gTP,*treeNodeIndex,index3);
	if (theTerminalDiskPage->Tag != TYFRAME)
		{
		FrameExit(*theTerminalDiskPage);
		}

	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = TREAL(theTerminalDiskPage->u.Real);
	prmv[2] = self->itsCodeKey;
	*ret = FDatabas_Load(gCP,gTP,3,prmv);
	ExitOnError(*ret);


	/* Close the database archive disk file. */

	if (mustClose)
		{
		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = TSYMBOL("refuse");
		FDatabas_Close(gCP,gTP,2,prmv);
		self->itsOdbID = NIL;
		self->itsIndex = NIL;
		self->itsTransactionOn = FALSE;
		}

	/* Manage a request for the major index key of a repository. */

	FrameExit(*ret);
	}

FrameExit(gCP->TObject_ERROR_ACCESSDENIED);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Associative Storage:
 
	Any object (must respond true to the isObject predicate) may be stored in the 
	ObjectRepository and associated with an index value. Both the index value and 
	the stored value may be of arbitrary complexity. An object may be removed from 
	the ObjectRepository by storing the value #void in association with its previous 
	index value.

Arguments:

	aGor	The ObjectRepository into which the object is to be stored.
	index	The index value to be associated with the object for later retrieval.

Return:
 
	result	The position of the object in the ObjecrRepository (see the ref procedure).

Example:

	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	(setq repo["one"] "Hello world")
	repo["one"]		==> "Hello world"
	(setq repo.two "Goodbye world")
	repo.two			==> "Goodbye world"
	(setq repo[34] "Not my world")
	repo[34]			==> "Not my world"

Note:	SmartLisp compiles the lexical forms repo["two"] and repo.two both into the following 
		invocations of ref, (ref repo "two"). The lexical form (setq repo["two"] "Goodbye world") 
		is compiled into the following invocation of set, (set repo "two"  "Goodbye world").

#endif

TVAL TDatabase_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
BOLE   mustClose = FALSE;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theIndex);
DeclareTVAL(savePath);
DeclareTVAL(ec);
DeclareTVAL(ret);
DeclareTVALArray(prmv,4);

EndFrame

self = selfTval.u.Repository;
*ret = selfTval;

/*  Check for the key in the buffer (if it can be found). */

if (self->itsBufferCount > 0)
	{
	prmv[0] = index1;
	prmv[1] = TOBJ(self->itsBufferKeys);
	*theIndex = FUtil3_Member(gCP,gTP,2,prmv);
	if (theIndex->Tag == TYNUM)
		{
		*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsBufferValues),*theIndex,newValue);
		ExitOnError(*ec);
		}
	else
		{
		*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsBufferKeys),TINT(self->itsBufferIndex),index1);
		ExitOnError(*ec);
		*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsBufferValues),TINT(self->itsBufferIndex),newValue);
		ExitOnError(*ec);
		if (++self->itsBufferIndex >= self->itsBufferCount)
			self->itsBufferIndex = 0;
		}
	}

/*  Retrieve the database archive index (if necessary). */

if (self->itsOdbID == NIL)
	{
	/* Open the database archive disk file. */
 
	*savePath = gCP->TSymbol_Path->itsGlobalValue;
	gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
	gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	prmv[0] = TINT(self->itsBaseFilePos);
	prmv[1] = TOBJ(self->itsFileName);
	prmv[2] = TSYMBOL("update");
	*ec = FDatabas_Open(gCP,gTP,3,prmv);
	gCP->TSymbol_Path->itsGlobalValue = *savePath;
	ExitOnError(*ec);
	if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
	*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
	ExitOnError(*ec);
	mustClose = TRUE;
	}

/*  Read the database disk page number from the ObjectRepository Directory, */
/*  and save the persistent object to the database archive disk file. */

*ec = TDatabase_IndexedDiskSave(gCP,gTP,selfTval,index1,newValue); 
if (isERROR(ec))
	{
	TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
	*ret = *ec;
	}

if (mustClose)
	{
	/* Close the database archive disk file. */
 
	*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
	ExitOnError(*ec);
	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = TSYMBOL("commit");
	FDatabas_Close(gCP,gTP,2,prmv);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
	}

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetIV2

------------------
Major Key Storage:
------------------
 
	Any Directory object may be stored in the repository under a major key value (the keys in the Directory then become the minor keys).

	Note: this retrval form is legal only when the repository has tree indexing mode turned on.

Arguments:

	repo		The ObjectRepository into which the object is to be stored.
	tree		The key word symbol |tree|.
	index2		The major key value.
	newValue	The Directory value to be saved in the ObjectRepository, under the specified major key value, for later retrieval.

Return:
 
	result		The valid frame id of the object in the ObjectRepository (see the ref procedure).

Example:

	(setq repo  (new ObjectRepository:  "myarchive.odb"  tree:))
	(setq repo[major: Test:] #{dir| A "This is record one" B "This is record two"}))
	
	repo[major: Test:]     ==> #{dir| A "This is record one" B "This is record two"}
	repo[minor: Test: B:]  ==> "This is record two"

	Note:	Saving #void into the ObjectRepository frees the specified frame id 
			deletes the object previously saved at that frame location in the
			ObjectRepository.

---------------------
Direct Frame Storage:
---------------------
 
	Any object (must respond true to the isObject predicate) may be stored in the 
	ObjectRepository directly into a frame id. The valid frame id is returned from 
	the save operation. The returned frame id may be used in subsequent reads of
	the ObjectRepository, see (ref repo frame: frameId).

Arguments:

	aGor		The ObjectRepository into which the object is to be stored.
	index1		The key word symbol |frame|.
	index2		Either #void or a valid frame id to be freed before saving.
	newValue	The value to be save in the ObjectRepository for later retrieval.

Return:
 
	result		The valid frame id of the object in the ObjectRepository (see the ref procedure).

Example:

	(setq repo  (new ObjectRepository:  "myarchive.odb"))
	(setq frame1 (setq repo[frame: #void] "Hello world"))
	(setq frame2 (setq repo[frame: #void] "Goodbye world"))
	(setq repo[1] "Not my world")
	(setq repo[2] "Not your world")
	(setq frame3 (inspect repo frame: 2))
	
	repo[frame: frame1]    ==> "Hello world"
	repo[frame: frame2]    ==> "Goodbye world"
	repo[frame: frame3]    ==> "Not your world"

	(setq frame2 (setq repo[frame: frame2] "Goodnight world"))

	repo[frame: frame2]    ==> "Goodnight world"

	(setq frame2 (setq repo[frame: frame2] #void))

	;; frame2 is no longer valid.

	repo[frame: frame2]    ==> ...unpredictable results or an error message...


	Note:	Saving #void into the ObjectRepository frees the specified frame id 
			deletes the object previously saved at that frame location in the
			ObjectRepository.

#endif

TVAL TDatabase_SetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2,TVAL newValue)
{
BOLE	mustClose = FALSE;	
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theDiskPage);
DeclareTVAL(savePath);
DeclareTVAL(ec);
DeclareTVAL(ret);
DeclareTVALArray(prmv,4);

EndFrame

self = (TDatabase*)asObject(&selfTval);

/* **************************************************** */
/* Manage all major index requests.                     */
/* repo[tree: index1]                                   */
/* **************************************************** */

if ((index1.Tag == TYSYMBOL) && (strcmp(SymbolArray(index1),"tree") == 0))
	{
	/* Manage a assign for the major index key of a repository. */

	*ret = TDatabase_SetIV1(gCP,gTP,selfTval,index2,newValue);
	FrameExit(*ret);
	}
else

/* **************************************************** */
/* Manage all frame index requests.                     */
/* repo[frame: index1]                                  */
/* **************************************************** */

if ((index1.Tag != TYSYMBOL) || (strcmp(SymbolArray(index1),"frame") != 0) ||
	((index2.Tag != TYREAL) && (index2.Tag != TYFRAME) && (index2.Tag != TYVOID)))
	{
	FrameExit(gCP->TObject_ERROR_BADINDEX);
	}

/*  Retrieve the database archive index (if necessary). */

if (self->itsOdbID == NIL)
	{
	/* Open the database archive disk file. */
 
	*savePath = gCP->TSymbol_Path->itsGlobalValue;
	gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
	gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
	prmv[0] = TINT(self->itsBaseFilePos);
	prmv[1] = TOBJ(self->itsFileName);
	prmv[2] = TSYMBOL("update");
	*ec = FDatabas_Open(gCP,gTP,3,prmv);
	gCP->TSymbol_Path->itsGlobalValue = *savePath;
	ExitOnError(*ec);
	if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
	*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
	ExitOnError(*ec);
	mustClose = TRUE;
	}

/*  Use the second index as the database disk frame id from the ObjectRepository, */
/*  and save the persistent object to the database archive disk file. */

if (index2.Tag == TYVOID) 
	{
	index2.Tag = TYFRAME;
	index2.u.Real = NILPAGEINDEX;
	}
*theDiskPage = index2;
if (theDiskPage->Tag == TYREAL) theDiskPage->Tag = TYFRAME;

/* Save the new value in the database archive disk file associated with the key. */

if (newValue.Tag != TYVOID)
	{

	/*  Native data types cannot be stored directly into a frame. */

	if (_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT)
		{
	    *ret = TERROR("!ObjectRepository: must be an object to save into a frame!"); 
		goto Last;
		}
	else
	/*  Valid Object types are stored on the disk. */

		{
		if (theDiskPage->Tag == TYREPOINSERT)
			{
			prmv[0] = TOBJ(self->itsOdbID);
			prmv[1] = TREAL(theDiskPage->u.Real);
			*ec = FDatabas_Free(gCP,gTP,2,prmv);
			if (ec->Tag == TYERROR)
				{
				*ret = TERROR("!ObjectRepository: must be an object to save into a frame!"); 
				goto Last;
				}
			}

		if (theDiskPage->Tag == TYFRAME)
			{
			prmv[0] = TOBJ(self->itsOdbID);
			prmv[1] = newValue;
			prmv[2] = *theDiskPage;
			prmv[3] = self->itsCodeKey;
			*theDiskPage = FDatabas_Save(gCP,gTP,4,prmv);
			if (isERROR(theDiskPage))
				{
				TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
				*ret = *theDiskPage; 
				goto Last;
				}
			theDiskPage->Tag = TYFRAME;
			}
		else
			{
			prmv[0] = TOBJ(self->itsOdbID);
			prmv[1] = newValue;
			prmv[2] = gCP->Tval_VOID;
			prmv[3] = self->itsCodeKey;
			*theDiskPage = FDatabas_Save(gCP,gTP,4,prmv);
			if (isERROR(theDiskPage))
				{
				TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
				*ret = *theDiskPage; 
				goto Last;
				}
			theDiskPage->Tag = TYFRAME;
			}
		}

	*ret = TREAL(theDiskPage->u.Real);
	}
else
	/* Erase the new value from the database archive disk file removing the key. */

	{
	if (theDiskPage->Tag == TYFRAME)
		{
		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = *theDiskPage;
		*ec = FDatabas_Free(gCP,gTP,2,prmv);
		if (isERROR(ec))
			{
			TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
			*ret = *ec;
			goto Last;
			}
		}
		
	theDiskPage->Tag = TYFRAME;
	theDiskPage->u.Real = NILPAGEINDEX;
	*ret = TREAL(theDiskPage->u.Real);
	}


/* Must we close the database archive disk file? */

Last:
if (mustClose)
	{
	/* Close the database archive disk file. */
 
	*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
	ExitOnError(*ec);
	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = TSYMBOL("commit");
	FDatabas_Close(gCP,gTP,2,prmv);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
	}

/* Return the new frame id where the object is stored. */

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetIV3

The SetIV3 function only has meaning if a librarian Lambda is attached
to the repository.


#endif

TVAL TDatabase_SetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2, TVAL index3,TVAL newValue)
{
BOLE	mustClose = FALSE;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(savePath);
DeclareTVAL(treeMainIndex);
DeclareTVAL(treeNodeIndex);
DeclareTVAL(theNodeIndexDiskPage);
DeclareTVAL(theTerminalDiskPage);
DeclareTVAL(ec);
DeclareTVALArray(prmv,4);
EndFrame

self = (TDatabase*)asObject(&selfTval);

/* **************************************************** */
/* Manage all major index requests.                     */
/* repo[tree: index2 index3]                           */
/* **************************************************** */

if ((index1.Tag == TYSYMBOL) && (strcmp(SymbolArray(index1),"tree") == 0) && (self->itsTreeOn == TRUE))
	{
	/*  Retrieve the database archive index (if necessary). */

	if (self->itsOdbID == NIL)
		{
		/* Open the database archive disk file. */
	 
		*savePath = gCP->TSymbol_Path->itsGlobalValue;
		gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
		gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
		prmv[0] = TINT(self->itsBaseFilePos);
		prmv[1] = TOBJ(self->itsFileName);
		prmv[2] = TSYMBOL("update");
		*ec = FDatabas_Open(gCP,gTP,3,prmv);
		gCP->TSymbol_Path->itsGlobalValue = *savePath;
		ExitOnError(*ec);
		if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 
		*ec = FDatabas_LoadIndex(gCP,gTP,self,self->itsCodeKey);
		ExitOnError(*ec);
		mustClose = TRUE;
		}

	/*  Read the database disk page number from the ObjectRepository Directory. */

	*treeMainIndex = TOBJ(self->itsIndex);
	*theNodeIndexDiskPage = TDirectory_GetIV1(gCP,gTP,*treeMainIndex,index2);
	if (theNodeIndexDiskPage->Tag != TYFRAME)
		{
		treeNodeIndex->u.Directory = TDirectory_New(gCP,gTP);
		treeNodeIndex->Tag = TYDIRECTORY;
		*theNodeIndexDiskPage = gCP->Tval_VOID;
		}
	else
		{
		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = *theNodeIndexDiskPage;
		prmv[2] = self->itsCodeKey;
		*treeNodeIndex = FDatabas_Load(gCP,gTP,3,prmv);
		if (treeNodeIndex->Tag == TYERROR)
			{
			*ec = *treeNodeIndex;
			goto Last;
			}
		if (treeNodeIndex->Tag != TYDIRECTORY)
			{
			*ec = TERROR("!repository.SetIV3: invalid node Tree repository index!");
			goto Last;
			}
		}

	*theTerminalDiskPage = TDirectory_GetIV1(gCP,gTP,*treeNodeIndex,index3);
	if (theTerminalDiskPage->Tag != TYFRAME)
		{
		*theTerminalDiskPage = gCP->Tval_VOID;
		}

	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = newValue;
	prmv[2] = *theTerminalDiskPage;
	prmv[3] = self->itsCodeKey;
	*theTerminalDiskPage = FDatabas_Save(gCP,gTP,4,prmv);
	if (theTerminalDiskPage->Tag == TYERROR)
		{
		*ec = *theTerminalDiskPage;
		goto Last;
		}
	*ec = TDirectory_SetIV1(gCP,gTP,*treeNodeIndex,index3,*theTerminalDiskPage);
	if (ec->Tag == TYERROR) goto Last;

	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = *treeNodeIndex;
	prmv[2] = *theNodeIndexDiskPage;
	prmv[3] = self->itsCodeKey;
	*theNodeIndexDiskPage = FDatabas_Save(gCP,gTP,4,prmv);
	if (theNodeIndexDiskPage->Tag == TYERROR)
		{
		*ec = *theNodeIndexDiskPage;
		goto Last;
		}
	*ec = TDirectory_SetIV1(gCP,gTP,*treeMainIndex,index2,*theNodeIndexDiskPage);
	if (ec->Tag == TYERROR) goto Last;

	/* Close the database archive disk file. */

	if (mustClose == TRUE)
		{
		/* Close the database archive disk file. */
	 
		*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
		ExitOnError(*ec);
		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = TSYMBOL("commit");
		FDatabas_Close(gCP,gTP,2,prmv);
		self->itsOdbID = NIL;
		self->itsIndex = NIL;
		self->itsTransactionOn = FALSE;
		}

	/* Manage a request for the major index key of a repository. */

	FrameExit(selfTval);
	}

*ec = gCP->TObject_ERROR_ACCESSDENIED;

Last:
if (mustClose == TRUE)
	{
	/* Close the database archive disk file. */
 
	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = TSYMBOL("refuse");
	FDatabas_Close(gCP,gTP,2,prmv);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
	}

FrameExit(*ec);
}


/*--------------------------------------------------------------------------------------- */
#if 0
length

Determine the number of objects currently stored in the ObjectRepository. 

#endif

TVAL TDatabase_Length(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TVAL   databaseOpen = TFUNCTION(FDatabas_Open);
TVAL   databaseClose = TFUNCTION(FDatabas_Close);
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(savePath);
DeclareTVAL(theItem);
DeclareTVAL(tmp);
DeclareTVAL(ec);

EndFrame

self = (TDatabase*)asObject(&selfTval);

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

 /*  Read the length of the ObjectRepository Directory. */
 
 *theItem = TOBJ(self->itsIndex);
 *theItem = FUtil2_Length(gCP,gTP,1,theItem);

 /* Close the database archive disk file. */
 
 *tmp = TSYMBOL("refuse");
 FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
 self->itsOdbID = NIL;
 self->itsIndex = NIL;
 self->itsTransactionOn = FALSE;
 }
else
 {
 /*  Read the length of the ObjectRepository Directory. */

 *theItem = TOBJ(self->itsIndex);
 *theItem = FUtil2_Length(gCP,gTP,1,theItem);

 }

FrameExit(*theItem);
}


/*--------------------------------------------------------------------------------------- */
#if 0
inspect
Overview				

The  inspect function returns statistical and status information concerning the 
specified ObjectRepository. The type of information to be returned is dependent 
upon the optional second argument.

Type:	 	Function

Syntax:	(inspect  aGor )

		(inspect  aGor  command)

		(inspect  aGor  command  key)


__________________________________________________________________
Arguments	Explanation								                           
aGor		The Object Repository  which is to be inspected.
Returns		Returns the size of the Database file in bytes.
__________________________________________________________________
Arguments	Explanation								                           
aGor		The Object Repository  which is to be inspected.
command		The inspection command. 
			If check: a full damage inspection of the database is performed.
			If directory: a copy of the database directory index is returned.
			If free: the number of free bytes remaining in the database is returned. 
			If index: the index allocation statistics for the database are displayed. 
			If open: returns true/false iff the Repository is open/closed.
			If pages: the page statistics for the database are displayed. 
			If show: the full statistics for the database are displayed. 
			If stats: a structure of the full statistics for the database is returned. 
Returns		Returns the value as determined by the command.
__________________________________________________________________
Arguments	Explanation								                           
aGor		The Object Repository  which is to be inspected.
command		The inspection command for the specified retrieval key. 
			If length: the number of bytes reserved for this object in the database is returned. 
			If date: the date and time stamp for this object is returned.
key			The retrieval key of the object to be inspected.
Returns		Returns the value as determined by the command.
__________________________________________________________________
  
  
Example1		
	
	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	
	(setq repo["one"] "Hello world")
 
	(inspect  aGor  length:  "one")	Returns 	26

#endif

TVAL TDatabase_Inspect(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL   selfTval = argv[0];
TVAL   databaseOpen = TFUNCTION(FDatabas_Open);
TVAL   databaseClose = TFUNCTION(FDatabas_Close);
BOLE   mustCloseOnExit = FALSE;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theDiskPage);
DeclareTVAL(savePath);
DeclareTVAL(theItem);
DeclareTVAL(tmp);
DeclareTVAL(ec);
DeclareTVALArray(prmv,5);

EndFrame


//  *selfTval = argv[0];


/*  Make sure this is an Object Repository. */

if (argv[0].Tag != TYOBJREPOSITORY)
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else	
	self = (TDatabase*)asObject(&argv[0]);

/*  Is this a check for an open/closed Object Repository? */

if ((argc == 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"open") == 0))
	{
	if (self->itsOdbID == NIL)
		{
		FrameExit(gCP->TObject_FALSE);
		}
	else
		{
		FrameExit(gCP->TObject_TRUE);
		}
	}

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

	/* Make sure we close the object repository on exit. */

	mustCloseOnExit = TRUE;
	}

/*  Perform the inspect on the ObjectRepository. */

if (argc == 3)
	{
	if ((argv[1].Tag == TYSYMBOL) && ((argv[2].Tag == TYREAL) || (argv[2].Tag == TYFRAME)) && (strcmp(SymbolArray(argv[1]),"frameCheck") == 0))
		{
		prmv[0] = selfTval;
		prmv[1] = argv[2];
		prmv[1].Tag = TYFRAME;
		prmv[2] = TSYMBOL("check");
		*theItem = FDatabas_Inspect(gCP,gTP,argc,&prmv[0]);
		FrameExit(*theItem);
		}

	*theDiskPage = TDatabase_RefIndex(gCP,gTP,TOBJ(self),argv[2]);
	if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
		{
		prmv[0] = selfTval;
		prmv[1] = *theDiskPage;
		prmv[2] = argv[1];
		*theItem = FDatabas_Inspect(gCP,gTP,argc,&prmv[0]);
		}
	else
	if (theDiskPage->Tag == TYVOID)
		{
		*theItem = TSTRING("empty");
		}
	else
		{
		*theItem = TSTRING("immediate");
		}
	}
else
if (argc == 2)
	{
	prmv[0] = selfTval;
	prmv[1] = argv[1];
	*theItem = FDatabas_Inspect(gCP,gTP,argc,&prmv[0]);
	}
else
	{
	prmv[0] = selfTval;
	*theItem = FDatabas_Inspect(gCP,gTP,argc,&prmv[0]);
	}


/*  Close the database archive index (if necessary). */

if (mustCloseOnExit == TRUE)
	{
	/* Close the database archive disk file. */
 
	*tmp = TSYMBOL("refuse");
	*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
	ExitOnError(*ec);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
	}

FrameExit(*theItem);
}

/*--------------------------------------------------------------------------------------- */
#if 0
BeginTransaction

Begin an ObjectRepository transaction. This initiates the beginning of "keyed" transactions.

#endif

TVAL    TDatabase_BeginTransaction(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL     databaseOpen = TFUNCTION(FDatabas_Open);
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(savePath);
DeclareTVAL(tmp);
DeclareTVAL(ec);
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,3);

EndFrame


self = asDatabase(&argv[0]);


/* Invoke the beginTrans member function from the librarian Lambda (if any). */

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
    {
    /* Invoke the beginTrans subfunction of the Lambda. */
    *indexProc = TLambda_GetPvMember(gCP,gTP,argv[0],"beginTrans");
    ExitOnError(*indexProc);
    *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,0,&prmv[0]);
    FrameExit(*ret);
    }

/*  There must be one argument and it must be an ObjectRepository. */

if ((argc != 1) || (argv[0].Tag != TYOBJREPOSITORY))
	{
	FrameExit(gCP->TObject_ERROR_INVALID);
	}

/*  Ignore request if an old transaction is in progress. */

if (self->itsOdbID != NIL)
	{
	FrameExit(TBOOL(TRUE));
	}


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

self->itsTransactionOn = TRUE;

FrameExit(TBOOL(TRUE));
}


/*--------------------------------------------------------------------------------------- */
#if 0
AbortTransaction

Abort an ObjectRepository transaction.

#endif

TVAL    TDatabase_AbortTransaction(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL     databaseClose = TFUNCTION(FDatabas_Close);
TVAL     makeVector = TFUNCTION(FMake_Vector);
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,3);

EndFrame

self = asDatabase(&argv[0]);

/* Invoke the abortTrans member function from the librarian Lambda (if any). */

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
    {
    /* Invoke the abortTrans subfunction of the Lambda. */
    *indexProc = TLambda_GetPvMember(gCP,gTP,argv[0],"abortTrans");
    ExitOnError(*indexProc);
    *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,0,&prmv[0]);
    FrameExit(*ret);
    }

/*  There must be at least one argument and it must be an ObjectRepository. */

if ((argc != 1) || (argv[0].Tag != TYOBJREPOSITORY))
 {
 FrameExit(gCP->TObject_ERROR_INVALID);
 }

/*  Return if no transaction is in progress. */

if (self->itsOdbID == NIL)
 {
 FrameExit(TBOOL(TRUE));
 }

/* Create the buffer object vector for buffering retrieved items. */

if (self->itsBufferCount > 0)
 {
 self->itsBufferKeys = FSmartbase_Eval(gCP,gTP,makeVector,1,TINT(self->itsBufferCount)).u.Vector;
 self->itsBufferValues = FSmartbase_Eval(gCP,gTP,makeVector,1,TINT(self->itsBufferCount)).u.Vector;
 self->itsBufferIndex = 0;
 }

/* Close the database archive disk file. */

*tmp = TSYMBOL("refuse");
FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
self->itsOdbID = NIL;
self->itsIndex = NIL;
self->itsTransactionOn = FALSE;

FrameExit(TBOOL(TRUE));
}


/*--------------------------------------------------------------------------------------- */
#if 0
CommitTransaction

Commit an ObjectRepository transaction.

#endif

TVAL    TDatabase_CommitTransaction(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL            databaseClose = TFUNCTION(FDatabas_Close);
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(ec);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,3);

EndFrame

self = asDatabase(&argv[0]);

/* Invoke the commitTrans member function from the librarian Lambda (if any). */

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
    {
    /* Invoke the commitTrans subfunction of the Lambda. */
    *indexProc = TLambda_GetPvMember(gCP,gTP,argv[0],"commitTrans");
    ExitOnError(*indexProc);
    *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,0,&prmv[0]);
    FrameExit(*ret);
    }

/*  There must be at least one argument and it must be an ObjectRepository. */

if ((argc != 1) || (argv[0].Tag != TYOBJREPOSITORY))
 {
 FrameExit(gCP->TObject_ERROR_INVALID);
 }

/*  Return if no transaction is in progress. */

if (self->itsOdbID == NIL)
 {
 FrameExit(TBOOL(TRUE));
 }

/* Close the archive database disk. */

self->itsTransactionOn = FALSE;
*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
if (isERROR(ec))
	{
	TDatabase_AbortTransaction(gCP,gTP,1,&argv[0]);
	FrameExit(*ec);
	}
*tmp = TSYMBOL("commit");
*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
ExitOnError(*ec);
self->itsOdbID = NIL;
self->itsIndex = NIL;
self->itsTransactionOn = FALSE;

FrameExit(TBOOL(TRUE));
}

/*--------------------------------------------------------------------------------------- */
#if 0
CheckPointTransaction

Checkpoint an ObjectRepository transaction.

#endif

TVAL    TDatabase_CheckPointTransaction(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL            databaseClose = TFUNCTION(FDatabas_Close);
TVAL            databaseOpen = TFUNCTION(FDatabas_Open);
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(ec);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareTVAL(savePath);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,3);

EndFrame

self = asDatabase(&argv[0]);

/* Invoke the commitTrans member function from the librarian Lambda (if any). */

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
    {
    /* Invoke the commitTrans subfunction of the Lambda. */
    *indexProc = TLambda_GetPvMember(gCP,gTP,argv[0],"commitTrans");
    ExitOnError(*indexProc);
    *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,0,&prmv[0]);
    FrameExit(*ret);
    }

/*  There must be at least one argument and it must be an ObjectRepository. */

if ((argc != 1) || (argv[0].Tag != TYOBJREPOSITORY))
 {
 FrameExit(gCP->TObject_ERROR_INVALID);
 }

/*  Return if no transaction is in progress. */

if (self->itsOdbID == NIL)
 {
 FrameExit(TBOOL(TRUE));
 }

/* Close the archive database disk. */

self->itsTransactionOn = FALSE;
*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
if (isERROR(ec))
	{
	TDatabase_AbortTransaction(gCP,gTP,1,&argv[0]);
	FrameExit(*ec);
	}
*tmp = TSYMBOL("commit");
*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
ExitOnError(*ec);


/* ReOpen the database archive disk file. */

*savePath = gCP->TSymbol_Path->itsGlobalValue;
gCP->TSymbol_Path->itsGlobalValue.Tag = TYTEXT;
gCP->TSymbol_Path->itsGlobalValue.u.Text[0] = 0;
*tmp = TSYMBOL("update");
*ec = FSmartbase_Eval(gCP,gTP,databaseOpen,3,TINT(self->itsBaseFilePos),TOBJ(self->itsFileName),*tmp);
gCP->TSymbol_Path->itsGlobalValue = *savePath;
ExitOnError(*ec);
if (ec->Tag != TYBYTEVECTOR) {self->itsOdbID = NIL;} else {self->itsOdbID = ec->u.ByteVector;} 

self->itsTransactionOn = TRUE;

FrameExit(TBOOL(TRUE));
}

/*--------------------------------------------------------------------------------------- */
#if 0
isTransaction

Does an ObjectRepository have an open transaction?

#endif

TVAL    TDatabase_IsTransaction(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,3);

EndFrame

self = asDatabase(&argv[0]);

/* Invoke the isTrans member function from the librarian Lambda (if any). */

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
    {
    /* Invoke the abortTrans subfunction of the Lambda. */
    *indexProc = TLambda_GetPvMember(gCP,gTP,argv[0],"isTrans");
    ExitOnError(*indexProc);
    *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,0,&prmv[0]);
    FrameExit(*ret);
    }

/*  There must be at least one argument and it must be an ObjectRepository. */

if ((argc != 1) || (argv[0].Tag != TYOBJREPOSITORY))
 {
 FrameExit(gCP->TObject_ERROR_INVALID);
 }

/*  Return true iff a transaction is in progress. */

if (self->itsOdbID == NIL)
 {
 FrameExit(TBOOL(FALSE));
 }
 else
 {
 FrameExit(TBOOL(TRUE));
 }

}

/*--------------------------------------------------------------------------------------- */
#if 0
SaveImmediate

Associative Storage:
 
  Any value may be stored immediate in the ObjectRepository index and associated with 
  a key value. Both the key value and the stored value may be of arbitrary complexity. 
  An object may be removed from the ObjectRepository by storing the value #void in 
  association with its previous key value.

Arguments:

 aGor	The ObjectRepository into which the object is to be stored.
 key	The key to be associated with the object for later retrieval.
 value 	The object to be stored.
Return:
 
 result The ObjectRepository.

Example:

 (setq  repo  (new ObjectRepository:  "myarchive.odb"))
 (saveImmediate repo "one"  "Hello world")
 repo["one"]       ==> "Hello world"

#endif

TVAL TDatabase_SaveImmediate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
TVAL		databaseOpen = TFUNCTION(FDatabas_Open);
TVAL		databaseClose = TFUNCTION(FDatabas_Close);
TVAL		databaseFree = TFUNCTION(FDatabas_Free);
TVAL		vectorDelete = TFUNCTION(FUtil3_VectorDelete);
TVAL		member = TFUNCTION(FUtil3_Member);
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(selfTval);
DeclareTVAL(index1);
DeclareTVAL(theIndex);
DeclareTVAL(theDiskPage);
DeclareTVAL(newValue);
DeclareTVAL(savePath);
DeclareTVAL(ec);
DeclareTVAL(tmp);
DeclareTVAL(ret);
EndFrame

/* Check for correct number of arguments */
if (argc < 3)
 	{
	*ret = TERROR("!saveImmediate: Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 3)
 	{
	*ret = TERROR("!saveImmediate: Too many arguments!");
	FrameExit(*ret);
	}

/* Check if first argument is an object repository */
if (argv[0].Tag != TYOBJREPOSITORY)
 	{
	*ret = TERROR("!saveImmediate: Expecting argument 1 to be an Object Repository!");
	FrameExit(*ret);
	}
else
	{
	*selfTval = argv[0];
	*index1 = argv[1];
	*newValue = argv[2];
	self = (TDatabase*)asObject(selfTval);
	}

/* This operation invalid when Tree indexing is on */

if (self->itsTreeOn == TRUE)
	{
	FrameExit(TERROR("!saveRepository: invalid operation when Tree indexing on!"));
	}

/*  Check for the key in the buffer (if it can be found). */
/*  Note:	If the key is found, then we return an error */
/*          message, because we cannot store immediate. */

if (self->itsBufferCount > 0)
	{
	*theIndex = FSmartbase_Eval(gCP,gTP,member,2,*index1,TOBJ(self->itsBufferKeys));
	if (theIndex->Tag == TYNUM)
 		{
		*ret = TERROR("!saveImmediate: Cannot save Repository if buffering is in effect!");
		FrameExit(*ret);
		}
	}

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

	/*  Read the database disk page number from the ObjectRepository Directory, */
	/*  and save the persistent object to the database archive index immediate. */

	*theDiskPage = TDatabase_RefIndex(gCP,gTP,TOBJ(self),*index1);

	/* Save the new value in the database archive index associated with the key. */

	if (newValue->Tag != TYVOID)
		{
		if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
			ExitOnError(*ec);
			}

		*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),*index1,*newValue);
		ExitOnError(*ec);
		}
	else
	/* Erase the new value from the database archive disk file removing the key. */

		{
		if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
			ExitOnError(*ec);
			}

		if (theDiskPage->Tag != TYVOID)
			{
			*theIndex = FSmartbase_Eval(gCP,gTP,member,2,*index1,TOBJ(self->itsIndex));
			ExitOnError(*theIndex);
			theDiskPage->Tag = TYFRAME;
			*ec = FSmartbase_Eval(gCP,gTP,vectorDelete,2,TOBJ(self->itsIndex),*theIndex);
			ExitOnError(*ec);
			}
		}
 

	/* Close the database archive disk file. */
 
	*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
	if (isERROR(ec))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
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
	/*  Read the database disk page number from the ObjectRepository Directory, */
	/*  and save the persistent object to the database archive disk file. */

	*theDiskPage = TDatabase_RefIndex(gCP,gTP,*selfTval,*index1);

	/* Save the new value in the database archive index associated with the key. */

	if (newValue->Tag != TYVOID)
		{
		if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
			ExitOnError(*ec);
			}

		*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),*index1,*newValue);
		ExitOnError(*ec);
		}
	else
	/* Erase the new value from the database archive disk file removing the key. */

		{
		if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
			ExitOnError(*ec);
			}

		if (theDiskPage->Tag != TYVOID)
			{
			*theIndex = FSmartbase_Eval(gCP,gTP,member,2,*index1,TOBJ(self->itsIndex));
			ExitOnError(*theIndex);
			theDiskPage->Tag = TYFRAME;
			*ec = FSmartbase_Eval(gCP,gTP,vectorDelete,2,TOBJ(self->itsIndex),*theIndex);
			ExitOnError(*ec);
			}
		}
	}

FrameExit(*selfTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
isImmediate

	Returns TRUE is the specified key value is stored immediate in the Object Repository,
	otherwise, FALSE is returned.

#endif

TVAL TDatabase_isImmediate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
TVAL            databaseOpen = TFUNCTION(FDatabas_Open);
TVAL            databaseClose = TFUNCTION(FDatabas_Close);
TVAL            member = TFUNCTION(FUtil3_Member);
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theIndex);
DeclareTVAL(index1);
DeclareTVAL(selfTval);
DeclareTVAL(theDiskPage);
DeclareTVAL(savePath);
DeclareTVAL(theResult);
DeclareTVAL(tmp);
DeclareTVAL(ec);
DeclareTVAL(ret);

EndFrame

/* Check for correct number of arguments */
if (argc < 2)
 	{
	*ret = TERROR("!isImmediate: Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 2)
 	{
	*ret = TERROR("!isImmediate: Too many arguments!");
	FrameExit(*ret);
	}

/* Check if first argument is an object repository */
if (argv[0].Tag != TYOBJREPOSITORY)
 	{
	*ret = TERROR("!isImmediate: Expecting argument 1 to be an Object Repository!");
	FrameExit(*ret);
	}
else
	{
	*selfTval = argv[0];
	*index1 = argv[1];
	self = (TDatabase*)asObject(selfTval);
	}

/* This operation invalid when Tree indexing is on */

if (self->itsTreeOn == TRUE)
	{
	FrameExit(TERROR("!saveRepository: invalid operation when Tree indexing on!"));
	}

/*  Check for the key in the buffer (if it can be found). */
/*  Note:	If found, we return FALSE, because its not */
/*			stored immediate if its in the buffer. */

if (self->itsBufferCount > 0)
	{
	*theIndex = FSmartbase_Eval(gCP,gTP,member,2,*index1,TOBJ(self->itsBufferKeys));
	if (theIndex->Tag == TYNUM)
		{
		FrameExit(gCP->Tval_FALSE);
		}
	}

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

    /*  Read the database disk page number from the ObjectRepository Directory. */

    *theDiskPage = TDatabase_RefIndex(gCP,gTP,TOBJ(self),*index1);

    /*  We return TRUE iff the disk page shows that the value is stored immediate. */
    /*  Note:	The disk page must be an integer to be stored on disk. */

    if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT) || (theDiskPage->Tag == TYVOID))
        {
        *theResult = gCP->Tval_FALSE;
        }
    else
        {
        *theResult = gCP->Tval_TRUE;
        }

    /* Close the database archive disk file. */

    *tmp = TSYMBOL("refuse");
    FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
    self->itsOdbID = NIL;
    self->itsIndex = NIL;
    self->itsTransactionOn = FALSE;
    }
else
    {
    /*  Read the database disk page number from the ObjectRepository Directory. */

    *theDiskPage = TDatabase_RefIndex(gCP,gTP,TOBJ(self),*index1);

    /*  We return TRUE iff the disk page shows that the value is stored immediate. */
    /*  Note:	The disk page must be an integer to be stored on disk. */

    if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT) || (theDiskPage->Tag == TYVOID))
        {
        *theResult = gCP->Tval_FALSE;
        }
    else
        {
        *theResult = gCP->Tval_TRUE;
        }
    }

FrameExit(*theResult);
}

/*--------------------------------------------------------------------------------------- */
#if 0
rename

Overview				

The  rename  function changes the retrieval key of an object stored in the specified 
ObjectRepository {aGor}, without moving, resaving, or disturbing the stored object. 
Only the retrieval key, in the ObjectRepository index is renamed. If there is already 
a retrieval key with the new name, an error is returned.

Type:	 	Function

Syntax:		(rename  aGor  oldKey  newKey)

When To Use	
	
The  rename  function is used to rename objects stored in an Object Repository. 
The same goal can be accomplished by retrieving the stored object, erasing the old copy, 
and resaving the object under the new retrieval key; but this would obviously require 
more time and resources than using the rename function.

__________________________________________________________________
Arguments	Explanation								                           
aGor		The ObjectRepository which will have its index altered.
oldKey		The retrieval key which is to be renamed. 
newKey		The new retrieval key. 
Returns		Returns true or an error. 
__________________________________________________________________
  
  
Example1
		
(setq  repo  (new ObjectRepository:  "myarchive.odb"))

(setq  repo.greeting  "Goodbye")

repo.greeting				Returns  "Goodbye"

(rename  repo  greeting:  farewell:)

repo.greeting				Returns  #void

repo.farewell				Returns  "Goodbye"

#endif

TVAL TDatabase_Rename(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{

TVAL		databaseOpen = TFUNCTION(FDatabas_Open);
TVAL		databaseClose = TFUNCTION(FDatabas_Close);
TVAL		member = TFUNCTION(FUtil3_Member);
BOLE		mustCloseOnExit = FALSE;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(selfTval);
DeclareTVAL(theIndex);
DeclareTVAL(oldKey);
DeclareTVAL(newKey);
DeclareTVAL(theDiskPage);
DeclareTVAL(savePath);
DeclareTVAL(ec);
DeclareTVAL(tmp);
DeclareTVAL(ret);

EndFrame


/* Check for correct number of arguments */
if (argc < 3)
 	{
	*ret = TERROR("!rename: Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 3)
 	{
	*ret = TERROR("!rename: Too many arguments!");
	FrameExit(*ret);
	}

/* Check if first argument is an object repository */
if (argv[0].Tag != TYOBJREPOSITORY)
 	{
	*ret = TERROR("!rename: Expecting argument 1 to be an Object Repository!");
	FrameExit(*ret);
	}
else
	{
	*selfTval = argv[0];
	*oldKey = argv[1];
	*newKey = argv[2];
	self = (TDatabase*)asObject(selfTval);
	}

/*  Check for the key in the buffer (if it can be found). */
/*  Note:	If the key is found, then we return an error */
/*          message, because we cannot store immediate. */

if (self->itsBufferCount > 0)
	{
	*theIndex = FSmartbase_Eval(gCP,gTP,member,2,*oldKey,TOBJ(self->itsBufferKeys));
	if (theIndex->Tag == TYNUM)
 		{
		*ret = TERROR("!rename: Cannot save a Repository with buffering in effect!");
		FrameExit(*ret);
		}
	}

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
  
	/* Make sure we close the object repository on exit. */

	mustCloseOnExit = TRUE;
	}


/*  Read the database disk page number from the ObjectRepository Directory. */
/*  If the new retrieval key is present, then there is a conflict. */

*theDiskPage = TDatabase_RefIndex(gCP,gTP,TOBJ(self),*newKey);
if (theDiskPage->Tag != TYVOID)
 	{
	*ret = TERROR("!rename: New key already in the specified Repository!");
	FrameExit(*ret);
	}

/*  Read the database disk page number from the ObjectRepository Directory. */
/*  If the old retrieval key is missing, then there is a conflict. */

*theDiskPage = TDatabase_RefIndex(gCP,gTP,TOBJ(self),*oldKey);
if (theDiskPage->Tag == TYVOID)
 	{
	*ret = TERROR("!rename: Old key not in the specified Repository!");
	FrameExit(*ret);
	}
/* Save the old value in the database index associated with the new key. */

*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),*newKey,*theDiskPage);
ExitOnError(*ec);

/* Erase the old key from the database index. */

*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),*oldKey,gCP->Tval_VOID);
ExitOnError(*ec);

/*  Close the database archive index (if necessary). */

if (mustCloseOnExit == TRUE)
	{
	/* Close the database archive disk file. */
 
	*ec = FDatabas_SaveIndex(gCP,gTP,self->itsOdbID,TOBJ(self->itsIndex),self->itsCodeKey);
	if (isERROR(ec))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,selfTval);
		FrameExit(*ec);
		}
	*tmp = TSYMBOL("commit");
	*ec = FSmartbase_Eval(gCP,gTP,databaseClose,2,TOBJ(self->itsOdbID),*tmp);
	ExitOnError(*ec);
	self->itsOdbID = NIL;
	self->itsIndex = NIL;
	self->itsTransactionOn = FALSE;
	}

FrameExit(gCP->Tval_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SizeOf

Return the disk size of a value.

#endif

TVAL    TDatabase_SizeOf(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL	 saveObject = TFUNCTION(FConio_saveObject);
StartFrame
DeclareTVAL(ret);
EndFrame

/*  There must be at one argument. */

if (argc != 1)
 {
 FrameExit(gCP->TObject_ERROR_INVALID);
 }

/*  Return size of TVAL if value is not an object. */

if (_TObject_TypeFlag(argv[0].Tag) != _TObject_TfTOBJECT)
 {
 FrameExit(TINT(sizeof(TVAL) + sizeof(OBRECORDHEADER)));
 }

/* Compute the disk size of the specified object. */

*ret = FSmartbase_Eval(gCP,gTP,saveObject,3,gCP->Tval_VOID,argv[0],gCP->Tval_FALSE);

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Associative Storage:
 
	Any object may be removed from the ObjectRepository by deleting its previous 
	index value.

Arguments:

	aGor	The ObjectRepository into which the object is to be stored.
	index	The index value to be deleted.

Return:
 
	result	The ObjectRepository with the index deleted.

#endif

TVAL TDatabase_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
TVAL   databaseOpen = TFUNCTION(FDatabas_Open);
TVAL   databaseClose = TFUNCTION(FDatabas_Close);
TVAL   databaseSave = TFUNCTION(FDatabas_Save);
TVAL   databaseFree = TFUNCTION(FDatabas_Free);
TVAL   vectorDelete = TFUNCTION(FUtil3_VectorDelete);
TVAL   member = TFUNCTION(FUtil3_Member);
TVAL   newValue = gCP->Tval_VOID;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theIndex);
DeclareTVAL(theDiskPage);
DeclareTVAL(savePath);
DeclareTVAL(ec);
DeclareTVAL(tmp);

EndFrame

self = (TDatabase*)asObject(&selfTval);

/*  Check for the key in the buffer (if it can be found). */

if (self->itsBufferCount > 0)
	{
	*theIndex = FSmartbase_Eval(gCP,gTP,member,2,index1,TOBJ(self->itsBufferKeys));
	if (theIndex->Tag == TYNUM)
		{
		*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsBufferValues),*theIndex,newValue);
		ExitOnError(*ec);
		}
	else
		{
		*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsBufferKeys),TINT(self->itsBufferIndex),index1);
		ExitOnError(*ec);
		*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsBufferValues),TINT(self->itsBufferIndex),newValue);
		ExitOnError(*ec);
		if (++self->itsBufferIndex >= self->itsBufferCount)
			self->itsBufferIndex = 0;
		}
	}

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

	/*  Read the database disk page number from the ObjectRepository Directory, */
	/*  and save the persistent object to the database archive disk file. */

	*theDiskPage = TDatabase_RefIndex(gCP,gTP,TOBJ(self),index1);

	/* Save the new value in the database archive disk file associated with the key. */

	if (newValue.Tag != TYVOID)
		{

		/*  Native data types must be stored immediate. */
	
		if (_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT)
			{
			if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
				{
				*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
				ExitOnError(*ec);
				}

			/*	We cannot save Integers immediate (convert them to real first). */
			/*	Note:	Integers signify disk page numbers as opposed to immediate values. */

			if (newValue.Tag == TYNUM)
				{
				newValue.Tag = TYREAL;
				newValue.u.Real = newValue.u.Int;
				}

			*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,newValue);
			ExitOnError(*ec);
			}
		else
		/*  Valid Object types are stored on the disk. */

			{
			if (theDiskPage->Tag == TYREPOINSERT)
				{
				*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
				ExitOnError(*ec);
				}

			if (theDiskPage->Tag == TYFRAME)
				{
				*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,TREAL(theDiskPage->u.Real),self->itsCodeKey);
				if (isERROR(theDiskPage))
					{
					TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
					FrameExit(*theDiskPage);
					}
				theDiskPage->Tag = TYFRAME;
				*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,*theDiskPage);
				ExitOnError(*ec);
				}
			else
				{
				*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,gCP->Tval_VOID,self->itsCodeKey);
				if (isERROR(theDiskPage))
					{
					TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
					FrameExit(*theDiskPage);
					}
				theDiskPage->Tag = TYFRAME;
				*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,*theDiskPage);
				ExitOnError(*ec);
				}
			}
		}
	else
		/* Erase the new value from the database archive disk file removing the key. */

		{
		if (theDiskPage->Tag == TYFRAME)
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
			ExitOnError(*ec);
			}
			
		if (theDiskPage->Tag != TYVOID)
			{
			*theIndex = FSmartbase_Eval(gCP,gTP,member,2,index1,TOBJ(self->itsIndex));
			ExitOnError(*theIndex);
			*ec = FSmartbase_Eval(gCP,gTP,vectorDelete,2,TOBJ(self->itsIndex),*theIndex);
			ExitOnError(*ec);
			}
		}
 

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
	/*  Read the database disk page number from the ObjectRepository Directory, */
	/*  and save the persistent object to the database archive disk file. */

	*theDiskPage = TDatabase_RefIndex(gCP,gTP,selfTval,index1);

	/* Save the new value in the database archive disk file associated with the key. */

	if (newValue.Tag != TYVOID)
		{

		/*  Native data types must be stored immediate. */
	
		if (_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT)
			{
			if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
				{
				*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
				ExitOnError(*ec);
				}

			*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,newValue);
			ExitOnError(*ec);
			}
		else
		/*  Valid Object types are stored on the disk. */

			{
			if (theDiskPage->Tag == TYREPOINSERT)
				{
				*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
				ExitOnError(*ec);
				}

			if (theDiskPage->Tag == TYFRAME)
				{
				*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,TREAL(theDiskPage->u.Real),self->itsCodeKey);
				if (isERROR(theDiskPage))
					{
					TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
					FrameExit(*theDiskPage);
					}
				theDiskPage->Tag = TYFRAME;
				*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,*theDiskPage);
				ExitOnError(*ec);
				}
			else
				{
				*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,gCP->Tval_VOID,self->itsCodeKey);
				if (isERROR(theDiskPage))
					{
					TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
					FrameExit(*theDiskPage);
					}
				theDiskPage->Tag = TYFRAME;
				*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,*theDiskPage);
				ExitOnError(*ec);
				}
			}
		}
	else
		/* Erase the new value from the database archive disk file removing the key. */

		{
		if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
			ExitOnError(*ec);
			}
			
		if (theDiskPage->Tag != TYVOID)
			{
			*theIndex = FSmartbase_Eval(gCP,gTP,member,2,index1,TOBJ(self->itsIndex));
			ExitOnError(*theIndex);
			*ec = FSmartbase_Eval(gCP,gTP,vectorDelete,2,TOBJ(self->itsIndex),*theIndex);
			ExitOnError(*ec);
			}
		}
	}

FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
IndexedDiskErase

Erase all disk pages allocated to the specified tree node Directory.
 
Arguments:

	aGor		The ObjectRepository which is to be referenced.
	nodeIndex	The node Directory whose allocated disk pages are to be freed.

Return:

	result	    Always returns true.

#endif

TVAL TDatabase_IndexedDiskErase(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TDirectory* nodeIndex)
{
NUM				n; 
NUM				N;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theIndex);
DeclareTVAL(theDiskPage);
DeclareTVAL(ec);
DeclareTVALArray(prmv,4);

EndFrame

self = (TDatabase*)asObject(&selfTval);
*theIndex = TOBJ(nodeIndex);

/* Free all allocated disk frames found in this node index Directory */

N = nodeIndex->itsMaxItemIndex;
for (n = 0; n < N; ++n)
	{
	*theDiskPage = PBindArray(*theIndex)[n].Value;
	if (theDiskPage->Tag == TYFRAME)
		{
		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = TREAL(theDiskPage->u.Real);
		*ec = FDatabas_Free(gCP,gTP,2,prmv);
		ExitOnError(*ec);
		}
	}

FrameExit(gCP->Tval_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
IndexedDiskLoad

Associative Retrieval:
 
	Any object, previously stored in the specified ObjectRepository, may be retrieved, 
	using its index value. Both the index value and the stored value may be of arbitrary 
	complexity. If no object has been stored under the specified index value, then #void 
	is returned.

	Note:	If the index argument is a frame id (TYFRAME), then the object is loaded from
			the direct ObjectRepository frame reference specified.  

Arguments:

	aGor	The ObjectRepository which is to be referenced.
	index	The index value whose associated object is to be returned.

Return:

	result	The object previously associated with the index value (see the set procedure).

Example:

	Note:	SmartLisp compiles the lexical forms repo["two"] and repo.two both into the 
			following invocations of ref, (ref repo "two").

	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	(setq repo["one"] "Hello world")
	repo["one"]       ==> "Hello world"
	(setq repo.two "Goodbye world")
	repo.two        ==> "Goodbye world"
	(setq repo[34] "Not my world")
	repo[34]        ==> "Not my world"

#endif

TVAL TDatabase_IndexedDiskLoad(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
TVAL            databaseLoad = TFUNCTION(FDatabas_Load);
PAGETABLE*		pageInfo;
NUM				filePos;
NUM				n; 
NUM				N;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareOBJ(TDatabase,insertRepo);
DeclareTVAL(theItem);
DeclareTVAL(treeMainIndex);
DeclareTVAL(treeNodeIndex);
DeclareTVAL(theDiskPage);
DeclareTVAL(theNodeIndexDiskPage);
DeclareTVAL(tmp);
DeclareTVAL(tmpKey);
DeclareTVAL(tmpValue);
DeclareTVALArray(prmv,4);

EndFrame

self = (TDatabase*)asObject(&selfTval);

if (self->itsTreeOn == TRUE)
	{
	if (self->itsIndex == NIL)
		{
		FrameExit(TERROR("!repository.IndexedDiskRead: invalid main Tree repository index!"));
		}
	/*  Check if we are retrieving directly using a disk frame key */

	if (index1.Tag == TYFRAME)
		{
		/*  Use the index as the database disk page number directly. */

		*theDiskPage = index1;
		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = TREAL(theDiskPage->u.Real);
		prmv[2] = self->itsCodeKey;
		*theItem = FDatabas_Load(gCP,gTP,3,prmv);
		ExitOnError(*theItem);
		}
	else
		{
		/*  Read the database disk page number from the ObjectRepository Directory. */

		*treeMainIndex = TOBJ(self->itsIndex);
		*theNodeIndexDiskPage = TDirectory_GetIV1(gCP,gTP,*treeMainIndex,index1);
		if (theNodeIndexDiskPage->Tag != TYFRAME)
			{
			FrameExit(gCP->Tval_VOID);
			}

		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = TREAL(theNodeIndexDiskPage->u.Real);
		prmv[2] = self->itsCodeKey;
		*treeNodeIndex = FDatabas_Load(gCP,gTP,3,prmv);
		ExitOnError(*treeNodeIndex);
		if (treeNodeIndex->Tag != TYDIRECTORY)
			{
			FrameExit(TERROR("!repository.IndexedDiskRead: invalid node Tree repository index!"));
			}

		N = treeNodeIndex->u.Directory->itsMaxItemIndex;
		for (n = 0; n < N; ++n)
			{
			*tmpKey = PBindArray(*treeNodeIndex)[n].Key;
			*tmpValue = PBindArray(*treeNodeIndex)[n].Value;
			if (tmpValue->Tag == TYFRAME)
				{
				prmv[0] = TOBJ(self->itsOdbID);
				prmv[1] = TREAL(tmpValue->u.Real);
				prmv[2] = self->itsCodeKey;
				*tmp = FDatabas_Load(gCP,gTP,3,prmv);
				if (isERROR(tmp))
					{
					TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
					FrameExit(*theDiskPage);
					}
				PBindArray(*treeNodeIndex)[n].Value = *tmp;
				}
			}

		*theItem = *treeNodeIndex;
		}
	}
else
	{
	/*  Check if we are retrieving directly using a disk frame key */

	if (index1.Tag == TYFRAME)
		{
		/*  Use the index as the database disk page number directly. */

		*theDiskPage = index1;
		}
	else
		{
		/*  Read the database disk page number from the ObjectRepository Directory. */
		*theDiskPage = TDatabase_RefIndex(gCP,gTP,selfTval,index1);
		}

	/*  Read the persistent object from the database archive disk file. */

	if (theDiskPage->Tag == TYFRAME)
		{
		*theItem = FSmartbase_Eval(gCP,gTP,databaseLoad,3,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real),self->itsCodeKey);
		ExitOnError(*theItem);
		}
	else
	/*  Return the inserted repository from the database archive disk file. */

	if (theDiskPage->Tag == TYREPOINSERT)
		{
		insertRepo = TDatabase_New(gCP,gTP);
		pageInfo = FDatabas_GetPageTable(gCP,gTP,TOBJ(self->itsOdbID));
		if (pageInfo == NULL) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		insertRepo->itsFileName = self->itsFileName;
		insertRepo->itsCodeKey = self->itsCodeKey;		
		filePos = FDatabas_FrameIDToFPos(gCP,gTP,theDiskPage->u.Real);
		insertRepo->itsBaseFilePos = pageInfo->baseFilePos + filePos;
		theItem->Tag = TYOBJREPOSITORY;
		theItem->u.Repository = TDatabase_UniqueObjectRepository(gCP,gTP,insertRepo);
		}
	else
		{
		*theItem = *theDiskPage;
		}
	}

FrameExit(*theItem);
}

/*--------------------------------------------------------------------------------------- */
#if 0
IndexedDiskSave

Associative Storage:
 
	Any object (must respond true to the isObject predicate) may be stored in the 
	ObjectRepository and associated with an index value. Both the index value and 
	the stored value may be of arbitrary complexity. An object may be removed from 
	the ObjectRepository by storing the value #void in association with its previous 
	index value.

Arguments:

	aGor	The ObjectRepository into which the object is to be stored.
	index	The index value to be associated with the object for later retrieval.

Return:
 
	result	The position of the object in the ObjecrRepository (see the ref procedure).

Example:

	(setq  repo  (new ObjectRepository:  "myarchive.odb"))
	(setq repo["one"] "Hello world")
	repo["one"]		==> "Hello world"
	(setq repo.two "Goodbye world")
	repo.two			==> "Goodbye world"
	(setq repo[34] "Not my world")
	repo[34]			==> "Not my world"

Note:	SmartLisp compiles the lexical forms repo["two"] and repo.two both into the following 
		invocations of ref, (ref repo "two"). The lexical form (setq repo["two"] "Goodbye world") 
		is compiled into the following invocation of set, (set repo "two"  "Goodbye world").

#endif

TVAL TDatabase_IndexedDiskSave(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
TVAL		databaseSave = TFUNCTION(FDatabas_Save);
TVAL		databaseFree = TFUNCTION(FDatabas_Free);
TVAL		vectorDelete = TFUNCTION(FUtil3_VectorDelete);
TVAL		member = TFUNCTION(FUtil3_Member);
NUM			n; 
NUM			N;
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theIndex);
DeclareTVAL(treeMainIndex);
DeclareTVAL(treeNodeIndex);
DeclareTVAL(theDiskPage);
DeclareTVAL(theNewValueDiskPage);
DeclareTVAL(theNodeIndexDiskPage);
DeclareTVAL(ec);
DeclareTVAL(tmpKey);
DeclareTVAL(tmpValue);
DeclareTVALArray(prmv,4);
EndFrame

self = selfTval.u.Repository;

if (self->itsTreeOn == TRUE)
	{
	/*  Read the database disk page number from the Main ObjectRepository Directory. */

	if (self->itsIndex == NIL)
		{
		FrameExit(TERROR("!repository.IndexedDiskSave: invalid main Tree repository index!"));
		}
	if (newValue.Tag != TYDIRECTORY)
		{
		FrameExit(TERROR("!repository.IndexedDiskSave: new value must be a Directory!"));
		}
	*treeMainIndex = TOBJ(self->itsIndex);
	*theNodeIndexDiskPage = TDirectory_GetIV1(gCP,gTP,*treeMainIndex,index1);
	ExitOnError(*theNodeIndexDiskPage);
	if (theNodeIndexDiskPage->Tag == TYFRAME)
		{
		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = *theNodeIndexDiskPage;
		prmv[2] = self->itsCodeKey;
		*treeNodeIndex = FDatabas_Load(gCP,gTP,3,prmv);
		ExitOnError(*treeNodeIndex);
		if (treeNodeIndex->Tag == TYDIRECTORY)
			{
			*ec = TDatabase_IndexedDiskErase(gCP,gTP,selfTval,treeNodeIndex->u.Directory);
			ExitOnError(*ec);
			}
		}

	treeNodeIndex->u.Directory = TDirectory_New(gCP,gTP);
	treeNodeIndex->Tag = treeNodeIndex->u.Directory->itsObjectType;

	N = newValue.u.Directory->itsMaxItemIndex;
	for (n = 0; n < N; ++n)
		{
		*tmpKey = PBindArray(newValue)[n].Key;
		*tmpValue = PBindArray(newValue)[n].Value;
		prmv[0] = TOBJ(self->itsOdbID);
		prmv[1] = *tmpValue;
		prmv[2] = gCP->Tval_VOID;
		prmv[3] = self->itsCodeKey;
		*theNewValueDiskPage = FDatabas_Save(gCP,gTP,4,prmv);
		if (isERROR(theNewValueDiskPage))
			{
			TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
			FrameExit(*theDiskPage);
			}
		theNewValueDiskPage->Tag = TYFRAME;
		*ec = FSmartbase_Set(gCP,gTP,3,*treeNodeIndex,*tmpKey,*theNewValueDiskPage);
		ExitOnError(*ec);
		}

	prmv[0] = TOBJ(self->itsOdbID);
	prmv[1] = *treeNodeIndex;
	prmv[2] = *theNodeIndexDiskPage;
	prmv[3] = self->itsCodeKey;
	*theNodeIndexDiskPage = FDatabas_Save(gCP,gTP,4,prmv);
	if (isERROR(theNodeIndexDiskPage))
		{
		TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
		FrameExit(*theDiskPage);
		}
	theNodeIndexDiskPage->Tag = TYFRAME;
	asMemo(theNodeIndexDiskPage) = treeNodeIndex->u.Directory->itsMaxItemIndex; /* Note: Set the length of the node in the memo portion of the value word */
	*ec = FSmartbase_Set(gCP,gTP,3,*treeMainIndex,index1,*theNodeIndexDiskPage);
	ExitOnError(*ec);
	goto Last;
	}
else
	{
	/*  Read the database disk page number from the ObjectRepository Directory, */
	/*  and save the persistent object to the database archive disk file. */

	*theDiskPage = TDatabase_RefIndex(gCP,gTP,selfTval,index1);

	/* Save the new value in the database archive disk file associated with the key. */

	if (newValue.Tag != TYVOID)
		{

		/*  Native data types must be stored immediate. */
	
		if (_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT)
			{
			if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
				{
				*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
				ExitOnError(*ec);
				}

			*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,newValue);
			ExitOnError(*ec);
			}
		else
		/*  Valid Object types are stored on the disk. */

			{
			if (theDiskPage->Tag == TYREPOINSERT)
				{
				*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
				ExitOnError(*ec);
				}

			if (theDiskPage->Tag == TYFRAME)
				{
				*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,TREAL(theDiskPage->u.Real),self->itsCodeKey);
				if (isERROR(theDiskPage))
					{
					TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
					FrameExit(*theDiskPage);
					}
				theDiskPage->Tag = TYFRAME;
				*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,*theDiskPage);
				ExitOnError(*ec);
				}
			else
				{
				*theDiskPage = FSmartbase_Eval(gCP,gTP,databaseSave,4,TOBJ(self->itsOdbID),newValue,gCP->Tval_VOID,self->itsCodeKey);
				if (isERROR(theDiskPage))
					{
					TDatabase_AbortTransaction(gCP,gTP,1,&selfTval);
					FrameExit(*theDiskPage);
					}
				theDiskPage->Tag = TYFRAME;
				*ec = FSmartbase_Set(gCP,gTP,3,TOBJ(self->itsIndex),index1,*theDiskPage);
				ExitOnError(*ec);
				}
			}
		}
	else
		/* Erase the new value from the database archive disk file removing the key. */

		{
		if ((theDiskPage->Tag == TYFRAME) || (theDiskPage->Tag == TYREPOINSERT))
			{
			*ec = FSmartbase_Eval(gCP,gTP,databaseFree,2,TOBJ(self->itsOdbID),TREAL(theDiskPage->u.Real));
			ExitOnError(*ec);
			}
			
		if (theDiskPage->Tag != TYVOID)
			{
			*theIndex = FSmartbase_Eval(gCP,gTP,member,2,index1,TOBJ(self->itsIndex));
			ExitOnError(*theIndex);
			*ec = FSmartbase_Eval(gCP,gTP,vectorDelete,2,TOBJ(self->itsIndex),*theIndex);
			ExitOnError(*ec);
			}
		}
	}

Last:
FrameExit(selfTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
PositionIndex

Summary:
 
	Read a key entry in the Database Object Repository index.

Arguments:

	aGor	The ObjectRepository which is to be referenced.
	index	The index value whose associated key is to be returned.

Return:

	result	The value previously associated with the index key (see the set procedure).

#endif

TVAL TDatabase_PositionIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(thePosition);
DeclareTVALArray(prmv,3);
EndFrame

self = selfTval.u.Repository;

prmv[0] = index1;
prmv[1] = TOBJ(self->itsIndex);
*thePosition = FUtil3_Member(gCP,gTP,2,prmv);

FrameExit(*thePosition);
}

/*--------------------------------------------------------------------------------------- */
#if 0
RefIndex

Summary:
 
	Read an entry in the Database Object Repository index.

Arguments:

	aGor	The ObjectRepository which is to be referenced.
	index	The index value whose associated object is to be returned.

Return:

	result	The value previously associated with the index key (see the set procedure).

#endif

TVAL TDatabase_RefIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theDiskPage);
EndFrame

self = selfTval.u.Repository;


*theDiskPage = TDirectory_GetIV1(gCP,gTP,TOBJ(self->itsIndex),index1);

FrameExit(*theDiskPage);
}

/*--------------------------------------------------------------------------------------- */
#if 0
RefIndexKey

Summary:
 
	Read a key entry in the Database Object Repository index.

Arguments:

	aGor	The ObjectRepository which is to be referenced.
	index	The index value whose associated key is to be returned.

Return:

	result	The value previously associated with the index key (see the set procedure).

#endif

TVAL TDatabase_RefIndexKey(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(theDiskPage);
EndFrame

self = selfTval.u.Repository;


*theDiskPage = TDirectory_GetIV2(gCP,gTP,TOBJ(self->itsIndex),index1,TINT(0));

FrameExit(*theDiskPage);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetInIndexOnly

Summary:
 
	Save an entry in the Database Object Repository index.

Arguments:

	aGor		The ObjectRepository which is to be referenced.
	index		The index value whose associated object is to be returned.
	newValue	The new value to be saved in the specified index key.

Return:

	result	The value previously associated with the index key (see the set procedure).

#endif

TVAL TDatabase_SetInIndexOnly(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL newValue)
{
StartFrame
DeclareOBJ(TDatabase,self);
DeclareTVAL(ret);
EndFrame

self = selfTval.u.Repository;


*ret = TDirectory_SetIV1(gCP,gTP,TOBJ(self->itsIndex),index1,newValue);

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TDatabase_New

Create a new TDatabase.

#endif

TDatabase*   TDatabase_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TDatabase,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TDatabase_Initialized) TDatabase_Init(gCP,gTP);

self = (TDatabase*)TObject_OperatorNew(gCP,gTP);

self->itsObjectType = TYOBJREPOSITORY;
self->itsFileName = NIL;
self->itsOdbID = NIL;
self->itsIndex = NIL;
self->itsBufferKeys = NIL;
self->itsBufferValues = NIL;
self->itsBaseFilePos = 0;
self->itsBufferCount = 0;
self->itsBufferIndex = 0;
self->itsCodeKey = TBOOL(TRUE);
self->itsTransactionOn = FALSE;
self->itsTreeOn = FALSE;
self->itsMaxItemIndex = 0;
self->itsNilArray = (HMChar)&self->itsImmediatePtr;
self->itsImmediatePtr = NULL;

FrameExit(self);
}


