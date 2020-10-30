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

#define _C_TSYMBOL
#define _SMARTBASE
#if 0
TSymbol.c

Implementation of the symbol class which stores a K&R C string in a variable length
object, provides for a global symbol/value table, and provides for local symbol/value
bindings.

Note:   Duplicate TSymbol objects containing the same K&R C string are eliminated
        by the TSymbol constructors. Therefore, TSymbol object may be compared for
        equality on the basis of the object handles alone. 

PARENT:             TString

AUTHORS:            Michael F. Korns

MODIFICATIONS:  
    
#endif

#include "tsymbol.h"
#include "tobjvec.h"
#include "tstruct.h"
#include "tdiction.h"
#include "tlambda.h"
#include "fproc.h"
#include "fobject.h"
#include "fconio.h"
#include "flisp.h"
#include "fcompile.h"
#include "fmake.h"
#include "twkspace.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_Init

Initialize the TSymbol class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TSymbol_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
TVAL			symbolTable;

/*  Don't initialize more than once. */
if (gCP->TSymbol_Initialized) return;
gCP->TSymbol_Initialized = TRUE;

/*  Initialize the new alpha sorted Symbol Table. */
gCP->TSymbol_SymbolTable_ActiveCount = 0;
gCP->TSymbol_SymbolTable_MaxEmptyCount = 1000;
gCP->TSymbol_SymbolTable_MinEmptyCount = 10;
gCP->TSymbol_SymbolTable_NewAllocationSize = 100;
gCP->TSymbol_SymbolTable = TObjVector_New(gCP,gTP);
symbolTable.Tag = TYOBJVECTOR;
symbolTable.u.ObjVector = gCP->TSymbol_SymbolTable;
TObjVector_SetMaxIndex(gCP,gTP,symbolTable,gCP->TSymbol_SymbolTable_NewAllocationSize);

/*  Initialize the new String type which is necessary for the Symbol class. */
TString_Init(gCP,gTP);

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					 TYSYMBOL,
					(LpCHAR)"Symbol",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&FMake_Symbol,
					&TSymbol_Mark,
					&TObject_GlobalMarkNever,
					&TSymbol_SymbolAnyCnv,
					&TSymbol_SymbolAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TString_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TString_Map, 
					&TString_Mapc,
					&TObject_PrintDefault,
					&TSymbol_Load,
					&TSymbol_Save,
					&TSymbol_ComputeSize,
					&FObject_CopyNever,
					&TSymbol_Doomed);

FSmartbase_NewType (gCP,
					gTP,
					 TYGLOBALS,
					(LpCHAR)"Globals",
					_TObject_TfNATIVE,
					0,
					(LpFNEW)&TObject_NewNever,
					&TObject_MarkNever,
					&TObject_GlobalMarkNever,
					&TSymbol_GlobalAnyCnv,
					&TSymbol_GlobalAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TSymbol_MapGlobal, 
					&TSymbol_MapcGlobal, 
					&TSymbol_PrintGlobal,
					&TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType (gCP,
					gTP,
					 TYQUOTEDSYMBOL,
					(LpCHAR)"QuotedSymbol",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&FMake_QuotedSymbol,
					&TSymbol_Mark,
					&TObject_GlobalMarkNever,
					&TSymbol_SymbolAnyCnv,
					&TSymbol_SymbolAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapDefault, 
					&TObject_Mapc, 
					&TSymbol_PrintQuoted,
					&TSymbol_Load,
					&TSymbol_Save,
					&TSymbol_ComputeSize,
					&FObject_CopyNever,
					&TSymbol_Doomed);
                            
/*  Create the NULL symbol which is available globally. */
gCP->TSymbol_NIL = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"");
FObject_Perm(gCP,gTP,(TObject*)gCP->TSymbol_NIL, TRUE);


/* Change names "TSpreadsheet_ to TSymbol " */
gCP->TSymbol_Formula = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"formula");
FObject_Perm(gCP,gTP,(TObject*)gCP->TSymbol_Formula,TRUE);
gCP->TSymbol_Script = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"script");
FObject_Perm(gCP,gTP,(TObject*)gCP->TSymbol_Script,TRUE);
gCP->TSymbol_Value = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"value");
FObject_Perm(gCP,gTP,(TObject*)gCP->TSymbol_Value,TRUE);
gCP->TSymbol_hostSend = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"hostSend"); 
FObject_Perm(gCP,gTP,(TObject*)gCP->TSymbol_hostSend,TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_CheckSymbolTable

Check the SmartBase Symbol Table for errors. An incorrect symbol table may occur for many reasons. 
There may be a non symbol type in the symbol table. The symbol table may be out of order. 

#endif

void    TSymbol_CheckSymbolTable(LpXCONTEXT gCP,LpTHREAD gTP)
{
LpCHAR				oldStr;
LpCHAR				newStr;
TSymbol*            aSymbol;
NUM                 cn;

/* First, we examine every Symbol in the current symbol table. Only */
/* if we find a problem do we then attempt to fix the symbol table. */
if (gCP->TSymbol_SymbolTable == NULL) goto FoundError;
newStr = "";
for(cn = 0; cn < gCP->TSymbol_SymbolTable_ActiveCount; cn++)
    {
	/*  Examine each symbol within the old symbol table. */
    
    aSymbol = (TSymbol*)(atHMObject(gCP->TSymbol_SymbolTable->itsObjectArray,cn));

	/* The current symbol must be a valid symbol. */
	if (!_VALIDOBJ(aSymbol) || 
		(aSymbol->itsObjectType != TYSYMBOL) ||
		((aSymbol->itsImmediatePtr == NULL) && ((aSymbol->itsCString == NULL) || (((LpMHANDLEINFO)aSymbol->itsCString)->Free == TRUE))))
		{
		/* WARNING: We have detected a corrupted symbol table.           */
		/*			We must correct the symbol table before proceeding.  */
		/*			Obviously, this should never happen, but if it does, */
		/*			the SmartBase engine attempts self repair.           */
		goto FoundError;
		}


	/* The current symbol must be unique and must be stored in sorted order. */
	oldStr = newStr;
	newStr = &atHMChar(aSymbol->itsCString,0);
	if ((cn > 0) && (strcmp(oldStr,newStr) >= 0))
		{
		/* WARNING: We have detected a corrupted symbol table.           */
		/*			We must correct the symbol table before proceeding.  */
		/*			Obviously, this should never happen, but if it does, */
		/*			the SmartBase engine attempts self repair.           */
		goto FoundError;
		}

    } /* end for */

/* If no errors were found, return to caller. */
return;

FoundError:
/*  Only during special debugging do we attempt to correct errors to the symbol table. */
FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
} /* end function */


/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_MakeUnique

Return a unique TSymbol containing the specified K&R C string. If a TSymbol object
already exists with the same contents, return the existing TSymbol object.

Note:   TSymbol objects are always unique. There are never two TSymbol object which
        contain the same K&R C string data. This allows us to save time in string
        compares, since we can compare TSymbols for equality by simply comparing 
        object handles. If the object handles are not the same, the TSymbol contents
        are not the same.

Note:	To increase the speed of Symbol lookup, we use a symbol hash table
		which is an object vector (accessed as a hash table) of alphabetically
		sorted symbol vectors (accessed by binary search). The size of the
		first hash table object vector is defined in TSymbol.h. 	
        Tests indicate that compiler is at least 6% faster with hashing enabled.

#endif

TSymbol*    TSymbol_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString)
{
COMPARE                 compareCode;
NUM                     indexOf;
NUM                     lengthOf;
HMChar                  theData;
LpOBJ					symbolTablePtr;
NUM						symbolTableIndex;
StartFrame
DeclareOBJ(TSymbol,theSymbol);  
DeclareTVAL(symbolTable);
EndFrame   

/*  Search for a matching symbol object. If we find a match, we return the matching  */
/*  TSymbol object. If no match is found, we create a new TSymbol object and stuff */
/*  the K&R C string data into the new TSymbol object. */

if(!gCP->TSymbol_Initialized)
    TSymbol_Init(gCP,gTP); 
    
/*  Note:	To increase the speed of Symbol lookup, we use a symbol table vector */
/*			which is sorted alphabetically on the text contents of each symbol. */
if ((!_CHECKOBJ(gCP->TSymbol_SymbolTable)) ||
    (gCP->TSymbol_SymbolTable_ActiveCount >= (gCP->TSymbol_SymbolTable->itsMaxItemIndex-1)))
	{
	/* WARNING: We have detected a corrupted symbol table. */
	TSymbol_CheckSymbolTable(gCP,gTP);
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}

symbolTableIndex = TSymbol_FindInSymbolTable(gCP,gTP,(LpCHAR)aString,&compareCode);
if (compareCode == EQUAL)
    {
    FrameExit((TSymbol*)(atHMObject(gCP->TSymbol_SymbolTable->itsObjectArray,symbolTableIndex)));
    }
    
/*  Since no match was found, we create a new TSymbol here. */
/*  Note:   We initialize its global value to nil, etc.     */
theSymbol = (TSymbol*)TSymbol_New(gCP, gTP);
theSymbol->itsGlobalValue = gCP->Tval_VOID;
theSymbol->itsVMEmulator = NULL;
theSymbol->itsUserTypeParent = NULL;
theSymbol->itsUserTypeMethods = NULL;
theSymbol->itsUserTypeFields = NULL;

/*  Now Stuff the K&R C string data into the TSymbol object.   */
/*  Note:   We set the itNeedsBars switch if the symbol        */                                                               
/*          contain imbedded blanks or special characters.     */
lengthOf = strlen((char*)aString);
if (lengthOf < (NUM)(_TSymbol_ImmediateSpace-1))
	{
	/*  We can stuff the K&R C string data into the object immediate space. */
    theSymbol->itsCString = (HMChar)&theSymbol->itsImmediatePtr;
	theSymbol->itsImmediatePtr = (CHAR*)&theSymbol->itsImmediateSpace[0];
	theSymbol->itsMaxItemIndex = lengthOf+1;
	/* Does the symbol contain imbedded blanks or special characters? */
	for (indexOf = 0; indexOf < lengthOf; ++indexOf)
		{
		if (!ISSYMBOL((NUM)aString[indexOf]) || ISPUNCT((NUM)aString[indexOf]))
			theSymbol->itNeedsBars = TRUE;
		theSymbol->itsImmediatePtr[indexOf] = aString[indexOf];
		}
	}
else
	{
	/*  We need to allocate space for the K&R C string data. */
	theData = (HMChar)FMemory_New(gCP, gTP, lengthOf+1,TRUE);
	atHMChar(theData, lengthOf) = 0;
	theSymbol->itsMaxItemIndex = lengthOf+1;
	theSymbol->itsCString = theData;
	/* Does the symbol contain imbedded blanks or special characters? */
	for (indexOf = 0; indexOf < lengthOf; ++indexOf)
		{
		if (!ISSYMBOL((NUM)aString[indexOf]) || ISPUNCT((NUM)aString[indexOf]))
			theSymbol->itNeedsBars = TRUE;
		atHMChar(theData, indexOf) = aString[indexOf];
		}
	}

/*  We now insert the Symbol object into the Symbol Table.       */
/*  Note:  The Symbol Table must be kept in alpha sorted order.  */
/*         We search a 2nd time because the new Symbol creation  */
/*         may have caused a garbage collection, which may have  */
/*         deleted doomed Symbols from the Symbol Table.         */
symbolTableIndex = TSymbol_FindInSymbolTable(gCP,gTP,(LpCHAR)aString,&compareCode);
if (compareCode == EQUAL)
    {
	/* WARNING: We have detected a corrupted symbol table.           */
	/* Note: We should NOT have found a match just before inserting. */
	TSymbol_CheckSymbolTable(gCP,gTP);
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    }
symbolTablePtr = (LpOBJ)*gCP->TSymbol_SymbolTable->itsObjectArray;                                                               
lengthOf = gCP->TSymbol_SymbolTable_ActiveCount++;
for (indexOf = lengthOf-1;indexOf >= symbolTableIndex; --indexOf)
	{
	symbolTablePtr[indexOf+1] = symbolTablePtr[indexOf];
	}
indexOf = symbolTableIndex;
symbolTablePtr[indexOf] = (OBJ)theSymbol;
lengthOf = gCP->TSymbol_SymbolTable->itsMaxItemIndex;
for (indexOf = gCP->TSymbol_SymbolTable_ActiveCount;indexOf < lengthOf; ++indexOf)
	{
	symbolTablePtr[indexOf] = NIL;
	}
   

/*  Do we need to allocate more space for the Symbol Table?  */
if ((gCP->TSymbol_SymbolTable->itsMaxItemIndex - gCP->TSymbol_SymbolTable_ActiveCount) <= gCP->TSymbol_SymbolTable_MinEmptyCount)
	{
	symbolTable->Tag = TYOBJVECTOR;
	symbolTable->u.ObjVector = gCP->TSymbol_SymbolTable;
	TObjVector_SetMaxIndex(gCP,gTP,*symbolTable,gCP->TSymbol_SymbolTable_ActiveCount+gCP->TSymbol_SymbolTable_NewAllocationSize);
	symbolTablePtr = (LpOBJ)*gCP->TSymbol_SymbolTable->itsObjectArray;                                                               
	lengthOf = gCP->TSymbol_SymbolTable->itsMaxItemIndex;
	for (indexOf = gCP->TSymbol_SymbolTable_ActiveCount;indexOf < lengthOf; ++indexOf)
		{
		symbolTablePtr[indexOf] = NIL;
		}
	}

FrameExit(theSymbol);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_SubString_MakeUnique

Return a unique TSymbol containing the specified K&R C string. If a TSymbol object
already exists with the same contents, return the existing TSymbol object.

Note:   TSymbol objects are always unique. There are never two TSymbol object which
        contain the same K&R C string data. This allows us to save time in string
        compares, since we can compare TSymbols for equality by simply comparing 
        object handles. If the object handles are not the same, the TSymbol contents
        are not the same.

#endif

TSymbol*    TSymbol_SubString_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString, NUM firstChar, NUM theLen)
{
HMChar                  theData;
StartFrame
DeclareOBJ(TSymbol,theSymbol);
EndFrame              

theData = (HMChar)FMemory_New(gCP, gTP, theLen+1,TRUE);

strncpy((char*)&atHMChar(theData,0), (const char*)&aString[firstChar], theLen);
atHMChar(theData,theLen) = 0;

theSymbol = TSymbol_MakeUnique(gCP,gTP,&atHMChar(theData,0));
FMemory_Free(gCP, gTP, (HMemory)theData);

FrameExit(theSymbol);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_GetIVGlobal

The get indexed value function for the Global Symbol native type.

#endif

TVAL TSymbol_GetIVGlobal(LpXCONTEXT gCP,LpTHREAD gTP,TVAL index)
{
gTP = gTP; // NOOP to hide unused parameter warning message
if (asTag(&index) == TYSYMBOL)
    {
    return(asSymbol(&index)->itsGlobalValue);
    }
return(gCP->TObject_ERROR_SYMBOLINDEXREQ);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_SetIVGlobal

The get indexed value function for the Global Symbol native type.

#endif

TVAL TSymbol_SetIVGlobal(LpXCONTEXT gCP,LpTHREAD gTP,TVAL index,TVAL newValue)
{
if (asTag(&index) == TYSYMBOL)
    {
    return(TSymbol_SetGlobalValue(gCP,gTP,asSymbol(&index),newValue));
    }
return(gCP->TObject_ERROR_SYMBOLINDEXREQ);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_MapGlobal

Scan through Object space and call the given procedure for every Object of type TYSYMBOL 
for whom itsGlobalValue is not void.

To store the results of the procedure invocation create a Structure which contains 
the Symbol found paired with the TVAL containing the result of the procedure invocation.

#endif

TVAL    TSymbol_MapGlobal(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc)
{
NUM             indexOf;
NUM             newRepeats;
StartFrame
DeclareOBJ(TSymbol,theSymbol);
DeclareOBJ(TStructure,theStructure);
DeclareTVAL(symbol);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

self = self; // NOOP to hide unused parameter warning message
/* Initialize */
newRepeats = 0; 
theStructure = NULL;
*ret = gCP->Tval_VOID;
asObject(ret) = NULL;

/* Search through Object space and call the given procedure for each TYSYMBOL found */
/* for whom itsGlobalValue is not void. */

for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if ((_TObject_ObjectFlag(indexOf) != _TObject_OfVOID) &&
        (_TObject_ObjectByIndex(indexOf)->itsObjectType == TYSYMBOL))
        {
        theSymbol = (TSymbol*)_TObject_ObjectByIndex(indexOf);
        if(asTag(&theSymbol->itsGlobalValue) != TYVOID)
            {
            
            asTag(symbol) = TYSYMBOL;
            asObject(symbol)  = (TObject*)theSymbol;
            
            /* Call the passed proc and store the result in a Structure */
            
            if(theStructure == NULL)
                {
                /* Create a Structure as needed */
                theStructure = TStructure_New(gCP,gTP);
                }
                
            /* Grow the Structure to hold the next result */
            
            FObject_SetMaxIndex(gCP,gTP,(TObject*)theStructure, newRepeats+1);

            *tmp = FSmartbase_Evalv(gCP,gTP,proc, 1, symbol);
            if(isERROR(tmp))
                {
                FrameExit(*tmp);
                }
            else
                atHMBind(theStructure->itsDictionaryArray,newRepeats).Value = *tmp;
                
            newRepeats++;
            }
        }
    }
if(theStructure != NULL)
    {
    asTag(ret) = TYSTRUCTURE;
    asObject(ret) = (TObject*)theStructure;
    }   
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_MapcGlobal

Scan through Object space and call the given procedure for every Object of type TYSYMBOL 
for whom itsGlobalValue is not void.

Return the result of the last evaluation as a TVAL.

#endif

TVAL    TSymbol_MapcGlobal(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TSymbol,theSymbol);
DeclareTVAL(symbol);
DeclareTVAL(ret);
EndFrame

self = self; // NOOP to hide unused parameter warning message
/* Initialize */
*ret = gCP->Tval_VOID;
asObject(ret) = NULL;

/* Search through Object space and call the given procedure for each TYSYMBOL found */
/* for whom itsGlobalValue is not void. */

for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if ((_TObject_ObjectFlag(indexOf) != _TObject_OfVOID) &&
        (_TObject_ObjectByIndex(indexOf)->itsObjectType == TYSYMBOL))
        {
        theSymbol = (TSymbol*)_TObject_ObjectByIndex(indexOf);
        if(asTag(&theSymbol->itsGlobalValue) != TYVOID)
            {
            asTag(symbol) = TYSYMBOL;
            asObject(symbol)  = (TObject*)theSymbol;
            
            /* Call the passed proc */
            
            if(asTag(&proc) == TYCFUNCTION )
                {
                *ret = (*asFunction(&proc))(gCP, gTP, 1, symbol);
                }
            else
            if(asTag(&proc) == TYCPROCEDURE )
                {
                *ret = (*(asSymbol(&proc)->itsCProcedure))(gCP, gTP, 1, symbol);
                }
            else
            if( asTag(&proc) == TYLAMBDA )
                {
                *ret = _VmEvaluate(asProcedure(&proc), 1, symbol);
                }
            else
                {
                FrameExit(gCP->TObject_ERROR_INVALID);
                }
            }
        }
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_PrintGlobal

Print routine for TYGLOBALS.

#endif

TVAL    TSymbol_PrintGlobal(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
gTP = gTP; // NOOP to hide unused parameter warning message
self = self; // NOOP to hide unused parameter warning message
size = size; // NOOP to hide unused parameter warning message
strcat((char*)buf,(const char*)"#<GlobalSymbolTable>");
return(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TSymbol_Load(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
long            theOffset;
CHAR            cSave;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argTval);
DeclareOBJ(TSymbol,it);
EndFrame

*ret = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    /*  We store the stringdata non-terminated so we must take that into account here. */
    /*  Since we know that a symbol has more data of its own allocated after the  */
    /*  itsStringData we may simply save a byte of this data, terminate the string */
    /*  create the symbol, and restore the byte. */
    
    cSave = TStringOnDiskPtr(aHMemory,0)->itsStringData[TStringOnDiskPtr(aHMemory,0)->itsMaxItemIndex];
    TStringOnDiskPtr(aHMemory,0)->itsStringData[TStringOnDiskPtr(aHMemory,0)->itsMaxItemIndex] = 0;
    it = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)TStringOnDiskPtr(aHMemory,0)->itsStringData);
    TStringOnDiskPtr(aHMemory,0)->itsStringData[TStringOnDiskPtr(aHMemory,0)->itsMaxItemIndex] = cSave;
    *ret = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TSymbol*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asObject(argTval) = (TObject*)it;
        asTag(argTval) = it->itsObjectType;
        theOffset = TSymbol_GetInheritedComputedSize(gCP,gTP,*argTval) - SIZEOF_TObjectOnDisk;
     it->itNeedsBars = TSymbolOnDiskPtr(aHMemory,theOffset)->itNeedsBars;

  /*  Load Symbol global values only if _saveTypes is true. */
  
  if ((gCP->TSymbol_SaveTypes->itsGlobalValue.Tag == TYBOLE) &&
      (gCP->TSymbol_SaveTypes->itsGlobalValue.u.Bool == TRUE))
   {
		 it->itsVMEmulator		= (TLambda*)TObject_CheckRegistration(gCP,gTP,TSymbolOnDiskPtr(aHMemory,theOffset)->itsVMEmulator);
         it->itsUserTypeParent	= (TSymbol*)TObject_CheckRegistration(gCP,gTP,TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeParent);
         it->itsUserTypeFields	= (TStructure*)TObject_CheckRegistration(gCP,gTP,TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeFields);
         it->itsUserTypeMethods = (TDictionary*)TObject_CheckRegistration(gCP,gTP,TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeMethods);
         it->itsGlobalValue		= TObject_LoadTval(gCP,gTP,TSymbolOnDiskPtr(aHMemory,theOffset)->itsGlobalValue);
         it->itsGlobalLock		= TSymbolOnDiskPtr(aHMemory,theOffset)->itsGlobalLock;
         }
        asTag(ret) = it->itsObjectType;
        asObject(ret) = (TObject*)it;
        }
    else
        *ret = gCP->TObject_ERROR_INVALID;
    }

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_PrintQuoted

Print a quoted symbol

#endif

TVAL TSymbol_PrintQuoted(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
NUM     qCnt;
StartFrame
DeclareTVAL(retTval);
EndFrame

qCnt = asQuoteCnt(&self);
if(*size + qCnt + 1 < gCP->TObject_MaxOutputLen)
    {
    if(qCnt > 1)
        {
        while(qCnt-- > 0)
            {
            buf[(*size)]    = '\'';
            buf[++(*size)]  = 0;
            }
        }
    asTag(&self) = TYSYMBOL;
    *retTval = TObject_PrintDefault(gCP,gTP, self,  size,  buf);
    if(qCnt == 1 )
        {
        if(*size + qCnt + 1 < gCP->TObject_MaxOutputLen)
            {
            buf[(*size)]    = ':';
            buf[++(*size)]  = 0;
            }
        else
            FrameExit(gCP->TObject_FALSE);
        }
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_FALSE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_SymbolAnyCmp

Compare a typed value to a value of type SYMBOL.

#endif

TVAL TSymbol_SymbolAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP, TVAL leftVal, TVAL rightVal)
{
REAL            tmpValue;
LpCHAR          aStringPtr;
StartFrame
DeclareOBJ(TSymbol,TmpSymbol);
DeclareTVAL(ErrValue);
EndFrame

if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&rightVal));

    if (aStringPtr == NULL)
		{
        FrameExit(gCP->TObject_ERROR_INVALID);
		}
    else
		{
		*ErrValue= TObject_strcmp(gCP,gTP,&atHMChar(asSymbol(&leftVal)->itsCString,0),aStringPtr);
        FrameExit(*ErrValue);
		}
    }
if (asTag(&rightVal) == TYTEXT)
    {
	*ErrValue= TObject_strcmp(gCP,gTP,&atHMChar(asSymbol(&leftVal)->itsCString,0),asText(&rightVal));
    FrameExit(*ErrValue);
    }
else
if (asTag(&rightVal) == TYBOLE)
    {
    if (asBool(&rightVal) == TRUE)
		{
		*ErrValue= TObject_strcmp(gCP,gTP,&atHMChar(asSymbol(&leftVal)->itsCString,0),(LpCHAR)"true");
        FrameExit(*ErrValue);
		}
    else
		{
		*ErrValue= TObject_strcmp(gCP,gTP,&atHMChar(asSymbol(&leftVal)->itsCString,0),(LpCHAR)"false");
        FrameExit(*ErrValue);
		}
    }
else
if (asTag(&rightVal) == TYCHAR)
    {
    if ((&atHMChar(asSymbol(&leftVal)->itsCString,0))[0] == asChar(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if ((&atHMChar(asSymbol(&leftVal)->itsCString,0))[0] < asChar(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYNUM) || (asTag(&rightVal) == TYPOINTER))
    {
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asSymbol(&leftVal)->itsCString,0));
    _TObject_ErrorChk(*ErrValue);

    if (tmpValue == asInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (tmpValue < asInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYCOMPARE) || (asTag(&rightVal) == TYTYPE))
    {
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asSymbol(&leftVal)->itsCString,0));
    _TObject_ErrorChk(*ErrValue);

    if (tmpValue == asShort(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (tmpValue < asShort(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (asTag(&rightVal) == TYREAL)
    {
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asSymbol(&leftVal)->itsCString,0));
    _TObject_ErrorChk(*ErrValue);

    if (tmpValue == asReal(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (tmpValue < asReal(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_GlobalAnyCmp

Compare a typed value to a value of type TyGLOBAL.

#endif

TVAL TSymbol_GlobalAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{

gTP = gTP; // NOOP to hide unused parameter warning message
if (asTag(&leftVal) == TYGLOBALS && asTag(&rightVal) == TYGLOBALS)
    {
    return(gCP->TObject_EQUAL);
    }
else
    return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_GlobalAnyCnv

Convert a typed value to a value of type TyGLOBAL.

#endif

TVAL TSymbol_GlobalAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{

gTP = gTP; // NOOP to hide unused parameter warning message
tTarget = tTarget; // NOOP to hide unused parameter warning message
if (asTag(&oldValue) == TYGLOBALS)
    {
    return(oldValue);
    }
else
    return(gCP->TObject_ERROR_INVALID);
}



/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_SymbolAnyCnv

Convert a typed value to a value of type TYSYMBOL.

#endif

TVAL TSymbol_SymbolAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP, TYPE tTarget, TVAL oldValue)
{
CHAR            typeName[256];
CHAR            temp[256];
LpCHAR          aStringPtr;
StartFrame
DeclareTVAL(retTval);
EndFrame

tTarget = tTarget; // NOOP to hide unused parameter warning message
*retTval = gCP->Tval_VOID;

if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
		aStringPtr = (LpCHAR)"#void";
    else
        {
        aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
        }

    if (aStringPtr == NULL)
        {
        strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&oldValue)));
        sprintf((char*)temp,"#<%s "INTFORMAT">",typeName,asObject(&oldValue)->itsObjectIndex);
        aStringPtr = (LpCHAR)&temp;
        }
    asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,aStringPtr);
	asTag(retTval) = TYSYMBOL;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYTEXT)
    {
    asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,oldValue.u.Text);
	asTag(retTval) = TYSYMBOL;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL)
        {
        strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&oldValue)));
        sprintf((char*)temp,"#<%s ("INTFORMAT")| "INTFORMAT" "INTFORMAT">",typeName,(NUM)ObjIdx(oldValue),(NUM)SubOff(oldValue),(NUM)SubLen(oldValue));
        aStringPtr = temp;
        asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,aStringPtr);
        }
    else
        {
        asObject(retTval) = (TObject*)TSymbol_SubString_MakeUnique(gCP,gTP,aStringPtr,0,SubLen(oldValue));
        }
    asTag(retTval) = TYSYMBOL;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYVOID)
    {
    asObject(retTval) = (TObject*)gCP->TSymbol_NIL;
	asTag(retTval) = TYSYMBOL;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYBOLE)
    {
    if (asBool(&oldValue) == TRUE)
        asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"true");
    else
        asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"false");
	asTag(retTval) = TYSYMBOL;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYCHAR)
    {
    temp[0] = asChar(&oldValue);
    temp[1] = 0;
    asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)temp);
 	asTag(retTval) = TYSYMBOL;
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    sprintf((char*)temp,INTFORMAT,asInt(&oldValue));
    asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,temp);
 	asTag(retTval) = TYSYMBOL;
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    sprintf((char*)temp,SHORTFORMAT,asShort(&oldValue));
    asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,temp);
 	asTag(retTval) = TYSYMBOL;
	FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYREAL)
    {
    TObject_sprintReal(gCP,gTP,(char*)temp,asReal(&oldValue));
    asObject(retTval) = (TObject*)TSymbol_MakeUnique(gCP,gTP,temp);
 	asTag(retTval) = TYSYMBOL;
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_BSearch

Search an alphabetically sorted symbol vector for the specified K&R string.
Returns a TVAL containing the index for the closest match found, maybe an exact match.
Also sets the compare code (based by reference) to indicate whether the index is an
exact match, is low, or is high.

WARNING:	Normally this function returns a numeric index value. If a corrupted symbol 
			table should ever be discovered, this function will return a boolean value 
			of FALSE!!

Note:	To increase the speed of Symbol lookup, we use a symbol hash table
		which is an object vector (accessed as a hash table) of alphabetically
		sorted symbol vectors (accessed by binary search). The size of the
		first hash table object vector is defined in TSymbol.h.
        Tests indicate that compiler is at least 6% faster with hashing enabled.

#endif

TVAL TSymbol_BSearch(LpXCONTEXT gCP,LpTHREAD gTP,TObjVector* alphaSortedVector, LpCHAR aString, COMPARE* compareCode)
{
register LpCHAR     newStr;
register LpCHAR     tstStr;
register HMObject   itsItemArray;
register TSymbol*	aSymbol;
register NUM        low;
register NUM        mid;
register NUM        high;
register NUM        count;
register NUM        compare = 0;
StartFrame
DeclareTVAL(retTval);
EndFrame

*compareCode = -1;
asTag(retTval) = TYNUM;
asInt(retTval) = mid = 0;
low = 0;

if ((alphaSortedVector == NULL) || !_VALIDOBJ(alphaSortedVector))
    {
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    }

if (alphaSortedVector->itsMaxItemIndex != 0)
    {
    high = count = alphaSortedVector->itsMaxItemIndex - 1;
    itsItemArray = alphaSortedVector->itsObjectArray;
    }   
else
    {
    FrameExit(*retTval);
    }


while ((low <= high) && (count-- >= 0))

    {   
    mid = (low + high) >> 1;
    aSymbol = (TSymbol*)atHMObject(itsItemArray,mid);
	if (!_VALIDOBJ(aSymbol) || 
		(aSymbol->itsObjectType != TYSYMBOL) ||
	    ((aSymbol->itsImmediatePtr == NULL) && ((aSymbol->itsCString == NULL) || (((LpMHANDLEINFO)aSymbol->itsCString)->Free == TRUE))))
		{
		/* WARNING: We have detected a corrupted symbol table. */
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
    tstStr = &atHMChar(aSymbol->itsCString,0);
    newStr = (LpCHAR)aString;
    
    for(; *newStr == *tstStr; newStr++, tstStr++)
		{
        if(*newStr == 0)
            {
            compare = 0;
            goto AllDone;
            }
		}
    
    compare = (((unsigned char)*newStr) - ((unsigned char)*tstStr));

    if ( compare < 0 )
        {
        high = mid - 1;
        }
    else 
        {
         low = mid + 1;
        }
   }
   
AllDone:

*compareCode = (compare != 0) ? ((compare < 0) ? LOW : HIGH) : EQUAL;
asInt(retTval) = mid;
FrameExit(*retTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_FindInSymbolTable

Search the alphabetically sorted Symbol Table for the specified K&R string.
Returns a NUM containing the index for the closest match found, maybe an exact match.
Also sets the compare code (based by reference) to indicate whether the index is an
exact match, is low, or is high.

Note:	To increase the speed of Symbol lookup, we use a symbol table which is
		an object vector of alphabetically sorted symbols (accessed by binary-hash search).

#endif

NUM TSymbol_FindInSymbolTable(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR findMeString,COMPARE* compareCode)
{
register LpCHAR     tableString;
register TSymbol**  symbolTable = NULL;
register TSymbol*	aSymbol;
register NUM        low;
register NUM        mid;
register NUM        high = 0;
register NUM        localLow;
register NUM        findIndex;
register NUM        localHigh;
register NUM        count = 0;

*compareCode = LOW;
low = findIndex = mid = 0;

/* Initialize for the binary search of the Symbol Table */
if ((gCP->TSymbol_SymbolTable == NULL) || !_VALIDOBJ(gCP->TSymbol_SymbolTable))
    {
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    }
else
if (gCP->TSymbol_SymbolTable_ActiveCount > 0)
    {
    high = count = gCP->TSymbol_SymbolTable_ActiveCount-1;
    symbolTable = (TSymbol**)*gCP->TSymbol_SymbolTable->itsObjectArray;
    }   
else
    {
    /* Always return an index of zero and a compare of LOW if Symbol Table is empty */
	*compareCode = LOW;
    return(findIndex);
    }


/* Perform a binary search of the Symbol Table */
while ((low <= high) && (count-- >= 0))
    {   
    mid = (low + high) >> 1;
    aSymbol = symbolTable[mid];
    tableString = objSymbolArray(aSymbol);
    
    if ( (*compareCode = strcmp(findMeString,tableString)) == 0 )
        {
        findIndex = mid;
        goto AllDone;
        }
    if ( *compareCode < 0 )
        {
        high = mid - 1;
        }
    else 
        {
         low = mid + 1;
        }
   }	
   
/* Make sure the resulting Symbol Table index is valid. */
if ((mid < 0) || (mid >= gCP->TSymbol_SymbolTable_ActiveCount))
	{
	/* WARNING: We have detected an invalid index. */
	TSymbol_CheckSymbolTable(gCP,gTP);
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}

/* Convert to a local linear search of the Symbol Table. */
*compareCode = HIGH;
localLow = (mid >= 2) ? (mid - 2) : 0;
localHigh = ((mid + 2) < gCP->TSymbol_SymbolTable_ActiveCount) ? (mid + 2) : gCP->TSymbol_SymbolTable_ActiveCount;
for (findIndex = localLow; findIndex < localHigh; ++findIndex)
	{
    aSymbol = symbolTable[findIndex];
    tableString = objSymbolArray(aSymbol);
    if ( (*compareCode = strcmp(findMeString,tableString)) <= 0 ) goto AllDone;
	}
   
AllDone:
return(findIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_AddSortedValue

Adds a new symbol object to the alpha-sorted symbol table vector.
Note:	To increase the speed of Symbol lookup, we use a symbol table vector
		which is sorted alphabetically on the text contents of each symbol.
#endif

TVAL TSymbol_AddSortedValue(LpXCONTEXT gCP,LpTHREAD gTP,TObjVector* alphaSortedVector, TSymbol* aTSymbol)
{
COMPARE             compareCode;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tvalIndex);
DeclareTVALArray(parms,3);

EndFrame

ret->Tag = TYBOLE;
ret->u.Bool = TRUE;

*tvalIndex = TSymbol_BSearch(gCP, gTP, alphaSortedVector, (char *)*aTSymbol->itsCString, &compareCode);
if (tvalIndex->Tag != TYNUM) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);


parms[0] = TOBJ(alphaSortedVector);
parms[1] = *tvalIndex;
parms[2] = TOBJ(aTSymbol);

if (compareCode < 0)
    {
    *ret = TObjVector_Insert(gCP, gTP, parms[0], parms[1], parms[2]);
    FrameExit(*ret);
    }
else
if (compareCode == 0)
    {
    FrameExit(gCP->Tval_TRUE);
    }
else
    {
    ++parms[1].u.Int;
    *ret = TObjVector_Insert(gCP, gTP, parms[0], parms[1], parms[2]);
    FrameExit(*ret);
    }


FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Doomed

Garbage collection is about to delete this object. Dispose of the string data.

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

void    TSymbol_Doomed(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
COMPARE         compareCode;
NUM				indexOf;
NUM				lengthOf;
LpOBJ			symbolTablePtr;
NUM				symbolTableIndex;
StartFrame
DeclareOBJ(TSymbol,self);
EndFrame


self = (TSymbol*)asObject(&selfTval);

/*  Note:	To increase the speed of Symbol lookup, we use a symbol table vector */
/*			which is sorted alphabetically on the text contents of each symbol. */
if ((!_CHECKOBJ(gCP->TSymbol_SymbolTable)) ||
    (gCP->TSymbol_SymbolTable_ActiveCount >= (gCP->TSymbol_SymbolTable->itsMaxItemIndex-1)))
	{
	/* WARNING: We have detected a corrupted symbol table. */
	TSymbol_CheckSymbolTable(gCP,gTP);
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}

/* Check for a symbol object whose text memory handle needs to be freed. */
 
if ((self->itsMaxItemIndex > 0) || (self->itsCString != NULL))
	{
	/* First check for a damaged symbol object, one whose handle is    */
	/* NULL or whose handle has been freed. If we have a damaged symbol */
	/* then we will terminate with a bad object system error.          */
	if ((self->itsImmediatePtr == NULL) && ((self->itsCString == NULL) || (((LpMHANDLEINFO)self->itsCString)->Free == TRUE)))
		{
		TSymbol_CheckSymbolTable(gCP,gTP);
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}

	/*  Remove the index from the TSymbol_SymbolTable                                    */
	/*  Note:	To increase the speed of Symbol lookup, we use a sorted symbol table     */
	/*			which is an object vector of alphabetically sorted symbols (accessed by  */
	/*			binary search).                                                          */

	symbolTableIndex = TSymbol_FindInSymbolTable(gCP,gTP,(LpCHAR)&atHMChar(self->itsCString,0),&compareCode);
	if (compareCode == EQUAL)
		{
		/*  We now remove the Symbol object from the Symbol Table.       */
		/*  Note:  The Symbol Table must be kept in alpha sorted order.  */
		symbolTablePtr = (LpOBJ)*gCP->TSymbol_SymbolTable->itsObjectArray;                                                               
		lengthOf = gCP->TSymbol_SymbolTable_ActiveCount;
		for (indexOf = symbolTableIndex; indexOf < lengthOf; ++indexOf)
			{
			symbolTablePtr[indexOf] = symbolTablePtr[indexOf+1];
			}
		symbolTablePtr[gCP->TSymbol_SymbolTable_ActiveCount--] = NIL;
		lengthOf = gCP->TSymbol_SymbolTable->itsMaxItemIndex;
		for (indexOf = gCP->TSymbol_SymbolTable_ActiveCount;indexOf < lengthOf; ++indexOf)
			{
			symbolTablePtr[indexOf] = NIL;
			}
		}
	else
		{
		/* WARNING: We have detected a corrupted symbol table.           */
		/*			We must correct the symbol table before proceeding.  */
		/*			Obviously, this should never happen, but if it does, */
		/*			the SmartBase engine attempts self repair.           */
		TSymbol_CheckSymbolTable(gCP,gTP);
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}

	TString_Doomed(gCP, gTP, selfTval);
	}

FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TSymbol_Mark(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
TSymbol*        self = (TSymbol*)asObject(&selfTval);

TObject_MarkTval(gCP,gTP,self->itsGlobalValue);
TObject_MarkObj(gCP,gTP,(TObject*)self->itsVMEmulator);
TObject_MarkObj(gCP,gTP,(TObject*)self->itsUserTypeParent);
TObject_MarkObj(gCP,gTP,(TObject*)self->itsUserTypeFields);
TObject_MarkObj(gCP,gTP,(TObject*)self->itsUserTypeMethods);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetInheritedComputedSize

Return the size requirement of your inherited ancestors.

#endif

long    TSymbol_GetInheritedComputedSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
NUM            theSize;

TString_ComputeSize(gCP, gTP, selfTval, &theSize);

return theSize;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TSymbol_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
TString_ComputeSize(gCP, gTP, selfTval, aSize);

*aSize += sizeof(TSymbolOnDisk);
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TSymbol_Save(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,HMemory aHMemory)
{
long            theOffset;
StartFrame
DeclareOBJ(TSymbol,self);
EndFrame

self = (TSymbol*)asObject(&selfTval);

/*  Save TObjectOnDisk and TStringOnDisk */

TString_Save(gCP,gTP, selfTval,  aHMemory);

/*  Save the string part of the Symbol on disk. */

theOffset = TSymbol_GetInheritedComputedSize(gCP,gTP,selfTval);
TSymbolOnDiskPtr(aHMemory,theOffset)->itNeedsBars = self->itNeedsBars;

/*  Save Symbol global values only if _saveTypes is true. */

if ((gCP->TSymbol_SaveTypes->itsGlobalValue.Tag == TYBOLE) &&
    (gCP->TSymbol_SaveTypes->itsGlobalValue.u.Bool == TRUE))
 {
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsGlobalValue	  = TObject_RegisterTval(gCP,gTP,self->itsGlobalValue);
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsVMEmulator	  = TObject_RegisterObject(gCP,gTP,(TObject*)self->itsVMEmulator);
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeParent  = TObject_RegisterObject(gCP,gTP,(TObject*)self->itsUserTypeParent);
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeFields  = TObject_RegisterObject(gCP,gTP,(TObject*)self->itsUserTypeFields);
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeMethods = TObject_RegisterObject(gCP,gTP,(TObject*)self->itsUserTypeMethods);
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsGlobalLock = self->itsGlobalLock;
 }
else
 {
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsGlobalValue = gCP->Tval_VOID;
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsVMEmulator = NIL;
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeParent = NIL;
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeFields = NIL;
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsUserTypeMethods = NIL;
 TSymbolOnDiskPtr(aHMemory,theOffset)->itsGlobalLock = FALSE;
 }

FrameExit(aHMemory);
}
/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TSymbol_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
CHAR            aChar;
NUM             indexOf;
HMChar          theData;
StartFrame
DeclareOBJ(TSymbol,self);
DeclareTVAL(value);
DeclareTVAL(err);
EndFrame

self = (TSymbol*)asObject(&selfTval);

/*  We only accept numeric indices. */

if(isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    indexOf = -1;

/*  Make sure string index is in range. */
if ((indexOf < 0) || (indexOf >= (self->itsMaxItemIndex - 1)))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  We only allow chars as elements of strings. */

*err = TObject_Convert(gCP,gTP,TYCHAR,newValue);
_TObject_ErrorChk(*err);

aChar = asChar(err);

/*  Now we create a temporary placeholder for the altered string. */
theData = (HMChar)FMemory_New(gCP, gTP, self->itsMaxItemIndex + 1,TRUE);
strcpy((char*)&atHMChar(theData, 0),(const char*)&atHMChar(self->itsCString,0));
atHMChar(theData, indexOf) = aChar;

/*  Now return a unique string containing the altered contents. */

*value = gCP->Tval_VOID;
asObj(value) = (OBJ)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)&atHMChar(theData, 0));
asTag(value)  = TYSYMBOL;
FMemory_Free(gCP, gTP, (HMemory)theData);
FrameExit(*value);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetGlobalValue

Set the global value of this symbol.

Note:   

#endif

TVAL TSymbol_GetGlobalValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TSymbol*        self = (TSymbol*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsGlobalValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetGlobalValue

Set the global value of this symbol.

Note:   

#endif

TVAL TSymbol_SetGlobalValue(LpXCONTEXT gCP,LpTHREAD gTP,TSymbol* self, TVAL newValue)
{
gTP = gTP; // NOOP to hide unused parameter warning message
/*  Set the global value of this symbol. */
self->itsGlobalValue = newValue;

return(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TSymbol_New

Create a new TSymbol.

#endif

TSymbol*    TSymbol_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TSymbol_Initialized) TSymbol_Init(gCP,gTP);

self = (TSymbol*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYSYMBOL;
self->itsMaxItemIndex = 0;
self->itsCString = NULL;
self->itsImmediatePtr = NULL;
self->itsGlobalLock = FALSE;
self->itNeedsBars = FALSE;
self->itsGlobalValue = gCP->Tval_VOID;
self->itsVMEmulator = NULL;
self->itsUserTypeParent = NULL;
self->itsUserTypeFields = NULL;
self->itsUserTypeMethods = NULL;
self->itsCProcedure = (LpFUNC)&TObject_NilFunction;

FrameExit(self);
}
