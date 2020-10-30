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
TDirectory.c

Implementation of the Directory class which stores a variable length set of 
(key,value) pairs stored as a linear array. It is one of the main mechanisms 
for variable bonding in SmartBase.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "tdirect.h"
#include "fpred2.h"
#include "fmake.h"
#include "tlambda.h"
#include "fproc.h"
#include "fconio.h"
#include "futil2.h"
#include "futil3.h"
#include "fobject.h"
#include "fmath2.h"
#include "fpred.h"
#include "fvmscpt.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TDirectory_Init

Initialize the TDirectory class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TDirectory_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TDirectory_Initialized) return;
gCP->TDirectory_Initialized     = TRUE;

/*  Initialize the new types for this class. */
FSmartbase_NewType (gCP,
					gTP,
					TYDIRECTORY,
					"Directory",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TDirectory_MakeNew,
					&TDirectory_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TDirectory_SetIV1,
					&TDirectory_SetIV2,
					&FObject_SetIV3Never,
					&TDirectory_GetIV1,
					&TDirectory_GetIV2,
					&FObject_GetIV3Never,
					&TDirectory_Map,
					&TDirectory_Mapc,
					&TObject_PrintObj,
					&TDirectory_Load,
					&TDirectory_Save,
					&TDirectory_ComputeSize,
					&TDirectory_Copy,
					&TDirectory_Doomed);

}

/*--------------------------------------------------------------------------------------- */
#if 0
IDirectory

Initialize a TDirectory object with an array of bondings.

#endif

TVAL    TDirectory_IDirectory(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[])
{
NUM             argIndex;
BOLE			asIsSw = FALSE;
NUM				startIndex = 0;
NUM				n;
TVAL			left;
TVAL			right;
TVAL			prmv[3];
StartFrame
DeclareOBJ(TDirectory,self);
DeclareTVAL(newKey);
DeclareTVAL(newValue);
DeclareTVAL(ret);
EndFrame

self = (TDirectory*)asObject(&selfTval);

/*  Check for an ellipses as the first argument. This would indicate to */
/*  create the new Directory 'unsorted as is'. */
if ((argc >= 1) && (argv[0].Tag == TYELLIPSES))
	{
	asIsSw = TRUE;
	--argc;
	startIndex = 1;
	}


/*  We must have an even number of arguments. The first argument of each pair is */
/*  the symbol (key) and the second argument is the value (value). */
if ((argc % 2) != 0)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Add each remaining bonding to the end of the Directory. */
argIndex = startIndex;
while (argIndex < argc)
    {
    /*  Add the pbinding to the Directory, and move to the next pbinding pair. */
    /*  Note:   Quoted Symbols and Pairs are converted to Symbols and Pairs. */

    *newKey = argv[argIndex++];
    if (newKey->Tag == TYQUOTEDSYMBOL)
        newKey->Tag = TYSYMBOL;
    if (newKey->Tag == TYQUOTEDPAIR)
        newKey->Tag = TYPAIR;
    *newValue = argv[argIndex++];
	if (asIsSw == FALSE)
		{
		TDirectory_SetIV1(gCP,gTP,selfTval,*newKey,*newValue);
		}
	else
		{
		if (self->itsMaxItemIndex > 0)
			{
			left = ((LpPBIND)*self->itsDirectoryArray)[self->itsMaxItemIndex-1].Key;
			right = *newKey;
			if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) < 0)
				{
				TDirectory_AddNewValue(gCP,gTP,selfTval,*newKey,*newValue);
				}
			else
				{
				TDirectory_SetIV1(gCP,gTP,selfTval,*newKey,*newValue);
				}
			}
		else
			{
			TDirectory_AddNewValue(gCP,gTP,selfTval,*newKey,*newValue);
			}
		}
	}

    
if (TESTON)
	{
	if ((self->itsMaxItemIndex > 0) &&
		(self->itsImmediatePtr == NULL) &&
		((self->itsDirectoryArray == NULL) || 
		(((LpMHANDLEINFO)self->itsDirectoryArray)->Free == TRUE)))
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	/* Make sure the keys are in ascending sorted order. */
	for (n = 0; n < self->itsMaxItemIndex - 1; ++n)
		{
		left = ((LpPBIND)*self->itsDirectoryArray)[n].Key;
		right = ((LpPBIND)*self->itsDirectoryArray)[n+1].Key;
		if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
			{
			prmv[0].u.Object = (TObject*)self;
			prmv[0].Tag = TYDIRECTORY;
			prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
			prmv[1].Tag = TYCFUNCTION;
			FMath2_Sort(gCP,gTP,2,&prmv[0]);
			}
		}
	/* Make sure the object content is valid */
	for (n = 0; n < self->itsMaxItemIndex; ++n)
		{
		if (!_VALIDTVAL(((LpPBIND)*self->itsDirectoryArray)[n].Key) ||
			!_VALIDTVAL(((LpPBIND)*self->itsDirectoryArray)[n].Value))
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
			}
		}
	if (!_CHECKTVAL(self->itsCdr))
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	}

/* Set the proper return value for a successful initialization. */

ret->Tag = TYDIRECTORY;
ret->u.Object = (TObject*)self;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

TDirectory_MakeNew

Return a Directory object with the specified initial bindings.  

Note:   If no arguments are specified, return (the empty Dictionary).
            
        (new Directory: X: 5 Y: 6)
        (new Directory: )
  
#endif

TVAL TDirectory_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     argIndex;
NUM                     maxIndex;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TDirectory,sp);
EndFrame

/*  Create the empty Structure */

maxIndex = argc;
*ret = gCP->Tval_VOID;
sp = TDirectory_New(gCP,gTP);
asTag(ret) = TYDIRECTORY;
asObject(ret) = (TObject*)sp;
if (argc == 0) FrameExit(*ret);

/*  Scan for the tail, if any. */

argIndex = 0;
while ((argIndex + 1) < argc)
    {
    
    /*  If we find the special symbol '.', set the Structure's cdr. */
    
    if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
        {
        sp->itsCdr = argv[argIndex+1];
        FrameExit( *ret);
        }
    TDirectory_SetIV1(gCP,gTP,*ret,argv[argIndex],argv[argIndex+1]);
    argIndex += 2;
    }

/*  Bindings must always come in pairs. */

if (argIndex < argc) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TDirectory_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TDirectory_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 cn;
StartFrame
DeclareOBJ(TDirectory,it);
DeclareOBJ(TDirectory,self);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TDirectory_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TDirectory*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asObject(itTval) = (TObject*)it;
        asTag(itTval) = it->itsObjectType;
        TDirectory_SetMaxIndex(gCP,gTP,*itTval, TDirectoryOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TDirectoryOnDiskPtr(aHMemory,0)->itsCdr);
        
		/* Load the itemArray containing the structure bindings from the disk */
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
			atHMPBind(it->itsDirectoryArray,cn).Key = TObject_LoadTval(gCP,gTP,TDirectoryOnDiskPtr(aHMemory,0)->itsItemArray[cn].Key);
			atHMPBind(it->itsDirectoryArray,cn).Value = TObject_LoadTval(gCP,gTP,TDirectoryOnDiskPtr(aHMemory,0)->itsItemArray[cn].Value);
           }
    
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
TDirectory_BSearchKey

Search the Directory bondings for the given object key.
Returns a TVAL containing the index for the closest match found, maybe an exact match.

#endif

TVAL TDirectory_BSearchKey(LpXCONTEXT gCP,LpTHREAD gTP,TDirectory* theSpace, TVAL theKey, COMPARE* compareCode)
{
register NUM    low, mid, high, compare = 0;
HMPBind         itsDirectoryArray;
StartFrame
DeclareTVAL(retTval);
DeclareTVAL(cmpTval);
DeclareTVALArray(item,2);

EndFrame

low = 0;
mid = 0;
*compareCode = -1;
asTag(retTval) = TYNUM;
asInt(retTval) = mid;

item[0] = theKey;

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

if(theSpace->itsMaxItemIndex != 0)
    {
    high = theSpace->itsMaxItemIndex - 1;
    itsDirectoryArray = theSpace->itsDirectoryArray;
    }   
else
    {
    FrameExit(*retTval);
    }
        
while ( low <= high )
    {   
    mid = (low + high) >> 1;
    
    item[1] = atHMPBind(itsDirectoryArray,mid).Key;
    *cmpTval = FPredicate2_Compare(gCP,gTP,2,&item[0]);
    compare = cmpTval->u.Int;

    if ( compare < 0 )
        {
        high = mid - 1;
        }
    else 
    if ( compare > 0 )
        {
         low = mid + 1;
        }
    else
        break;
   }
   
*compareCode = (compare != 0) ? ((compare < 0) ? LOW : HIGH) : EQUAL;
asInt(retTval) = mid;
FrameExit(*retTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TDirectory_MakeUniqueKey

Maintains a numerically sorted TDirectory for later bsearch. Sort is on the Key object.
Returns a tval containing the index of the found or created item.

#endif

TVAL TDirectory_MakeUniqueKey(LpXCONTEXT gCP,LpTHREAD gTP,TDirectory* self, TVAL theKey, TVAL valueData)
{
COMPARE     compareCode;
StartFrame
DeclareTVAL(selfTval);
DeclareTVAL(indexTval);
EndFrame

/*  Init the search space array */

if (self == NULL)
    FrameExit(gCP->TObject_FALSE);

asObject(selfTval) = (TObject*)self;
asTag(selfTval) = self->itsObjectType;

if (self->itsMaxItemIndex > 0)
    {
    *indexTval = TDirectory_BSearchKey(gCP, gTP, self, theKey, &compareCode);
      
    if (compareCode < 0)
        {
        TDirectory_Insert(gCP, gTP, *selfTval, *indexTval, theKey, valueData);
        }
    else 
    if (compareCode > 0)
        {
        asInt(indexTval)++;
        TDirectory_Insert(gCP, gTP, *selfTval, *indexTval, theKey, valueData);
        }
    else 
        {
        /* The key already exists */
		if (valueData.Tag == TYVOID)
			{
			TDirectory_Delete(gCP, gTP, *selfTval, *indexTval);
			}
		else
			{
			atHMPBind(self->itsDirectoryArray,indexTval->u.Int).Value = valueData;
			}
        FrameExit(*indexTval);
        }
    }
else
    {
    asInt(indexTval) = 0;
    TDirectory_AddNewValue(gCP, gTP, *selfTval, theKey, valueData);
    }
    
FrameExit(*indexTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Map

Make a copy of *self and call the given proc on each element, storing the result in place.

#endif

TVAL TDirectory_Map(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM             index;
StartFrame
DeclareOBJ(TDirectory,copyEnv);
DeclareOBJ(TDirectory,self);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

self = (TDirectory*)asObject(&selfTval);

/* Make a copy of myself since map returns an object of the same type */

copyEnv = (TDirectory*)TDirectory_Copy(gCP,gTP,selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    *tmp = atHMPBind(copyEnv->itsDirectoryArray,index).Value;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if (isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        atHMPBind(copyEnv->itsDirectoryArray,index).Value = *ret;
    }
asTag(ret) = TYDIRECTORY;
asObject(ret) = (TObject*)copyEnv;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mapc

Loop through self->itsDirectoryArray and call the specified function.

#endif

TVAL TDirectory_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM             index;
StartFrame
DeclareOBJ(TDirectory,self);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

self = (TDirectory*)asObject(&selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    *tmp = atHMPBind(self->itsDirectoryArray,index).Value;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if (isERROR(ret))
        break;
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

void    TDirectory_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TDirectory*     self = (TDirectory*)asObject(&selfTval);
NUM             indexOf;

/*  Mark the Directory's items so they won't be garbage collected. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    TObject_MarkTval(gCP,gTP,atHMPBind(self->itsDirectoryArray,indexOf).Key);
    TObject_MarkTval(gCP,gTP,atHMPBind(self->itsDirectoryArray,indexOf).Value);
    }

}

/*--------------------------------------------------------------------------------------- */
#if 0
Doomed

Garbage collection is about to delete this object. Dispose of the Directory data.

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

void    TDirectory_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TDirectory*    self   = (TDirectory*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsDirectoryArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsDirectoryArray);
self->itsMaxItemIndex = 0;	
self->itsDirectoryArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TDirectory_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
TDirectory*    self   = (TDirectory*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/* Compute the size of the object header */
*aSize = SIZEOF_TObjectOnDisk;

/* Compute the size of the structure and all its bindings */
*aSize += ((NUM)&((TDirectoryOnDisk*)0)->itsItemArray[self->itsMaxItemIndex]);

ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TDirectory_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory)
{
long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TDirectory,self);
EndFrame


self = (TDirectory*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TDirectoryOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TDirectoryOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);

/*  Save the structure binding (key,value) into a disk record */

for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
	TDirectoryOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn].Key = TObject_RegisterTval(gCP,gTP,atHMPBind(self->itsDirectoryArray,cn).Key);
	TDirectoryOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn].Value = TObject_RegisterTval(gCP,gTP,atHMPBind(self->itsDirectoryArray,cn).Value);
   }


FrameExit(aHMemory);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TDirectory.

#endif

TObject*    TDirectory_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TDirectory,self);
DeclareOBJ(TDirectory,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TDirectory*)selfTval.u.Object;

theCopy = TDirectory_New(gCP,gTP);


tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;


TDirectory_SetMaxIndex(gCP,gTP,*tmpTval,self->itsMaxItemIndex);


if (self->itsDirectoryArray != NULL)
    {
    _FMemory_memcpy(&atHMPBind(theCopy->itsDirectoryArray,0),&atHMPBind(self->itsDirectoryArray,0),(LONG)(self->itsMaxItemIndex*sizeof(PBIND)));
    }

theCopy->itsCdr = self->itsCdr;

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TDirectory_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TDirectory*    self = (TDirectory*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

#endif

TVAL TDirectory_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
NUM             cn;
StartFrame
DeclareOBJ(TDirectory,self);
EndFrame

self = (TDirectory*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TDirectory_ImmediateSpace/(NUM)sizeof(PBIND)))
	{
	if (self->itsDirectoryArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(PBIND));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)PBindArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(PBIND));
		if ((self->itsDirectoryArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsDirectoryArray);
		}
	self->itsDirectoryArray = (HMPBind)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item Directory handle. */
if (self->itsDirectoryArray == NULL)
    {
    self->itsDirectoryArray = (FMPBind**)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(PBIND)),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    for(cn = 0; cn < newRepeats; cn++)
        {
        atHMPBind(self->itsDirectoryArray,cn).Key = gCP->Tval_VOID;
        atHMPBind(self->itsDirectoryArray,cn).Value = gCP->Tval_VOID;
        }
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsDirectoryArray = (FMPBind**)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(PBIND)),TRUE);
	_FMemory_memcpy((char*)PBindArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(PBIND));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if(newRepeats != self->itsMaxItemIndex)
    {
    self->itsDirectoryArray = (FMPBind**)FMemory_Resize(gCP, gTP, (FMemory**)self->itsDirectoryArray,(LONG)(newRepeats*sizeof(PBIND)));
	self->itsImmediatePtr = NULL;
    if (self->itsMaxItemIndex < newRepeats)
        {
        for(cn = self->itsMaxItemIndex; cn < newRepeats; cn++)
            {
            atHMPBind(self->itsDirectoryArray,cn).Key = gCP->Tval_VOID;
            atHMPBind(self->itsDirectoryArray,cn).Value = gCP->Tval_VOID;
            }
        }
    self->itsMaxItemIndex = newRepeats;
    }

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

Note:   A Structure object may be indexed either by a key value or by
        a numeric value.

#endif

TVAL TDirectory_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{

NUM             indexOf;
COMPARE         compareCode;

StartFrame
DeclareOBJ(TDirectory,self);
EndFrame

self = (TDirectory*)asObject(&selfTval);

/*  Look for the key in the Directory. */

index1 = TDirectory_BSearchKey(gCP, gTP, self, index1, &compareCode);
if (compareCode != 0)
    FrameExit(gCP->Tval_VOID);

/*  Look for a numeric key. */

if (isNumIndex(&index1))
    {
    /*  We accept numeric indices. */
    indexOf = asNumIndex(&index1);

    /*  Make sure index is in range. */
    if (indexOf < 0)
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    /*  Make sure index is in range. */
    if (indexOf >= self->itsMaxItemIndex)
        FrameExit(gCP->TObject_VOID);
    
    /*  Return the bound value. */
    FrameExit(atHMPBind(self->itsDirectoryArray,indexOf).Value);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV2

Return the indexed value from the repeating portion of this object.

Note:   A Structure object may be indexed either by an object value or by
        a numeric value.

#endif

TVAL TDirectory_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1, TVAL index2)
{
NUM             indexOf;
NUM             whichNdx;
StartFrame
DeclareOBJ(TDirectory,self);
DeclareTVAL(retValue);
EndFrame

self = (TDirectory*)asObject(&selfTval);

if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"sorted") == 0))
    {
    /* Reference the Structure in sorted fasion. */
	FrameExit(TDirectory_GetIV1(gCP,gTP,selfTval,index2));
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"unsorted") == 0))
    {
    /* Reference the Structure in binary unique unsorted fasion. */
    
	for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
		{
		*retValue = FPredicate2_FullCompare(gCP, gTP, index2, atHMPBind(self->itsDirectoryArray,indexOf).Key);
		if (retValue->u.Int == 0)
			{
			FrameExit(atHMPBind(self->itsDirectoryArray,indexOf).Value);
			}
		}
	FrameExit(gCP->TObject_VOID);
	}
else
if (isNumIndex(&index1) && isNumIndex(&index2))
    {
    /*  We accept numeric indices. */
    indexOf = asNumIndex(&index1);
    whichNdx = asNumIndex(&index2);

    /*  Make sure index is in range. */
    if (!inRange(indexOf, 0,self->itsMaxItemIndex) || !inRange(whichNdx, 0,2))
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    /*  Return the appropriate bound value. */
    
    if(whichNdx != 0)
        *retValue = atHMPBind(self->itsDirectoryArray,indexOf).Value;
    else
        {
        *retValue = atHMPBind(self->itsDirectoryArray,indexOf).Key;
        }
    FrameExit(*retValue);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   A Structure object may be indexed either by a object value or by
        a numeric value.

#endif

TVAL TDirectory_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM				n;
TVAL			left;
TVAL			right;
TVAL			prmv[3];
StartFrame
DeclareOBJ(TDirectory,self);
EndFrame


self = (TDirectory*)asObject(&selfTval);

/*  Save the new value in the Directory by the specified key. */

TDirectory_MakeUniqueKey(gCP,gTP,self,index1,newValue);

if (TESTON)
	{
	if ((self->itsMaxItemIndex > 0) &&
		(self->itsImmediatePtr == NULL) &&
		((self->itsDirectoryArray == NULL) || 
		(((LpMHANDLEINFO)self->itsDirectoryArray)->Free == TRUE)))
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	/* Make sure the keys are in ascending sorted order. */
	for (n = 0; n < self->itsMaxItemIndex - 1; ++n)
		{
		left = ((LpPBIND)*self->itsDirectoryArray)[n].Key;
		right = ((LpPBIND)*self->itsDirectoryArray)[n+1].Key;
		if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
			{
			prmv[0].u.Object = (TObject*)self;
			prmv[0].Tag = TYDIRECTORY;
			prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
			prmv[1].Tag = TYCFUNCTION;
			FMath2_Sort(gCP,gTP,2,&prmv[0]);
			}
		}
	/* Make sure the object content is valid */
	for (n = 0; n < self->itsMaxItemIndex; ++n)
		{
		if (!_VALIDTVAL(((LpPBIND)*self->itsDirectoryArray)[n].Key) ||
			!_VALIDTVAL(((LpPBIND)*self->itsDirectoryArray)[n].Value))
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
			}
		}
	if (!_CHECKTVAL(self->itsCdr))
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	}

FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV2

Set the indexed value in the repeating portion of this object.

Note:   A Directory object must be indexed by two numeric index values. If the second index
		value is a 0, the key, at the first index location, will be replaced. If the second 
		index value is a 1, the value, at the first index location, will be replaced.

#endif

TVAL TDirectory_SetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL newValue)
{
NUM             indexOf;
NUM             whichNdx;
NUM				n;
TVAL			left;
TVAL			right;
TVAL			prmv[3];
StartFrame
DeclareOBJ(TDirectory,self);
DeclareTVAL(retValue);
DeclareTVAL(newKey);
EndFrame

self = (TDirectory*)asObject(&selfTval);

/* We accept the first index to be last: */

if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL)) && (strcmp(SymbolArray(index1),"last") == 0))
    {
    /* add a new Binding */
	*newKey = index2;
    TDirectory_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex + 1);
    atHMPBind(self->itsDirectoryArray,self->itsMaxItemIndex-1).Key = *newKey;
    atHMPBind(self->itsDirectoryArray,self->itsMaxItemIndex-1).Value = newValue;
	if (TESTON) goto SelfTestMode;
	FrameExit(selfTval);
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"sorted") == 0))
    {
    /* Set the Structure in binary unique sorted fasion. */
	FrameExit(TDirectory_SetIV1(gCP,gTP,selfTval,index2,newValue));
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"unsorted") == 0))
    {
    /* Set the Structure in sequential unsorted fasion. */
    
	for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
		{
		*retValue = FPredicate2_FullCompare(gCP, gTP, index2, atHMPBind(self->itsDirectoryArray,indexOf).Key);
		if (retValue->u.Int == 0)
			{
			/* Update an existing binding. */
			atHMPBind(self->itsDirectoryArray,indexOf).Value = newValue;
			if (TESTON) goto SelfTestMode;
			FrameExit(selfTval);
			}
		}

    /* add a new Binding */
	*newKey = index2;
    TDirectory_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex + 1);
    atHMPBind(self->itsDirectoryArray,self->itsMaxItemIndex-1).Key = *newKey;
    atHMPBind(self->itsDirectoryArray,self->itsMaxItemIndex-1).Value = newValue;
	if (TESTON) goto SelfTestMode;
	FrameExit(selfTval);
	}
else
if (isNumIndex(&index1) && isNumIndex(&index2))
    {
    /*  We accept numeric indices. */
    indexOf = asNumIndex(&index1);
    whichNdx = asNumIndex(&index2);

    /*  Make sure index is in range. */
    if (!inRange(indexOf, 0,self->itsMaxItemIndex) || !inRange(whichNdx, 0,2))
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    /*  Set the appropriate bound value. */
    
    if(whichNdx != 0)
		{
        atHMPBind(self->itsDirectoryArray,indexOf).Value = newValue;
		}
    else
        {
        atHMPBind(self->itsDirectoryArray,indexOf).Key = newValue;
        }
	if (TESTON) goto SelfTestMode;
	FrameExit(selfTval);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

SelfTestMode:
if ((self->itsMaxItemIndex > 0) &&
	(self->itsImmediatePtr == NULL) &&
	((self->itsDirectoryArray == NULL) || 
	(((LpMHANDLEINFO)self->itsDirectoryArray)->Free == TRUE)))
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}
/* Make sure the keys are in ascending sorted order. */
for (n = 0; n < self->itsMaxItemIndex - 1; ++n)
	{
	left = ((LpPBIND)*self->itsDirectoryArray)[n].Key;
	right = ((LpPBIND)*self->itsDirectoryArray)[n+1].Key;
	if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
		{
		prmv[0].u.Object = (TObject*)self;
		prmv[0].Tag = TYDIRECTORY;
		prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
		prmv[1].Tag = TYCFUNCTION;
		FMath2_Sort(gCP,gTP,2,&prmv[0]);
		}
	}
/* Make sure the object content is valid */
for (n = 0; n < self->itsMaxItemIndex; ++n)
	{
	if (!_VALIDTVAL(((LpPBIND)*self->itsDirectoryArray)[n].Key) ||
		!_VALIDTVAL(((LpPBIND)*self->itsDirectoryArray)[n].Value))
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	}
if (!_CHECKTVAL(self->itsCdr))
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}
FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TDirectory_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newKey, TVAL newValue)
{
StartFrame
DeclareOBJ(TDirectory,self);
EndFrame

self = (TDirectory*)asObject(&selfTval);

/* add a new Bonding to the end of the Directory */

TDirectory_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex + 1);
atHMPBind(self->itsDirectoryArray,self->itsMaxItemIndex-1).Key = newKey;
atHMPBind(self->itsDirectoryArray,self->itsMaxItemIndex-1).Value = newValue;

    
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Directory is resized.

#endif

TVAL TDirectory_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{
register LpPBIND        targetPtr;
register LpPBIND        sourcePtr;
register LpPBIND        haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TDirectory,self);
DeclareTVALArray(parm,2);

EndFrame

self = (TDirectory*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    {
    deleteIndex = asNumIndex(&index);
    }
else
    {
    parm[0] = selfTval;
    parm[1] = index;
    index =  FUtil2_Boundp(gCP,gTP,2,&parm[0]);
    if (isNumIndex(&index))
        {
        deleteIndex = asNumIndex(&index);
        }
    else
        {
        FrameExit(gCP->TObject_OK);
        }
    }
    
/*  Make sure TDirectory index is in range. */

if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move the remaining values Directory Array  */
/*  to the position that is being deleted      */

sourcePtr = &atHMPBind(self->itsDirectoryArray,deleteIndex+1);
targetPtr = &atHMPBind(self->itsDirectoryArray,deleteIndex);
haltPtr = &atHMPBind(self->itsDirectoryArray,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }


/*  Resize the TDirectory down one position */

TDirectory_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the higher values are moved up one position and
        the TDirectory is resized.

#endif

TVAL TDirectory_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index, TVAL newObject, TVAL newValue)
{
register LpPBIND        targetPtr;
register LpPBIND        sourcePtr;
register LpPBIND        insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TDirectory,self);
EndFrame

self = (TDirectory*)asObject(&selfTval);

/*  We only accept numeric indices. */

if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  We never insert #void values. */

if (newValue.Tag == TYVOID) FrameExit(gCP->TObject_OK);

/*  Make sure TDirectory index is in range. */

if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the TDirectory up one position */

TDirectory_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the TDirectory up one position */

sourcePtr = &atHMPBind(self->itsDirectoryArray,(self->itsMaxItemIndex-2));
targetPtr = &atHMPBind(self->itsDirectoryArray,(self->itsMaxItemIndex-1));
insertPtr = &atHMPBind(self->itsDirectoryArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
    
/*  Insert the new value in the TDirectory at the specified position */

atHMPBind(self->itsDirectoryArray,insertIndex).Value = newValue;
atHMPBind(self->itsDirectoryArray,insertIndex).Key = newObject;
    
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TDirectory_Print

Convert a Directory object into an ascii string and append it to an output buffer. 
Following the extended tradition, the Directory object is displayed as a series of 
elements as follows: 

#endif

TVAL TDirectory_Print(LpXCONTEXT gCP, LpTHREAD gTP, TDirectory* theEnv, LpNUM size, LpCHAR buf)
{
BOLE					PrintingBinding = FALSE; 
NUM						PrintIndex = 0;
NUM                     indexOf;
NUM                     startIndex;
NUM                     endIndex;
StartFrame
DeclareTVAL(ec);
EndFrame

/*  Quit if the output string is already too long */

if ((*size) + 4 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
/*  Show Structure prefix */

buf[*size]      = '#';
buf[++(*size)]  = '{';
buf[++(*size)]  = 'd';
buf[++(*size)]  = 'i';
buf[++(*size)]  = 'r';
buf[++(*size)]  = '|';
buf[++(*size)]  = '|';
buf[++(*size)]  = ' ';
buf[++(*size)]  = 0;

/*  Show all or part of the Structure's itsDictionaryArray */

if(PrintingBinding == FALSE)
    {
    startIndex = 0;
    endIndex = theEnv->itsMaxItemIndex;
    }
else
    {
    startIndex = PrintIndex;
    endIndex = startIndex + 1;
    }

for(indexOf = startIndex; indexOf < endIndex; indexOf++)
    {
    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
    
    *ec = FConio_sprintn(gCP,gTP,buf,size,atHMPBind(theEnv->itsDirectoryArray,indexOf).Key);
    _TObject_ErrorChk(*ec);

    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[*size]      = ' ';
    buf[++(*size)]  = 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,atHMPBind(theEnv->itsDirectoryArray,indexOf).Value);
    _TObject_ErrorChk(*ec);
    if (indexOf != theEnv->itsMaxItemIndex - 1)
        {
        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ' ';
        buf[++(*size)]  = 0;
        }
    }


/*  Show the Directory's cdr if it is not void and we are not printing a single binding */

if (!PrintingBinding && asTag(&theEnv->itsCdr))
    {
    if ((*size) + 4 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[*size]      = ' ';
    buf[++(*size)]  = '.';
    buf[++(*size)]  = ' ';
    buf[++(*size)]  = 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,theEnv->itsCdr);
    _TObject_ErrorChk(*ec);
    }


/*  Show Directory suffix */

if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = '}';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TDirectory_New

Create a new TDirectory.

#endif

TDirectory*    TDirectory_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
TDirectory*    self;
EndFrame

/*  This class must be initialized. */
if (!gCP->TDirectory_Initialized) TDirectory_Init(gCP,gTP);

self = (TDirectory*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYDIRECTORY;
self->itsMaxItemIndex = 0;
self->itsDirectoryArray = NULL;
self->itsImmediatePtr = NULL;
FrameExit(self);
}
