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

#define _C_TDICTIONARY
#define _SMARTBASE
#if 0
TDictionary.c

Implementation of the Dictionary class which stores a variable length set of 
(symbol,value) pairs stored as a linear array. Adopted from the Scheme dialect, 
it is one of the main mechanisms for variable bonding in SmartLisp.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "tdiction.h"
#include "fpred2.h"
#include "fmake.h"
#include "futil3.h"
#include "fmath2.h"
#include "fpred.h"
#include "fvmscpt.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TDictionary_Init

Initialize the TDictionary class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TDictionary_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
TYPE        tmp;
/*  Don't initialize more than once. */
if (gCP->TDictionary_Initialized) return;
gCP->TDictionary_Initialized     = TRUE;

/*  Initialize the new types for this class. */
tmp  = FSmartbase_NewType  (gCP,
							gTP,
							TYDICTIONARY,
							"Dictionary",
							_TObject_TfTOBJECT,
							sizeof(OBJ),
							(LpFNEW)&FMake_Dictionary,
							&TDictionary_Mark,
							&TObject_GlobalMarkNever,
							&FObject_ObjAnyCnv,
							&FObject_CompareNever,
							&TDictionary_SetIV1,
							&TDictionary_SetIV2,
							&FObject_SetIV3Never,
							&TDictionary_GetIV1,
							&TDictionary_GetIV2,
							&FObject_GetIV3Never,
							&TDictionary_Map,
							&TDictionary_Mapc,
							&TObject_PrintObj,
							&TDictionary_Load,
							&TDictionary_Save,
							&TDictionary_ComputeSize,
							&TDictionary_Copy,
							&TDictionary_Doomed);

}

/*--------------------------------------------------------------------------------------- */
#if 0
IDictionary

Initialize a TDictionary object with an array of bondings.

#endif

TVAL    TDictionary_IDictionary(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[])
{
NUM             argIndex;
BOLE			asIsSw = FALSE;
NUM				startIndex = 0;
NUM				n;
TVAL			left;
TVAL			right;
TVAL			prmv[3];
StartFrame
DeclareOBJ(TDictionary,self);
DeclareOBJ(TObject,newKey);
DeclareTVAL(tvalKey);
DeclareTVAL(newValue);
DeclareTVAL(ret);
EndFrame


self = (TDictionary*)asObject(&selfTval);

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

/*  Add each remaining bonding to the end of the Dictionary. */
argIndex = startIndex;
while (argIndex < argc)
    {
    /*  Convert text and string keys to symbolic keys. */
    /*  Note:   Symbolic keys guarantee uniqueness. */
    
    if ((argv[argIndex].Tag == TYTEXT) || (argv[argIndex].Tag == TYSTRING) || (argv[argIndex].Tag == TYSTRINGSUBSTR))
        {
        /* argv[argIndex] = TSymbol_SymbolAnyCnv(TYSYMBOL,argv[argIndex]);*/
        argv[argIndex] = TString_StringAnyCnv(gCP,gTP,TYSTRING,argv[argIndex]);
        }

    /*  Add the bonding to the Structure, and move to the next bonding pair. */
    /*  Note:   Each bonding must always begin with an object. */

    if (_TObject_TypeFlag(asTag(&argv[argIndex])) != _TObject_TfTOBJECT)
		{
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
    else
        {
        *tvalKey = argv[argIndex++];
		if (tvalKey->Tag == TYQUOTEDSYMBOL)
			tvalKey->Tag = TYSYMBOL;
		if (tvalKey->Tag == TYQUOTEDPAIR)
			tvalKey->Tag = TYPAIR;
        *newValue = argv[argIndex++];
		if (asIsSw == FALSE)
			{
			TDictionary_SetIV1(gCP,gTP,selfTval,*tvalKey,*newValue);
			}
		else
			{
			if (self->itsMaxItemIndex > 0)
				{
				left = TOBJ(((LpBIND)*self->itsDictionaryArray)[self->itsMaxItemIndex-1].Key);
				right = *tvalKey;
				if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) < 0)
					{
					TDictionary_AddNewValue(gCP,gTP,selfTval,*tvalKey,*newValue);
					}
				else
					{
					TDictionary_SetIV1(gCP,gTP,selfTval,*tvalKey,*newValue);
					}
				}
			else
				{
				TDictionary_AddNewValue(gCP,gTP,selfTval,*tvalKey,*newValue);
				}
			}
		}
    }

if (TESTON)
	{
	if ((self->itsMaxItemIndex > 0) &&
		(self->itsImmediatePtr == NULL) &&
		((self->itsDictionaryArray == NULL) || 
		(((LpMHANDLEINFO)self->itsDictionaryArray)->Free == TRUE)))
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	/* Make sure the keys are in ascending sorted order. */
	for (n = 0; n < self->itsMaxItemIndex - 1; ++n)
		{
		left = TOBJ(((LpBIND)*self->itsDictionaryArray)[n].Key);
		right = TOBJ(((LpBIND)*self->itsDictionaryArray)[n+1].Key);
		if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
			{
			prmv[0].u.Object = (TObject*)self;
			prmv[0].Tag = TYDICTIONARY;
			prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
			prmv[1].Tag = TYCFUNCTION;
			FMath2_Sort(gCP,gTP,2,&prmv[0]);
			}
		}
	/* Make sure the object content is valid */
	for (n = 0; n < self->itsMaxItemIndex; ++n)
		{
		if (!_VALIDOBJ(((LpBIND)*self->itsDictionaryArray)[n].Key) ||
			!_VALIDTVAL(((LpBIND)*self->itsDictionaryArray)[n].Value))
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

ret->Tag = TYDICTIONARY;
ret->u.Object = (TObject*)self;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TDictionary_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TDictionary_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{

NUM                 cn;
StartFrame
DeclareOBJ(TDictionary,it);
DeclareOBJ(TDictionary,self);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TDictionary_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = self = (TDictionary*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asObject(itTval) = (TObject*)it;
        asTag(itTval) = it->itsObjectType;
        TDictionary_SetMaxIndex(gCP,gTP,*itTval, TDictionaryOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TDictionaryOnDiskPtr(aHMemory,0)->itsCdr);
        
		/* Load the itemArray containing the structure bindings from the disk stream */
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
			atHMBind(it->itsDictionaryArray,cn).Key = TObject_CheckRegistration(gCP,gTP,TDictionaryOnDiskPtr(aHMemory,0)->itsItemArray[cn].Key);
			atHMBind(it->itsDictionaryArray,cn).Value = TObject_LoadTval(gCP,gTP,TDictionaryOnDiskPtr(aHMemory,0)->itsItemArray[cn].Value);
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
TDictionary_BSearchKey

Search the Dictionary bondings for the given object key.
Returns a TVAL containing the index for the closest match found, maybe an exact match.

#endif

TVAL TDictionary_BSearchKey(LpXCONTEXT gCP,LpTHREAD gTP,TDictionary* theSpace, TObject* anObject, COMPARE* compareCode)
{
register NUM    low, mid, high, compare = 0;
HMBind          itsDictionaryArray;

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

item[0].u.Object = anObject;
item[0].Tag = item[0].u.Object->itsObjectType;

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

if(theSpace->itsMaxItemIndex != 0)
    {
    high = theSpace->itsMaxItemIndex - 1;
    itsDictionaryArray = theSpace->itsDictionaryArray;
    }   
else
    {
    FrameExit(*retTval);
    }
        
while ( low <= high )
    {   
    mid = (low + high) >> 1;
    
    if (atHMBind(itsDictionaryArray,mid).Key == NULL)
        { 
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
        }
    else
        {
        item[1].u.Object = atHMBind(itsDictionaryArray,mid).Key;
        item[1].Tag = item[1].u.Object->itsObjectType;
        }
    if (item[1].u.Object == NULL) item[1] = gCP->Tval_VOID;
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
TDictionary_MakeUniqueKey

Maintains a numerically sorted TDictionary for later bsearch. Sort is on the Key object.
Returns a tval containing the index of the found or created item.

#endif

TVAL TDictionary_MakeUniqueKey(LpXCONTEXT gCP,LpTHREAD gTP,TDictionary* self, TObject* keyObject, TVAL valueData)
{
COMPARE     compareCode;
StartFrame
DeclareTVAL(selfTval);
DeclareTVAL(objectTval);
DeclareTVAL(indexTval);
EndFrame

/*  Init the search space array */

if( self == NULL )
    FrameExit(gCP->TObject_FALSE);

asObject(selfTval) = (TObject*)self;
asTag(selfTval) = self->itsObjectType;
asObject(objectTval) = keyObject;
asTag(objectTval) = keyObject->itsObjectType;

if ( self->itsMaxItemIndex > 0 )
    {
    *indexTval = TDictionary_BSearchKey(gCP, gTP, self, keyObject, &compareCode);
      
    if ( compareCode < 0 )
        {
        TDictionary_Insert(gCP, gTP, *selfTval, *indexTval, *objectTval, valueData);
        }
    else 
    if ( compareCode > 0 )
        {
        asInt(indexTval)++;
        TDictionary_Insert(gCP, gTP, *selfTval, *indexTval, *objectTval, valueData);
        }
    else 
        {
        /* The key already exists, so set the */
        /* Dictionary bonding element to the new value. */
        atHMBind(self->itsDictionaryArray,indexTval->u.Int).Value = valueData;
        FrameExit(*indexTval);
        }
    }
else
    {
    asInt(indexTval) = 0;
    TDictionary_AddNewValue(gCP, gTP, *selfTval, *objectTval, valueData);
    }
    
FrameExit(*indexTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Map

Make a copy of *self and call the given proc on each element, storing the result in place.

#endif

TVAL TDictionary_Map(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM             index;
StartFrame
DeclareOBJ(TDictionary,copyEnv);
DeclareOBJ(TDictionary,self);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

self = (TDictionary*)asObject(&selfTval);

/* Make a copy of myself since map returns an object of the same type */

copyEnv = (TDictionary*)TDictionary_Copy(gCP,gTP,selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    *tmp = atHMBind(copyEnv->itsDictionaryArray,index).Value;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if (isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        atHMBind(copyEnv->itsDictionaryArray,index).Value = *ret;
    }
asTag(ret) = TYDICTIONARY;
asObject(ret) = (TObject*)copyEnv;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mapc

Loop through self->itsDictionaryArray and call the specified function.

#endif

TVAL TDictionary_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM             index;
StartFrame
DeclareOBJ(TDictionary,self);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

self = (TDictionary*)asObject(&selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    *tmp = atHMBind(self->itsDictionaryArray,index).Value;
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

void    TDictionary_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TDictionary*    self = (TDictionary*)asObject(&selfTval);
NUM             indexOf;

/*  Mark the Dictionary's items so they won't be garbage collected. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    TObject_MarkObj(gCP,gTP,atHMBind(self->itsDictionaryArray,indexOf).Key);
    TObject_MarkTval(gCP,gTP,atHMBind(self->itsDictionaryArray,indexOf).Value);
    }

}

/*--------------------------------------------------------------------------------------- */
#if 0
Doomed

Garbage collection is about to delete this object. Dispose of the Dictionary data.

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

void    TDictionary_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TDictionary*    self   = (TDictionary*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsDictionaryArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsDictionaryArray);
self->itsMaxItemIndex = 0;	
self->itsDictionaryArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TDictionary_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
TDictionary*    self   = (TDictionary*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/* Compute the size of the object header */
*aSize = SIZEOF_TObjectOnDisk;

/* Compute the size of the dictionary and all its bondings */
*aSize += ((NUM)&((TDictionaryOnDisk*)0)->itsItemArray[self->itsMaxItemIndex]);

ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TDictionary_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory)
{

long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TDictionary,self);
EndFrame

self   = (TDictionary*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TDictionaryOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TDictionaryOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);

/*  Save the structure binding (key,value) into a disk record */

for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
	TDictionaryOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn].Key = TObject_RegisterObject(gCP,gTP,atHMBind(self->itsDictionaryArray,cn).Key);
	TDictionaryOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn].Value = TObject_RegisterTval(gCP,gTP,atHMBind(self->itsDictionaryArray,cn).Value);
   }


FrameExit(aHMemory);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TDictionary.

#endif

TObject*    TDictionary_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TDictionary,self);
DeclareOBJ(TDictionary,theCopy);
DeclareTVAL(tmpTval);
EndFrame
self = (TDictionary*)selfTval.u.Object;

theCopy = TDictionary_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

TDictionary_SetMaxIndex(gCP, gTP, *tmpTval, self->itsMaxItemIndex);


if (self->itsDictionaryArray != NULL)
    {
    _FMemory_memcpy(&atHMBind(theCopy->itsDictionaryArray,0),&atHMBind(self->itsDictionaryArray,0),(LONG)(self->itsMaxItemIndex*sizeof(BIND)));
    }

theCopy->itsCdr = self->itsCdr;

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TDictionary_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TDictionary*    self = (TDictionary*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

#endif

TVAL TDictionary_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
NUM             cn;
StartFrame
DeclareOBJ(TDictionary,self);
EndFrame

self = (TDictionary*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TDictionary_ImmediateSpace/(NUM)sizeof(BIND)))
	{
	if (self->itsDictionaryArray == NULL) 
		{
		//_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(BIND));
		for(cn = 0; cn < newRepeats; cn++)
			{
			((BIND*)self->itsImmediateSpace)[cn].Key = (TObject*)gCP->TLambda_nil;
			((BIND*)self->itsImmediateSpace)[cn].Value = gCP->Tval_VOID;
			}
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)BondArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(BIND));
		if ((self->itsDictionaryArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsDictionaryArray);
		for(cn = self->itsMaxItemIndex; cn < newRepeats; cn++)
			{
			((BIND*)self->itsImmediateSpace)[cn].Key = (TObject*)gCP->TLambda_nil;
			((BIND*)self->itsImmediateSpace)[cn].Value = gCP->Tval_VOID;
			}
		}
	self->itsDictionaryArray = (FMBind**)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item Dictionary handle. */
if (self->itsDictionaryArray == NULL)
    {
    self->itsDictionaryArray = (FMBind**)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(BIND)),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    for(cn = 0; cn < newRepeats; cn++)
        {
        atHMBind(self->itsDictionaryArray,cn).Key = (TObject*)gCP->TLambda_nil;
        atHMBind(self->itsDictionaryArray,cn).Value = gCP->Tval_VOID;
        }
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsDictionaryArray = (FMBind**)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(BIND)),TRUE);
	_FMemory_memcpy((char*)BondArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(BIND));
	for(cn = self->itsMaxItemIndex; cn < newRepeats; cn++)
		{
		atHMBind(self->itsDictionaryArray,cn).Key = (TObject*)gCP->TLambda_nil;
		atHMBind(self->itsDictionaryArray,cn).Value = gCP->Tval_VOID;
		}
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if(newRepeats != self->itsMaxItemIndex)
    {
    self->itsDictionaryArray = (FMBind**)FMemory_Resize(gCP, gTP, (FMemory**)self->itsDictionaryArray,(LONG)(newRepeats*sizeof(BIND)));
	self->itsImmediatePtr = NULL;
    if (self->itsMaxItemIndex < newRepeats)
        {
        for(cn = self->itsMaxItemIndex; cn < newRepeats; cn++)
            {
            atHMBind(self->itsDictionaryArray,cn).Key = (TObject*)gCP->TLambda_nil;
            atHMBind(self->itsDictionaryArray,cn).Value = gCP->Tval_VOID;
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

Note:   a Structure object may be indexed either by a key value or by
        a numeric value.

#endif

TVAL TDictionary_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
COMPARE         compareCode;
StartFrame
DeclareOBJ(TDictionary,self);
EndFrame

self = (TDictionary*)asObject(&selfTval);

/*  Convert text and string keys to symbolic keys. */
/*  Note:   Symbolic keys guarantee uniqueness. */

if ((index1.Tag == TYTEXT) || (index1.Tag == TYSTRING) || (index1.Tag == TYSTRINGSUBSTR))
    {
    /* index1 = TSymbol_SymbolAnyCnv(TYSYMBOL,index1);*/
    index1 = TString_StringAnyCnv(gCP,gTP,TYSTRING,index1);
    }

/*  Look for an object key. */

if (_TObject_TypeFlag(asTag(&index1)) == _TObject_TfTOBJECT)
    {
    /*  We accept object indices */
    
    index1 = TDictionary_BSearchKey(gCP, gTP, self, asObject(&index1), &compareCode);

    if (compareCode != 0)
        FrameExit(gCP->Tval_VOID);
    }

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
    FrameExit(atHMBind(self->itsDictionaryArray,indexOf).Value);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV2

Return the indexed value from the repeating portion of this object.

Note:   a Structure object may be indexed either by an object value or by
        a numeric value.

#endif

TVAL TDictionary_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1, TVAL index2)
{
NUM             indexOf;
NUM             whichNdx;
StartFrame
DeclareTVAL(retValue);
DeclareOBJ(TDictionary,self);
EndFrame

self = (TDictionary*)asObject(&selfTval);

/* We accept the first index to be last: */

if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"sorted") == 0))
    {
    /* Reference the Structure in binary unique sorted fasion. */
	FrameExit(TDictionary_GetIV1(gCP,gTP,selfTval,index2));
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"unsorted") == 0))
    {
    /* Reference the Structure in sequential unsorted fasion. */
    
	for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
		{
		*retValue = FPredicate2_FullCompare(gCP, gTP, index2, TOBJ(atHMBind(self->itsDictionaryArray,indexOf).Key));
		if (retValue->u.Int == 0)
			{
			/* Update an existing binding. */
			FrameExit(atHMBind(self->itsDictionaryArray,indexOf).Value);
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
        *retValue = atHMBind(self->itsDictionaryArray,indexOf).Value;
    else
        {
		if (atHMBind(self->itsDictionaryArray,indexOf).Key == NULL) {FrameExit(gCP->TObject_VOID);}
        asTag(retValue) = atHMBind(self->itsDictionaryArray,indexOf).Key->itsObjectType;
        asObject(retValue) = atHMBind(self->itsDictionaryArray,indexOf).Key;
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

TVAL TDictionary_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM             indexOf;
NUM				n;
TVAL			left;
TVAL			right;
TVAL			prmv[3];
StartFrame
DeclareOBJ(TDictionary,self);
DeclareTVAL(tmpIndex);
EndFrame

self = (TDictionary*)asObject(&selfTval);

/*  Convert text and string keys to symbolic keys. */
/*  Note:   Symbolic keys guarantee uniqueness. */

if ((index1.Tag == TYTEXT) || (index1.Tag == TYSTRING) || (index1.Tag == TYSTRINGSUBSTR))
    {
    /* index1 = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,index1);*/
    *tmpIndex = TString_StringAnyCnv(gCP,gTP,TYSTRING,index1);
	index1 = *tmpIndex;
    }

/*  We accept object indices. */

if (_TObject_TypeFlag(asTag(&index1)) == _TObject_TfTOBJECT)
    {
	/* If the new value is #void, delete the key from the dictionary */
	if (newValue.Tag == TYVOID)
		{
		TDictionary_Delete(gCP,gTP,selfTval,index1);
		}
	else
		{
		TDictionary_MakeUniqueKey(gCP,gTP,self, asObject(&index1), newValue);
		}

	if (TESTON) goto SelfTestMode;
    FrameExit(selfTval);
    }
else
/*  We accept numeric indices. */
if (isNumIndex(&index1))
    {
    indexOf = asNumIndex(&index1);
    /*  Make sure index is in range. */
    if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
        FrameExit(gCP->TObject_ERROR_INVALID);
    
    /*  Set the Dictionary bonding element to the new value. */
    if (newValue.Tag == TYVOID)
		{
		TDictionary_Delete(gCP,gTP,selfTval,index1);
		}
	else
		{
		atHMBind(self->itsDictionaryArray,indexOf).Value = newValue;
		}

	if (TESTON) goto SelfTestMode;
    FrameExit(selfTval);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Perform mimimum self tests when appropriate. */
SelfTestMode:
if ((self->itsMaxItemIndex > 0) &&
	(self->itsImmediatePtr == NULL) &&
	((self->itsDictionaryArray == NULL) || 
	(((LpMHANDLEINFO)self->itsDictionaryArray)->Free == TRUE)))
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}
/* Make sure the keys are in ascending sorted order. */
for (n = 0; n < self->itsMaxItemIndex - 1; ++n)
	{
	left = TOBJ(((LpBIND)*self->itsDictionaryArray)[n].Key);
	right = TOBJ(((LpBIND)*self->itsDictionaryArray)[n+1].Key);
	if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
		{
		prmv[0].u.Object = (TObject*)self;
		prmv[0].Tag = TYDICTIONARY;
		prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
		prmv[1].Tag = TYCFUNCTION;
		FMath2_Sort(gCP,gTP,2,&prmv[0]);
		}
	}
/* Make sure the object content is valid */
for (n = 0; n < self->itsMaxItemIndex; ++n)
	{
	if (!_VALIDOBJ(((LpBIND)*self->itsDictionaryArray)[n].Key) ||
		!_VALIDTVAL(((LpBIND)*self->itsDictionaryArray)[n].Value))
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
SetIV2

Set the indexed value in the repeating portion of this object.

Note:   A Dictionary object must be indexed by two numeric index values. If the second index
		value is a 0, the key, at the first index location, will be replaced. If the second 
		index value is a 1, the value, at the first index location, will be replaced.

#endif

TVAL TDictionary_SetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL newValue)
{
NUM             indexOf;
NUM             whichNdx;
NUM				n;
TVAL			left;
TVAL			right;
StartFrame
DeclareOBJ(TDictionary,self);
DeclareTVAL(retValue);
DeclareTVAL(newKey);
EndFrame

self = (TDictionary*)asObject(&selfTval);

/* We accept the first index to be last: */

if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL)) && (strcmp(SymbolArray(index1),"last") == 0))
    {
    /* add a new Binding */
	if (_TObject_TypeFlag(asTag(&index2)) != _TObject_TfTOBJECT) {FrameExit(gCP->TObject_ERROR_BADIDXORKEY);}
	*newKey = index2;
    TDictionary_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex + 1);
    atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Key = newKey->u.Object;
    atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Value = newValue;
	if (TESTON) goto SelfTestMode;
	FrameExit(selfTval);
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"sorted") == 0))
    {
    /* Set the Dictionary in binary unique sorted fasion. */
	FrameExit(TDictionary_SetIV1(gCP,gTP,selfTval,index2,newValue));
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"unsorted") == 0))
    {
    /* Set the Structure in sequential unsorted fasion. */
    
	if (_TObject_TypeFlag(asTag(&index2)) != _TObject_TfTOBJECT) {FrameExit(gCP->TObject_ERROR_BADIDXORKEY);}
	for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
		{
		*retValue = FPredicate2_FullCompare(gCP, gTP, index2, TOBJ(atHMBind(self->itsDictionaryArray,indexOf).Key));
		if (retValue->u.Int == 0)
			{
			/* Update an existing binding. */
			atHMBind(self->itsDictionaryArray,indexOf).Value = newValue;
			if (TESTON) goto SelfTestMode;
			FrameExit(selfTval);
			}
		}

    /* add a new Binding */
	*newKey = index2;
    TDictionary_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex + 1);
    atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Key = newKey->u.Object;
    atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Value = newValue;
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
        atHMBind(self->itsDictionaryArray,indexOf).Value = newValue;
		}
    else
        {
 		if (_TObject_TypeFlag(asTag(&newValue)) != _TObject_TfTOBJECT) {FrameExit(gCP->TObject_ERROR_BADIDXORKEY);}
		atHMBind(self->itsDictionaryArray,indexOf).Key = newValue.u.Object;
        }
	if (TESTON) goto SelfTestMode;
	FrameExit(selfTval);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Perform mimimum self tests when appropriate. */
SelfTestMode:
if ((self->itsMaxItemIndex > 0) &&
	(self->itsImmediatePtr == NULL) &&
	((self->itsDictionaryArray == NULL) || 
	 (((LpMHANDLEINFO)self->itsDictionaryArray)->Free == TRUE)))
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}
/* Make sure the keys are in ascending sorted order. */
for (n = 0; n < self->itsMaxItemIndex - 1; ++n)
	{
	left = TOBJ(((LpBIND)*self->itsDictionaryArray)[n].Key);
	right = TOBJ(((LpBIND)*self->itsDictionaryArray)[n+1].Key);
	if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	}
FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TDictionary_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newKey, TVAL newValue)
{
StartFrame
DeclareOBJ(TDictionary,self);
EndFrame

self = (TDictionary*)asObject(&selfTval);

if (_TObject_TypeFlag(asTag(&newKey)) != _TObject_TfTOBJECT)
    FrameExit(gCP->TObject_ERROR_INVALID);

/* add a new Bonding to the end of the Dictionary */

TDictionary_SetMaxIndex(gCP, gTP, selfTval, self->itsMaxItemIndex + 1);
atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Key = asObject(&newKey);
atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Value = newValue;

    
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Dictionary is resized.

#endif

TVAL TDictionary_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{

register LpBIND         targetPtr;
register LpBIND         sourcePtr;
register LpBIND         haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TDictionary,self);
DeclareTVALArray(parm,2);

EndFrame

self = (TDictionary*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    {
    deleteIndex = asNumIndex(&index);
    }
else
    {
    parm[0] = selfTval;
    parm[1] = index;
    index =  FUtil2_Boundp(gCP,gTP,2, &parm[0]);
    if (isNumIndex(&index))
        {
        deleteIndex = asNumIndex(&index);
        }
    else
        {
        FrameExit(gCP->TObject_OK);
        }
    }
    
/*  Make sure TDictionary index is in range. */

if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the TDictionary down one position */

sourcePtr = &atHMBind(self->itsDictionaryArray,deleteIndex+1);
targetPtr = &atHMBind(self->itsDictionaryArray,deleteIndex);
haltPtr = &atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }
    
/*  Resize the TDictionary down one position */

TDictionary_SetMaxIndex(gCP, gTP, selfTval, self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the haigher values are moved up one position and
        the TDictionary is resized.

#endif

TVAL TDictionary_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index, TVAL newObject, TVAL newValue)
{
register LpBIND         targetPtr;
register LpBIND         sourcePtr;
register LpBIND         insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TDictionary,self);
EndFrame

self = (TDictionary*)asObject(&selfTval);

/*  We only accept numeric indices. */

if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  We never insert #void values. */

if (newValue.Tag == TYVOID) FrameExit(gCP->TObject_OK);

/*  Make sure TDictionary index is in range. */

if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the TDictionary up one position */

TDictionary_SetMaxIndex(gCP, gTP, selfTval, self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the TDictionary up one position */

sourcePtr = &atHMBind(self->itsDictionaryArray,(self->itsMaxItemIndex-2));
targetPtr = &atHMBind(self->itsDictionaryArray,(self->itsMaxItemIndex-1));
insertPtr = &atHMBind(self->itsDictionaryArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
    
/*  Insert the new value in the TDictionary at the specified position */

atHMBind(self->itsDictionaryArray,insertIndex).Value = newValue;
atHMBind(self->itsDictionaryArray,insertIndex).Key = asObject(&newObject);
    
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TDictionary_Print

Convert a Dictionary object into an ascii string and append it to an output buffer. 
Following the extended tradition, the Dictionary object is displayed as a series of 
elements as follows: 

#endif

TVAL TDictionary_Print(LpXCONTEXT gCP,LpTHREAD gTP,TDictionary* theEnv, LpNUM size, LpCHAR buf)
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
buf[++(*size)]  = 'c';
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
    
    *ec = FConio_sprintn(gCP,gTP,buf,size,TOBJ(atHMBind(theEnv->itsDictionaryArray,indexOf).Key));
    _TObject_ErrorChk(*ec);

    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[*size]      = ' ';
    buf[++(*size)]  = 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,atHMBind(theEnv->itsDictionaryArray,indexOf).Value);
    _TObject_ErrorChk(*ec);
    if (indexOf != theEnv->itsMaxItemIndex - 1)
        {
        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ' ';
        buf[++(*size)]  = 0;
        }
    }


/*  Show the Dictionary's cdr if it is not void and we are not printing a single binding */

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


/*  Show Dictionary suffix */

if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = '}';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TDictionary_New

Create a new TDictionary.

#endif

TDictionary*    TDictionary_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TDictionary,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TDictionary_Initialized) TDictionary_Init(gCP,gTP);

self = (TDictionary*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYDICTIONARY;
self->itsMaxItemIndex = 0;
self->itsDictionaryArray = NULL;
self->itsImmediatePtr = NULL;
FrameExit(self);
}
