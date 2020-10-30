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

#define _C_TOBJVECTOR
#define _SMARTBASE
#if 0
TObjVector.c

Implementation of the array class which stores a variable number of items of any TObject 
descendent in a linear array. The manifest typing system together with C++ methods 
controls and size the data items and the array itself.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tobjvec.h"
#include "tlambda.h"
#include "fconio.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TObjVector_Init

Initialize the TObjVector class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TObjVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TObjVector_Initialized) return;
gCP->TObjVector_Initialized  = TRUE;
TObject_Init(gCP,gTP);

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					 TYOBJVECTOR,
					(LpCHAR)"ObjVector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TObjVector_MakeNew,
					&TObjVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TObjVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TObjVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapDefault, 
					&TObject_Mapc,
					&TObjVector_Print,
					&TObjVector_Load,
					&TObjVector_Save,
					&TObjVector_ComputeSize,
					&TObjVector_Copy,
					&TObjVector_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObjVector_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TObjVector_Load(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
StartFrame
DeclareOBJ(TObjVector,it);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TObjVector_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TObjVector*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asObject(itTval) = (TObject*)it;
        asTag(itTval) = it->itsObjectType;
        TObjVector_LoadFcn(gCP,gTP,*itTval, aHMemory,theFileID);
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
TObjVector_BSearch

Search the Object vector for the given object.
Returns a TVAL containing the index for the closest match found, maybe an exact match.

#endif

TVAL TObjVector_BSearch(LpXCONTEXT gCP,LpTHREAD gTP,TObjVector* theSpace, TObject* anObject, COMPARE* compareCode)
{
register NUM    low, mid, high, compare;
HMObject        itsObjectArray;
StartFrame
DeclareTVAL(retTval);
EndFrame

low = 0;
mid = 0;
compare = 0;
*compareCode = -1;
asTag(retTval) = TYNUM;
asInt(retTval) = mid;

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

if(theSpace->itsMaxItemIndex != 0)
    {
    high = theSpace->itsMaxItemIndex - 1;
    itsObjectArray = theSpace->itsObjectArray;
    }   
else
    {
    FrameExit(*retTval);
    }
        
while ( low <= high )
    {   
    mid = (low + high) >> 1;
    
    compare = (NUM)anObject - (NUM)atHMObject(itsObjectArray,mid);

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
TObjVector_MakeUnique

Maintains a numerically sorted TObjVector for later bsearch. 
Returns a tval containing the indicated vector.

#endif

TVAL TObjVector_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,TObjVector* theSpace, TObject* anObject)
{
COMPARE     compareCode;
StartFrame
DeclareTVAL(spaceTval);
DeclareTVAL(indexTval);
DeclareTVAL(aTval);
EndFrame

/*  Init the search space array */

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

asObject(spaceTval) = (TObject*)theSpace;
asTag(spaceTval)	= theSpace->itsObjectType;
asObject(aTval) = anObject;
asTag(aTval) = anObject->itsObjectType;

if ( theSpace->itsMaxItemIndex > 0 ){
    *indexTval = TObjVector_BSearch(gCP,gTP,theSpace,anObject,&compareCode);
      
    if ( compareCode < 0 )
        {
        TObjVector_Insert(gCP, gTP, *spaceTval, *indexTval, *aTval);
        }
    else 
    if ( compareCode > 0 )
        {
        asInt(indexTval)++;
        TObjVector_Insert(gCP, gTP, *spaceTval, *indexTval, *aTval);
        }
    else 
        {
        /* collision */
        
        }
    }
else
    {
    asInt(indexTval) = 0;
    TObjVector_AddNewValue(gCP,gTP,*spaceTval, *aTval);
    }
    
anObject = atHMObject(theSpace->itsObjectArray,asInt(indexTval));
asObject(aTval) = anObject;
asTag(aTval) = anObject->itsObjectType;
FrameExit(*aTval);
}

/*  CConversion */

/*--------------------------------------------------------------------------------------- */
#if 0
IArray

Initialize a TObjVector object with a new tail(cdr) and an array of items.

#endif

void    TObjVector_IArray(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TObjVector,self);
EndFrame

self = (TObjVector*)asObject(&selfTval);

/*  Reshape the Array's array to be the correct size. */
TObjVector_SetMaxIndex(gCP,gTP,selfTval, argc);

/*  Set the array's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
	/*  Convert text values to string values. */
	if (argv[indexOf].Tag == TYTEXT || argv[indexOf].Tag == TYSTRINGSUBSTR)
		{
		argv[indexOf] = TString_StringAnyCnv(gCP,gTP,TYSTRING,argv[indexOf]);
		}

	if (_TObject_TypeFlag(asTag(&argv[indexOf])) == _TObject_TfTOBJECT)
        {
        atHMObject(self->itsObjectArray,indexOf) = asObject(&argv[indexOf]);
        }
    else
        {
        atHMObject(self->itsObjectArray,indexOf) = NULL;
        }
    }

/*  Set the array's tail(cdr). */
self->itsCdr = newCdr;
FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TObjVector_Mark(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
TObjVector*     self = (TObjVector*)asObject(&selfTval);
NUM             indexOf;

/*  Mark the array's Lisp tail(cdr) so its won't be garbage collected. */

TObject_MarkTval(gCP,gTP,self->itsCdr);

/*  Mark the array's items so they won't be garbage collected. */

for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    TObject_MarkObj(gCP,gTP,atHMObject(self->itsObjectArray,indexOf));
    }
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

void    TObjVector_Doomed(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
TObjVector*     self = (TObjVector*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsObjectArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsObjectArray);
self->itsMaxItemIndex = 0;
self->itsObjectArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TObjVector_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
TObjVector*     self = (TObjVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

*aSize += SIZEOF_TObjVectorOnDisk + self->itsMaxItemIndex * sizeof(OBJ);
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TObjVector_Save(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,HMemory aHMemory)
{
NUM                theOffset;
NUM                 cn;
NUM*                onDiskData;
StartFrame
DeclareOBJ(TObjVector,self);
DeclareOBJ(TObject,theObject);
EndFrame

self = (TObjVector*)asObject(&selfTval);


TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TObjVectorOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TObjVectorOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);

for(cn = 0, onDiskData = TObjVectorOnDiskData(aHMemory,theOffset);
cn < self->itsMaxItemIndex; cn++)
    {
	theObject = atHMObject(self->itsObjectArray,cn);

    /*  We can only save valid objects. */
	if ((theObject != NULL) && _VALIDOBJ(theObject))
		{
		TObjVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn] = TObject_RegisterObject(gCP,gTP,theObject);
		}
    /*  All invalid objects are saved as NIL. */
	else
		{
		TObjVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn] = -1;
		}
    }

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Load

The specified OODBMS manager is about to load this object. Convert your data from 
its handle format into your internal format.

#endif

void    TObjVector_LoadFcn(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory,NUM theFileID)
{
NUM                 cn;
NUM*                onDiskData;
NUM					theDiskObject;
StartFrame
DeclareOBJ(TObjVector,self);
EndFrame

theFileID = theFileID; // NOOP to hide unused parameter warning message
self = (TObjVector*)asObject(&selfTval);

TObjVector_SetMaxIndex(gCP, gTP, selfTval, TObjVectorOnDiskPtr(aHMemory,0)->itsMaxItemIndex);

self->itsCdr = TObject_LoadTval(gCP,gTP,TObjVectorOnDiskPtr(aHMemory,0)->itsCdr);
        
for(cn = 0, onDiskData = TObjVectorOnDiskData(aHMemory,0);cn < self->itsMaxItemIndex; cn++)
    {
	theDiskObject = TObjVectorOnDiskPtr(aHMemory,0)->itsItemArray[cn];

    /*  We can only load valid objects. */
	if (theDiskObject >= 0)
		{
		atHMObject(self->itsObjectArray,cn) = (TObject*)TObject_CheckRegistration(gCP,gTP,theDiskObject);
		}
    /*  All invalid objects are loaded as NIL. */
	else
		{
		atHMObject(self->itsObjectArray,cn) = (TObject*)NULL;
		}
    }
FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TObjVector.

#endif

TObject*    TObjVector_Copy(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TObjVector,self);
DeclareOBJ(TObjVector,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TObjVector*)selfTval.u.Object;

theCopy = TObjVector_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

TObjVector_SetMaxIndex(gCP,gTP,*tmpTval,self->itsMaxItemIndex);
FObject_SetCdr(gCP,gTP,(TObject*)theCopy,self->itsCdr);

if (self->itsObjectArray != NULL)
    {
    _FMemory_memcpy((LpCHAR)&atHMObject(theCopy->itsObjectArray,0),(LpCHAR)&atHMObject(self->itsObjectArray,0),(NUM)(self->itsMaxItemIndex*sizeof(TObject*)));
    }

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TObjVector_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TObjVector*     self = (TObjVector*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

#endif

TVAL TObjVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
NUM			n;
NUM			oldMaxItemIndex;
LpOBJ		objPtr;
StartFrame
DeclareOBJ(TObjVector,self);
EndFrame

self = (TObjVector*)selfTval.u.Object;
oldMaxItemIndex = self->itsMaxItemIndex;

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);


/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TObjVector_ImmediateSpace/(NUM)sizeof(TObject*)))
	{
	if (self->itsObjectArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(TObject*));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)ObjArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(TObject*));
		if ((self->itsObjectArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsObjectArray);
		}
	self->itsObjectArray = (HMObject)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
if (self->itsObjectArray == NULL)
    {
    self->itsObjectArray = (HMObject)FMemory_New(gCP, gTP, (NUM)(newRepeats*sizeof(TObject*)),TRUE);
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsObjectArray = (HMObject)FMemory_New(gCP, gTP, (NUM)(newRepeats*sizeof(TObject*)),TRUE);
	_FMemory_memcpy((char*)ObjArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(TObject*));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsObjectArray = (HMObject)FMemory_Resize(gCP, gTP, (HMemory)self->itsObjectArray,(NUM)(newRepeats*sizeof(TObject*)));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newRepeats)
	{
	objPtr = (LpOBJ)&ObjArray(selfTval)[oldMaxItemIndex];
	for (n = oldMaxItemIndex; n < newRepeats; ++n)
		{
		*(objPtr++) = NIL;
		}
	}
	
FrameExit(gCP->TObject_OK);

}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TObjVector_GetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TObjVector,self);
DeclareTVAL(retValue);
EndFrame

self = (TObjVector*)selfTval.u.Object;

/*  We only accept numeric indices. */
if (isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  Make sure aray index is in range. */
if (indexOf < 0)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Make sure aray index is in range. */
if (indexOf >= self->itsMaxItemIndex)
    FrameExit(gCP->TObject_VOID);

/*  Convert the object item into a tval. */
if (atHMObject(self->itsObjectArray,indexOf) == NULL)
	{
    FrameExit(gCP->TObject_VOID);
	}
else
    {
    asTag(retValue) = ((TObject*)(atHMObject(self->itsObjectArray,indexOf)))->itsObjectType;
    asObject(retValue) = atHMObject(self->itsObjectArray,indexOf);
    if ((retValue->Tag > TYMAXVALIDTYPE) || 
        (retValue->u.Object->itsObjectType > TYMAXVALIDTYPE) ||
        (retValue->Tag < TYVOID) || 
        (retValue->u.Object->itsObjectType < TYVOID))
        {
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
        }
    FrameExit(*retValue);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TObjVector_SetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TObjVector,self);
EndFrame

self = (TObjVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Make sure array index is in range. If too large, then grow the */
/*  array dynamically to receive the new value. */
if (indexOf < 0)
	{
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
if (indexOf >= self->itsMaxItemIndex)
   {
    TObjVector_SetMaxIndex(gCP,gTP,selfTval,indexOf+1);
   }

/*  Convert text values to string values. */
if (newValue.Tag == TYTEXT || newValue.Tag == TYSTRINGSUBSTR)
	{
	newValue = TString_StringAnyCnv(gCP,gTP,TYSTRING,newValue);
	}

/*  Save the new value into an Object. If it can't be converted into an      */
/*  object, then save the NIL object. Void is always stored as a NIL object. */
if (newValue.Tag == TYVOID)
	{
    atHMObject(self->itsObjectArray,indexOf) = NULL;
	}
else
if (_TObject_TypeFlag(asTag(&newValue)) == _TObject_TfTOBJECT)
    {
    if ((newValue.Tag > TYMAXVALIDTYPE) || 
        (newValue.u.Object->itsObjectType > TYMAXVALIDTYPE) ||
        (newValue.Tag < TYVOID) || 
        (newValue.u.Object->itsObjectType < TYVOID))
        {
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
        }
    atHMObject(self->itsObjectArray,indexOf) = asObject(&newValue);
    }
else
    atHMObject(self->itsObjectArray,indexOf) = NULL;

FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TObjVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
StartFrame
DeclareOBJ(TObjVector,self);
DeclareTVAL(index);
DeclareTVAL(ret);
EndFrame

self = (TObjVector*)asObject(&selfTval);

asInt(index) = self->itsMaxItemIndex;
asTag(index) = TYNUM;

*ret = TObjVector_SetIV1(gCP,gTP,selfTval, *index, newValue);
FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TObjVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{
register LpOBJ          targetPtr;
register LpOBJ          sourcePtr;
register LpOBJ          haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TObjVector,self);
EndFrame

self = (TObjVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the array down one position */

sourcePtr   = (LpOBJ)&atHMObject(self->itsObjectArray,deleteIndex+1);
targetPtr   = (LpOBJ)&atHMObject(self->itsObjectArray,deleteIndex);
haltPtr     = (LpOBJ)&atHMObject(self->itsObjectArray,self->itsMaxItemIndex);

while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }

/*  Resize the Vector down one position */
TObjVector_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the higher values are moved up one position and
        the Vector is resized.

#endif

TVAL TObjVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue)
{
register LpOBJ          targetPtr;
register LpOBJ          sourcePtr;
register LpOBJ          insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TObjVector,self);
EndFrame

self = (TObjVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the array up one position */
TObjVector_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the array up one position */

sourcePtr = (LpOBJ)&atHMObject(self->itsObjectArray,(self->itsMaxItemIndex-2));
targetPtr = (LpOBJ)&atHMObject(self->itsObjectArray,(self->itsMaxItemIndex-1));
insertPtr = (LpOBJ)&atHMObject(self->itsObjectArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }

/*  Save the new value into an Object. If it can't be converted into an */
/*  object, then save the NIL object. */
if (_TObject_TypeFlag(asTag(&newValue)) == _TObject_TfTOBJECT)
    {
    atHMObject(self->itsObjectArray,insertIndex) = asObject(&newValue);
    }
else
    atHMObject(self->itsObjectArray,insertIndex) = NULL;
    
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetCdr

Return the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TObjVector_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TObjVector*     self = (TObjVector*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsCdr);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetCdr

Set the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TObjVector_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TObjVector*     self = (TObjVector*)asObject(&selfTval);
gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObjVector_New

Create a new TObjVector.

#endif

TObjVector* TObjVector_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TObjVector,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TObjVector_Initialized) TObjVector_Init(gCP,gTP);

self = (TObjVector*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYOBJVECTOR;
self->itsMaxItemIndex = 0;
self->itsObjectArray = NULL;
self->itsImmediatePtr = NULL;

FrameExit(self);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a ObjVector object into an ascii string and append it to an output buffer. 

#endif

TVAL     TObjVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
NUM                     indexOf;
StartFrame	
DeclareOBJ(TObjVector,self);
DeclareTVAL(ec);
DeclareTVAL(item);
EndFrame

self = (TObjVector*)asObject(&selfTval);

/*  Quit if the output string is already too long */

if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Matrix prefix */

buf[*size]      = '#';
buf[++(*size)]  = '(';
buf[++(*size)]  = 'o';
buf[++(*size)]  = 'b';
buf[++(*size)]  = 'j';
buf[++(*size)]  = '|';
buf[++(*size)]  = ' ';
buf[++(*size)]	= 0;

for(indexOf = 0; indexOf < self->itsMaxItemIndex; indexOf++)
    {
	*item = FSmartbase_Ref(gCP,gTP,2,selfTval,TINT(indexOf));
    *ec = FConio_sprintn(gCP,gTP,buf,size,*item);
    _TObject_ErrorChk(*ec);
    
     if (*size + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[*size]      = ' ';
    buf[++(*size)]  = 0;
    }

/*  Show the Vector's cdr if it is not void */

if (asTag(&self->itsCdr))
    {
     if (*size + 4 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[*size]      = ' ';
    buf[++(*size)]  = '.';
    buf[++(*size)]  = ' ';
    buf[++(*size)]  = 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,self->itsCdr);
    _TObject_ErrorChk(*ec);
    }


/*  Show Matrix suffix */

 if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = ')';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0

TObjVector_MakeNew

Return a Vector object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Vector with zeros. 
        If too few arguments are specified, fill the Vector with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (new ObjVector: 5)             =>      #(0 0 0 0 0) 
        (new ObjVector: 5 1)           =>      #(1 1 1 1 1) 
        (new ObjVector: 5 1 2 3)       =>      #(1 2 3 1 2) 
        (new ObjVector: 5 1 2 3 . 6)   =>      #(1 2 3 1 2 . 6) 
        
#endif

TVAL TObjVector_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM						size = 0;
NUM						sizeIndex = 0;
NUM                     vectorIndex = 0;
NUM                     startIndex = sizeIndex + 1;
NUM                     argIndex = sizeIndex + 1;
NUM                     cdrIndex = argc;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareTVAL(ivTval);
DeclareOBJ(TObjVector,ap);
EndFrame
 
/*  The first argument should be the requested size. */

if (argc == 0)
    {
    size = 0;
	sizeIndex = 0;
    }
else
    {
	if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL)) sizeIndex = 1;
    if (!isNumIndex(&argv[sizeIndex]))
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    else
        {
        size = asNumIndex(&argv[sizeIndex]);
        }
	}
    

if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
ndx->Tag = TYNUM;
ndx->u.Int = size;

/*  Create the Object Vector object */

ap = TObjVector_New(gCP,gTP);
FObject_SetMaxIndex(gCP,gTP,(TObject*)ap, size);
asObject(ret)   = (TObject*)ap;
asTag(ret)		= TYOBJVECTOR;
asTag(fill)     = TYNUM;
asInt(fill)     = 0;

/*  Initialize the object vector only if necessary (important time saving). */

startIndex = sizeIndex + 1;
argIndex = sizeIndex + 1;
if (argc > startIndex)
    {
    asObject(ivTval) = (TObject*)ap;
    asTag(ivTval) = ap->itsObjectType;
    while (vectorIndex < size)
        {
        if (argIndex >= argc) argIndex = startIndex;
        if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            argIndex = startIndex;
            }
        if (argIndex < argc) *fill = argv[argIndex++];
        
        asInt(ndx) = vectorIndex++;
    
        (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx,*fill);
        }
     
    
    /*  Save the optional cdr argument */
    
    if ((argIndex < argc) && 
        (asTag(&argv[argIndex]) == TYPCODE) && 
        (asShort(&argv[argIndex]) == PERIODTOK))
        {
        cdrIndex = argIndex + 1;
        }
    if (cdrIndex < argc)
        {
        FObject_SetCdr(gCP,gTP,(TObject*)ap, argv[cdrIndex]);
        }
    }
    
FrameExit(*ret);
}
