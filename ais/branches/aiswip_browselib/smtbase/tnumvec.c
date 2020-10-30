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
TNumVec.c

Implementation of the Number vector class which stores a variable number of
Numbers in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tnumvec.h"
#include "tlambda.h"
#include "fconio.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TNumVector_Init

Initialize the TNumVector class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TNumVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TNumVector_Initialized) return;
gCP->TNumVector_Initialized  = TRUE;

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					 TYNUMVECTOR,
					(LpCHAR)"NumVector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TNumVector_MakeNew,
					&TNumVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TNumVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TNumVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_Map,
					&TObject_Mapc,
					&TNumVector_Print,
					&TNumVector_Load,
					&TNumVector_Save,
					&TNumVector_ComputeSize,
					&TNumVector_Copy,
					&TNumVector_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNumVector_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TNumVector_Load(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 cn;
StartFrame
DeclareOBJ(TNumVector,it);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TNumVector_New(gCP, gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TNumVector*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asTag(itTval) = it->itsObjectType;
        asObject(itTval) = (TObject*)it;
        
        TNumVector_SetMaxIndex(gCP,gTP,*itTval,TNumVectorOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TNumVectorOnDiskPtr(aHMemory,0)->itsCdr);
                
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
            atHMReal(it->itsRealArray,cn) = TNumVectorOnDiskPtr(aHMemory,0)->itsItemArray[cn];
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
TNumVector_BSearch

Search the Object vector for the given object.
Returns a TVAL containing the index for the closest match found, maybe an exact match.

#endif

TVAL TNumVector_BSearch(LpXCONTEXT gCP,LpTHREAD gTP,TNumVector* theSpace, REAL aReal, COMPARE* compareCode)
{
register NUM    low, mid, high;
REAL            compare;
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

if(theSpace->itsMaxItemIndex)
    {
    high = theSpace->itsMaxItemIndex - 1;
    }   
else
    {
    FrameExit(*retTval);
    }
        
while ( low <= high )
    {   
    mid = (low + high) >> 1;
    
    compare = aReal - atHMReal(theSpace->itsRealArray,mid);

    if ( compare < 0.0 )
        {
        high = mid - 1;
        }
    else 
    if ( compare > 0.0 )
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
TNumVector_MakeUnique

Maintains a numerically sorted TNumVector for later bsearch. 
Returns a tval containing the indicated value.

#endif

TVAL TNumVector_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,TNumVector* theSpace, REAL aReal)
{
COMPARE     compareCode;
StartFrame
DeclareTVAL(selfTval);
DeclareTVAL(indexTval);
DeclareTVAL(aTval);
EndFrame


/*  Init the search space array */

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

asTag(selfTval) = theSpace->itsObjectType;
asObject(selfTval) = (TObject*)theSpace;

asReal(aTval) = aReal;
asTag(aTval) = TYREAL;

if ( theSpace->itsMaxItemIndex > 0 ){
    *indexTval = TNumVector_BSearch(gCP, gTP, theSpace,  aReal, &compareCode);
      
    if ( compareCode < 0 )
        {
        TNumVector_Insert(gCP, gTP, *selfTval, *indexTval, *aTval);
        }
    else 
    if ( compareCode > 0 )
        {
        asInt(indexTval)++;
        TNumVector_Insert(gCP, gTP, *selfTval, *indexTval, *aTval);
        }
    else 
        {
        /* collision */
        
        }
    }
else
    {
    asInt(indexTval) = 0;
    TNumVector_AddNewValue(gCP,gTP,*selfTval,*aTval);
    }
    
asReal(aTval) =  atHMReal(theSpace->itsRealArray,asInt(indexTval));
asTag(aTval) = TYREAL;
FrameExit(*aTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNumVector_New

Create a new TNumVector.

#endif

TNumVector* TNumVector_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TNumVector,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TNumVector_Initialized) TNumVector_Init(gCP,gTP);

self = (TNumVector*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYNUMVECTOR;
self->itsMaxItemIndex = 0;
self->itsRealArray = NULL;
self->itsImmediatePtr = NULL;
FrameExit(self);
}

/*--------------------------------------------------------------------------------------- */
#if 0
INumVector

Initialize a TNumVector object with a new tail(cdr) and an array of items.

#endif

void    TNumVector_INumVector(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TNumVector,self);
EndFrame

self = (TNumVector*)asObject(&selfTval);

/*  Reshape the bit vectors's array to be the correct size. */
TNumVector_SetMaxIndex(gCP,gTP,selfTval,argc);

/*  Set the bit vector's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    switch (argv[indexOf].Tag)
        {
        case TYNUM:
            atHMReal(self->itsRealArray,indexOf) = asInt(&argv[indexOf]);
            break;
            
        case TYMONEY:
        case TYDATE:
        case TYREAL:
            atHMReal(self->itsRealArray,indexOf) = asReal(&argv[indexOf]);
            break;
            
        case TYBOLE:
            atHMReal(self->itsRealArray,indexOf) = asBool(&argv[indexOf]);
            break;
            
        default:
            atHMReal(self->itsRealArray,indexOf) = 0;
            break;
        }
    }

/*  Set the integer vector's tail(cdr). */
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

void    TNumVector_Mark(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
TNumVector*     self = (TNumVector*)asObject(&selfTval);
/*  Mark the number vector's Lisp tail(cdr) so its won't be garbage collected. */

TObject_MarkTval(gCP,gTP,self->itsCdr);

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

void    TNumVector_Doomed(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
TNumVector*     self = (TNumVector*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsRealArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsRealArray);
self->itsMaxItemIndex = 0;
self->itsRealArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TNumVector_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
TNumVector*     self = (TNumVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;
*aSize += SIZEOF_TNumVectorOnDisk + (self->itsMaxItemIndex * sizeof(REAL));
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TNumVector_Save(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,HMemory aHMemory)
{
long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TNumVector,self);
EndFrame

self = (TNumVector*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TNumVectorOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TNumVectorOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);
for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
    TNumVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn] = atHMReal(self->itsRealArray,cn);
    }

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TNumVector.

#endif

TObject*    TNumVector_Copy(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TNumVector,self);
DeclareOBJ(TNumVector,theCopy);
DeclareTVAL(tmpTval);
EndFrame
self = (TNumVector*)selfTval.u.Object;

theCopy = TNumVector_New(gCP, gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag		= theCopy->itsObjectType;

TNumVector_SetMaxIndex(gCP,gTP,*tmpTval,self->itsMaxItemIndex);


FObject_SetCdr(gCP,gTP,(TObject*)theCopy, self->itsCdr);
if (self->itsRealArray != NULL)
    {
    _FMemory_memcpy((LpCHAR)&atHMReal(theCopy->itsRealArray,0),(LpCHAR)&atHMReal(self->itsRealArray,0),(LONG)(self->itsMaxItemIndex*sizeof(REAL)));
    }

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TNumVector_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TNumVector*     self = (TNumVector*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   Remember the repeating portion of this object is measured in integers!

#endif

TVAL TNumVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
NUM			n;
NUM			oldMaxItemIndex;
LpREAL		ptr;
StartFrame
DeclareOBJ(TNumVector,self);
EndFrame

self = (TNumVector*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
oldMaxItemIndex = self->itsMaxItemIndex;

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TNumVector_ImmediateSpace/(NUM)sizeof(REAL)))
	{
	if (self->itsRealArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(REAL));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)RealArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(REAL));
		if ((self->itsRealArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsRealArray);
		}
	self->itsRealArray = (HMReal)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
if (self->itsRealArray == NULL)
    {
    self->itsRealArray = (HMReal)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(REAL)),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsRealArray = (HMReal)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(REAL)),TRUE);
	_FMemory_memcpy((char*)RealArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(REAL));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsRealArray = (HMReal)FMemory_Resize(gCP, gTP, (HMemory)self->itsRealArray,(LONG)(newRepeats*sizeof(REAL)));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newRepeats)
	{
	ptr = (LpREAL)&RealArray(selfTval)[oldMaxItemIndex];
	for (n = oldMaxItemIndex; n < newRepeats; ++n)
		{
		*(ptr++) = 0.0;
		}
	}
	
FrameExit(gCP->TObject_OK);

}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TNumVector_GetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TNumVector,self);
DeclareTVAL(retValue);
EndFrame

self = (TNumVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  Make sure array index is in range. */
if (indexOf < 0)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Make sure array index is in range. */
if (indexOf >= self->itsMaxItemIndex)
    FrameExit(gCP->TObject_VOID);

/*  Convert the nth real item into a tval. */
asReal(retValue) = atHMReal(self->itsRealArray,indexOf);
asTag(retValue) = TYREAL;
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TNumVector_SetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TNumVector,self);
DeclareTVAL(ret);
EndFrame

self = (TNumVector*)asObject(&selfTval);


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
    TNumVector_SetMaxIndex(gCP,gTP,selfTval,indexOf+1);

/*  Save the new value as the nth integer item. */
switch (newValue.Tag)
    {
    case TYNUM:
        atHMReal(self->itsRealArray,indexOf) = asInt(&newValue);
        break;
        
    case TYMONEY:
    case TYDATE:
    case TYREAL:
        atHMReal(self->itsRealArray,indexOf) = asReal(&newValue);
        break;
        
    case TYBOLE:
        atHMReal(self->itsRealArray,indexOf) = asBool(&newValue);
        break;
        
    default:
        atHMReal(self->itsRealArray,indexOf) = 0;
        break;
    }
    
asTag(ret) = self->itsObjectType;
asObject(ret) = (TObject*)self;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TNumVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
StartFrame
DeclareOBJ(TNumVector,self);
DeclareTVAL(index);
EndFrame

self = (TNumVector*)asObject(&selfTval);

asInt(index) = self->itsMaxItemIndex;
asTag(index) = TYNUM;

FrameExit(TNumVector_SetIV1(gCP,gTP,selfTval, *index, newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TNumVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{
register LpREAL         targetPtr;
register LpREAL         sourcePtr;
register LpREAL         haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TNumVector,self);
EndFrame

self = (TNumVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the array down one position */

sourcePtr = &atHMReal(self->itsRealArray,deleteIndex+1);
targetPtr = &atHMReal(self->itsRealArray,deleteIndex);
haltPtr = &atHMReal(self->itsRealArray,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }

/*  Resize the Vector down one position */
TNumVector_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the haigher values are moved up one position and
        the Vector is resized.

#endif

TVAL TNumVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue)
{
register LpREAL         targetPtr;
register LpREAL         sourcePtr;
register LpREAL         insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TNumVector,self);
EndFrame

self = (TNumVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */

if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the array up one position */
TNumVector_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the Vector up one position */

sourcePtr = &atHMReal(self->itsRealArray,(self->itsMaxItemIndex-2));
targetPtr = &atHMReal(self->itsRealArray,(self->itsMaxItemIndex-1));
insertPtr = &atHMReal(self->itsRealArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
    
/*  Insert the new value in the Vector at the specified position */
atHMReal(self->itsRealArray,insertIndex) = asReal(&newValue);

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

TVAL TNumVector_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TNumVector*     self = (TNumVector*)asObject(&selfTval);
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

TVAL TNumVector_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TNumVector*     self = (TNumVector*)asObject(&selfTval);
gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a NumVector object into an ascii string and append it to an output buffer. 

#endif

TVAL     TNumVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
NUM                     indexOf;
StartFrame	
DeclareOBJ(TNumVector,self);
DeclareTVAL(ec);
DeclareTVAL(item);
EndFrame

self = (TNumVector*)asObject(&selfTval);

/*  Quit if the output string is already too long */

if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Vector prefix */

buf[*size]      = '#';
buf[++(*size)]  = '(';
buf[++(*size)]  = 'n';
buf[++(*size)]  = 'u';
buf[++(*size)]  = 'm';
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


/*  Show Vector suffix */

 if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = ')';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

TNumVector_MakeNew

Return a Vector object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Vector with zeros. 
        If too few arguments are specified, fill the Vector with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (new NumVector 5)             =>      #(0 0 0 0 0) 
        (new NumVector 5 1)           =>      #(1 1 1 1 1) 
        (new NumVector 5 1 2 3)       =>      #(1 2 3 1 2) 
        (new NumVector 5 1 2 3 . 6)   =>      #(1 2 3 1 2 . 6) 
        
#endif

TVAL TNumVector_MakeNew(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     size = 0;
NUM                     sizeIndex = 0;
NUM                     vectorIndex = 0;
NUM                     startIndex = sizeIndex + 1;
NUM                     argIndex = sizeIndex + 1;
NUM                     cdrIndex = argc;
TVAL                    ivTval;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareOBJ(TNumVector,np);
EndFrame
 
/*  The first argument should be the requested size. */

if (argc == 0)
    {
    size = 0;
    }
else
    {
    if (!isNumIndex(&argv[sizeIndex]))
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    else
        {
        size = asNumIndex(&argv[sizeIndex]);
        }
    
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    asTag(ndx) = TYNUM;

    /*  This is a request to construct a number vector. */
    
    asTag(ret) = TYVOID;
    asObj(ret) = 0;

    np = TNumVector_New(gCP, gTP);
    asObject(&ivTval) = (TObject*)np;
    asTag(&ivTval) = np->itsObjectType;
    TNumVector_SetMaxIndex(gCP,gTP,ivTval,size);
    
    /*  Initialize the Number Vector object */
    
    asTag(ret)		= TYNUMVECTOR;
    asObject(ret)   = (TObject*)np;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    
    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > startIndex)
        {
        asObject(&ivTval) = (TObject*)np;
        asTag(&ivTval) = np->itsObjectType;
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
        
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP,gTP, ivTval, *ndx, *fill);
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
            FObject_SetCdr(gCP,gTP,(TObject*)np, argv[cdrIndex]);
            }
        }
    }

FrameExit(*ret);
}
