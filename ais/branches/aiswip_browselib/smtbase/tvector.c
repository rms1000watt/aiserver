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

#define _C_TVECTOR
#define _SMARTBASE
#if 0
TVector.c

Implementation of the vector class which stores a variable number of items of any
type in a linear vector. The manifest typing system is used to control and size the
data items and the array itself.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tvector.h"
#include "fconio.h"
#include "fmake.h"
#include "tsymbol.h"
#include "futil3.h"
/*--------------------------------------------------------------------------------------- */
#if 0
TVector_Init

Initialize the TVector class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TVector_Initialized) return;
gCP->TVector_Initialized     = TRUE;
TObject_Init(gCP,gTP);

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					 TYVECTOR,
					(LpCHAR)"Vector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&FMake_Vector,
					&TVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TVector_Map, 
					&TVector_Mapc,
					&TObject_PrintObj,
					&TVector_Load,
					&TVector_Save,
					&TVector_ComputeSize,
					&TVector_Copy,
					&TVector_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TVector_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TVector_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 cn;
StartFrame
DeclareOBJ(TVector,it);
DeclareTVAL(retTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TVector_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TVector*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        FObject_SetMaxIndex(gCP,gTP,(TObject*)it,TVectorOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        it->itsAttributes = (TObjVector*)TObject_CheckRegistration(gCP,gTP,TVectorOnDiskPtr(aHMemory,0)->itsAttributes);
		it->itsCdr = TObject_LoadTval(gCP,gTP,TVectorOnDiskPtr(aHMemory,0)->itsCdr);

		/* Load the itemArray from the disk stream */
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
            atHMTval(it->itsTvalArray,cn) = TObject_LoadTval(gCP,gTP,TVectorOnDiskPtr(aHMemory,0)->itsItemArray[cn]);
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
TVector_BSearch

Search a Tvector examining the indicated data.

#endif

TVAL TVector_BSearch(LpXCONTEXT gCP,LpTHREAD gTP,TVector* theSpace, NUM start, NUM len, TVAL newData, COMPARE* compareCode)
{
register NUM    low, mid, high, compare, cn;
LpCHAR          savP;
LpCHAR          srcP;
LpCHAR          tstP;

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
    
savP = ((LpCHAR)(&newData)) + start;
len -= 1;

while ( low <= high )
    {   
    mid = (low + high) >> 1;
    tstP = ((LpCHAR)&(atHMTval(theSpace->itsTvalArray,mid))) + start;
    srcP = savP;
    
    for(cn = 0; *srcP == *tstP && cn < len; cn++, srcP++, tstP++) {}
    compare = *srcP - *tstP;

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
        {
        break;
        }
   }

*compareCode = (compare != 0) ? ((compare < 0) ? LOW : HIGH) : EQUAL;
asInt(retTval) = mid;
FrameExit(*retTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TVector_MakeUnique

Maintains anumerically sorted TVector for later bsearch. 
Returns a tval containing the indicated vector.

#endif

TVAL TVector_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,TVector* theSpace, NUM start, NUM len, TVAL newData)
{
COMPARE     compareCode;
StartFrame
DeclareTVAL(indexTval);
DeclareTVAL(spaceTval);
EndFrame

/*  Init the search space array */

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

asObject(spaceTval) = (TObject*)theSpace;
asTag(spaceTval) = theSpace->itsObjectType;

if ( theSpace->itsMaxItemIndex > 0 ){
    *indexTval = TVector_BSearch(gCP,gTP,theSpace,start,len,newData,&compareCode);
      
    if ( compareCode < 0 )
        {
        TVector_Insert(gCP, gTP, *spaceTval, *indexTval, newData);
        }
    else 
    if ( compareCode > 0 )
        {
        asInt(indexTval)++;
        TVector_Insert(gCP, gTP, *spaceTval, *indexTval, newData);
        }
    else 
        {
        /* collision */
        
        }
    }
else
    {
    asInt(indexTval) = 0;
    TVector_AddNewValue(gCP,gTP,*spaceTval, newData);
    }
    
FrameExit(atHMTval(theSpace->itsTvalArray,asInt(indexTval)));
    
}


/*--------------------------------------------------------------------------------------- */
#if 0
IVector

Initialize a TVector object with a new tail(cdr) and an array of items.

#endif

TVAL    TVector_IVector(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr)
{
NUM             indexOf;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TVector,self);
EndFrame

self = (TVector*)asObject(&selfTval);

/*  Reshape the Vector's array to be the correct size. */
TVector_SetMaxIndex(gCP, gTP, selfTval, argc);

/*  Set the vector's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    atHMTval(self->itsTvalArray,indexOf) = argv[indexOf];

/*  Set the vector's tail(cdr). */
self->itsCdr = newCdr;

asTag(ret) = TYVECTOR;
asObject(ret) = (TObject*)self;
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TVector_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TVector,self);
EndFrame

self = (TVector*)asObject(&selfTval);

/*  Mark the vector's Lisp tail(cdr) so its won't be garbage collected. */
TObject_MarkTval(gCP,gTP,self->itsCdr);

/* Mark the vector's Attributes vector containing the column names, if present */
if (self->itsAttributes != NULL)
	TObject_MarkObj(gCP,gTP,(TObject*)self->itsAttributes);

/*  Mark the vector's items so they won't be garbage collected. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    TObject_MarkTval(gCP,gTP,atHMTval(self->itsTvalArray,indexOf));
    }
FrameReturn;
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

void    TVector_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TVector*    self = (TVector*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsTvalArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsTvalArray);
self->itsMaxItemIndex = 0;	
self->itsTvalArray = NULL;
self->itsAttributes = NULL;
}


/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TVector_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
TVector*        self = (TVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/* Compute the size of the object header */
*aSize = SIZEOF_TObjectOnDisk;

/* Compute the size of the structure and all its bindings */
*aSize += ((NUM)&((TVectorOnDisk*)0)->itsItemArray[self->itsMaxItemIndex]);

ALLIGNME(*aSize);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TVector_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TVector,self);
EndFrame

self = (TVector*)asObject(&selfTval);

/*  Save itsObjectOnDisk */

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

/*  Save TVectorOnDisk */

TVectorOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TVectorOnDiskPtr(aHMemory,theOffset)->itsAttributes = TObject_RegisterObject(gCP,gTP,(TObject*)self->itsAttributes);
TVectorOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);

/*  Save the tval array in a compressed form into a stream in disk record */

for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
    TVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn] = TObject_RegisterTval(gCP,gTP,atHMTval(self->itsTvalArray,cn));
   }

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TVector.

#endif

TObject*    TVector_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame	
DeclareOBJ(TVector,self);
DeclareOBJ(TVector,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TVector*)selfTval.u.Object;

theCopy = TVector_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

if (self->itsTvalArray != NULL)
	{
	TVector_IVector(gCP, gTP, *tmpTval, self->itsMaxItemIndex,&atHMTval(self->itsTvalArray,0),self->itsCdr);
	}
theCopy->itsAttributes = self->itsAttributes;

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TVector_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TVector*    self = (TVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

#endif

TVAL TVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
NUM			n;
NUM			oldMaxItemIndex;
LpTVAL		ptr;
StartFrame	
DeclareOBJ(TVector,self);
EndFrame

self = (TVector*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
oldMaxItemIndex = self->itsMaxItemIndex;

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TVector_ImmediateSpace/(NUM)sizeof(TVAL)))
	{
	if (self->itsTvalArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(TVAL));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)TvalArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(TVAL));
		if ((self->itsTvalArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsTvalArray);
		}
	self->itsTvalArray = (HMTval)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
if (self->itsTvalArray == NULL)
    {
    self->itsTvalArray = (HMTval)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(TVAL)),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsTvalArray = (HMTval)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(TVAL)),TRUE);
	_FMemory_memcpy((char*)TvalArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(TVAL));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsTvalArray = (HMTval)FMemory_Resize(gCP, gTP, (HMemory)self->itsTvalArray,(LONG)(newRepeats*sizeof(TVAL)));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newRepeats)
	{
	ptr = (LpTVAL)&TvalArray(selfTval)[oldMaxItemIndex];
	for (n = oldMaxItemIndex; n < newRepeats; ++n)
		{
		*(ptr++) = gCP->Tval_VOID;
		}
	}
	
FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TVector_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
StartFrame	
DeclareOBJ(TVector,self);
DeclareOBJ(TObjVector,attv);
EndFrame

self = (TVector*)asObject(&selfTval);

/*  Convert text and string keys to symbolic keys. */
/*  Note:   Symbolic keys guarantee uniqueness. */

if ((index1.Tag == TYTEXT) || (index1.Tag == TYSTRING) || (index1.Tag == TYQUOTEDSYMBOL) || (index1.Tag == TYSTRINGSUBSTR))
    {
    index1 = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,index1);
    }

/*  We accept object indices. */

if ((_TObject_TypeFlag(asTag(&index1)) == _TObject_TfTOBJECT) &&
	(self->itsAttributes != NULL))
    {
	attv = self->itsAttributes;
    for (indexOf = 0; (indexOf < self->itsMaxItemIndex) && (indexOf < attv->itsMaxItemIndex); ++indexOf)
        {
		/* Check if the index matches the symbol in the attributes vector */
		if (atHMObject(attv->itsObjectArray,indexOf) == asObject(&index1))
			{
			/*  Make sure array index is in range. */
			if (indexOf >= self->itsMaxItemIndex) FrameExit(gCP->TObject_VOID);
			/* Return the value in the Target vector */
			FrameExit(atHMTval(self->itsTvalArray,indexOf));
			}
        }
        
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    }
else
/*  We accept numeric indices. */
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

FrameExit(atHMTval(self->itsTvalArray,indexOf));
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.  The index may be symbolic or
numeric

Note:   

#endif

TVAL TVector_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
StartFrame	
NUM             indexOf;
DeclareOBJ(TVector,self);
DeclareOBJ(TObjVector,attv);
DeclareTVAL(ret);
EndFrame

/* Manage a simple numeric index quickly */
if ((index1.Tag == TYNUM) && inRange(index1.u.Int,0,Vector((selfTval))->itsMaxItemIndex))
	{
	TvalArray((selfTval))[index1.u.Int] = newValue;
	FrameExit(selfTval);
	}

self = (TVector*)asObject(&selfTval);

/*  Convert text and string keys to symbolic keys. */
/*  Note:   Symbolic keys guarantee uniqueness. */

if ((index1.Tag == TYTEXT) || (index1.Tag == TYSTRING) || (index1.Tag == TYQUOTEDSYMBOL) || (index1.Tag == TYSTRINGSUBSTR))
    {
    index1 = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,index1);
    }


/*  We accept object indices. */

if ((_TObject_TypeFlag(asTag(&index1)) == _TObject_TfTOBJECT) &&
	(self->itsAttributes != NULL))
    {
	attv = self->itsAttributes;
    for (indexOf = 0; indexOf < attv->itsMaxItemIndex; ++indexOf)
        {
		/* Check if the index matches the symbol in the attributes vector */
		if (atHMObject(attv->itsObjectArray,indexOf) == asObject(&index1))
			{
			/* Check the index is within range of the Target vector */
			if (indexOf < self->itsMaxItemIndex)
				{
				/* Replace the value in the Target vector */
				atHMTval(self->itsTvalArray,indexOf) = newValue;
				}
			else
				{
				/* Replace the value in the Target vector */
				TVector_SetMaxIndex(gCP,gTP,selfTval, indexOf+1);
				atHMTval(self->itsTvalArray,indexOf) = newValue;
				}

            FrameExit(selfTval);
			}
        }
        
    for (indexOf = 0; (indexOf < self->itsMaxItemIndex) && (indexOf < attv->itsMaxItemIndex); ++indexOf)
        
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    }
else
/*  We accept numeric indices. */
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
    TVector_SetMaxIndex(gCP,gTP,selfTval, indexOf+1);

/*  Save the new value in the vector. */
atHMTval(self->itsTvalArray,indexOf) = newValue;

asTag(ret) = TYVECTOR;
asObject(ret) = (TObject*)self;

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
StartFrame	
DeclareOBJ(TVector,self);
DeclareTVAL(index);
EndFrame

self = (TVector*)asObject(&selfTval);

asReal(index) = self->itsMaxItemIndex;
asTag(index) = TYREAL;

FrameExit(TVector_SetIV1(gCP,gTP,selfTval, *index, newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{
register LpTVAL         targetPtr;
register LpTVAL         sourcePtr;
register LpTVAL         haltPtr;
NUM                     deleteIndex;
StartFrame	
DeclareOBJ(TVector,self);
EndFrame

self = (TVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure Vector index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the Vector down one position */

sourcePtr = &atHMTval(self->itsTvalArray,deleteIndex+1);
targetPtr = &atHMTval(self->itsTvalArray,deleteIndex);
haltPtr = &atHMTval(self->itsTvalArray,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }
    
/*  Resize the Vector down one position */
TVector_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the haigher values are moved up one position and
        the Vector is resized.

#endif

TVAL TVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue)
{
register LpTVAL         targetPtr;
register LpTVAL         sourcePtr;
register LpTVAL         insertPtr;
NUM                     insertIndex;
StartFrame	
DeclareOBJ(TVector,self);
DeclareTVAL(err);
EndFrame

self = (TVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
	{
	*err = TERROR("!vectorInsert: Invalid Index Argument!");
    FrameExit(*err);
	}
    
/*  Make sure Vector index is in range. */
if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
	{ 
	*err = TERROR("!vectorInsert: Index out of range!");  
	FrameExit(*err);
	}

/*  Resize the Vector up one position */
TVector_SetMaxIndex(gCP, gTP, selfTval, self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the Vector up one position */

sourcePtr = &atHMTval(self->itsTvalArray,(self->itsMaxItemIndex-2));
targetPtr = &atHMTval(self->itsTvalArray,(self->itsMaxItemIndex-1));
insertPtr = &atHMTval(self->itsTvalArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
    
/*  Insert the new value in the Vector at the specified position */
atHMTval(self->itsTvalArray,insertIndex) = newValue;
    
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

TVAL TVector_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TVector*    self = (TVector*)asObject(&selfTval);

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

TVAL TVector_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TVector*    self = (TVector*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a Vector object into an ascii string and append it to an output buffer. 

#endif

TVAL     TVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
NUM                     indexOf;
StartFrame	
DeclareOBJ(TVector,self);
DeclareTVAL(ec);
EndFrame

self = (TVector*)asObject(&selfTval);

 
/*  Quit if the output string is already too long */

if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Vector prefix */

buf[*size]      = '#';
buf[++(*size)]  = '(';
buf[++(*size)]  = 0;

for(indexOf = 0; indexOf < self->itsMaxItemIndex; indexOf++)
    {
    *ec = FConio_sprintn(gCP,gTP,buf,size,atHMTval(self->itsTvalArray,indexOf));
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
TVector_New

Create a new TVector.

#endif

TVector*    TVector_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TVector,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TVector_Initialized) TVector_Init(gCP,gTP);

self = (TVector*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYVECTOR;
self->itsMaxItemIndex = 0;
self->itsCdr = gCP->Tval_VOID;
self->itsTvalArray = NULL;
self->itsAttributes = NULL;
self->itsImmediatePtr = NULL;

FrameExit(self);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Map

Make a copy of *this and call the given proc on each element, storing the result in place.

#endif

TVAL TVector_Map(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc)
{
TVector*    self = (TVector*)asObject(&selfTval);

NUM         index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TVector,copyVector);
EndFrame

/* Make a copy of myself since map returns an object of the same type */
copyVector = (TVector*)TVector_Copy(gCP,gTP,selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    *tmp = atHMTval(copyVector->itsTvalArray,index);
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        atHMTval(copyVector->itsTvalArray,index) = *ret;
    }
asObject(ret) = (TObject*)copyVector;
asTag(ret) = TYVECTOR;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mapc

Loop through itsTvalArray and call the specified function.

#endif

TVAL TVector_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM         index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TVector,self);
EndFrame

self = (TVector*)asObject(&selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    *tmp = atHMTval(self->itsTvalArray,index);
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        break;
    }
FrameExit(*ret);
}
