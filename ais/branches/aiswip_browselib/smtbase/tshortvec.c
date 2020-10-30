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

#define _C_TSTRING 
#define _SMARTBASE
#if 0
TShtVector.c

Implementation of the Short vector class which stores a variable number of
short integers in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tshortvec.h"
#include "tlambda.h"
#include "fconio.h"

/*  Function declarations */

/*--------------------------------------------------------------------------------------- */
#if 0
TShtVector_Init

Initialize the TShtVector class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TShtVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TShtVector_Initialized) return;
gCP->TShtVector_Initialized  = TRUE;

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					TYSHORTVECTOR,
					(LpCHAR)"ShortVector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TShtVector_MakeNew,
					&TShtVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TShtVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TShtVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_Map,
					&TObject_Mapc,
					&TShtVector_Print,
					&TShtVector_Load,
					&TShtVector_Save,
					&TShtVector_ComputeSize,
					&TShtVector_Copy,
					&TShtVector_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TShtVector_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TShtVector_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 cn;
StartFrame
DeclareOBJ(TShtVector,it);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TShtVector_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TShtVector*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asObject(itTval) = (TObject*)it;
        asTag(itTval) = it->itsObjectType;
        TShtVector_SetMaxIndex(gCP,gTP,*itTval,TShtVectorOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TShtVectorOnDiskPtr(aHMemory,0)->itsCdr);
                
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
            atHMShort(it->itsShortArray,cn) = TShtVectorOnDiskPtr(aHMemory,0)->itsItemArray[cn];
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
IShtVector

Initialize a TShtVector object with a new tail(cdr) and an array of items.

#endif

void    TShtVector_IShtVector(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TShtVector,self);
EndFrame

self = (TShtVector*)asObject(&selfTval);

/*  Reshape the bit vectors's array to be the correct size. */
TShtVector_SetMaxIndex(gCP,gTP,selfTval,argc);

/*  Set the bit vector's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        atHMShort(self->itsShortArray,indexOf) = asInt(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYSHORT)
        {
        atHMShort(self->itsShortArray,indexOf) = asShort(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYCHAR)
        {
        atHMShort(self->itsShortArray,indexOf) = asChar(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYREAL)
        {
        atHMShort(self->itsShortArray,indexOf) = asReal(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        atHMShort(self->itsShortArray,indexOf) = asBool(&argv[indexOf]);
        }
    else
        {
        atHMShort(self->itsShortArray,indexOf) = 0;
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

void    TShtVector_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TShtVector* self = (TShtVector*)asObject(&selfTval);

/*  Mark the bit vector's Lisp tail(cdr) so its won't be garbage collected. */

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

void    TShtVector_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TShtVector* self = (TShtVector*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsShortArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsShortArray);
self->itsMaxItemIndex = 0;
self->itsShortArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TShtVector_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
TShtVector* self = (TShtVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

*aSize += SIZEOF_TShtVectorOnDisk + (self->itsMaxItemIndex * sizeof(SHORT));
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TShtVector_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TShtVector,self);
EndFrame

self = (TShtVector*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TShtVectorOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TShtVectorOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);
for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
    TShtVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn] = atHMShort(self->itsShortArray,cn);
    }

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TShtVector.

#endif

TObject*    TShtVector_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TShtVector,self);
DeclareOBJ(TShtVector,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TShtVector*)selfTval.u.Object;

theCopy = TShtVector_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

TShtVector_SetMaxIndex(gCP,gTP,*tmpTval,self->itsMaxItemIndex);
TShtVector_SetCdr(gCP,gTP,*tmpTval,self->itsCdr);


if (self->itsShortArray != NULL)
    {
    _FMemory_memcpy((LpCHAR)&atHMShort(theCopy->itsShortArray,0),(LpCHAR)&atHMShort(self->itsShortArray,0),(LONG)(self->itsMaxItemIndex*sizeof(SHORT)));
    }

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TShtVector_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TShtVector* self = (TShtVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   Rember the repeating portion of this object is measured in integers!

#endif

TVAL TShtVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats)
{
NUM			n;
NUM			oldMaxItemIndex;
LpSHORT		ptr;
StartFrame
DeclareOBJ(TShtVector,self);
EndFrame

self = (TShtVector*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
oldMaxItemIndex = self->itsMaxItemIndex;

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TShtVector_ImmediateSpace/(NUM)sizeof(SHORT)))
	{
	if (self->itsShortArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(SHORT));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)ShortArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(SHORT));
		if ((self->itsShortArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsShortArray);
		}
	self->itsShortArray = (HMShort)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
if (self->itsShortArray == NULL)
    {
    self->itsShortArray = (HMShort)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(SHORT)),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsShortArray = (HMShort)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(SHORT)),TRUE);
	_FMemory_memcpy((char*)ShortArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(SHORT));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsShortArray = (HMShort)FMemory_Resize(gCP, gTP, (HMemory)self->itsShortArray,(LONG)(newRepeats*sizeof(SHORT)));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newRepeats)
	{
	ptr = (LpSHORT)&ShortArray(selfTval)[oldMaxItemIndex];
	for (n = oldMaxItemIndex; n < newRepeats; ++n)
		{
		*(ptr++) = 0;
		}
	}
	
FrameExit(gCP->TObject_OK);

}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TShtVector_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TShtVector,self);
DeclareTVAL(retValue);
EndFrame

self = (TShtVector*)asObject(&selfTval);

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

/*  Convert the nth integer item into a tval. */
asInt(retValue) = atHMShort(self->itsShortArray,indexOf);
asTag(retValue) = TYNUM;
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TShtVector_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TShtVector,self);
DeclareTVAL(retValue);
EndFrame

self = (TShtVector*)asObject(&selfTval);

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
    TShtVector_SetMaxIndex(gCP,gTP,selfTval,indexOf+1);

/*  Save the tval as the nth integer item. */
if (asTag(&newValue) == TYNUM)
    {
    atHMShort(self->itsShortArray,indexOf) = asInt(&newValue);
    }
else
if (asTag(&newValue) == TYSHORT)
    {
    atHMShort(self->itsShortArray,indexOf) = asShort(&newValue);
    }
else
if (asTag(&newValue) == TYREAL)
    {
    atHMShort(self->itsShortArray,indexOf) = asReal(&newValue);
    }
else
if (asTag(&newValue) == TYBOLE)
    {
    atHMShort(self->itsShortArray,indexOf) = asBool(&newValue);
    }
else
if (asTag(&newValue) == TYCHAR)
    {
    atHMShort(self->itsShortArray,indexOf) = asChar(&newValue);
    }
else
    {
    atHMShort(self->itsShortArray,indexOf) = 0;
    }

asTag(retValue) = self->itsObjectType;
asObject(retValue) = (TObject*)self;
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TShtVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue)
{
StartFrame
DeclareOBJ(TShtVector,self);
DeclareTVAL(index);
DeclareTVAL(ret);
EndFrame

self = (TShtVector*)asObject(&selfTval);
asInt(index) = self->itsMaxItemIndex;
asTag(index) = TYNUM;

*ret = TShtVector_SetIV1(gCP,gTP,selfTval, *index, newValue);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TShtVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{
register LpSHORT        targetPtr;
register LpSHORT        sourcePtr;
register LpSHORT        haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TShtVector,self);
EndFrame

self = (TShtVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the array down one position */

sourcePtr = &atHMShort(self->itsShortArray,deleteIndex+1);
targetPtr = &atHMShort(self->itsShortArray,deleteIndex);
haltPtr = &atHMShort(self->itsShortArray,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }
    
/*  Resize the Vector down one position */
TShtVector_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex-1);
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the haigher values are moved up one position and
        the Vector is resized.

#endif

TVAL TShtVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index,TVAL newValue)
{
register LpSHORT        targetPtr;
register LpSHORT        sourcePtr;
register LpSHORT        insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TShtVector,self);
DeclareTVAL(ret);
EndFrame

self = (TShtVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the array up one position */
TShtVector_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the array up one position */

sourcePtr = &atHMShort(self->itsShortArray,(self->itsMaxItemIndex-2));
targetPtr = &atHMShort(self->itsShortArray,(self->itsMaxItemIndex-1));
insertPtr = &atHMShort(self->itsShortArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
        
/*  Save the new value into the integer vector. */
*ret = TShtVector_SetIV1(gCP,gTP,selfTval, index, newValue);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetCdr

Return the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TShtVector_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TShtVector*     self = (TShtVector*)asObject(&selfTval);

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

TVAL TShtVector_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TShtVector*     self = (TShtVector*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TShtVector_New

Create a new TShtVector.

#endif

TShtVector* TShtVector_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TShtVector,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TShtVector_Initialized) TShtVector_Init(gCP,gTP);

self = (TShtVector*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYSHORTVECTOR;
self->itsMaxItemIndex = 0;
self->itsShortArray = NULL;
self->itsImmediatePtr = NULL;
FrameExit(self);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a ShortVector object into an ascii string and append it to an output buffer. 

#endif

TVAL     TShtVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
NUM                     indexOf;
StartFrame	
DeclareOBJ(TShtVector,self);
DeclareTVAL(ec);
DeclareTVAL(item);
EndFrame

self = (TShtVector*)asObject(&selfTval);

/*  Quit if the output string is already too long */

if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Vector prefix */

buf[*size]      = '#';
buf[++(*size)]  = '(';
buf[++(*size)]  = 's';
buf[++(*size)]  = 'h';
buf[++(*size)]  = 'o';
buf[++(*size)]  = 'r';
buf[++(*size)]  = 't';
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

TShtVector_MakeNew

Return a Vector object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Vector with zeros. 
        If too few arguments are specified, fill the Vector with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (new short: 5)             =>      #(short| 0 0 0 0 0) 
        (new short: 5 1)           =>      #(short| 1 1 1 1 1) 
        (new short: 5 1 2 3)       =>      #(short| 1 2 3 1 2) 
        (new short: 5 1 2 3 . 6)   =>      #(short| 1 2 3 1 2 . 6) 
        
#endif

TVAL TShtVector_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     size = 0;
NUM                     sizeIndex = 1;
NUM                     vectorIndex = 0;
NUM                     startIndex = sizeIndex + 1;
NUM                     argIndex = sizeIndex + 1;
NUM                     cdrIndex = argc;
StartFrame
DeclareOBJ(TShtVector,ip);
DeclareTVAL(ret);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareTVAL(ivTval);
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
        size = isNumIndex(&argv[sizeIndex]);
        }
    
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    asTag(ndx) = TYSHORT;

    /*  This is a request to construct an integer vector. */
    
    asTag(ret) = TYVOID;
    asObj(ret) = NIL;

    ip = TShtVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)ip, size);
    
    /*  Initialize the Integer Vector object */
    
    asTag(ret)		= TYSHORTVECTOR;
    asObject(ret)   = (TObject*)ip;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    
    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > sizeIndex)
        {
        asObject(ivTval) = (TObject*)ip;
        asTag(ivTval) = ip->itsObjectType;
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
        
            (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP, *ivTval, *ndx, *fill);
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
            FObject_SetCdr(gCP,gTP,(TObject*)ip, argv[cdrIndex]);
            }
        }
    }


FrameExit(*ret);
}
