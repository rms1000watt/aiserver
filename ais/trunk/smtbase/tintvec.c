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
TIntVector.c

Implementation of the Integer vector class which stores a variable number of
integers in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tintvec.h"
#include "tlambda.h"
#include "fconio.h"

/*  Function declarations */

/*--------------------------------------------------------------------------------------- */
#if 0
TIntVector_Init

Initialize the TIntVector class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TIntVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TIntVector_Initialized) return;
gCP->TIntVector_Initialized  = TRUE;

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					TYINTVECTOR,
					(LpCHAR)"IntVector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TIntVector_MakeNew,
					&TIntVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TIntVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TIntVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_Map,
					&TObject_Mapc,
					&TIntVector_Print,
					&TIntVector_Load,
					&TIntVector_Save,
					&TIntVector_ComputeSize,
					&TIntVector_Copy,
					&TIntVector_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TIntVector_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TIntVector_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 cn;
StartFrame
DeclareOBJ(TIntVector,it);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TIntVector_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TIntVector*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asObject(itTval) = (TObject*)it;
        asTag(itTval) = it->itsObjectType;
        TIntVector_SetMaxIndex(gCP,gTP,*itTval,TIntVectorOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TIntVectorOnDiskPtr(aHMemory,0)->itsCdr);
                
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
            atHMInt(it->itsIntArray,cn) = TIntVectorOnDiskPtr(aHMemory,0)->itsItemArray[cn];
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
IIntVector

Initialize a TIntVector object with a new tail(cdr) and an array of items.

#endif

void    TIntVector_IIntVector(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TIntVector,self);
EndFrame

self = (TIntVector*)asObject(&selfTval);

/*  Reshape the bit vectors's array to be the correct size. */
TIntVector_SetMaxIndex(gCP,gTP,selfTval,argc);

/*  Set the bit vector's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        atHMInt(self->itsIntArray,indexOf) = asInt(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYREAL)
        {
        atHMInt(self->itsIntArray,indexOf) = asReal(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        atHMInt(self->itsIntArray,indexOf) = asBool(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYCHAR)
        {
        atHMInt(self->itsIntArray,indexOf) = asChar(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYSHORT)
        {
        atHMInt(self->itsIntArray,indexOf) = asShort(&argv[indexOf]);
        }
    else
        {
        atHMInt(self->itsIntArray,indexOf) = 0;
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

void    TIntVector_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TIntVector* self = (TIntVector*)asObject(&selfTval);

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

void    TIntVector_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TIntVector* self = (TIntVector*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsIntArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsIntArray);
self->itsMaxItemIndex = 0;
self->itsIntArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TIntVector_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
TIntVector* self = (TIntVector*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

*aSize += SIZEOF_TIntVectorOnDisk + (self->itsMaxItemIndex * sizeof(NUM));
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TIntVector_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TIntVector,self);
EndFrame

self = (TIntVector*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TIntVectorOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TIntVectorOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);
for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
    TIntVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn] = atHMInt(self->itsIntArray,cn);
    }

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TIntVector.

#endif

TObject*    TIntVector_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TIntVector,self);
DeclareOBJ(TIntVector,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TIntVector*)selfTval.u.Object;

theCopy = TIntVector_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

TIntVector_SetMaxIndex(gCP,gTP,*tmpTval,self->itsMaxItemIndex);
TIntVector_SetCdr(gCP,gTP,*tmpTval,self->itsCdr);


if (self->itsIntArray != NULL)
    {
    _FMemory_memcpy((LpCHAR)&atHMInt(theCopy->itsIntArray,0),(LpCHAR)&atHMInt(self->itsIntArray,0),(LONG)(self->itsMaxItemIndex*sizeof(NUM)));
    }

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TIntVector_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TIntVector* self = (TIntVector*)asObject(&selfTval);

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

TVAL TIntVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats)
{
NUM			n;
NUM			oldMaxItemIndex;
LpNUM		ptr;
StartFrame
DeclareOBJ(TIntVector,self);
EndFrame

self = (TIntVector*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
oldMaxItemIndex = self->itsMaxItemIndex;

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TIntVector_ImmediateSpace/(NUM)sizeof(NUM)))
	{
	if (self->itsIntArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(NUM));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)IntArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(NUM));
		if ((self->itsIntArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsIntArray);
		}
	self->itsIntArray = (HMInt)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
if (self->itsIntArray == NULL)
    {
    self->itsIntArray = (HMInt)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(NUM)),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsIntArray = (HMInt)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(NUM)),TRUE);
	_FMemory_memcpy((char*)IntArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(NUM));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsIntArray = (HMInt)FMemory_Resize(gCP, gTP, (HMemory)self->itsIntArray,(LONG)(newRepeats*sizeof(NUM)));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newRepeats)
	{
	ptr = (LpNUM)&IntArray(selfTval)[oldMaxItemIndex];
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

TVAL TIntVector_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TIntVector,self);
DeclareTVAL(retValue);
EndFrame

self = (TIntVector*)asObject(&selfTval);

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
asInt(retValue) = atHMInt(self->itsIntArray,indexOf);
asTag(retValue) = TYNUM;
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TIntVector_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TIntVector,self);
DeclareTVAL(retValue);
EndFrame

self = (TIntVector*)asObject(&selfTval);

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
    TIntVector_SetMaxIndex(gCP,gTP,selfTval,indexOf+1);

/*  Save the tval as the nth integer item. */
if (asTag(&newValue) == TYNUM)
    {
    atHMInt(self->itsIntArray,indexOf) = asInt(&newValue);
    }
else
if (asTag(&newValue) == TYREAL)
    {
    atHMInt(self->itsIntArray,indexOf) = asReal(&newValue);
    }
else
if (asTag(&newValue) == TYBOLE)
    {
    atHMInt(self->itsIntArray,indexOf) = asBool(&newValue);
    }
else
if (asTag(&newValue) == TYCHAR)
    {
    atHMInt(self->itsIntArray,indexOf) = asChar(&newValue);
    }
else
if (asTag(&newValue) == TYSHORT)
    {
    atHMInt(self->itsIntArray,indexOf) = asShort(&newValue);
    }
else
    {
    atHMInt(self->itsIntArray,indexOf) = 0;
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

TVAL TIntVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue)
{
StartFrame
DeclareOBJ(TIntVector,self);
DeclareTVAL(index);
DeclareTVAL(ret);
EndFrame

self = (TIntVector*)asObject(&selfTval);
asInt(index) = self->itsMaxItemIndex;
asTag(index) = TYNUM;

*ret = TIntVector_SetIV1(gCP,gTP,selfTval, *index, newValue);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TIntVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{
register LpNUM          targetPtr;
register LpNUM          sourcePtr;
register LpNUM          haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TIntVector,self);
EndFrame

self = (TIntVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the array down one position */

sourcePtr = &atHMInt(self->itsIntArray,deleteIndex+1);
targetPtr = &atHMInt(self->itsIntArray,deleteIndex);
haltPtr = &atHMInt(self->itsIntArray,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }
    
/*  Resize the Vector down one position */
TIntVector_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex-1);
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the haigher values are moved up one position and
        the Vector is resized.

#endif

TVAL TIntVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index,TVAL newValue)
{
register LpNUM          targetPtr;
register LpNUM          sourcePtr;
register LpNUM          insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TIntVector,self);
DeclareTVAL(ret);
EndFrame

self = (TIntVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the array up one position */
TIntVector_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the array up one position */

sourcePtr = &atHMInt(self->itsIntArray,(self->itsMaxItemIndex-2));
targetPtr = &atHMInt(self->itsIntArray,(self->itsMaxItemIndex-1));
insertPtr = &atHMInt(self->itsIntArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
        
/*  Save the new value into the integer vector. */
*ret = TIntVector_SetIV1(gCP,gTP,selfTval, index, newValue);
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

TVAL TIntVector_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TIntVector*     self = (TIntVector*)asObject(&selfTval);

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

TVAL TIntVector_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TIntVector*     self = (TIntVector*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TIntVector_New

Create a new TIntVector.

#endif

TIntVector* TIntVector_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TIntVector,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TIntVector_Initialized) TIntVector_Init(gCP,gTP);

self = (TIntVector*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYINTVECTOR;
self->itsMaxItemIndex = 0;
self->itsIntArray = NULL;
self->itsImmediatePtr = NULL;
FrameExit(self);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a IntVector object into an ascii string and append it to an output buffer. 

#endif

TVAL     TIntVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
NUM                     indexOf;
StartFrame	
DeclareOBJ(TIntVector,self);
DeclareTVAL(ec);
DeclareTVAL(item);
EndFrame

self = (TIntVector*)asObject(&selfTval);

/*  Quit if the output string is already too long */

if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Vector prefix */

buf[*size]      = '#';
buf[++(*size)]  = '(';
buf[++(*size)]  = 'i';
buf[++(*size)]  = 'n';
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

TIntVector_MakeNew

Return a Vector object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Vector with zeros. 
        If too few arguments are specified, fill the Vector with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (new IntVector 5)             =>      #(0 0 0 0 0) 
        (new IntVector 5 1)           =>      #(1 1 1 1 1) 
        (new IntVector 5 1 2 3)       =>      #(1 2 3 1 2) 
        (new IntVector 5 1 2 3 . 6)   =>      #(1 2 3 1 2 . 6) 
        
#endif

TVAL TIntVector_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     size = 0;
NUM                     sizeIndex = 1;
NUM                     vectorIndex = 0;
NUM                     startIndex = sizeIndex + 1;
NUM                     argIndex = sizeIndex + 1;
NUM                     cdrIndex = argc;
StartFrame
DeclareOBJ(TIntVector,ip);
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
    asTag(ndx) = TYNUM;

    /*  This is a request to construct an integer vector. */
    
    asTag(ret) = TYVOID;
    asObj(ret) = 0;

    ip = TIntVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)ip, size);
    
    /*  Initialize the Integer Vector object */
    
    asTag(ret)		= TYINTVECTOR;
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
