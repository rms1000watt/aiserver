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

#define _C_TBYTEVEC
#define _SMARTBASE

#if 0
TByteVector.c

Implementation of the Byte vector class which stores a variable number of
bytes in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tbytevec.h"
#include "tlambda.h"


/*--------------------------------------------------------------------------------------- */
#if 0
TByteVector_Init

Initialize the TByteVector class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TByteVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TByteVector_Initialized) return;
gCP->TByteVector_Initialized     = TRUE;

/*  Initialize the new type for this class. */
FSmartbase_NewType  (gCP,
					 gTP,
					TYBYTEVECTOR,
					(LpCHAR)"ByteVector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TByteVector_MakeNew,
					&TByteVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TByteVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TByteVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_Map,
					&TObject_Mapc,
					&TByteVector_Print,
					&TByteVector_Load,
					&TByteVector_Save,
					&TByteVector_ComputeSize,
					&TByteVector_Copy,
					&TByteVector_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TByteVector_Print
 
Print method for TYSTRING.
 
#endif

TVAL TByteVector_Print(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf)
{
NUM                 n;
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

self = (TByteVector*)asObject(&selfTval);

n = strlen(&atHMChar(self->itsByteArray,0));
if(*size + (2 + n) < gCP->TObject_MaxOutputLen)
    {
    sprintf((char*)&buf[*size], "\"%s\"", &atHMChar(self->itsByteArray,0) );
    *size += (2 + n);
    }
FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TByteVector_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TByteVector_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory anHMemory, NUM theFileID, NUM bResolve)
{
NUM					cn;
StartFrame
DeclareOBJ(TByteVector,it);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TByteVector_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TByteVector*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        itTval->u.Object = (TObject*)it;
        itTval->Tag = it->itsObjectType;
        
        TByteVector_SetMaxIndex(gCP, gTP, *itTval, TByteVectorOnDiskPtr(anHMemory,0)->itsMaxItemIndex);
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TByteVectorOnDiskPtr(anHMemory,0)->itsCdr);
                
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
            atHMChar(it->itsByteArray,cn) = TByteVectorOnDiskPtr(anHMemory,0)->itsItemArray[cn];
            }
        
        retTval->Tag = it->itsObjectType;
        retTval->u.Object = (TObject*)it;
        }
    else
        *retTval = gCP->TObject_ERROR_INVALID;
    }

FrameExit(*retTval);
}

/* -------------------------------------------------------------------------------------- */
#if 0
IByteVector

Initialize a TByteVector object with a new tail(cdr) and an array of items.

#endif

void    TByteVector_IByteVector(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

self = (TByteVector*)asObject(&selfTval);

/*  Reshape the Byte vectors's array to be the correct size. */
TByteVector_SetMaxIndex(gCP,gTP,selfTval,argc);

/*  Set the Byte vector's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        atHMChar(self->itsByteArray,indexOf) = asInt(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYREAL)
        {
        atHMChar(self->itsByteArray,indexOf) = asReal(&argv[indexOf]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        atHMChar(self->itsByteArray,indexOf) = asBool(&argv[indexOf]);
        }
    else
        {
        atHMChar(self->itsByteArray,indexOf) = 0;
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

void    TByteVector_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{

StartFrame
DeclareOBJ(TByteVector,self);
EndFrame


self     = (TByteVector*)asObject(&selfTval);

/*  Mark the byte vector's Lisp tail(cdr) so its won't be garbage collected. */

TObject_MarkTval(gCP,gTP,self->itsCdr);
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

void    TByteVector_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TByteVector*    self     = (TByteVector*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsByteArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsByteArray);
self->itsMaxItemIndex = 0;																				 
self->itsByteArray = NULL;
self->itsImmediatePtr = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TByteVector_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

self     = (TByteVector*)asObject(&selfTval);
*aSize = SIZEOF_TObjectOnDisk;

*aSize += SIZEOF_TByteVectorOnDisk + (self->itsMaxItemIndex * sizeof(CHAR));
ALLIGNME(*aSize);
FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TByteVector_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory)
{
NUM                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

self     = (TByteVector*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TByteVectorOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TByteVectorOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);
for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
    TByteVectorOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn] = atHMChar(self->itsByteArray,cn);
    }

FrameExit(aHMemory);
}
/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TByteVector.

#endif

TObject*    TByteVector_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TByteVector,self);
DeclareOBJ(TByteVector,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TByteVector*)selfTval.u.Object;


theCopy = TByteVector_New(gCP,gTP);


tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

TByteVector_SetMaxIndex(gCP,gTP,*tmpTval, self->itsMaxItemIndex);
TByteVector_SetCdr(gCP,gTP,*tmpTval,self->itsCdr);


if (self->itsByteArray != NULL)
    {
    _FMemory_memcpy((LpCHAR)&atHMChar(theCopy->itsByteArray,0),(LpCHAR)&atHMChar(self->itsByteArray,0),(NUM)(self->itsMaxItemIndex*sizeof(CHAR)));
    }

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TByteVector_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

self     = (TByteVector*)asObject(&selfTval);
FrameExit(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   Rember the repeating portion of this object is measured in integers!

#endif

TVAL TByteVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
NUM				n;
NUM				oldMaxItemIndex;
LpCHAR			ptr;
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

self     = (TByteVector*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
oldMaxItemIndex = self->itsMaxItemIndex;

/*  Do not allow a resize for exactly the current size */
if (newRepeats == self->itsMaxItemIndex) FrameExit(gCP->TObject_OK);

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats < (NUM)(_TByteVector_ImmediateSpace+1))
	{
	if (self->itsByteArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(CHAR));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,ByteArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(CHAR));
		if ((self->itsByteArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsByteArray);
		}
	self->itsByteArray = (HMChar)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	ByteArray(selfTval)[newRepeats] = 0;
	}
else
/*  Either create or resize the item array handle. */
/*  Note: We always allocate a hidden byte at the end */
/*        of the byte array in which we place a zero. */
/*        this prevents Byte Vectors, treated as strings */
/*        from overrunning memory. */
if (self->itsByteArray == NULL)
    {
    self->itsByteArray = (HMChar)FMemory_New(gCP, gTP, (NUM)((newRepeats*sizeof(CHAR))+1),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	ByteArray(selfTval)[newRepeats] = 0;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsByteArray = (HMChar)FMemory_New(gCP, gTP, (NUM)((newRepeats*sizeof(CHAR))+1),TRUE);
	_FMemory_memcpy(ByteArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(CHAR));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	ByteArray(selfTval)[newRepeats] = 0;
	}
else
    {
    self->itsByteArray = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)self->itsByteArray,(NUM)((newRepeats*sizeof(CHAR))+1));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	ByteArray(selfTval)[newRepeats] = 0;
    }

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newRepeats)
	{
	ptr = (LpCHAR)&ByteArray(selfTval)[oldMaxItemIndex];
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

TVAL TByteVector_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TByteVector,self);
DeclareTVAL(retValue);
EndFrame


self     = (TByteVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL)) && (strcmp(SymbolArray(index1),"AppendLength") == 0))
	{
    indexOf = asNumIndex(&index1);
    retValue->u.Int = self->itsMemoLength;
    retValue->Tag = TYNUM;
    goto Last;
    }
else
/*  We only accept numeric indices. */
if (isNumIndex(&index1))
	{
    indexOf = asNumIndex(&index1);
    }
else
	{
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    }
    
/*  Make sure array index is in range. */
if (indexOf < 0)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Make sure array index is in range. */
if (indexOf >= self->itsMaxItemIndex)
    FrameExit(gCP->TObject_VOID);

/*  Convert the nth integer item into a tval. */
retValue->u.Char = atHMChar(self->itsByteArray,indexOf);
retValue->Tag = TYCHAR;

Last:
FrameExit(*retValue);

}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TByteVector_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TByteVector,self);
DeclareTVAL(ret);
EndFrame

self     = (TByteVector*)asObject(&selfTval);

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
    TByteVector_SetMaxIndex(gCP,gTP,selfTval,indexOf+1);

/*  Save the tval as the nth integer item. */
if (asTag(&newValue) == TYCHAR)
    {
    atHMChar(self->itsByteArray,indexOf) = asChar(&newValue);
    }
else
if (asTag(&newValue) == TYNUM)
    {
    atHMChar(self->itsByteArray,indexOf) = asInt(&newValue);
    }
else
if (asTag(&newValue) == TYREAL)
    {
    atHMChar(self->itsByteArray,indexOf) = asReal(&newValue);
    }
else
if (asTag(&newValue) == TYBOLE)
    {
    atHMChar(self->itsByteArray,indexOf) = asBool(&newValue);
    }
else
    {
    atHMChar(self->itsByteArray,indexOf) = 0;
    }

asTag(ret) = TYBYTEVECTOR;
asObject(ret) = (TObject*)self;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TByteVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
StartFrame
DeclareOBJ(TByteVector,self);
DeclareTVAL(index);
EndFrame

self     = (TByteVector*)asObject(&selfTval);

asInt(index) =self->itsMaxItemIndex;
asTag(index) = TYNUM;

FrameExit(TByteVector_SetIV1(gCP,gTP,selfTval, *index, newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TByteVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index)
{
register LpCHAR         targetPtr;
register LpCHAR         sourcePtr;
register LpCHAR         haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

self = (TByteVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the array down one position */

sourcePtr = &atHMChar(self->itsByteArray,deleteIndex+1);
targetPtr = &atHMChar(self->itsByteArray,deleteIndex);
haltPtr = &atHMChar(self->itsByteArray,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }

/*  Resize the Vector down one position */
TByteVector_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the haigher values are moved up one position and
        the Vector is resized.

#endif

TVAL TByteVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index,TVAL newValue)
{
register LpCHAR         targetPtr;
register LpCHAR         sourcePtr;
register LpCHAR         insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

self  = (TByteVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((insertIndex < 0) || (insertIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the array up one position */
TByteVector_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the array up one position */

sourcePtr = &atHMChar(self->itsByteArray,(self->itsMaxItemIndex-2));
targetPtr = &atHMChar(self->itsByteArray,(self->itsMaxItemIndex-1));
insertPtr = &atHMChar(self->itsByteArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
        
/*  Save the new value into the integer vector. */
FrameExit(TByteVector_SetIV1(gCP,gTP,selfTval, index,newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetCdr

Return the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TByteVector_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{

TByteVector*    self     = (TByteVector*)asObject(&selfTval);

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

TVAL TByteVector_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TByteVector*    self     = (TByteVector*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TByteVector_MakeUnique

Return a TByteVector containing the specified binary string. A binary string is not
terminated with a null character (as is a K&R ascii string).

#endif

TByteVector*    TByteVector_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString,NUM firstChar,NUM theLen)
{
StartFrame
DeclareOBJ(TByteVector,theVector);
DeclareTVAL(selfTval);
EndFrame  

/*  We create a new TByteVector here. The we */
/*  allocate a handle for the binary string data. */

theVector = TByteVector_New(gCP,gTP);
selfTval->u.ByteVector = theVector;
selfTval->Tag = theVector->itsObjectType;
TByteVector_SetMaxIndex(gCP,gTP,*selfTval,theLen);

/*  Now Stuff the binary string data into the TByteVector object. */

memcpy((char*)ByteArray(*selfTval), (const char*)&aString[firstChar], theLen);
FrameExit(theVector);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TByteVector_New

Create a new TByteVector.

#endif

TByteVector*    TByteVector_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TByteVector,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TByteVector_Initialized) TByteVector_Init(gCP,gTP);

self = (TByteVector*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYBYTEVECTOR;
self->itsMaxItemIndex = 0;
self->itsByteArray = (HMChar)&self->itsImmediatePtr;
self->itsImmediatePtr = &self->itsImmediateSpace[0];
self->itsImmediateSpace[0] = 0;
FrameExit(self);
}


/*--------------------------------------------------------------------------------------- */
#if 0

TByteVector_MakeNew

Return a ByteVector object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Vector with zeros. 
        If too few arguments are specified, fill the Vector with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (new ByteVector 5)             =>      #(0 0 0 0 0) 
        (new ByteVector 5 1)           =>      #(1 1 1 1 1) 
        (new ByteVector 5 1 2 3)       =>      #(1 2 3 1 2) 
        (new ByteVector 5 1 2 3 . 6)   =>      #(1 2 3 1 2 . 6) 
        
#endif

TVAL TByteVector_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     size;
NUM                     vectorIndex;
NUM                     argIndex;
NUM                     cdrIndex;
StartFrame
DeclareTVAL(ivTval);
DeclareTVAL(ret);
DeclareTVAL(err);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareOBJ(TByteVector,cp);
EndFrame
 
asTag(ndx) = TYNUM;
asTag(ret) = TYVOID;
asObj(ret) = NIL;

/*  This is a request to construct a byte vector. */
    
if (argc == 0)
    {
    size = 0;
    }
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[0]);
    if(isERROR(err))
        {
        FrameExit(*err);
        }
    else
        size = asInt(err);
        }
    
if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
cp = TByteVector_New(gCP,gTP);
asObject(ivTval) = (TObject*)cp;
asTag(ivTval) = cp->itsObjectType;
TByteVector_SetMaxIndex(gCP,gTP,*ivTval,size);
    
/*  Initialize the Byte Vector object */
    
asTag(ret)		= TYBYTEVECTOR;
asObject(ret)   = (TObject*)cp;
asTag(fill)     = TYNUM;
asInt(fill)     = 0;
vectorIndex             = 0;
argIndex                = 1;
cdrIndex                = argc;
    
/*  Initialize the vector only if necessary (important time saving). */
if (argc > 1)
    {
    asObject(ivTval) = (TObject*)cp;
    asTag(ivTval) = cp->itsObjectType;
    while (vectorIndex < size)
        {
        if (argIndex >= argc) argIndex = 1;
        if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            argIndex = 1;
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
        FObject_SetCdr(gCP,gTP,(TObject*)cp, argv[cdrIndex]);
        }
    }


FrameExit(*ret);
}
