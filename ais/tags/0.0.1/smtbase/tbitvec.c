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
TBitVector.c

Implementation of the Bit vector class which stores a variable number of
bits in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tbitvec.h"
#include "tintvec.h"
#include "tnumvec.h"
#include "tlambda.h"
#include "fproc.h"
#include "fconio.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TBitVector_Init

Initialize the TBitVector class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TBitVector_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{

/*  Don't initialize more than once. */
if (gCP->TBitVector_Initialized) return;
gCP->TBitVector_Initialized  = TRUE;

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					TYBITVECTOR,
					(LpCHAR)"BitVector",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TBitVector_MakeNew,
					&TBitVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TBitVector_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TBitVector_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_Map,
					&TObject_Mapc,
					&TBitVector_Print,
					&TBitVector_Load,
					&TBitVector_Save,
					&TBitVector_ComputeSize,
					&TBitVector_Copy,
					&TBitVector_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TBitVector_Functions_Init

Initialize the TBitVector class functions.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TBitVector_Functions_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ec);
DeclareOBJ(TSymbol,aSymbol);
EndFrame

/*  Don't initialize more than once. */
if (gCP->TBitVector_Functions_Initialized) return;
gCP->TBitVector_Functions_Initialized  = TRUE;

/* Register the BitVector Functions contained in this package */

*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"findBlock",(LpFUNC)&TBitVector_FindBlock);
if (ec->Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"freeBlock",(LpFUNC)&TBitVector_FreeBlock);
if (ec->Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"setBlock",(LpFUNC)&TBitVector_SetBlock);
if (ec->Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"bitToNumberVector",(LpFUNC)&TBitVector_BitToNumberVector);
if (ec->Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"bitToIntegerVector",(LpFUNC)&TBitVector_BitToIntegerVector);
if (ec->Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

FrameReset;
}

/*--------------------------------------------------------------------------------------- */
#if 0
TBitVector_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TBitVector_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory anHMemory, NUM theFileID, NUM bResolve)
{
NUM					cn;
StartFrame
DeclareOBJ(TBitVector,it);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TBitVector_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TBitVector*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        itTval->u.Object = (TObject*)it;
        itTval->Tag = it->itsObjectType;
        
        TBitVector_SetMaxIndex(gCP,gTP,*itTval, TBitVectorOnDiskPtr(anHMemory,0)->itsMaxItemIndex);
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TBitVectorOnDiskPtr(anHMemory,0)->itsCdr);
                
        for(cn = 0;cn < ((it->itsMaxItemIndex+7)/8); cn++)
            {
            atHMChar(it->itsBitArray,cn) = TBitVectorOnDiskPtr(anHMemory,0)->itsItemArray[cn];
            }
        
        retTval->Tag = it->itsObjectType;
        retTval->u.Object = (TObject*)it;
        }
    else
        *retTval = gCP->TObject_ERROR_INVALID;
    }

FrameExit(*retTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
IBitVector

Initialize a TBitVector object with a new tail(cdr) and an array of items.

#endif

void    TBitVector_IBitVector(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TBitVector,self); 
EndFrame

self = (TBitVector*)asObject(&selfTval);

/*  Reshape the bit vectors's array to be the correct size. */
TBitVector_SetMaxIndex(gCP,gTP,selfTval,argc);

/*  Set the bit vector's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    if ((asTag(&argv[indexOf]) == TYNUM) && (asInt(&argv[indexOf]) == 1))
        {
        atHMChar(self->itsBitArray,(NUM)(indexOf/8)) |= gCP->TBitVector_OrMasks[(indexOf%8)];
        }
    else
    if ((asTag(&argv[indexOf]) == TYREAL) && (asReal(&argv[indexOf]) == 1))
        {
        atHMChar(self->itsBitArray,(NUM)(indexOf/8)) |= gCP->TBitVector_OrMasks[(indexOf%8)];
        }
    else
    if ((asTag(&argv[indexOf]) == TYBOLE) && (asBool(&argv[indexOf]) == TRUE))
        {
        atHMChar(self->itsBitArray,(NUM)(indexOf/8)) |= gCP->TBitVector_OrMasks[(indexOf%8)];
        }
    else
        {
        atHMChar(self->itsBitArray,(NUM)(indexOf/8)) &= gCP->TBitVector_AndMasks[(indexOf%8)];
        }
    }

/*  Set the bit vector's tail(cdr). */
self->itsCdr = newCdr;
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

void    TBitVector_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TBitVector* self = (TBitVector*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsBitArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsBitArray);
self->itsMaxItemIndex = 0;
self->itsBitArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of Handle required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TBitVector_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
StartFrame
DeclareOBJ(TBitVector,self);
EndFrame

self = (TBitVector*)asObject(&selfTval);
*aSize = SIZEOF_TObjectOnDisk;

*aSize += SIZEOF_TBitVectorOnDisk + (NUM)((self->itsMaxItemIndex+7)/8);
ALLIGNME(*aSize);

FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TBitVector_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory anHMemory)
{
long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TBitVector,self);
EndFrame

self = (TBitVector*)asObject(&selfTval);

TObjectOnDiskPtr(anHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TBitVectorOnDiskPtr(anHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TBitVectorOnDiskPtr(anHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);

for(cn = 0;cn < ((self->itsMaxItemIndex+7)/8); cn++)
    {
    TBitVectorOnDiskPtr(anHMemory,theOffset)->itsItemArray[cn] = atHMChar(self->itsBitArray,cn);
    }

FrameExit(anHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TBitVector.

#endif

TObject*    TBitVector_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{

StartFrame
DeclareOBJ(TBitVector,self);
DeclareOBJ(TBitVector,theCopy);
DeclareTVAL(tmpTval);
EndFrame
self = (TBitVector*)selfTval.u.Object;



theCopy = TBitVector_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

TBitVector_SetMaxIndex(gCP,gTP,*tmpTval, self->itsMaxItemIndex);
TBitVector_SetCdr(gCP,gTP,*tmpTval,self->itsCdr);

if (self->itsBitArray != NULL)
    {
    _FMemory_memcpy((LpCHAR)&atHMChar(theCopy->itsBitArray,0),
                        (LpCHAR)&atHMChar(self->itsBitArray,0),
                        (LONG)((self->itsMaxItemIndex+7)/8));
    }

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TBitVector_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TBitVector,self);
EndFrame


self =  (TBitVector*)asObject(&selfTval);

FrameExit(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   Remember the repeating portion of this object is measured in bits!

#endif

TVAL TBitVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats)
{
StartFrame
DeclareOBJ(TBitVector,self);
EndFrame

self     =  (TBitVector*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	return(gCP->TObject_ERROR_BADIDXORKEY);

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((((newRepeats+7)/8)) <= (NUM)_TBitVector_ImmediateSpace)
	{
	if (self->itsBitArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,(((newRepeats+7)/8)));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,BitArray(selfTval),((min(newRepeats,self->itsMaxItemIndex)+7)/8));
		if ((self->itsBitArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsBitArray);
		}
	self->itsBitArray = (HMChar)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
if (self->itsBitArray == NULL)
    {
    self->itsBitArray = (HMChar)FMemory_New(gCP, gTP, (LONG)(((newRepeats+7)/8)),TRUE);
    self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsBitArray = (HMChar)FMemory_New(gCP, gTP,(LONG)(((newRepeats+7)/8)),TRUE);
	_FMemory_memcpy(BitArray(selfTval),self->itsImmediateSpace,((min(newRepeats,self->itsMaxItemIndex)+7)/8));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsBitArray = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)self->itsBitArray,(LONG)(((newRepeats+7)/8)));
    self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }

FrameExit(gCP->TObject_OK);

}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TBitVector_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TBitVector,self);
DeclareTVAL(retValue);
EndFrame

self = (TBitVector*)asObject(&selfTval);

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

/*  Convert the nth bit item into a tval. */
asInt(retValue) = ((atHMChar(self->itsBitArray,(NUM)(indexOf/8)) & gCP->TBitVector_OrMasks[(indexOf%8)]) != 0);
asTag(retValue) = TYNUM;
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TBitVector_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TBitVector,self);
DeclareTVAL(ret);
EndFrame

self  =  (TBitVector*)asObject(&selfTval);

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
    TBitVector_SetMaxIndex(gCP,gTP,selfTval,indexOf+1);

/*  Save the tval as the nth bit item. */
if ((asTag(&newValue) == TYNUM) && (asInt(&newValue) == 1))
    {
    atHMChar(self->itsBitArray,(NUM)(indexOf/8)) |= gCP->TBitVector_OrMasks[(indexOf%8)];
    }
else
if ((asTag(&newValue) == TYREAL) && (asReal(&newValue) == 1))
    {
    atHMChar(self->itsBitArray,(NUM)(indexOf/8)) |= gCP->TBitVector_OrMasks[(indexOf%8)];
    }
else
if ((asTag(&newValue) == TYBOLE) && (asBool(&newValue) == TRUE))
    {
    atHMChar(self->itsBitArray,(NUM)(indexOf/8)) |= gCP->TBitVector_OrMasks[(indexOf%8)];
    }
else
    {
    atHMChar(self->itsBitArray,(NUM)(indexOf/8)) &= gCP->TBitVector_AndMasks[(indexOf%8)];
    }
asTag(ret)		= TYBITVECTOR;
asObject(ret)   = (TObject*)self;


FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TBitVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue)
{
StartFrame
DeclareOBJ(TBitVector,self);
DeclareTVAL(index);
EndFrame

self     =  (TBitVector*)asObject(&selfTval);

asInt(index) = self->itsMaxItemIndex;
asTag(index) = TYNUM;

FrameExit(TBitVector_SetIV1(gCP,gTP,selfTval,*index, newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TBitVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index)
{
NUM             indexOf;
NUM             deleteIndex;
NUM             tmpBit;
StartFrame
DeclareOBJ(TBitVector,self);
EndFrame

self     =  (TBitVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the array down one position */
for (indexOf = deleteIndex+1; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    tmpBit = atHMChar(self->itsBitArray,(NUM)(indexOf/8)) &= gCP->TBitVector_OrMasks[(indexOf%8)];
    if (tmpBit == 1)
        {
        atHMChar(self->itsBitArray,(NUM)((indexOf-1)/8)) |= gCP->TBitVector_OrMasks[((indexOf-1)%8)];
        }
    else
        {
        atHMChar(self->itsBitArray,(NUM)((indexOf-1)/8)) &= gCP->TBitVector_AndMasks[((indexOf-1)%8)];
        }
    }
    
/*  Resize the Vector down one position */
TBitVector_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the higher values are moved up one position and
        the Vector is resized.

#endif

TVAL TBitVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue)
{
NUM             indexOf;
NUM             insertIndex;
NUM             tmpBit;
StartFrame
DeclareOBJ(TBitVector,self);
EndFrame

self     =  (TBitVector*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure array index is in range. */
if ((insertIndex < 0) || (insertIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the array up one position */
TBitVector_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the array up one position */
for (indexOf = (self->itsMaxItemIndex-1); indexOf >= insertIndex; --indexOf)
    {
    tmpBit = atHMChar(self->itsBitArray,(NUM)(indexOf/8)) &= gCP->TBitVector_OrMasks[(indexOf%8)];
    if (tmpBit == 1)
        {
        atHMChar(self->itsBitArray,(NUM)((indexOf+1)/8)) |= gCP->TBitVector_OrMasks[((indexOf+1)%8)];
        }
    else
        {
        atHMChar(self->itsBitArray,(NUM)((indexOf+1)/8)) &= gCP->TBitVector_AndMasks[((indexOf+1)%8)];
        }
    }
        
/*  Save the new value into the bit vector. */
FrameExit(TBitVector_SetIV1(gCP,gTP,selfTval,index,newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TBitVector_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TBitVector,self);
EndFrame


self =  (TBitVector*)asObject(&selfTval);

/*  Mark the number vector's Lisp tail(cdr) so its won't be garbage collected. */

TObject_MarkTval(gCP,gTP,self->itsCdr);
FrameReturn;

}

/*--------------------------------------------------------------------------------------- */
#if 0
GetCdr

Return the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TBitVector_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TBitVector*     self     =  (TBitVector*)asObject(&selfTval);

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

TVAL TBitVector_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TBitVector*     self     =  (TBitVector*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TBitVector_New

Create a new TBitVector.

#endif

TBitVector* TBitVector_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TBitVector,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TBitVector_Initialized) TBitVector_Init(gCP,gTP);

self = (TBitVector*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYBITVECTOR;
self->itsMaxItemIndex = 0;
self->itsBitArray = NULL;
self->itsImmediatePtr = NULL;
FrameExit(self);
}

/*--------------------------------------------------------------------------------------- */
#if 0

TBitVector_MakeNew

Return a BitVector object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Vector with zeros. 
        If too few arguments are specified, fill the Vector with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (new BitVector 5)           =>      #(0 0 0 0 0) 
        (new BitVector 5 1)         =>      #(1 1 1 1 1) 
        (new BitVector 5 1 0 1)     =>      #(1 0 1 1 0) 
        (new BitVector 5 1 1 0 . 1)   =>    #(1 1 0 1 1 . 1) 
        
#endif

TVAL TBitVector_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     size;
NUM                     vectorIndex;
NUM                     argIndex;
NUM                     cdrIndex;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareOBJ(TBitVector,bp);
EndFrame
 
asTag(ndx) = TYNUM;

/*  This is a request to construct a bit vector. */
    
ret->Tag		= TYVOID;
ret->u.Object = NULL;

if (argc == 0)
    {
    size = 0;
    }
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[0]);
    if (isERROR(err))
        {
        FrameExit(*err);
        }
    else
        {
        size = asInt(err);
        }
    }
    
if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
bp = TBitVector_New(gCP,gTP);
ret->u.Object		= (TObject*)bp;
ret->Tag			= bp->itsObjectType;
TBitVector_SetMaxIndex(gCP,gTP,*ret, size);
    
/*  Initialize the Bit Vector object */
    
asTag(fill)     = TYNUM;
asInt(fill)     = 0;
vectorIndex             = 0;
argIndex                = 1;
cdrIndex                = argc;
    
/*  Initialize the vector only if necessary (important time saving). */
if (argc > 1)
    {
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
        
        (*_TObject_TypeSetIV1(ret->Tag))(gCP,gTP, *ret, *ndx, *fill);
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
        FObject_SetCdr(gCP,gTP,(TObject*)bp, argv[cdrIndex]);
        }
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
/*
FindBlock

Return the bit index value for the first available block of the specified length. Vacant
blocks are represented by 0 bits, while occupied blocks are represented by 1 bits. 

For example:

	(setq myBlock (findBlock bitVector 3))

The above findBlock invocation returns the bit index of the first vacant block (which is
immediately followed by two other vacant blocks). If there do not exist three vacant blocks
immediately adjacent anywhere within the bit vector, then false is returned.

Note:	This functions uses the cdr of the BitVector to store the last location found.
		The last location found, is pulled from the BitVector's cdr as a memoed hint
		showing where to start looking the next time findBlock is called.

*/

/* Here follows the table of largest consecutive bit runs */
/* for each of the 256 bit values of a byte. */

typedef struct {NUM largestRun;		/* Largest run of vacant blocks in byte */
                NUM startIndex;		/* Starting bit index of largest run within byte */
                NUM begRunSize;		/* Largest starting run of vacant blocks at first of byte */
                NUM endRunSize;		/* Largest trailing run of vacant blocks at end of byte */
               } runElement;

static runElement runTable[256] = {
								 {8,  0,  8,  8},		/* 0000 0000  */
								 {7,  0,	 7,  0},		/* 0000 0001  */
								 {6,  0,	 6,  1},		/* 0000 0010  */
								 {6,  0,	 6,  0},		/* 0000 0011  */
								 {5,  0,	 5,  2},		/* 0000 0100  */
								 {5,  0,	 5,  0},		/* 0000 0101  */
								 {5,  0,	 5,  1},		/* 0000 0110  */
								 {5,  0,	 5,  0},		/* 0000 0111  */

								 {4,  0,	 4,  3},		/* 0000 1000  */
								 {4,  0,	 4,  0},		/* 0000 1001  */
								 {4,  0,	 4,  1},		/* 0000 1010  */
								 {4,  0,	 4,  0},		/* 0000 1011  */
								 {4,  0,	 4,  2},		/* 0000 1100  */
								 {4,  0,	 4,  0},		/* 0000 1101  */
								 {4,  0,	 4,  1},		/* 0000 1110  */
								 {4,  0,	 4,  0},		/* 0000 1111  */

								 {4,  4,	 3,  4},		/* 0001 0000  */
								 {3,  0,	 3,  0},		/* 0001 0001  */
								 {3,  0,	 3,  1},		/* 0001 0010  */
								 {3,  0,	 3,  0},		/* 0001 0011  */
								 {3,  0,	 3,  2},		/* 0001 0100  */
								 {3,  0,	 3,  0},		/* 0001 0101  */
								 {3,  0,	 3,  1},		/* 0001 0110  */
								 {3,  0,	 3,  0},		/* 0001 0111  */

								 {3,  0,	 3,  3},		/* 0001 1000  */
								 {3,  0,	 3,  0},		/* 0001 1001  */
								 {3,  0,	 3,  1},		/* 0001 1010  */
								 {3,  0,	 3,  0},		/* 0001 1011  */
								 {3,  0,	 3,  2},		/* 0001 1100  */
								 {3,  0,	 3,  0},		/* 0001 1101  */
								 {3,  0,	 3,  1},		/* 0001 1110  */
								 {3,  0,	 3,  0},		/* 0001 1111  */

								 {5,  3,	 2,  5},		/* 0010 0000  */
								 {4,  3,	 2,  0},		/* 0010 0001  */
								 {3,  3,	 2,  1},		/* 0010 0010  */
								 {3,  3,	 2,  0},		/* 0010 0011  */
								 {2,  0,	 2,  2},		/* 0010 0100  */
								 {2,  0,	 2,  0},		/* 0010 0101  */
								 {2,  0,	 2,  1},		/* 0010 0110  */
								 {2,  0,	 2,  0},		/* 0010 0111  */

								 {3,  5,	 2,  3},		/* 0010 1000  */
								 {2,  0,	 2,  0},		/* 0010 1001  */
								 {2,  0,	 2,  1},		/* 0010 1010  */
								 {2,  0,	 2,  0},		/* 0010 1011  */
								 {2,  0,	 2,  2},		/* 0010 1100  */
								 {2,  0,	 2,  0},		/* 0010 1101  */
								 {2,  0,	 2,  1},		/* 0010 1110  */
								 {2,  0,	 2,  0},		/* 0010 1111  */

								 {4,  4,	 2,  4},		/* 0011 0000  */
								 {3,  4,	 2,  0},		/* 0011 0001  */
								 {2,  0,	 2,  1},		/* 0011 0010  */
								 {2,  0,	 2,  0},		/* 0011 0011  */
								 {2,  0,	 2,  2},		/* 0011 0100  */
								 {2,  0,	 2,  0},		/* 0011 0101  */
								 {2,  0,	 2,  1},		/* 0011 0110  */
								 {2,  0,	 2,  0},		/* 0011 0111  */

								 {3,  5,	 2,  3},		/* 0011 1000  */
								 {2,  0,	 2,  0},		/* 0011 1001  */
								 {2,  0,	 2,  1},		/* 0011 1010  */
								 {2,  0,	 2,  0},		/* 0011 1011  */
								 {2,  0,	 2,  2},		/* 0011 1100  */
								 {2,  0,	 2,  0},		/* 0011 1101  */
								 {2,  0,	 2,  1},		/* 0011 1110  */
								 {2,  0,	 2,  0},		/* 0011 1111  */

								 {6,  2,	 1,  6},		/* 0100 0000  */
								 {5,  2,	 1,  0},		/* 0100 0001  */
								 {4,  2,	 1,  1},		/* 0100 0010  */
								 {4,  2,	 1,  0},		/* 0100 0011  */
								 {3,  2,	 1,  2},		/* 0100 0100  */
								 {3,  2,	 1,  0},		/* 0100 0101  */
								 {3,  2,	 1,  1},		/* 0100 0110  */
								 {3,  2,	 1,  0},		/* 0100 0111  */

								 {3,  5,	 1,  3},		/* 0100 1000  */
								 {2,  2,	 1,  0},		/* 0100 1001  */
								 {2,  2,	 1,  1},		/* 0100 1010  */
								 {2,  2,	 1,  0},		/* 0100 1011  */
								 {2,  2,	 1,  2},		/* 0100 1100  */
								 {2,  2,	 1,  0},		/* 0100 1101  */
								 {2,  2,	 1,  1},		/* 0100 1110  */
								 {2,  2,	 1,  0},		/* 0100 1111  */

								 {4,  4,	 1,  4},		/* 0101 0000  */
								 {3,  4,	 1,  0},		/* 0101 0001  */
								 {2,  4,	 1,  1},		/* 0101 0010  */
								 {2,  4,	 1,  0},		/* 0101 0011  */
								 {2,  6,	 1,  2},		/* 0101 0100  */
								 {1,  0,	 1,  0},		/* 0101 0101  */
								 {1,  0,	 1,  1},		/* 0101 0110  */
								 {1,  0,	 1,  0},		/* 0101 0111  */

								 {3,  5,	 1,  3},		/* 0101 1000  */
								 {2,  5,	 1,  0},		/* 0101 1001  */
								 {1,  0,	 1,  1},		/* 0101 1010  */
								 {1,  0,	 1,  0},		/* 0101 1011  */
								 {2,  6,	 1,  2},		/* 0101 1100  */
								 {1,  0,	 1,  0},		/* 0101 1101  */
								 {1,  0,	 1,  1},		/* 0101 1110  */
								 {1,  0,	 1,  0},		/* 0101 1111  */

								 {5,  3,	 1,  5},		/* 0110 0000  */
								 {4,  3,	 1,  0},		/* 0110 0001  */
								 {3,  3,	 1,  1},		/* 0110 0010  */
								 {3,  3,	 1,  0},		/* 0110 0011  */
								 {2,  3,	 1,  2},		/* 0110 0100  */
								 {2,  3,	 1,  0},		/* 0110 0101  */
								 {2,  3,	 1,  1},		/* 0110 0110  */
								 {2,  3,	 1,  0},		/* 0110 0111  */

								 {3,  5,	 1,  3},		/* 0110 1000  */
								 {2,  5,	 1,  0},		/* 0110 1001  */
								 {1,  0,	 1,  1},		/* 0110 1010  */
								 {1,  0,	 1,  0},		/* 0110 1011  */
								 {2,  6,	 1,  2},		/* 0110 1100  */
								 {1,  0,	 1,  0},		/* 0110 1101  */
								 {1,  0,	 1,  1},		/* 0110 1110  */
								 {1,  0,	 1,  0},		/* 0110 1111  */

								 {4,  4,	 1,  4},		/* 0111 0000  */
								 {3,  4,	 1,  0},		/* 0111 0001  */
								 {2,  4,	 1,  1},		/* 0111 0010  */
								 {2,  4,	 1,  0},		/* 0111 0011  */
								 {2,  6,	 1,  2},		/* 0111 0100  */
								 {1,  0,	 1,  0},		/* 0111 0101  */
								 {1,  0,	 1,  1},		/* 0111 0110  */
								 {1,  0,	 1,  0},		/* 0111 0111  */

								 {3,  5,	 1,  3},		/* 0111 1000  */
								 {2,  5,	 1,  0},		/* 0111 1001  */
								 {1,  0,	 1,  1},		/* 0111 1010  */
								 {1,  0,	 1,  0},		/* 0111 1011  */
								 {2,  6,	 1,  2},		/* 0111 1100  */
								 {1,  0,	 1,  0},		/* 0111 1101  */
								 {1,  0,	 1,  1},		/* 0111 1110  */
								 {1,  0,	 1,  0},		/* 0111 1111  */

								 {7,  1,  0,	 7},		/* 1000 0000  */
								 {6,  1,  0,	 0},		/* 1000 0001  */
								 {5,  1,  0,	 1},		/* 1000 0010  */
								 {5,  1,  0,	 0},		/* 1000 0011  */
								 {4,  1,  0,	 2},		/* 1000 0100  */
								 {4,  1,  0,	 0},		/* 1000 0101  */
								 {4,  1,  0,	 1},		/* 1000 0110  */
								 {4,  1,  0,	 0},		/* 1000 0111  */

								 {3,  1,  0,	 3},		/* 1000 1000  */
								 {3,  1,  0,	 0},		/* 1000 1001  */
								 {3,  1,  0,	 1},		/* 1000 1010  */
								 {3,  1,  0,	 0},		/* 1000 1011  */
								 {3,  1,  0,	 2},		/* 1000 1100  */
								 {3,  1,  0,	 0},		/* 1000 1101  */
								 {3,  1,  0,	 1},		/* 1000 1110  */
								 {3,  1,  0,	 0},		/* 1000 1111  */

								 {4,  4,  0,	 4},		/* 1001 0000  */
								 {3,  4,  0,	 0},		/* 1001 0001  */
								 {2,  1,  0,	 1},		/* 1001 0010  */
								 {2,  1,  0,	 0},		/* 1001 0011  */
								 {2,  1,  0,	 2},		/* 1001 0100  */
								 {2,  1,  0,	 0},		/* 1001 0101  */
								 {2,  1,  0,	 1},		/* 1001 0110  */
								 {2,  1,  0,	 0},		/* 1001 0111  */

								 {3,  5,  0,	 3},		/* 1001 1000  */
								 {2,  1,  0,	 0},		/* 1001 1001  */
								 {2,  1,  0,	 1},		/* 1001 1010  */
								 {2,  1,  0,  0},		/* 1001 1011  */
								 {2,  1,  0,  2},		/* 1001 1100  */
								 {2,  1,  0,  0},		/* 1001 1101  */
								 {2,  1,  0,  1},		/* 1001 1110  */
								 {2,  1,  0,  0},		/* 1001 1111  */

								 {5,  3,  0,  5},		/* 1010 0000  */
								 {4,  3,  0,  0},		/* 1010 0001  */
								 {3,  3,  0,  1},		/* 1010 0010  */
								 {3,  3,  0,  0},		/* 1010 0011  */
								 {2,  3,  0,  2},		/* 1010 0100  */
								 {2,  3,  0,  0},		/* 1010 0101  */
								 {2,  3,  0,  1},		/* 1010 0110  */
								 {2,  3,  0,  0},		/* 1010 0111  */

								 {3,  5,  0,  3},		/* 1010 1000  */
								 {2,  5,  0,  0},		/* 1010 1001  */
								 {1,  1,  0,  1},		/* 1010 1010  */
								 {1,  1,  0,  0},		/* 1010 1011  */
								 {2,  6,  0,  2},		/* 1010 1100  */
								 {1,  1,  0,  0},		/* 1010 1101  */
								 {1,  1,  0,  1},		/* 1010 1110  */
								 {1,  1,  0,  0},		/* 1010 1111  */

								 {4,  4,  0,  4},		/* 1011 0000  */
								 {3,  4,  0,  0},		/* 1011 0001  */
								 {2,  4,  0,  1},		/* 1011 0010  */
								 {2,  4,  0,  0},		/* 1011 0011  */
								 {2,  6,  0,  2},		/* 1011 0100  */
								 {1,  1,  0,  0},		/* 1011 0101  */
								 {1,  1,  0,  1},		/* 1011 0110  */
								 {1,  1,  0,  0},		/* 1011 0111  */

								 {3,  5,  0,  3},		/* 1011 1000  */
								 {2,  5,  0,  0},		/* 1011 1001  */
								 {1,  1,  0,  1},		/* 1011 1010  */
								 {1,  1,  0,  0},		/* 1011 1011  */
								 {2,  6,  0,  2},		/* 1011 1100  */
								 {1,  1,  0,  0},		/* 1011 1101  */
								 {1,  1,  0,  1},		/* 1011 1110  */
								 {1,  1,  0,  0},		/* 1011 1111  */

								 {6,  2,  0,  6},		/* 1100 0000  */
								 {5,  2,  0,  0},		/* 1100 0001  */
								 {4,  2,  0,  1},		/* 1100 0010  */
								 {4,  2,  0,  0},		/* 1100 0011  */
								 {3,  2,  0,  2},		/* 1100 0100  */
								 {3,  2,  0,  0},		/* 1100 0101  */
								 {3,  2,  0,  1},		/* 1100 0110  */
								 {3,  2,  0,  0},		/* 1100 0111  */

								 {3,  5,  0,  3},		/* 1100 1000  */
								 {2,  2,  0,  0},		/* 1100 1001  */
								 {2,  2,  0,  1},		/* 1100 1010  */
								 {2,  2,  0,  0},		/* 1100 1011  */
								 {2,  2,  0,  2},		/* 1100 1100  */
								 {2,  2,  0,  0},		/* 1100 1101  */
								 {2,  2,  0,  1},		/* 1100 1110  */
								 {2,  2,  0,  0},		/* 1100 1111  */

								 {4,  4,  0,  4},		/* 1101 0000  */
								 {3,  4,  0,  0},		/* 1101 0001  */
								 {2,  4,  0,  1},		/* 1101 0010  */
								 {2,  4,  0,  0},		/* 1101 0011  */
								 {2,  6,  0,  2},		/* 1101 0100  */
								 {1,  2,  0,  0},		/* 1101 0101  */
								 {1,  2,  0,  1},		/* 1101 0110  */
								 {1,  2,  0,  0},		/* 1101 0111  */

								 {3,  5,  0,  3},		/* 1101 1000  */
								 {2,  5,  0,  0},		/* 1101 1001  */
								 {1,  2,  0,  1},		/* 1101 1010  */
								 {1,  2,  0,  0},		/* 1101 1011  */
								 {2,  6,  0,  2},		/* 1101 1100  */
								 {1,  2,  0,  0},		/* 1101 1101  */
								 {1,  2,  0,  1},		/* 1101 1110  */
								 {1,  2,  0,  0},		/* 1101 1111  */

								 {5,  3,  0,  5},		/* 1110 0000  */
								 {4,  3,  0,  0},		/* 1110 0001  */
								 {3,  3,  0,  1},		/* 1110 0010  */
								 {3,  3,  0,  0},		/* 1110 0011  */
								 {2,  3,  0,  2},		/* 1110 0100  */
								 {2,  3,  0,  0},		/* 1110 0101  */
								 {2,  3,  0,  1},		/* 1110 0110  */
								 {2,  3,  0,  0},		/* 1110 0111  */

								 {3,  5,  0,  3},		/* 1110 1000  */
								 {2,  5,  0,  0},		/* 1110 1001  */
								 {1,  3,  0,  1},		/* 1110 1010  */
								 {1,  3,  0,  0},		/* 1110 1011  */
								 {2,  6,  0,  2},		/* 1110 1100  */
								 {1,  3,  0,  0},		/* 1110 1101  */
								 {1,  3,  0,  1},		/* 1110 1110  */
								 {1,  3,  0,  0},		/* 1110 1111  */

								 {4,  4,  0,  4},		/* 1111 0000  */
								 {3,  4,  0,  0},		/* 1111 0001  */
								 {2,  4,  0,  1},		/* 1111 0010  */
								 {2,  4,  0,  0},		/* 1111 0011  */
								 {2,  6,  0,  2},		/* 1111 0100  */
								 {1,  4,  0,  0},		/* 1111 0101  */
								 {1,  4,  0,  1},		/* 1111 0110  */
								 {1,  4,  0,  0},		/* 1111 0111  */

								 {3,  5,  0,  3},		/* 1111 1000  */
								 {2,  5,  0,  0},		/* 1111 1001  */
								 {1,  5,  0,  1},		/* 1111 1010  */
								 {1,  5,  0,  0},		/* 1111 1011  */
								 {2,  6,  0,  2},		/* 1111 1100  */
								 {1,  6,  0,  0},		/* 1111 1101  */
								 {1,  7,  0,  1},		/* 1111 1110  */
								 {0, -1,  0,  0},		/* 1111 1111  */

							   }; /* end firstRun table */

TVAL TBitVector_FindBlock(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
/* Register allocations for speed */
register	runElement*		re;
register	unsigned char*	bitArrayPtr;
register	NUM				foundBitAt = -1;
register	NUM             byteIndex = 0;
register	NUM				bitIndex = 0;
register	NUM             vectorByteLen;

/* Automatic variables normally allocated */
NUM             vectorBitLen;
NUM             requestedSize;
NUM				remainingSize;
NUM				memoStartByte;
StartFrame
DeclareOBJ(TBitVector,self);
DeclareTVAL(retValue);
DeclareTVAL(err);
EndFrame

/* We accept two and only two arguments. */
if (argc != 2)
	{
	*err = TERROR("!findBlock: Invalid Argument Count!");
    FrameExit(*err);
	}

/* The first argument must be a Bit Vector. */
if (argv[0].Tag != TYBITVECTOR)
	{
	*err = TERROR("!findBlock: Arg 1 must be a Bit Vector!");
    FrameExit(*err);
	}

/* The second argument must be Numeric. */
if (!isNumIndex(&argv[1]))
	{
	*err = TERROR("!findBlock: Arg 2 must be Numeric!");
    FrameExit(*err);
	}


/* Convert the arguments from TVAL's to immediate C values. */
self = (TBitVector*)asObject(&argv[0]);
requestedSize = asNumIndex(&argv[1]);
vectorBitLen = self->itsMaxItemIndex;
vectorByteLen = vectorBitLen / 8;
if ((vectorBitLen%8) != 0) ++vectorByteLen;
bitArrayPtr = (unsigned char*)BitArray(argv[0]);

/* Retrieve the memoed start bit (if any). */
if ((self->itsCdr.Tag == TYNUM) && (self->itsCdr.u.Int >= 0) && (self->itsCdr.u.Int < vectorByteLen))
		{
		memoStartByte = self->itsCdr.u.Int;
		}
	else
		{
		memoStartByte = 0;
		}

/* Search the bit array for 'N' consecutive vacant blocks. */
/* Note:  C will not allow us to address each bit, so we use */
/*        a series of byte masks and a state algorithm to */
/*        get at the individual bits in each byte of the bit array. */
/* Note2: Here we search from the memoed byte location to the end */
/*        of the bit vector. This speeds up the search because */
/*        we usually find empty spaces at the memoed locations. */
remainingSize = requestedSize;
for (byteIndex = memoStartByte, bitIndex = (byteIndex * 8); byteIndex < vectorByteLen; ++byteIndex, bitIndex += 8)
	{
	/* We use the next eight bits in the bit array */
	/* to address the proper run table element. */
	re = &runTable[bitArrayPtr[byteIndex]];

	/* Are we looking for the first vacant block? */
	if (foundBitAt < 0)
		/* We are looking for the first vacant block. */
		{
		/* Have we found a run large enough? */
		HighFirstRun:
		if (re->largestRun >= requestedSize)
			/* We found a run large enough. */
			{
			foundBitAt = bitIndex + re->startIndex; 

			/*  Check to make sure we didn't go over bit length. */
			if ((foundBitAt + requestedSize) >= vectorBitLen) 
				goto NaiveRun;
			else
				goto Found;
			}

		/* Do we have an end run to use as a beginning? */
		if (re->endRunSize > 0)
			/* We have an end run large enough to use as a beginning. */
			{
			foundBitAt = bitIndex + 8 - re->endRunSize; 
			remainingSize = requestedSize - re->endRunSize; 
			}
		}
	else
	/* We have already found the first vacant block. */
	/* We are looking for consecutive vacant blocks. */
		{
		/* Have we found a consecutive run large enough? */
		if (re->begRunSize >= remainingSize)
			/* We found a consecutive run large enough. */
			{
			/*  Check to make sure we didn't go over bit length. */
			if ((foundBitAt + requestedSize) >= vectorBitLen) 
				goto NaiveRun;
			else
				goto Found;
			}

		/* Do we have a completely vacant eight blocks? */
		if (re->begRunSize == 8)
			/* We have a completely vacant eight blocks. */
			{
			remainingSize -= re->begRunSize; 
			}
		else
			/* We do not have a consecutive run large enough. */
			{
			foundBitAt = -1; 
			remainingSize = requestedSize;
			goto HighFirstRun;
			}
		}
	} /* end of byte loop */


/* Search the bit array for 'N' consecutive vacant blocks. */
/* Note:  C will not allow us to address each bit, so we use */
/*        a series of byte masks and a state algorithm to */
/*        get at the individual bits in each byte of the bit array. */
/* Note2: Here we search from the zero location to the memoed */
/*        location because we did not find a vacant block  */
/*        sequence at the memoed locations. */
NaiveRun:
memoStartByte += ((requestedSize /8 ) + 1);
if (vectorByteLen < memoStartByte) memoStartByte = vectorByteLen;
remainingSize = requestedSize;
for (byteIndex = 0, bitIndex = 0; byteIndex < memoStartByte; ++byteIndex, bitIndex += 8)
	{
	/* We use the next eight bits in the bit array */
	/* to address the proper run table element. */
	re = &runTable[bitArrayPtr[byteIndex]];

	/* Are we looking for the first vacant block? */
	if (foundBitAt < 0)
		/* We are looking for the first vacant block. */
		{
		/* Have we found a run large enough? */
		LowFirstRun:
		if (re->largestRun >= requestedSize)
			/* We found a run large enough. */
			{
			foundBitAt = bitIndex + re->startIndex; 
			goto Found;
			}

		/* Do we have an end run to use as a beginning? */
		if (re->endRunSize > 0)
			/* We have an end run large enough to use as a beginning. */
			{
			foundBitAt = bitIndex + 8 - re->endRunSize; 
			remainingSize = requestedSize - re->endRunSize; 
			}
		}
	else
	/* We have already found the first vacant block. */
	/* We are looking for consecutive vacant blocks. */
		{
		/* Have we found a consecutive run large enough? */
		if (re->begRunSize >= remainingSize)
			/* We found a consecutive run large enough. */
			{
			goto Found;
			}

		/* Do we have a completely vacant eight blocks? */
		if (re->begRunSize == 8)
			/* We have a completely vacant eight blocks. */
			{
			remainingSize -= re->begRunSize; 
			}
		else
			/* We do not have a consecutive run large enough. */
			{
			foundBitAt = -1; 
			remainingSize = requestedSize;
			goto LowFirstRun;
			}
		}
	} /* end of byte loop */


/*  If we cannot find the requested vacant blocks, return false. */
NotFound:
retValue->u.Bool = FALSE;
retValue->Tag = TYBOLE;
FrameExit(*retValue);


/*  We have found a consecutive block of the requested size. */
Found:

/*  Check to make sure we didn't go over bit length. */
if ((foundBitAt + requestedSize) >= vectorBitLen) goto NotFound;

/*  Mark the found block and its immediate neighbors as occupied. */
for (remainingSize = 0; remainingSize < requestedSize; ++remainingSize)
	{
	bitIndex = foundBitAt + remainingSize;
	byteIndex = bitIndex/8;
	bitArrayPtr[byteIndex] |= gCP->TBitVector_OrMasks[(bitIndex%8)];
	} /* end set bits occupied loop */

/*  Memo the current byte index in the BitVector cdr for the next search. */

self->itsCdr = TINT(foundBitAt/8);

/*  Convert the bit index of the available block into a tval. */
retValue->u.Int = foundBitAt;
retValue->Tag = TYNUM;
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FreeBlock

Sets the specified blocks to vacant. Vacant blocks are represented by 0 bits, 
while occupied blocks are represented by 1 bits. 

For example:

	(freeBlock bitVector i 3)

The above freeBlock invocation sets the 3 blocks (starting at bit index i) to be vacant.
After this invocation:
				
				bitVector[i] = 0
				bitVector[(+ i 1)] = 0
				bitVector[(+ i 2)] = 0

#endif

TVAL TBitVector_FreeBlock(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
/* Register allocations for speed */
register	unsigned char*	bitArrayPtr;
register	NUM				foundBitAt = -1;
register	NUM             byteIndex = 0;
register	NUM				bitIndex = 0;

/* Automatic variables normally allocated */
NUM             vectorBitLen;
NUM             requestedSize;
NUM				remainingSize;
StartFrame
DeclareOBJ(TBitVector,self);
DeclareTVAL(retValue);
DeclareTVAL(err);
EndFrame

/* We accept three and only three arguments. */
if (argc != 3)
	{
	*err = TERROR("!findBlock: Invalid Argument Count!");
    FrameExit(*err);
	}

/* The first argument must be a Bit Vector. */
if (argv[0].Tag != TYBITVECTOR)
	{
	*err = TERROR("!findBlock: Arg 1 must be a Bit Vector!");
    FrameExit(*err);
	}

/* The second argument must be Numeric. */
if (!isNumIndex(&argv[1]))
	{
	*err = TERROR("!findBlock: Arg 2 must be Numeric!");
    FrameExit(*err);
	}

/* The third argument must be Numeric. */
if (!isNumIndex(&argv[2]))
	{
	*err = TERROR("!findBlock: Arg 3 must be Numeric!");
    FrameExit(*err);
	}

/* Convert the arguments from TVAL's to immediate C values. */
self = (TBitVector*)asObject(&argv[0]);
foundBitAt = asNumIndex(&argv[1]);
requestedSize = asNumIndex(&argv[2]);
vectorBitLen = self->itsMaxItemIndex;
bitArrayPtr = (unsigned char*)BitArray(argv[0]);

/* The second and third arguments must not be out of range. */
if ((foundBitAt < 0) || ((foundBitAt + requestedSize) >= vectorBitLen))
	{
	*err = TERROR("!findBlock: indices out of range!");
    FrameExit(*err);
	}

/*  Mark the found block and its immediate neighbors as vacant. */
for (remainingSize = 0; remainingSize < requestedSize; ++remainingSize)
	{
	bitIndex = foundBitAt + remainingSize;
	byteIndex = bitIndex/8;
	bitArrayPtr[byteIndex] &= gCP->TBitVector_AndMasks[(bitIndex%8)];
	} /* end set bits vacant loop */

/*  Convert the bit index of the available block into a tval. */
retValue->u.Int = foundBitAt + requestedSize;
retValue->Tag = TYNUM;
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetBlock

Sets the specified blocks to occupied. Occupied blocks are represented by 1 bits, 
while vacant blocks are represented by 0 bits. 

For example:

	(setBlock bitVector i 3)

The above freeBlock invocation sets the 3 blocks (starting at bit index i) to be occupied.
After this invocation:
				
				bitVector[i] = 1
				bitVector[(+ i 1)] = 1
				bitVector[(+ i 2)] = 1

#endif

TVAL TBitVector_SetBlock(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
/* Register allocations for speed */
register	unsigned char*	bitArrayPtr;
register	NUM				foundBitAt = -1;
register	NUM             byteIndex = 0;
register	NUM				bitIndex = 0;

/* Automatic variables normally allocated */
NUM             vectorBitLen;
NUM             requestedSize;
NUM				remainingSize;
StartFrame
DeclareOBJ(TBitVector,self);
DeclareTVAL(retValue);
DeclareTVAL(err);
EndFrame

/* We accept three and only three arguments. */
if (argc != 3)
	{
	*err = TERROR("!findBlock: Invalid Argument Count!");
    FrameExit(*err);
	}

/* The first argument must be a Bit Vector. */
if (argv[0].Tag != TYBITVECTOR)
	{
	*err = TERROR("!findBlock: Arg 1 must be a Bit Vector!");
    FrameExit(*err);
	}

/* The second argument must be Numeric. */
if (!isNumIndex(&argv[1]))
	{
	*err = TERROR("!findBlock: Arg 2 must be Numeric!");
    FrameExit(*err);
	}

/* The third argument must be Numeric. */
if (!isNumIndex(&argv[2]))
	{
	*err = TERROR("!findBlock: Arg 3 must be Numeric!");
    FrameExit(*err);
	}

/* Convert the arguments from TVAL's to immediate C values. */
self = (TBitVector*)asObject(&argv[0]);
foundBitAt = asNumIndex(&argv[1]);
requestedSize = asNumIndex(&argv[2]);
vectorBitLen = self->itsMaxItemIndex;
bitArrayPtr = (unsigned char*)BitArray(argv[0]);

/* The second and third arguments must not be out of range. */
if ((foundBitAt < 0) || ((foundBitAt + requestedSize) >= vectorBitLen))
	{
	*err = TERROR("!findBlock: indices out of range!");
    FrameExit(*err);
	}

/*  Mark the found block and its immediate neighbors as occupied. */
for (remainingSize = 0; remainingSize < requestedSize; ++remainingSize)
	{
	bitIndex = foundBitAt + remainingSize;
	byteIndex = bitIndex/8;
	bitArrayPtr[byteIndex] |= gCP->TBitVector_OrMasks[(bitIndex%8)];
	} /* end set bits vacant loop */

/*  Convert the bit index of the available block into a tval. */
retValue->u.Int = foundBitAt + requestedSize;
retValue->Tag = TYNUM;
FrameExit(*retValue);
}



/*--------------------------------------------------------------------------------------- */
#if 0
BitToNumberVector

The  bitToNumberVector  Function converts a Bit Vector into a Number Vector. 
The Bit vector is treated as a series of 48-bit groups. Each 48-bit group is converted 
into a Number as follows: (a) the first 8 bits become the signed exponent, and 
(b) the trailing 40 bits become the signed mantissa.
 
Only Bit Vectors, which are an exact multiple of 48 bits in length, can be converted. 
If a Bit Vector is input, which is not an even multiple of 48 bits in length, 
an error message is returned; otherwise the Bit Vector is converted into a Number Vector 
containing one number for every 48 bits in the input Bit vector. 

For instance, passing a Bit Vector of length 144 will return a Number Vector of length 3; 
while, passing a Bit Vector of length 100 will return an error message.


Syntax:	(bitToNumberVector  bitVector)
        (bitToNumberVector  bitVector  numVector)


Example:

	(setq b (new Vector: bit: 48 1 0 0 0 0 0 1 0 0))
	(setq v (new Vector: number: 1))
	(setq b[47] 1)	  
	(bitToNumberVector b v)  	 Returns 	v[0] ==  .25
	(setq b[8] 1)	  
	(bitToNumberVector b v)  	 Returns 	v[0] ==  -.25

Hint:

	When a Bit Vector has been evolved, using a genetic algorithm, as a genome, 
	the bitToNumberVector function is an efficient way to convert the Bit Vector 
	genome into a Number Vector for direct use in solving the target problem.


#endif

TVAL TBitVector_BitToNumberVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
/* Register allocations for speed */
register	REAL			mantissa;
register	REAL            exponent;
register	REAL*			numArrayPtr;
register	POINTER			bitArrayPtr;

/* Automatic variables normally allocated */
NUM				bitGroupLen = sizeof(signed char) * 6 * 8;
NUM             bitVectorLen;
NUM             numVectorLen;
NUM             oldNumVectorLen;
NUM				n;
TVAL			parmv[2];
StartFrame
DeclareTVAL(bitVector);
DeclareTVAL(numVector);
DeclareTVAL(numLength);
DeclareTVAL(err);
EndFrame

/* We accept only one argument. */
if ((argc < 1) || (argc > 2))
	{
	*err = TERROR("!bitToNumberVector: Invalid Argument Count!");
    FrameExit(*err);
	}
*bitVector = argv[0];
bitArrayPtr = (POINTER)FSmartbase_VectorPtr(gCP,gTP,*bitVector);

/* The first argument must be a Bit Vector. */
if (bitVector->Tag != TYBITVECTOR)
	{
	*err = TERROR("!bitToNumberVector: Arg 1 must be a Bit Vector!");
    FrameExit(*err);
	}

/* The second argument must be a Number Vector (if any). */
if ((argc == 2) && (argv[1].Tag != TYNUMVECTOR))
	{
	*err = TERROR("!bitToNumberVector: Arg 2 must be a Number Vector!");
    FrameExit(*err);
	}

/* The first argument must be an exact multiple of 48 bits in length. */
bitVectorLen = FSmartbase_VectorLen(gCP,gTP,*bitVector);
if ((bitVectorLen % bitGroupLen) != 0)
	{
	*err = TERROR("!bitToNumberVector: Arg 1 must be an even multiple of 48 bits in length!");
    FrameExit(*err);
	}

/* Create the result Number Vector (if no second argument). */
if (argc == 1) 
	{
	numVectorLen = bitVectorLen / bitGroupLen;
	*numLength = TINT(numVectorLen);
	parmv[0] = gCP->Tval_VOID;
	parmv[1] = TINT(numVectorLen);
	*numVector = TNumVector_MakeNew(gCP,gTP,2,&parmv[0]); 
	ExitOnError(*numVector);
	}
else
if (argc == 2) 
	{
	*numVector = argv[1];
	}
else
	{
	*err = TERROR("!bitToNumberVector: Invalid Argument Count!");
    FrameExit(*err);
	}

/* Make sure Number Vector is of the proper length. */

oldNumVectorLen = FSmartbase_VectorLen(gCP,gTP,*numVector);
numVectorLen = bitVectorLen / bitGroupLen;
if (oldNumVectorLen != numVectorLen)
	{
	*err = TNumVector_SetMaxIndex(gCP,gTP,*numVector,numVectorLen);
	ExitOnError(*err);
	}
numArrayPtr = (REAL*)FSmartbase_VectorPtr(gCP,gTP,*numVector);

/* Convert the Bit Vector contents into Numbers. */

for (n = 0; n < numVectorLen; ++n)
	{
	exponent = *((signed char*)(bitArrayPtr++));
	mantissa = *((signed char*)(bitArrayPtr++)) * 256.0 * 256.0 * 256.0 * 256.0;
	mantissa += *((unsigned char*)(bitArrayPtr++)) * 256.0 * 256.0 * 256.0;
	mantissa += *((unsigned char*)(bitArrayPtr++)) * 256.0 * 256.0;
	mantissa += *((unsigned char*)(bitArrayPtr++)) * 256.0;
	mantissa += *((unsigned char*)(bitArrayPtr++));
	numArrayPtr[n] = mantissa * pow(10.0,exponent);
	}

/*  Return the converted Number Vector. */
FrameExit(*numVector);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a BitVector object into an ascii string and append it to an output buffer. 

#endif

TVAL     TBitVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
NUM                     indexOf;
StartFrame	
DeclareOBJ(TBitVector,self);
DeclareTVAL(ec);
DeclareTVAL(item);
EndFrame

self = (TBitVector*)asObject(&selfTval);

/*  Quit if the output string is already too long */

if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Vector prefix */

buf[*size]      = '#';
buf[++(*size)]  = '(';
buf[++(*size)]  = 'b';
buf[++(*size)]  = 'i';
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
BitToIntegerVector

The  bitToIntegerVector  Function converts a Bit Vector into an Integer Vector. 
The Bit vector is treated as a series of 32-bit groups. Each 32-bit group is converted 
into an Integer as follows: (a) the first bit becomes the sign, and 
(b) the trailing 31 bits become the magnitude.
 
Only Bit Vectors, which are an exact multiple of 32 bits in length, can be converted. 
If a Bit Vector is input, which is not an even multiple of 32 bits in length, 
an error message is returned; otherwise the Bit Vector is converted into an Integer Vector 
containing one number for every 32 bits in the input Bit vector. 

For instance, passing a Bit Vector of length 96 will return an Integer Vector of length 3; 
while, passing a Bit Vector of length 100 will return an error message.


Syntax:	(bitToIntegerVector  bitVector)
        (bitToIntegerVector  bitVector  intVector)


Example:

	(setq b (new Vector: bit: 32 1 0 0 0 0 0 1 0 0))
	(setq v (new Vector: number: 1))
	(setq b[31] 1)	  
	(bitToIntegerVector b v)  	 Returns 	v[0] ==  1
	(setq b[30] 1)	  
	(bitToNumberVector b v)  	 Returns 	v[0] ==  2

Hint:

	When a Bit Vector has been evolved, using a genetic algorithm, as a genome, 
	the bitToIntegerVector function is an efficient way to convert the Bit Vector 
	genome into an Integer Vector for direct use in solving the target problem.


#endif

TVAL TBitVector_BitToIntegerVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
/* Register allocations for speed */
register	NUM				magnitude;
register	LpNUM			numArrayPtr;
register	LpCHAR			bitArrayPtr;

/* Automatic variables normally allocated */
NUM				bitGroupLen = sizeof(NUM) * 8;
NUM             bitVectorLen;
NUM             numVectorLen;
NUM             oldNumVectorLen;
NUM				n;
TVAL			parmv[2];
StartFrame
DeclareTVAL(bitVector);
DeclareTVAL(numVector);
DeclareTVAL(numLength);
DeclareTVAL(err);
EndFrame

/* We accept only one argument. */
if ((argc < 1) || (argc > 2))
	{
	*err = TERROR("!bitToIntegerVector: Invalid Argument Count!");
    FrameExit(*err);
	}
*bitVector = argv[0];
bitArrayPtr = (LpCHAR)FSmartbase_VectorPtr(gCP,gTP,*bitVector);

/* The first argument must be a Bit Vector. */
if (bitVector->Tag != TYBITVECTOR)
	{
	*err = TERROR("!bitToIntegerVector: Arg 1 must be a Bit Vector!");
    FrameExit(*err);
	}

/* The second argument must be an Integer Vector (if any). */
if ((argc == 2) && (argv[1].Tag != TYINTVECTOR))
	{
	*err = TERROR("!bitToIntegerVector: Arg 2 must be an Integer Vector!");
    FrameExit(*err);
	}

/* The first argument must be an exact multiple of 32 bits in length. */
bitVectorLen = FSmartbase_VectorLen(gCP,gTP,*bitVector);
if ((bitVectorLen % bitGroupLen) != 0)
	{
	*err = TERROR("!bitToIntegerVector: Arg 1 must be an even multiple of 32 bits in length!");
    FrameExit(*err);
	}

/* Create the result Integer Vector (if no second argument). */
if (argc == 1) 
	{
	numVectorLen = bitVectorLen / bitGroupLen;
	*numLength = TINT(numVectorLen);
	parmv[0] = gCP->Tval_VOID;
	parmv[1] = TINT(numVectorLen);
	*numVector = TIntVector_MakeNew(gCP,gTP,2,&parmv[0]); 
	ExitOnError(*numVector);
	}
else
if (argc == 2) 
	{
	*numVector = argv[1];
	}
else
	{
	*err = TERROR("!bitToIntegerVector: Invalid Argument Count!");
    FrameExit(*err);
	}

/* Make sure Number Vector is of the proper length. */

oldNumVectorLen = FSmartbase_VectorLen(gCP,gTP,*numVector);
numVectorLen = bitVectorLen / bitGroupLen;
if (oldNumVectorLen != numVectorLen)
	{
	*err = TIntVector_SetMaxIndex(gCP,gTP,*numVector,numVectorLen);
	ExitOnError(*err);
	}
numArrayPtr = (LpNUM)FSmartbase_VectorPtr(gCP,gTP,*numVector);

/* Convert the Bit Vector contents into Numbers. */

for (n = 0; n < numVectorLen; ++n)
	{
	magnitude = *((signed char*)(bitArrayPtr++)) * 256.0 * 256.0 * 256.0;
	magnitude += *((unsigned char*)(bitArrayPtr++)) * 256.0 * 256.0;
	magnitude += *((unsigned char*)(bitArrayPtr++)) * 256.0;
	magnitude += *((unsigned char*)(bitArrayPtr++));
	numArrayPtr[n] = magnitude;
	}

/*  Return the converted Integer Vector. */
FrameExit(*numVector);
}
