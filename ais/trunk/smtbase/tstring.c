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
TString.c 

Implementation of the string class which stores a K&R C string in a variable length
object.

Note:   Duplicate TString objects containing the same K&R C string are eliminated
        by the TString constructors. Therefore, TString object may be compared for
        equality on the basis of the object handles alone. 

PARENT:             TObject 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "tstring.h"
#include "tobjvec.h"
#include "tsymbol.h"
#include "tstruct.h"
#include "fobject.h"
#include "fconvert.h"
#include "fmake.h"

#undef      USEBSEARCH


/*--------------------------------------------------------------------------------------- */
#if 0
TString_Init

Initialize the TString class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TString_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */

if (gCP->TString_Initialized) return;
gCP->TString_Initialized     = TRUE;
TObject_Init(gCP,gTP);

/*  Initialize the new type for this class. */

FSmartbase_NewType (gCP,
					gTP,
					TYSTRING,
					(LpCHAR)"String",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&FMake_String,
					&TObject_MarkNever,
					&TObject_GlobalMarkNever,
					&TString_StringAnyCnv,
					&TString_StringAnyCmp,
					&TString_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TString_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TString_Map, 
					&TString_Mapc,
					&TString_Print,
					&TString_Load,
					&TString_Save,
					&TString_ComputeSize,
					&TString_Copy,
					&TString_Doomed);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYSTRINGSUBSTR,
                    (LpCHAR)"Substring",
                    _TObject_TfIOBJECT,
                    sizeof(TVAL),
                    (LpFNEW)&FMake_SubstringT,
                    &TObject_MarkTval,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &TStringSubstringT_SubstringAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &TStringSubstringT_GetIV1,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_Map,
                    &TStringSubstringT_Mapc,
                    &TStringSubstringT_Print,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

/*  Override the previous tval conversion function with one for this class. */

TObject_NewConvert(gCP,gTP,TYTVAL,&TString_TvalAnyCnv);

/*  Create the NIL string which is available globally. */

gCP->TString_NIL = TString_MakeUnique(gCP,gTP,(LpCHAR)"");
FObject_Perm(gCP, gTP, (TObject*)gCP->TString_NIL, TRUE);

TSymbol_Init(gCP,gTP);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_MakeUnique

Return a unique TString containing the specified K&R C string. If a TString object
already exists with the same contents, return the existing TString object.
        
#endif

TString*    TString_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString)
{
HMChar                  theData;
NUM                     lengthOf;
StartFrame
DeclareOBJ(TString,theString);
EndFrame              

/*  We create a new TString here, then we */
/*  allocate a handle for the K&R C string data. */

theString = TString_New(gCP,gTP);
lengthOf = strlen((char*)aString);

/*  Can we stuff the K&R C string data into the object header? */

if (lengthOf < (NUM)(_TString_ImmediateSpace-1))
	{
	theString->itsCString = (HMChar)&theString->itsImmediatePtr;
	theString->itsImmediatePtr = (CHAR*)&theString->itsImmediateSpace[0];
	theString->itsMaxItemIndex = lengthOf+1;
	strcpy((char*)theString->itsImmediatePtr,(const char*)aString);
	FrameExit(theString);
	}

/*  Now Stuff the K&R C string data into the TString object. */

theData = (HMChar)FMemory_New(gCP, gTP, lengthOf+1,TRUE);
strcpy((char*)&atHMChar(theData,0),(const char*)aString);
theString->itsMaxItemIndex = lengthOf+1;
theString->itsCString = theData;
theString->itsImmediatePtr = NULL;

FrameExit(theString);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_SubString_MakeUnique

Return a unique TString containing the specified K&R C string. If a TString object
already exists with the same contents, return the existing TString object.

#endif

TString*    TString_SubString_MakeUnique(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR aString, NUM firstChar, NUM theLen)
{
NUM                     lengthOf;
HMChar                  theData;
StartFrame
DeclareOBJ(TString,theString);
EndFrame  

/*  We create a new TString here, then we */
/*  allocate a handle for the K&R C string data. */

theString = TString_New(gCP,gTP);
lengthOf = theLen;

/*  Can we stuff the K&R C string data into the object header? */

if (lengthOf < (NUM)(_TString_ImmediateSpace-1))
	{
	theString->itsCString = (HMChar)&theString->itsImmediatePtr;
	theString->itsImmediatePtr = (CHAR*)&theString->itsImmediateSpace[0];
	strncpy((char*)theString->itsImmediatePtr, (const char*)&aString[firstChar], lengthOf);
	theString->itsMaxItemIndex = lengthOf+1;
	theString->itsImmediateSpace[lengthOf] = 0;
	FrameExit(theString);
	}

/*  Now Stuff the K&R C string data into the TString object. */

theData = (HMChar)FMemory_New(gCP, gTP, lengthOf+1,TRUE);
strncpy((char*)&atHMChar(theData,0), (const char*)&aString[firstChar], lengthOf);
atHMChar(theData,lengthOf) = 0;
theString->itsMaxItemIndex = lengthOf+1;
theString->itsCString = theData;
theString->itsImmediatePtr = NULL;

FrameExit(theString);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_Doomed

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

void    TString_Doomed(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
TString*    self = (TString*)asObject(&selfTval);

if (self->itsImmediatePtr != NULL)
	{
	self->itsCString = NULL;
	self->itsImmediatePtr = NULL;
	self->itsMaxItemIndex = 0;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsCString);
self->itsCString = NULL;
self->itsImmediatePtr = NULL;
self->itsMaxItemIndex = 0;
return;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TString_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
TString*    self = (TString*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

/*  Round to an even boundary so that all structure pointers align */

*aSize += SIZEOF_TStringOnDisk + self->itsMaxItemIndex;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_Save

The specified OODBMS manager is about to save this object. Convert yourself to a disk
format append yourself to the handle and return.

#endif

HMemory TString_Save(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
long            theOffset;
StartFrame
DeclareOBJ(TString,self);
EndFrame

self = (TString*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

/*  Save aStringOnDisk */

TStringOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
strcpy((char*)TStringOnDiskPtr(aHMemory,theOffset)->itsStringData, (const char*)&atHMChar(self->itsCString,0));

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TString_Load(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 lengthOf;
LpCHAR              pStr;
HMChar              theData;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TString,it);
EndFrame

it = NULL;
*ret = gCP->TObject_VOID;

if(bResolve == 0)
    {
    it = TString_New(gCP,gTP);
    *ret = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TString*)TObject_CheckRegistration(gCP,gTP,theFileID);
    pStr = (LpCHAR)TStringOnDiskPtr(aHMemory,0)->itsStringData;
    lengthOf = strlen((char*)pStr);

	/*  Can we stuff the K&R C string data into the object header? */

	if (lengthOf < (NUM)(_TString_ImmediateSpace -1))
		{
		it->itsCString = (HMChar)&it->itsImmediatePtr;
		it->itsImmediatePtr = (CHAR*)&it->itsImmediateSpace[0];
		it->itsMaxItemIndex = lengthOf+1;
		strcpy((char*)it->itsImmediatePtr,(const char*)pStr);
		if(it != NULL)
			{
			asTag(ret) = (CHAR)it->itsObjectType;
			asObject(ret) = (TObject*)it;
			}
		else
			*ret = gCP->TObject_ERROR_INVALID;
		FrameExit(*ret);
		}

    /*  Now Stuff the K&R C string data into the TString object. */
    
    theData = (HMChar)FMemory_New(gCP, gTP, lengthOf+1,TRUE);
    strcpy((char*)&atHMChar(theData,0),(const char*)pStr);
    it->itsMaxItemIndex = lengthOf+1;
    it->itsCString = theData;
	it->itsImmediatePtr = NULL;
    
    if(it != NULL)
        {
        asTag(ret) = (CHAR)it->itsObjectType;
        asObject(ret) = (TObject*)it;
        }
    else
        *ret = gCP->TObject_ERROR_INVALID;
    }
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TString.

#endif

TObject*    TString_Copy(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TString,self);
DeclareOBJ(TString,theCopy);
EndFrame

self = (TString*)selfTval.u.Object;

theCopy = TString_New(gCP,gTP);

FObject_SetMaxIndex(gCP,gTP,(TObject*)theCopy,self->itsMaxItemIndex);

if (self->itsCString != NULL)
    {
    _FMemory_memcpy(&atHMChar(theCopy->itsCString,0),&atHMChar(self->itsCString,0),(LONG)(self->itsMaxItemIndex*sizeof(CHAR)));
    }

FrameExit((TObject*)theCopy);

}

/*--------------------------------------------------------------------------------------- */
#if 0
GetStringPtr

Return a string pointer representing the ascii version of the data
contained in this object.

#endif

LpCHAR TString_GetStringPtr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TString*        self = (TString*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(&atHMChar(self->itsCString,0));
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetNumericValue

If possible return a tval containing the numeric representation of the data contained 
in this object.

#endif

TVAL TString_GetNumericValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TString,self);
DeclareTVAL(result);
DeclareTVAL(ErrCode);
EndFrame

self = (TString*)asObject(&selfTval);

*ErrCode = TObject_ator(gCP,gTP,&asReal(result),&atHMChar(self->itsCString,0));
_TObject_ErrorChk(*ErrCode);

asTag(result) = TYREAL;
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TString_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TString*        self = (TString*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   A TString object has a maximum length which INCLUDES the null terminator.
 
#endif

TVAL TString_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats)
{
StartFrame
DeclareOBJ(TString,self);
EndFrame

self = (TString*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats < (NUM)_TString_ImmediateSpace)
	{
	if (self->itsCString == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(CHAR));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,CharArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(CHAR));
		if ((self->itsCString != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsCString);
		}
	self->itsCString = (HMChar)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	CharArray(selfTval)[newRepeats] = 0;
	}
else
/*  Either create or resize the item array handle. */
/*  Note: We always allocate a hidden byte at the end */
/*        of the byte array in which we place a zero. */
/*        this prevents Byte Vectors, treated as strings */
/*        from overrunning memory. */
if (self->itsCString == NULL)
    {
    self->itsCString = (HMChar)FMemory_New(gCP, gTP, (LONG)((newRepeats*sizeof(CHAR))+1),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	CharArray(selfTval)[newRepeats] = 0;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsCString = (HMChar)FMemory_New(gCP, gTP, (LONG)((newRepeats*sizeof(CHAR))+1),TRUE);
	_FMemory_memcpy(CharArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(CHAR));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	CharArray(selfTval)[newRepeats] = 0;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsCString = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)self->itsCString,(LONG)((newRepeats*sizeof(CHAR))+1));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	CharArray(selfTval)[newRepeats] = 0;
    }
/*  Can we stuff the K&R C string data into the object header? */

if (self->itsImmediatePtr != NULL)
	{
	if (newRepeats < (NUM)(_TString_ImmediateSpace-1))
		{
		self->itsImmediateSpace[newRepeats] = 0;
		self->itsMaxItemIndex = newRepeats;
		}
	else
		{
		self->itsCString = (HMChar)FMemory_New(gCP, gTP, (LONG)((newRepeats*sizeof(CHAR))+1),TRUE);
		self->itsMaxItemIndex = newRepeats;
		strcpy((char*)&atHMChar(self->itsCString,0),(char*)self->itsImmediatePtr);
		self->itsImmediatePtr = NULL;
		}

	FrameExit(gCP->TObject_OK);
	}

/*  Either create or resize the item array handle. */
/*  Note: We always allocate a hidden byte at the end */
/*        of the char array in which we place a zero. */
/*        this prevents strings from overrunning memory. */
if (self->itsCString == NULL)
    {
    self->itsCString = (HMChar)FMemory_New(gCP, gTP, (LONG)((newRepeats*sizeof(CHAR))+1),TRUE);
    self->itsMaxItemIndex = newRepeats;
    }
else
    {
    self->itsCString = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)self->itsCString,(LONG)((newRepeats*sizeof(CHAR))+1));
    self->itsMaxItemIndex = newRepeats;
    }

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TString_GetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TString,self);
DeclareTVAL(value);
EndFrame

self = (TString*)asObject(&selfTval);

/*  We only accept numeric indices. */

if(isNumIndex(&index1))
    indexOf = (NUM)asNumIndex(&index1);
else
    indexOf = self->itsMaxItemIndex;
    
/*  Make sure string index is in range. */
if (indexOf < 0)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Make sure array index is in range. */
if (indexOf >= (self->itsMaxItemIndex - 1))
    FrameExit(gCP->TObject_VOID);

asTag(value)  = TYCHAR;
asChar(value) = atHMChar(self->itsCString,indexOf);
FrameExit(*value);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_SetIV1

Set the indexed value in the repeating portion of this object.
This method is only meaningful when the requirement for unique strings is relaxed.

Note:   

#endif

TVAL TString_SetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
CHAR            aChar;
CHAR*           aTextPtr;
NUM             indexOf;
NUM             fillIndex;
NUM				currentLen;
NUM             maxDataIndex;
StartFrame
DeclareTVAL(retString);
DeclareTVAL(err);
EndFrame

/*  We only accept numeric indices. */

if (!isNumIndex(&index1))
	{
    FrameExit(gCP->TObject_ERROR_NUMINDEXREQ);
	}
else
	{ 
    indexOf = (NUM)asNumIndex(&index1);
	}

*retString = selfTval;

/*  We only allow chars as elements of strings. */

*err = TObject_Convert(gCP,gTP,TYCHAR, newValue);
ExitOnError(*err);
aChar = (CHAR)asChar(err);

/*  Now determine whether this is a TYTEXT or a TYSTRING */
if(retString->Tag == TYTEXT)
    {
    aTextPtr = retString->u.Text;
	maxDataIndex = MAXAISWORDTEXTLEN-1;
	currentLen = strnlen(aTextPtr,maxDataIndex);

	/*  Make sure string index is in range. */

	if (indexOf < 0)
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	else
	if (indexOf >= currentLen)
		{
		if (indexOf < (MAXAISWORDTEXTLEN - 1))
			{
			for (fillIndex = currentLen; fillIndex <= indexOf; ++fillIndex)
				{
				aTextPtr[fillIndex] = ' ';
				}
			aTextPtr[fillIndex] = 0;
			}
		else
			{
			retString->u.Object = (TObject*)TString_MakeUnique(gCP,gTP,aTextPtr);
			retString->Tag = TYSTRING;
			goto StringManager;
			}
		}
    }
else

if(retString->Tag == TYSTRING)
    {
	StringManager:
    aTextPtr = CharArray(*retString);
	maxDataIndex = String(*retString)->itsMaxItemIndex;
	currentLen = strnlen(aTextPtr,maxDataIndex);

	/*  Make sure string index is in range. */

	if (indexOf < 0)
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	else
	if (indexOf >= currentLen)
		{
		if (indexOf < (maxDataIndex - 1))
			{
			for (fillIndex = currentLen; fillIndex <= indexOf; ++fillIndex)
				{
				aTextPtr[fillIndex] = ' ';
				}
			aTextPtr[fillIndex] = 0;
			}
		else
			{
			TString_SetMaxIndex(gCP,gTP,*retString, indexOf+2);
			aTextPtr = CharArray(*retString);
			maxDataIndex = String(*retString)->itsMaxItemIndex;
			currentLen = strnlen(aTextPtr,maxDataIndex);
			for (fillIndex = currentLen; fillIndex <= indexOf; ++fillIndex)
				{
				aTextPtr[fillIndex] = ' ';
				}
			aTextPtr[fillIndex] = 0;
			}
		}
    }
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
    

/*  Now we set the specified indexed value */

aTextPtr[indexOf] = aChar;

FrameExit(*retString);

}


/*--------------------------------------------------------------------------------------- */
#if 0
TString_Map

Make a copy of *this and call the given proc on each element, storing the result in place.

#endif

TVAL TString_Map(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL proc)
{
NUM             index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TString,copyStr);
DeclareOBJ(TString,self);
EndFrame

self = (TString*)asObject(&selfTval);

/* Make a copy of myself since map returns an object of the same type */

copyStr = (TString*)TString_Copy(gCP,gTP,selfTval);

asTag(tmp) = TYCHAR;

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    asChar(tmp) = atHMChar(self->itsCString,index);
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        atHMChar(copyStr->itsCString,index) = (CHAR)asChar(ret);
    }
asTag(ret) = TYSTRING;
asObject(ret) = (TObject*)copyStr;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_Mapc

Loop through itsBindingArray and call the specified function.

#endif

TVAL TString_Mapc(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL proc)
{
NUM                 index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TString,self);
EndFrame

self = (TString*)asObject(&selfTval);

asTag(tmp) = TYCHAR;
for(index=0; index < self->itsMaxItemIndex; index++)
    {
    asChar(tmp) = atHMChar(self->itsCString,index);
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        break;
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_Print
 
Print method for TYSTRING.
 
#endif

TVAL TString_Print(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf)
{
NUM                 n;
StartFrame
DeclareOBJ(TString,self);
EndFrame

self = (TString*)asObject(&selfTval);

n = strlen(&atHMChar(self->itsCString,0));
if(*size + (2 + n) < gCP->TObject_MaxOutputLen)
    {
    sprintf((char*)&buf[*size], "\"%s\"", &atHMChar(self->itsCString,0) );
    *size += (2 + n);
    }
FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_StringAnyCmp

Compare a typed value to a value of type TYSTRING.

#endif

TVAL TString_StringAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
REAL            tmpValue;
LpCHAR          aStringPtr;
StartFrame
DeclareTVAL(ErrValue);
DeclareTVAL(ret);
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
		*ret = TObject_strcmp(gCP,gTP,&atHMChar(asString(&leftVal)->itsCString,0),aStringPtr);
        FrameExit(*ret);
		}
    }
if (asTag(&rightVal) == TYTEXT) 
    {
	*ret = TObject_strcmp(gCP,gTP,&atHMChar(asString(&leftVal)->itsCString,0),asText(&rightVal));
    FrameExit(*ret);
    }
else
if (asTag(&rightVal) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

    *ret = TString_substrlcmp(gCP, gTP, &atHMChar(asString(&leftVal)->itsCString,0), aStringPtr, SubLen(rightVal));
    FrameExit(*ret);
    }
else
if (asTag(&rightVal) == TYBOLE)
    {
    if (asBool(&rightVal) == TRUE)
		{
		*ret = TObject_strcmp(gCP,gTP,&atHMChar(asString(&leftVal)->itsCString,0),(LpCHAR)"true");
        FrameExit(*ret);
		}
    else
		{
		*ret = TObject_strcmp(gCP,gTP,&atHMChar(asString(&leftVal)->itsCString,0),(LpCHAR)"false");
        FrameExit(*ret);
		}
    }
else
if (asTag(&rightVal) == TYCHAR)
    {
    if (atHMChar(asString(&leftVal)->itsCString,0) == asChar(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (atHMChar(asString(&leftVal)->itsCString,0) < asChar(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYNUM) || (asTag(&rightVal) == TYPOINTER))
    {
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asString(&leftVal)->itsCString,0));
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
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asString(&leftVal)->itsCString,0));
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
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asString(&leftVal)->itsCString,0));
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
TString_StringAnyCnv

Convert a typed value to a value of type TYSTRING.

#endif

TVAL TString_StringAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
CHAR            typeName[256];
CHAR            temp[1024];
LpCHAR          aStringPtr;
StartFrame
DeclareTVAL(retTval);
EndFrame      

tTarget = tTarget; // NOOP to hide unused parameter warning message
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
        sprintf((char*)temp,"#<%s "INTFORMAT">",(const char*)typeName,asObject(&oldValue)->itsObjectIndex);
        aStringPtr = (LpCHAR)&temp;
        }

    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,aStringPtr);
	asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYTEXT) 
    {
    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,asText(&oldValue));
	asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL)
        {
        strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&oldValue)));
        sprintf((char*)temp,"(mid #<String "SHORTFORMAT"> "SHORTFORMAT" "SHORTFORMAT")",ObjIdx(oldValue),SubOff(oldValue),SubLen(oldValue));
        aStringPtr = (LpCHAR)&temp;
        asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,aStringPtr);
        }
    else
        {
        asObject(retTval) = (TObject*)TString_SubString_MakeUnique(gCP,gTP,aStringPtr,0,SubLen(oldValue));
        }
    asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYVOID)
    {
    asObject(retTval) = (TObject*)gCP->TString_NIL;
	asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYBOLE)
    {
    if (asBool(&oldValue) == TRUE)
		{
        asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)"true");
		asTag(retTval) = TYSTRING;
		}
    else
		{
        asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)"false");
		asTag(retTval) = TYSTRING;
		}
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYCHAR)
    {
    temp[0] = (CHAR)asChar(&oldValue);
    temp[1] = 0;
    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)temp);
	asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    sprintf((char*)temp,INTFORMAT,asInt(&oldValue));
    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)temp);
	asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    sprintf((char*)temp,SHORTFORMAT,asShort(&oldValue));
    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)temp);
	asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYREAL)
    {
    TObject_sprintReal(gCP,gTP,(char*)temp,asReal(&oldValue));
    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)temp);
	asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYBRICKROW)
    {
    strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&oldValue)));
    sprintf((char*)temp,"#<Brick "SHORTFORMAT">["SHORTFORMAT"]", ObjIdx(oldValue), RowIdx(oldValue));
    aStringPtr = (LpCHAR)&temp;

    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,aStringPtr);
    asTag(retTval) = TYSTRING;
    FrameExit(*retTval);    
    }
else
if (asTag(&oldValue) == TYBRICKFIELD)
    {
    strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&oldValue)));
    sprintf((char*)temp,"#<Brick "SHORTFORMAT">["SHORTFORMAT"]["SHORTFORMAT"]", ObjIdx(oldValue), RowIdx(oldValue), FldIdx(oldValue));
    aStringPtr = (LpCHAR)&temp;

    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,aStringPtr);
    asTag(retTval) = TYSTRING;
    FrameExit(*retTval);    
    }
else
if (asTag(&oldValue) == TYMATRIXROW)
    {
    strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&oldValue)));
    if (FldIdx(oldValue) >= 0)
        sprintf((char*)temp,"#<Matrix "SHORTFORMAT">["SHORTFORMAT"]["SHORTFORMAT"]", ObjIdx(oldValue), RowIdx(oldValue), FldIdx(oldValue));
    else
        sprintf((char*)temp,"#<Matrix "SHORTFORMAT">["SHORTFORMAT"]", ObjIdx(oldValue), RowIdx(oldValue));
    aStringPtr = (LpCHAR)&temp;

    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,aStringPtr);
    asTag(retTval) = TYSTRING;
    FrameExit(*retTval);    
    }
else
if (asTag(&oldValue) == TYNUMMATRIXROW)
    {
    strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&oldValue)));
    if (FldIdx(oldValue) >= 0)
        sprintf((char*)temp,"#<NumMatrix "SHORTFORMAT">["SHORTFORMAT"]["SHORTFORMAT"]", ObjIdx(oldValue), RowIdx(oldValue), FldIdx(oldValue));
    else
        sprintf((char*)temp,"#<NumMatrix "SHORTFORMAT">["SHORTFORMAT"]", ObjIdx(oldValue), RowIdx(oldValue));
    aStringPtr = (LpCHAR)&temp;

    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,aStringPtr);
    asTag(retTval) = TYSTRING;
    FrameExit(*retTval);    
    }
else
	FrameExit(FConvert_ToString(gCP,gTP,1,&oldValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TString_TvalAnyCnv

Convert a typed value to a value of type TVAL.

#endif

TVAL TString_TvalAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
StartFrame
DeclareTVAL(ec);
EndFrame      

tTarget = tTarget; // NOOP to hide unused parameter warning message
if (asTag(&oldValue) == TYTEXT || asTag(&oldValue) == TYERROR || asTag(&oldValue) == TYSTRING)
    {
    FrameExit(oldValue);
    }
else
    {
    *ec = TObject_Convert(gCP,gTP,asTag(&oldValue),oldValue);
    FrameExit(*ec);
    }
}


/*--------------------------------------------------------------------------------------- */
#if 0
TString_New

Create a new TString.

#endif

TString*    TString_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TString,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TString_Initialized) TString_Init(gCP,gTP);

self = (TString*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYSTRING;
self->itsMaxItemIndex = 0;
self->itsCString = NULL;
self->itsImmediatePtr = NULL;

FrameExit(self);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Compares a string and a substring.

#endif

int substrlcmp(const char *str, const char *substr, size_t substrlen)
{
    return substr2cmp(str, substr, strlen(str), substrlen);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Compares a substring and a string.

#endif

int substrrcmp(const char *substr, size_t substrlen, const char *str)
{
    return substr2cmp(substr, str, substrlen, strlen(str));
}

/*--------------------------------------------------------------------------------------- */
#if 0
Compares two substrings.

#endif

int substr2cmp(const char *str1, const char *str2, size_t len1, size_t len2)
{
size_t indexOf = 0;

/* improvised comparison procedure */
/* the return values are patterned after the return values of strncmp */
/* Note: return values of strncmp and strcmp are different in some occasions */

while ((indexOf < len1) && (indexOf < len2))
    {
    if (str1[indexOf] != str2[indexOf])
        return (str1[indexOf] - str2[indexOf]);
    ++indexOf;
    }

if (len1 > len2)
    return (str1[len2]);
else
if (len1 < len2)
    return (-str2[len1]);

return 0;
}

/*--------------------------------------------------------------------------------------- */

#if 0
TString_substrlcmp

The TString_substrlcmp function compares a string and a substring and returns a TVAL result.

#endif

TVAL TString_substrlcmp(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pString,LpCHAR pSubstr,NUM Length)
{
StartFrame 
DeclareTVAL(result);
EndFrame

asTag(result) = TYCOMPARE;
asCompare(result) = substrlcmp((char*)pString,(const char*)pSubstr,(size_t)Length);
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TString_substrrcmp

The TString_substrrcmp function compares a substring and a string and returns a TVAL result.

#endif

TVAL TString_substrrcmp(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pSubstr,NUM Length,LpCHAR pString)
{
StartFrame 
DeclareTVAL(result);
EndFrame

asTag(result) = TYCOMPARE;
asCompare(result) = substrrcmp((const char*)pSubstr,(size_t)Length,(char*)pString);
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TString_substr2cmp

The TString_substr2cmp function compares two substrings and returns a TVAL result.

#endif

TVAL TString_substr2cmp(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pSubstr1,LpCHAR pSubstr2,NUM Length1,NUM Length2)
{
StartFrame 
DeclareTVAL(result);
EndFrame

asTag(result) = TYCOMPARE;
asCompare(result) = substr2cmp((char*)pSubstr1,(const char*)pSubstr2,(size_t)Length1,(size_t)Length2);
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TStringSubstringT_Save(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, HMemory aHMemory)
{
NUM         theOffset;

StartFrame
DeclareOBJ(TObject,objPtr);
EndFrame

objPtr = (TObject*)_TObject_MainObjectList(ObjIdx(selfTval));

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = TYSTRINGSUBSTR;
theOffset = SIZEOF_TObjectOnDisk;

TStringSubstringTOnDiskPtr(aHMemory,theOffset)->itsObject = TObject_RegisterObject(gCP,gTP,objPtr);
TStringSubstringTOnDiskPtr(aHMemory,theOffset)->itsOffset = SubOff(selfTval);
TStringSubstringTOnDiskPtr(aHMemory,theOffset)->itsLength = SubLen(selfTval);

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TStringSubstring_SubstringAnyCmp

Compare a typed value to a value of type TYSTRINGSUBSTR.

#endif

TVAL TStringSubstringT_SubstringAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP, TVAL leftVal, TVAL rightVal)
{
REAL            tmpValue;
LpCHAR          aStringPtr;
StartFrame
DeclareTVAL(ErrValue);
DeclareTVAL(ret);
EndFrame

if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&rightVal));
    if (aStringPtr == NIL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    *ret = TObject_strcmp(gCP,gTP,&atHMChar(asString(&leftVal)->itsCString,0),aStringPtr);
    FrameExit(*ret);
    }
if (asTag(&rightVal) == TYTEXT) 
    {
	*ret = TObject_strcmp(gCP,gTP,&atHMChar(asString(&leftVal)->itsCString,0),asText(&rightVal));
    FrameExit(*ret);
    }
else
if (asTag(&rightVal) == TYBOLE)
    {
    if (asBool(&rightVal) == TRUE)
		{
		*ret = TObject_strcmp(gCP,gTP,&atHMChar(asString(&leftVal)->itsCString,0),(LpCHAR)"true");
        FrameExit(*ret);
		}
    else
		{
		*ret = TObject_strcmp(gCP,gTP,&atHMChar(asString(&leftVal)->itsCString,0),(LpCHAR)"false");
        FrameExit(*ret);
		}
    }
else
if (asTag(&rightVal) == TYCHAR)
    {
    if (atHMChar(asString(&leftVal)->itsCString,0) == asChar(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (atHMChar(asString(&leftVal)->itsCString,0) < asChar(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYNUM) || (asTag(&rightVal) == TYPOINTER))
    {
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asString(&leftVal)->itsCString,0));
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
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asString(&leftVal)->itsCString,0));
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
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&atHMChar(asString(&leftVal)->itsCString,0));
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
if (asTag(&rightVal) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

    *ret = TString_substrlcmp(gCP, gTP, &atHMChar(asString(&leftVal)->itsCString,0), aStringPtr, SubLen(rightVal));
    FrameExit(*ret);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void TStringSubstringT_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk + SIZEOF_TStringSubstringTOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TStringSubstringT_GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TStringSubstringT_GetIV1(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, TVAL index1)
{
NUM         indexOf;
NUM         maxLength;

HMChar      hmCharPtr = NIL;
TObject*    objPtr = NIL;
NUM         offset = 0;
NUM         length = 0;

StartFrame
DeclareTVAL(value);
EndFrame

objPtr = (TObject*)_TObject_MainObjectList(ObjIdx(selfTval));
if (objPtr == NIL)
    FrameExit(gCP->TObject_ERROR_INVALID);
    
offset = SubOff(selfTval);
length = SubLen(selfTval);

/*  We only accept numeric indices. */
if(isNumIndex(&index1))
    indexOf = (NUM)asNumIndex(&index1);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/* Make sure string index is in range. */
if (indexOf < 0 || indexOf >= length)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* Get a pointer to our data. */
switch(objPtr->itsObjectType)
    {
    case TYSTRING:
        hmCharPtr = ((TString*)objPtr)->itsCString;
        maxLength = ((TString*)objPtr)->itsMaxItemIndex;
        break;
    case TYSYMBOL:
    case TYQUOTEDSYMBOL:
        hmCharPtr = ((TSymbol*)objPtr)->itsCString;
        maxLength = ((TSymbol*)objPtr)->itsMaxItemIndex;
        break;
    case TYBYTEVECTOR:
        hmCharPtr = ((TByteVector*)objPtr)->itsByteArray;
        maxLength = ((TByteVector*)objPtr)->itsMaxItemIndex;
        break;
    case TYBRICK:
        hmCharPtr = ((TBrick*)objPtr)->itsFieldArray;
        maxLength = ((TBrick*)objPtr)->itsMaxItemIndex;
        break;
    default:
        /* Unsupported TVAL type */
        FrameExit(gCP->TObject_ERROR_BADTYPE);
        break;
    }

/* The actual index is computed by adding the offset. */
indexOf += offset;

/* Make sure index does not go over the valid area. */
if (indexOf >= maxLength)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
value->Tag = TYCHAR;
value->u.Char = atHMChar(hmCharPtr,indexOf);

FrameExit(*value);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TStringSubstringT_Mapc

Loop through itsBindingArray and call the specified function.

#endif

TVAL TStringSubstringT_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM         indexOf = 0;
NUM         maxLength = 0;

HMChar      hmCharPtr = NULL;
TObject*    objPtr = NIL;
NUM         offset = 0;
NUM         length = 0;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

objPtr = (TObject*)_TObject_MainObjectList(ObjIdx(selfTval));
if (objPtr == NIL)
    FrameExit(gCP->TObject_ERROR_INVALID);
    
offset = SubOff(selfTval);
length = SubLen(selfTval);

/* Get a pointer to our data. */
switch(objPtr->itsObjectType)
    {
    case TYSTRING:
        hmCharPtr = ((TString*)objPtr)->itsCString;
        maxLength = ((TString*)objPtr)->itsMaxItemIndex;
        break;
    case TYSYMBOL:
    case TYQUOTEDSYMBOL:
        hmCharPtr = ((TSymbol*)objPtr)->itsCString;
        maxLength = ((TSymbol*)objPtr)->itsMaxItemIndex;
        break;
    case TYBYTEVECTOR:
        hmCharPtr = ((TByteVector*)objPtr)->itsByteArray;
        maxLength = ((TByteVector*)objPtr)->itsMaxItemIndex;
        break;
    case TYBRICK:
        hmCharPtr = ((TBrick*)objPtr)->itsFieldArray;
        maxLength = ((TBrick*)objPtr)->itsMaxItemIndex;
        break;
    default:
        /* Unsupported TVAL type */
        FrameExit(gCP->TObject_ERROR_BADTYPE);
        break;
    }

/* Make sure index does not go over the valid area. */
if ((offset + length) >= maxLength)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
tmp->Tag = TYCHAR;

for(indexOf = 0; indexOf < length; indexOf++)
    {
    tmp->u.Char = atHMChar(hmCharPtr,offset + indexOf);

    *ret = FSmartbase_Evalv(gCP,gTP,proc,1,tmp);
    ExitOnError(*ret);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TStringSubstringT_Print
 
Print method for TYSTRINGSUBSTR.
 
#endif

TVAL TStringSubstringT_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size,LpCHAR buf)
{
NUM         maxLength = 0;

HMChar      hmCharPtr = NULL;
TObject*    objPtr = NIL;
NUM         offset = 0;
NUM         length = 0;

StartFrame
EndFrame

objPtr = (TObject*)_TObject_MainObjectList(ObjIdx(selfTval));
if (objPtr == NIL)
    FrameExit(gCP->TObject_ERROR_INVALID);
    
offset = SubOff(selfTval);
length = SubLen(selfTval);

/* Get a pointer to our data. */
switch(objPtr->itsObjectType)
    {
    case TYSTRING:
        hmCharPtr = ((TString*)objPtr)->itsCString;
        maxLength = ((TString*)objPtr)->itsMaxItemIndex;
        break;
    case TYSYMBOL:
    case TYQUOTEDSYMBOL:
        hmCharPtr = ((TSymbol*)objPtr)->itsCString;
        maxLength = ((TSymbol*)objPtr)->itsMaxItemIndex;
        break;
    case TYBYTEVECTOR:
        hmCharPtr = ((TByteVector*)objPtr)->itsByteArray;
        maxLength = ((TByteVector*)objPtr)->itsMaxItemIndex;
        break;
    case TYBRICK:
        hmCharPtr = ((TBrick*)objPtr)->itsFieldArray;
        maxLength = ((TBrick*)objPtr)->itsMaxItemIndex;
        break;
    default:
        /* Unsupported TVAL type */
        FrameExit(gCP->TObject_ERROR_BADTYPE);
        break;
    }

/* Make sure index does not go over the valid area. */
if ((offset + length) > maxLength)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

if(*size + (3 + length) < gCP->TObject_MaxOutputLen)
    {
    buf[*size] = '"';
    ++(*size);

    strncpy(&buf[*size], &atHMChar(hmCharPtr,offset), length);
    *size += length;

    buf[*size] = '"';
    buf[++(*size)] = 0;
    }

FrameExit(gCP->TObject_TRUE);
}

LpCHAR TStringSubstringT_GetStringPtr(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval)
{
TObject* objPtr = NIL;
LpCHAR charPtr = NIL;

gTP = gTP; // NOOP to hide unused parameter warning message
if (selfTval.Tag == TYSTRINGSUBSTR)
    {
    objPtr = (TObject*)_TObject_MainObjectList(ObjIdx(selfTval));
    if (objPtr != NIL)
        {
        switch(objPtr->itsObjectType)
            {
            case TYSTRING:
                charPtr = (LpCHAR)*((TString*)objPtr)->itsCString + SubOff(selfTval);
                break;
            case TYSYMBOL:
            case TYQUOTEDSYMBOL:
                charPtr = (LpCHAR)*((TSymbol*)objPtr)->itsCString + SubOff(selfTval);
                break;
            case TYBYTEVECTOR:
                charPtr = (LpCHAR)*((TByteVector*)objPtr)->itsByteArray + SubOff(selfTval);
                break;
            case TYBRICK:
                charPtr = (LpCHAR)*((TBrick*)objPtr)->itsFieldArray + SubOff(selfTval);
                break;
            default:
                /* The caller should check for NULL return */
                charPtr = NIL;
                break;
            }
        }
    }

return(charPtr);
}

