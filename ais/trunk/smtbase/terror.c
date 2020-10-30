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

#define _C_TERROR
#define _SMARTBASE
#if 0
TError.c 

Implementation of the error class which stores an error messages as a K&R 
C string in a variable length object.

Note:   Duplicate TError objects containing the same K&R C string are eliminated
        by the TError constructors. Therefore, TError object may be compared for
        equality on the basis of the object handles alone. 

PARENT:             TObject 

AUTHORS:            Gilda Cabral

MODIFICATIONS:  

#endif

#include "tstring.h"
#include "terror.h"
#include "tobjvec.h"
#include "tsymbol.h"
#include "tstruct.h"
#include "fmake.h"
#include "fconio.h"

#undef      USEBSEARCH

/*--------------------------------------------------------------------------------------- */
#if 0
TError_Init

Initialize the TError class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TError_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */

if (gCP->TError_Initialized) return;
gCP->TError_Initialized     = TRUE;
TObject_Init(gCP,gTP);

/*  Initialize the new type for this class. */

FSmartbase_NewType (gCP,
					gTP,
					TYERROR,
                    (LpCHAR)"Error",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&TError_MakeError,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &TError_StringAnyCnv,
                    &TError_StringAnyCmp,
                    &TError_SetIV1,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &TError_GetIV1,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TError_Map, 
                    &TError_Mapc,
                    &TError_Print,
                    &TError_Load,
                    &TError_Save,
					&TError_ComputeSize,
					&TError_Copy,
					&TError_Doomed);


/*  Override the previous tval conversion function with one for this class. */

TObject_NewConvert(gCP,gTP,TYTVAL,&TError_TvalAnyCnv);

TSymbol_Init(gCP,gTP);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_MakeUnique

Return a unique TError containing the specified K&R C string. If a TError object
already exists with the same contents, return the existing TError object.
        
#endif

TError*    TError_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString)
{
HMChar                  theData;
NUM                     lengthOf;
StartFrame
DeclareOBJ(TError,theString);
EndFrame              

/*  We create a new TError here, then we */
/*  allocate a handle for the K&R C string data. */

theString = TError_New(gCP, gTP);
lengthOf = strlen((char*)aString);

/*  Can we stuff the K&R C string data into the object header? */

if (lengthOf < (NUM)(_TError_ImmediateSpace-1))
	{
	theString->itsCString = (HMChar)&theString->itsImmediatePtr;
	theString->itsImmediatePtr = (CHAR*)&theString->itsImmediateSpace[0];
	theString->itsMaxItemIndex = lengthOf+1;
	strcpy((char*)theString->itsImmediatePtr,(const char*)aString);
	FrameExit(theString);
	}

/*  Now Stuff the K&R C string data into the TError object. */

theData = (HMChar)FMemory_New(gCP, gTP, lengthOf+1, TRUE);
strcpy((char*)&atHMChar(theData,0),(const char*)aString);
theString->itsMaxItemIndex = lengthOf+1;
theString->itsCString = theData;

FrameExit(theString);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_SubString_MakeUnique

Return a unique TError containing the specified K&R C string. If a TError object
already exists with the same contents, return the existing TError object.

#endif

TError*    TError_SubString_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString, NUM firstChar, NUM theLen)
{
NUM                     lengthOf;
HMChar                  theData;
StartFrame
DeclareOBJ(TError,theString);
EndFrame  

/*  We create a new TError here, then we */
/*  allocate a handle for the K&R C string data. */

theString = TError_New(gCP, gTP);
lengthOf = theLen;

/*  Can we stuff the K&R C string data into the object header? */

if (lengthOf < (NUM)(_TError_ImmediateSpace-1))
	{
	theString->itsCString = (HMChar)&theString->itsImmediatePtr;
	theString->itsImmediatePtr = (CHAR*)&theString->itsImmediateSpace[0];
	strncpy((char*)theString->itsImmediatePtr, (const char*)&aString[firstChar], lengthOf);
	theString->itsMaxItemIndex = lengthOf+1;
	theString->itsImmediateSpace[lengthOf] = 0;
	FrameExit(theString);
	}

/*  Now Stuff the K&R C string data into the TError object. */

theData = (HMChar)FMemory_New(gCP, gTP, lengthOf+1, TRUE);
strncpy((char*)&atHMChar(theData,0), (const char*)&aString[firstChar], lengthOf);
atHMChar(theData,lengthOf) = 0;
theString->itsMaxItemIndex = lengthOf+1;
theString->itsCString = theData;
FrameExit(theString);
}



/*--------------------------------------------------------------------------------------- */
#if 0
TError_MakeError

Return an Error object with the specified initial value.

Note:   (makeError  "MySymbol")   =>  !MySymbol!
        (makeError  "xyz")        =>  !xyz!

  
#endif

TVAL TError_MakeError(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
LpCHAR  aStringPtr = NULL;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TString,sp);
EndFrame

/*  We must have exactly one argument. */

*ret = gCP->Tval_VOID;
if (argc != 1 ) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Create the String with the argument specified. */

if (asTag(&argv[0]) == TYTEXT)
    {
    sp = (TString*)TError_MakeUnique(gCP,gTP,(LpCHAR)&asText(&argv[0]));
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

    sp = (TString*)TError_SubString_MakeUnique(gCP, gTP, aStringPtr, 0, SubLen(argv[0]));
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp = (TString*)TError_MakeUnique(gCP,gTP,&atHMChar((LpCHAR)asString(&argv[0])->itsCString,0));
    }
else
if (asTag(&argv[0]) == TYSYMBOL)
    {
    sp = (TString*)TError_MakeUnique(gCP,gTP,&atHMChar((LpCHAR)asSymbol(&argv[0])->itsCString,0));
    }
else
    {
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    }

asTag(ret) = TYERROR;
asObject(ret) = (TObject*)sp;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_Doomed

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

void    TError_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TError*     self = (TError*)asObject(&selfTval);

if (self->itsImmediatePtr != NULL)
	{
	self->itsCString = NULL;
	self->itsImmediatePtr = NULL;
	self->itsMaxItemIndex = 0;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsCString);
self->itsCString = NULL;
self->itsMaxItemIndex = 0;
return;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TError_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
TError*    self = (TError*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

/*  Round to an even boundary so that all structure pointers align */

*aSize += SIZEOF_TErrorOnDisk + self->itsMaxItemIndex;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_Save

The specified OODBMS manager is about to save this object. Convert yourself to a disk
format append yourself to the handle and return.

#endif

HMemory TError_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
long            theOffset;
TError*			self = (TError*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

/*  Save aStringOnDisk */

TErrorOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
strcpy((char*)TErrorOnDiskPtr(aHMemory,theOffset)->itsStringData, (const char*)&atHMChar(self->itsCString,0));

return(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TError_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 lengthOf;
LpCHAR              pStr;
HMChar              theData;
StartFrame
DeclareOBJ(TError,it);
DeclareTVAL(ret);
EndFrame

it = NULL;
*ret = gCP->TObject_VOID;

if(bResolve == 0)
    {
    it = TError_New(gCP, gTP);
    *ret = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TError*)TObject_CheckRegistration(gCP,gTP,theFileID);
    pStr = (LpCHAR)TErrorOnDiskPtr(aHMemory,0)->itsStringData;
    lengthOf = strlen((char*)pStr);

	/*  Can we stuff the K&R C string data into the object header? */

	if (lengthOf < (NUM)(_TError_ImmediateSpace -1))
		{
		it->itsCString = (HMChar)&it->itsImmediatePtr;
		it->itsImmediatePtr = (CHAR*)&it->itsImmediateSpace[0];
		it->itsMaxItemIndex = lengthOf+1;
		strcpy((char*)it->itsImmediatePtr,(const char*)pStr);
		if(it != NULL)
			{
			asTag(ret) = it->itsObjectType;
			asObject(ret) = (TObject*)it;
			}
		else
			*ret = gCP->TObject_ERROR_INVALID;
		FrameExit(*ret);
		}

    /*  Now Stuff the K&R C string data into the TError object. */
    
    theData = (HMChar)FMemory_New(gCP, gTP, lengthOf+1, TRUE);
    strcpy((char*)&atHMChar(theData,0),(const char*)pStr);
    it->itsMaxItemIndex = lengthOf+1;
    it->itsCString = theData;
    
    if(it != NULL)
        {
        asTag(ret) = it->itsObjectType;
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

Make a copy of a TError.

#endif

TObject*    TError_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TError,self);
DeclareOBJ(TError,theCopy);
EndFrame
self = (TError*)selfTval.u.Object;

theCopy = TError_New(gCP, gTP);

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

LpCHAR TError_GetStringPtr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TError*        self = (TError*)asObject(&selfTval);
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

TVAL TError_GetNumericValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TError,self);
DeclareTVAL(result);
DeclareTVAL(ErrCode);
EndFrame

self = (TError*)asObject(&selfTval);

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

NUM TError_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TError*        self = (TError*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   A TError object can only have its maximum length reset
        when it is created.
 
#endif

TVAL TError_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats)
{
StartFrame
DeclareOBJ(TError,self);
EndFrame

self = (TError*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Can we stuff the K&R C string data into the object header? */

if (self->itsImmediatePtr != NULL)
	{
	if (newRepeats < (NUM)(_TError_ImmediateSpace-1))
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
	CharArray(selfTval)[newRepeats] = 0;
    }
else
    {
    self->itsCString = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)self->itsCString,(LONG)((newRepeats*sizeof(CHAR))+1));
    self->itsMaxItemIndex = newRepeats;
	CharArray(selfTval)[newRepeats] = 0;
    }

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TError_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TError,self);
DeclareTVAL(value);
EndFrame

self = (TError*)asObject(&selfTval);

/*  We only accept numeric indices. */

if(isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    indexOf = -1;
    
/*  Make sure string index is in range. */
if (indexOf < 0)
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Make sure array index is in range. */
if (indexOf >= self->itsMaxItemIndex)
    FrameExit(gCP->TObject_VOID);

asTag(value)  = TYCHAR;
asChar(value) = atHMChar(self->itsCString,indexOf);
FrameExit(*value);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_SetIV1

Set the indexed value in the repeating portion of this object.
This method is only meaningful when the requirement for unique strings is relaxed.

Note:   

#endif

TVAL TError_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
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
    indexOf = asNumIndex(&index1);
	}

*retString = selfTval;

/*  We only allow chars as elements of strings. */

*err = TObject_Convert(gCP,gTP,TYCHAR,newValue);
ExitOnError(*err);
aChar = asChar(err);

/*  Now determine whether this  TYERROR */
if(retString->Tag == TYERROR)
    {
    aTextPtr = ErrorArray(*retString);
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
			TError_SetMaxIndex(gCP,gTP,*retString,indexOf+2);
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
TError_Map

Make a copy of *this and call the given proc on each element, storing the result in place.

#endif

TVAL TError_Map(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc)
{
NUM             index;
StartFrame
DeclareOBJ(TError,self);
DeclareOBJ(TError,copyStr);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

self = (TError*)asObject(&selfTval);
copyStr  = (TError*)_TObject_FrameTobj(&copyStr);

/* Make a copy of myself since map returns an object of the same type */

copyStr = (TError*)TError_Copy(gCP,gTP,selfTval);

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
        atHMChar(copyStr->itsCString,index) = asChar(ret);
    }
asTag(ret) = TYERROR;
asObject(ret) = (TObject*)copyStr;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_Mapc

Loop through itsBindingArray and call the specified function.

#endif

TVAL TError_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc)
{
NUM                 index;
StartFrame
DeclareOBJ(TError,self);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

self = (TError*)asObject(&selfTval);

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
TError_Print
 
Print method for TYERROR.
 
#endif

TVAL TError_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf)
{
NUM                 n;
StartFrame
DeclareOBJ(TError,self);
EndFrame

self = (TError*)asObject(&selfTval);

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
TError_StringAnyCmp

Compare a typed value to a value of type TYERROR.

#endif

TVAL TError_StringAnyCmp(LpXCONTEXT gCP,LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
REAL            tmpValue;
LpCHAR          aStringPtr;
StartFrame
DeclareOBJ(TError,TmpString);
DeclareTVAL(ErrValue);           
DeclareTVAL(ret);           
EndFrame

if (asTag(&rightVal) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

    *ret = TString_substrlcmp(gCP, gTP, &atHMChar(asString(&leftVal)->itsCString,0), aStringPtr, SubLen(rightVal));
    FrameExit(*ret);
    }
else
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
		*ret = TObject_strcmp(gCP, gTP, &atHMChar(asString(&leftVal)->itsCString,0),aStringPtr);
        FrameExit(*ret);
		}
    }
if (asTag(&rightVal) == TYTEXT)
    {
	*ret = TObject_strcmp(gCP, gTP, &atHMChar(asString(&leftVal)->itsCString,0),asText(&rightVal)); 
    FrameExit(*ret);
    }
else
if (asTag(&rightVal) == TYBOLE)
    {
    if (asBool(&rightVal) == TRUE)
		{
		*ret = TObject_strcmp(gCP, gTP, &atHMChar(asString(&leftVal)->itsCString,0),(LpCHAR)"true");
        FrameExit(*ret);
		}
    else
		{
		*ret = TObject_strcmp(gCP, gTP, &atHMChar(asString(&leftVal)->itsCString,0),(LpCHAR)"false");
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
		{
        FrameExit(gCP->TObject_HIGH);
		}
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TError_StringAnyCnv

Convert a typed value to a value of type TYERROR.

#endif

TVAL TError_StringAnyCnv(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
CHAR            typeName[256];
CHAR            temp[1024];
LpCHAR          aStringPtr;
StartFrame
DeclareTVAL(retTval);           
EndFrame 

tTarget = tTarget; // NOOP to hide unused parameter warning message
asTag(retTval) = TYERROR;

if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL)
        {
        strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&oldValue)));
        sprintf((char*)temp,"#<%s "INTFORMAT">",(const char*)typeName,asObject(&oldValue)->itsObjectIndex);
        aStringPtr = (LpCHAR)&temp;
        asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,aStringPtr);
        }
    else
        {
        asObject(retTval) = (TObject*)TError_SubString_MakeUnique(gCP, gTP, aStringPtr, 0, SubLen(oldValue));
        }

    FrameExit(*retTval);
    }
else
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

    asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,aStringPtr);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYTEXT)
    {
    asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,asText(&oldValue));
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRING)
    {
    asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,CharArray(oldValue));
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYVOID)
    {
    asObject(retTval) = (TObject*)gCP->TError_NIL;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYBOLE)
    {
    if (asBool(&oldValue) == TRUE)
        asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,(LpCHAR)"true");
    else
        asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,(LpCHAR)"false");
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYCHAR)
    {
    temp[0] = asChar(&oldValue);
    temp[1] = 0;
    asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,(LpCHAR)temp);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    sprintf((char*)temp,INTFORMAT,asInt(&oldValue));
    asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,(LpCHAR)temp);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    sprintf((char*)temp,SHORTFORMAT,asShort(&oldValue));
    asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,(LpCHAR)temp);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYREAL)
    {
    TObject_sprintReal(gCP,gTP,(char*)temp,asReal(&oldValue));
    asObject(retTval) = (TObject*)TError_MakeUnique(gCP,gTP,(LpCHAR)temp);
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_TvalAnyCnv

Convert a typed value to a value of type TVAL.

#endif

TVAL TError_TvalAnyCnv(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
StartFrame
DeclareTVAL(ec);           
EndFrame 

tTarget = tTarget; // NOOP to hide unused parameter warning message
if (asTag(&oldValue) == TYTEXT || asTag(&oldValue) == TYERROR)
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
TError_sprintf

The TError_sprintf function is the internal routine to do the conversion process. 
This routine is passed a format string. By scanning the format string until 
a format code is encountered, information can be extracted to determine 
what argument is present on the stack and processing can be handled. 

#endif

TVAL TError_sprintf(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR dest,LpCHAR fmt, ...)
{
va_list         args;
NUM             i;
POINTER         argArray[_FSmartbase_MAXARGUMENTS];
StartFrame
DeclareTVAL(err);
EndFrame

/* Gather the variable arguments into an array. */

va_start(args,fmt);
for (i = 0; i < _FSmartbase_MAXARGUMENTS; i++)
    {
    argArray[i] = va_arg(args,POINTER);
    }
va_end(args);
    
/* Format the arguments into the destination string buffer. */

FConio_vsprintf(gCP,gTP,dest,fmt,&argArray[0]);
*err = FSmartbase_Error(gCP, gTP, dest);
FrameExit(*err);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TError_New

Create a new TError.

#endif

TError*    TError_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TError,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TError_Initialized) TError_Init(gCP, gTP);

self = (TError*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYERROR;
self->itsMaxItemIndex = 0;
self->itsCString = NULL;
self->itsImmediatePtr = NULL;

FrameExit(self);
}
