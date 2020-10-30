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

#define _C_TPAIR
#define _SMARTBASE
#if 0
TPair.c

Methods for the Pair class which demonstrates the OODBMS class hierarchy. TPair
is a descendent of TObject.

Note:   self class contains the support for the C++ extensions such as: automated
        mark and sweep garbage collection, manifest types, and oodbms save and load.


PARENT:             TObject

AUTHORS:            Michael F. Korns

#endif

#include "tpair.h"
#include "fmake.h"

/*  Function declarations */

/*--------------------------------------------------------------------------------------- */
#if 0
TPair_Init

Initialize the TPair class.

#endif

void    TPair_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
if (gCP->TPair_Initialized) return;
gCP->TPair_Initialized = TRUE;
TObject_Init(gCP,gTP);

FSmartbase_NewType (gCP,
					gTP,
					 TYPAIR,
                    (LpCHAR)"Pair",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&FMake_Pair,
                    &TPair_Mark,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &TPair_SetIV1,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &TPair_GetIV1,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TPair_Map, 
                    &TPair_Mapc,
                    &TObject_PrintObj,
                    &TPair_Load,
					&TPair_Save,
					&TPair_ComputeSize,
					&TPair_Copy,
					&TPair_Doomed);
                            
FSmartbase_NewType (gCP,
					gTP,
					 TYELLIPSES,
					(LpCHAR)"Ellipses",
					_TObject_TfNATIVE,
					sizeof(NUM),
					(LpFNEW)&TObject_NewNever,
					&TObject_MarkNever,
					&TObject_GlobalMarkNever,
					&FObject_IntAnyCnv,
					&TObject_IntAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapDefault,
					&TObject_Mapc,
					&TPair_PrintEllipses,
					&TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
                            
FSmartbase_NewType (gCP,
					gTP,
					 TYQUOTEDPAIR,
					(LpCHAR)"QuotedPair",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&FMake_Pair,
					&TPair_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TPair_SetIV1,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&TPair_GetIV1,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapDefault,
					&TObject_Mapc,
					&TPair_PrintQuoted,
					&TPair_Load,
					&TPair_Save,
					&TPair_ComputeSize,
					&TPair_Copy,
					&TPair_Doomed);
                            
FSmartbase_NewType (gCP,
					gTP,
					 TYSPECIALFORM,
					(LpCHAR)"SpecialForm",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TObject_NewNever,
					&TSymbol_Mark,
					&TObject_GlobalMarkNever,
					&TSymbol_SymbolAnyCnv,
					&TSymbol_SymbolAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapDefault, 
					&TObject_Mapc, 
					&FProcedure_PrintSpecialForm,
					&TSymbol_Load,
					&TSymbol_Save,
					&TSymbol_ComputeSize,
					&FObject_CopyNever,
					&TSymbol_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TPair_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TPair_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
TPairOnDisk*		diskRecord;
StartFrame
DeclareOBJ(TPair,it);
DeclareTVAL(retTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TPair_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TPair*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
		diskRecord = TPairOnDiskPtr(aHMemory,0);
        it->itsCar = TObject_LoadTval(gCP,gTP,diskRecord->itsCar);
        it->itsCdr = TObject_LoadTval(gCP,gTP,diskRecord->itsCdr);
        asTag(retTval) = it->itsObjectType;
        asObject(retTval) = (TObject*)it;
        }
    else
		{
        *retTval = gCP->TObject_ERROR_INVALID;
		}

    }
    
FrameExit(*retTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TPair_PrintQuoted

Print a quoted pair.

#endif

TVAL TPair_PrintQuoted(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
CHAR    tmpBuf[256];
NUM     nQuotes;
NUM     nLen;

/*  Prepend a series of ' before the Symbol */

nQuotes = asQuoteCnt(&self);

_FMemory_memset(tmpBuf, (NUM)'\'', nQuotes);
tmpBuf[nQuotes] = 0;
nLen = nQuotes;
asTag(&self) = TYPAIR;
if(*size + nLen < gCP->TObject_MaxOutputLen)
    {
    strcat((char*)buf, (const char*)tmpBuf);
    *size += nLen;
    TPair_Print(gCP,gTP,self,size,buf);
    return(gCP->TObject_TRUE);
    }

return(gCP->TObject_FALSE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TPair_PrintEllipses


#endif

TVAL TPair_PrintEllipses(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
 
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
self = self; // NOOP to hide unused parameter warning message
/*  Quit if the output string is already too long */

if (*size + 4 > gCP->TObject_MaxOutputLen) 
    {
    return(gCP->TObject_FALSE);
    }
strcat((char*)buf, (const char*)"...");

*size += 3;

return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Map

Make a copy of this and call the given proc on each element, storing the result in place.

#endif

TVAL TPair_Map(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
TPair* self       = (TPair*)asObject(&selfTval);

StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TPair,copyPair);
DeclareOBJ(TPair,lastPair);
DeclareOBJ(TPair,firstPair);
EndFrame


copyPair = self;
lastPair = NULL;
do
    {
    copyPair = (TPair*)TPair_Copy(gCP,gTP,TOBJ(copyPair));
    if (lastPair != NULL )
        {
        /* fix up the itsCdr references in the copy */
        asObject(&lastPair->itsCdr) = (TObject*)copyPair;
        }
    else
        {
        /* save the firstpair in the copy chain */
        firstPair = copyPair;
        }
    
    *tmp = copyPair->itsCar;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if (isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        copyPair->itsCar = *ret;

        
    if (asTag(&copyPair->itsCdr) == TYPAIR)
        {
        lastPair = copyPair;
        copyPair = asPair(&copyPair->itsCdr);
        }
    else
        break;
    }
while (TRUE);
        
asTag(ret) = TYPAIR;
asObject(ret) = (TObject*)firstPair;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mapc

Traverse the pair chain and call the specified function.

#endif

TVAL TPair_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
TPair*      self = (TPair*)asObject(&selfTval);

StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TPair,curPair);
EndFrame

for (curPair = self; ;curPair = asPair(&curPair->itsCdr))
    {
    *tmp = curPair->itsCar;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if (isERROR(ret) || (asTag(&curPair->itsCdr)) != TYPAIR)
        break;
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Traverse the pair chain to get the indexed Car tval.

#endif

TVAL    TPair_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,self);
EndFrame

self = (TPair*)asObject(&selfTval);

/*  The Pair accepts symbolic indices as special attributes */
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
    {
    /*  An attribute of Value returns the Pair itself. */    
    if (strcmp(SymbolArray(index1),"Value") == 0)
		{
        FrameExit(selfTval);
		}
    /*  An attribute of Pair returns the boolean value: true. */    
    if (strcmp(SymbolArray(index1),"Pair") == 0)
		{
        FrameExit(gCP->Tval_TRUE);
		}
    else
        {
        /*  Return a missing attribute as void (like Structure and Vector). */
        FrameExit(gCP->Tval_VOID);
        }       
    }
else 
/*  The Pair accepts numeric indices as substitutes for repeated cdr's */
if(isNumIndex(&index1))
    {
    for(indexOf = asNumIndex(&index1), curPair = self; indexOf >= 0; indexOf-- )
        {
        if(indexOf == 0)
            {
            FrameExit(curPair->itsCar);    
            }
        else
        if ((asTag(&curPair->itsCdr) == TYPAIR) || (asTag(&curPair->itsCdr) == TYQUOTEDPAIR))
            {
            curPair = asPair(&curPair->itsCdr);
            }
        else
            {
            /*  Return the end of the pair chain as void (like Structure and Vector). */
            FrameExit(gCP->Tval_VOID);
            }       
        }
    }
FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Recursively traverse the pair chain to set the indexed Car tval.

#endif

TVAL    TPair_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,self);
DeclareTVAL(ret);
EndFrame

self = (TPair*)asObject(&selfTval);

if(isNumIndex(&index1))
    {
    indexOf = asNumIndex(&index1);
    if(indexOf < 0 )
		{
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
		}
    else
    for( curPair = self; indexOf >= 0; indexOf-- )
        {
        if(indexOf == 0)
            {
            curPair->itsCar = newValue;
            asTag(ret) = self->itsObjectType;
            asObject(ret) = (TObject*)self;
            FrameExit(*ret);
            }
        else
        if ((asTag(&curPair->itsCdr) == TYPAIR) || (asTag(&curPair->itsCdr) == TYQUOTEDPAIR))
            {
            curPair = asPair(&curPair->itsCdr);
            }       
        else
            {
            /*  Handle the end of the pair chain or an error condition. */
            FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
            }       
        }
    }
FrameExit(gCP->TObject_ERROR_NUMINDEXREQ);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIndexedPair

Recursively traverse the pair chain and return the indicated pair* in a tval.

#endif

TVAL    TPair_GetIndexedPair(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,self);
DeclareTVAL(retTval);
EndFrame

self = (TPair*)asObject(&selfTval);



if(isNumIndex(&index) )
    {
    indexOf = asNumIndex(&index);
    if(indexOf < 0 )
		{
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
		}
    else
    for( curPair = self; indexOf >= 0; indexOf-- )
        {
        if(indexOf == 0)
            {
            asTag(retTval) = TYPAIR;
            asObject(retTval) = (TObject*)curPair;
            FrameExit(*retTval);
            }
        else
        if ((asTag(&curPair->itsCdr) == TYPAIR) || (asTag(&curPair->itsCdr) == TYQUOTEDPAIR))
            {
            curPair = asPair(&curPair->itsCdr);
            }       
        else
            {
            /*  Handle the end of the pair chain or an error condition. */
            FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
            }       
        }
    }
FrameExit(gCP->TObject_ERROR_NUMINDEXREQ);
}

/*--------------------------------------------------------------------------------------- */
#if 0
IPair

Initialize a TPair object.

#endif

void    TPair_IPair(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newCar,TVAL newCdr)
{
TPair*      self = (TPair*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCar = newCar;
self->itsCdr = newCdr;
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TPair_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
#define	    maxPairLength 1000
TPair*      self = selfTval.u.Pair;
TVAL		head[maxPairLength+1];
TPair*		tail[maxPairLength+1];
NUM			pairCount = 0;
NUM			pairIndex = 0;

/* To avoid stack overflow we buffer mark requests here. */

head[pairCount] = self->itsCar;
tail[pairCount] = self;

TryAgain:
while ((pairCount < maxPairLength) && 
	   ((tail[pairCount]->itsCdr.Tag == TYPAIR) || (tail[pairCount]->itsCdr.Tag == TYQUOTEDPAIR)))
	{
	head[pairCount+1] = tail[pairCount]->itsCdr.u.Pair->itsCar;
	tail[pairCount+1] = tail[pairCount]->itsCdr.u.Pair;
	pairCount++;
	}

/* Mark all of the heads */

for (pairIndex = 0; pairIndex <= pairCount; ++pairIndex)
	{
	TObject_MarkTval(gCP,gTP,head[pairIndex]);
	}

/* Do a simple mark of all of the tail Pair objects. */

for (pairIndex = 0; pairIndex < pairCount; ++pairIndex)
	{
	_TObject_MarkFlag(tail[pairIndex]) |= _TObject_OfMARK;
	}

/* Do a recursive mark of the last tail. */

if ((pairCount == maxPairLength) && ((_TObject_MarkFlag(tail[pairIndex]) & _TObject_OfMARK) == 1))
	{
	head[0] = head[pairCount];
	tail[0] = tail[pairCount];
	pairCount = 0;
	goto TryAgain;
	}
else
	{
	TObject_MarkObj(gCP,gTP,(TObject*)tail[pairCount]);
	}
}

/*--------------------------------------------------------------------------------------- */
#if 0
Doomed

Garbage collection is about to delete this object. Perform any necessary clean up.

#endif

void    TPair_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
/*  Pairs do not contain any memory handles, so this is a noop. */
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TPair_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

*aSize += SIZEOF_TPairOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

Note:   Use indirections off the handle so that Macintosh memory compaction
        will not introduce intermittant system crashes.

#endif

HMemory TPair_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
long                theOffset;
TPair*              self = (TPair*)asObject(&selfTval);
TPairOnDisk*		diskRecord;

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;
diskRecord = TPairOnDiskPtr(aHMemory,theOffset);

diskRecord->itsCar = TObject_RegisterTval(gCP,gTP,self->itsCar);
diskRecord->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);

return(aHMemory);
}

/*--------------------------------------------------------------------------------------- */

#if 0
Copy

Make a copy of a TPair.

#endif

TObject*    TPair_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TPair*      self = (TPair*)asObject(&selfTval);
StartFrame

DeclareOBJ(TPair,result);
DeclareOBJ(TPair,theCopy);
DeclareOBJ(TPair,tmpCopy);
EndFrame

if (!_VALIDTVAL(selfTval))
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}

result = theCopy = TPair_New(gCP,gTP);

/* Loop through the List until the Cdr is no longer a Pair. */
while (selfTval.Tag == TYPAIR)
	{
	if (!_VALIDTVAL(self->itsCar))
		{
		// itsCar has a null pointer.
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	else
	if (self->itsCar.Tag == TYPAIR)
	   {
	   theCopy->itsCar.u.Object = TPair_Copy(gCP,gTP,self->itsCar);
	   theCopy->itsCar.Tag = theCopy->itsCar.u.Object->itsObjectType;
	   }
	else
	   {
	   theCopy->itsCar = self->itsCar;
	   }

	if (!_VALIDTVAL(self->itsCdr))
		{
		self->itsCdr = gCP->Tval_VOID;
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
	else
	if (self->itsCdr.Tag == TYPAIR)
	   {
	   selfTval = self->itsCdr;
	   self = (TPair*)selfTval.u.Object;
	   tmpCopy = TPair_New(gCP,gTP);
	   theCopy->itsCdr.u.Object = (TObject*)tmpCopy;
	   theCopy->itsCdr.Tag = TYPAIR;
	   theCopy = tmpCopy;
	   }
	else
	   {
	   theCopy->itsCdr = self->itsCdr;
	   selfTval = theCopy->itsCdr;
	   }

	} /* end while */


FrameReset;
FrameExit((TObject*)result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
GetCdr

Return the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TPair_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TPair*      self = (TPair*)asObject(&selfTval);

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

TVAL TPair_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TPair*      self = (TPair*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a Pair object into an ascii string and append it to an output buffer. 
Following the Lisp tradition, the Pair object is displayed as a series of 
elements as follows: 

                        (Value[0] Value[1] ... Value[n])

#endif

TVAL     TPair_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf)
{
StartFrame
DeclareTVAL(ec);
DeclareOBJ(TPair,self);
EndFrame

self = (TPair*)asObject(&selfTval);

/*  Quit if the output string is already too long */
if (!_VALIDTVAL(selfTval))
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}
else
if (*size > gCP->TObject_MaxOutputLen) 
    {
    FrameExit(gCP->TObject_FALSE);
    }
    
/*  Show List prefix */
buf[*size]      = '(';
buf[++(*size)]  = 0;

*ec = TPair_PrintCont(gCP,gTP,selfTval,size,buf);
_TObject_ErrorChk(*ec);

if (buf[*size-1] == ' ')
    {
    buf[*size-1]    = ')';
    }
else
    {
    buf[*size]      = ')';
    buf[++(*size)]  = 0;
    }
    
FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
PrintCont

Convert a Pair object into an ascii string and append it to an output buffer. 
Following the Lisp tradition, the Pair object is displayed as a series of 
elements as follows: 

                        (Value[0] Value[1] ... Value[n])

#endif

TVAL     TPair_PrintCont(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf)
{
StartFrame
DeclareTVAL(ec);
DeclareOBJ(TPair,self);
EndFrame
 
self = (TPair*)asObject(&selfTval);
/*  Quit if the output string is already too long */

if (*size > gCP->TObject_MaxOutputLen) 
    {
    FrameExit(gCP->TObject_FALSE);
    }

/* Print the car portion of the Pair. */
if (_VALIDTVAL(self->itsCar))
	{
	*ec = FConio_sprintn(gCP,gTP,buf,size,self->itsCar);
	_TObject_ErrorChk(*ec);
	}
else
	{
	self->itsCar = gCP->Tval_VOID;
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}

/* Print the cdr portion of the Pair. */
if (isNullTval(&self->itsCdr))
    {
    FrameExit(gCP->TObject_TRUE);
    }
else
if (!_VALIDTVAL(self->itsCdr))
	{
	self->itsCdr = gCP->Tval_VOID;
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}
else
if (asTag(&self->itsCdr) == TYPAIR )
    {
    buf[*size]      = ' ';
    buf[++(*size)]  = 0;

	*ec = TPair_PrintCont(gCP,gTP,self->itsCdr, size,buf);
    FrameExit(*ec);
    }
else
    {
    buf[*size]      = ' ';
    buf[++(*size)]  = '.';
    buf[++(*size)]  = ' ';
    buf[++(*size)]  = 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,self->itsCdr);
	FrameExit(*ec);
    }

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TPair_New

Create a new TPair.

#endif

TPair*  TPair_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TPair,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TPair_Initialized) TPair_Init(gCP,gTP);

self = (TPair*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYPAIR;
self->itsCar = gCP->TObject_VOID;
self->itsCdr = gCP->TObject_VOID;
self->itsMaxItemIndex = 0;
self->itsNilArray = (HMChar)&self->itsImmediatePtr;
self->itsImmediatePtr = (char*)&self->itsCar;
FrameExit(self);
}
