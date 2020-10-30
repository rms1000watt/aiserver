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

#define _C_TSTRUCTURE
#define _SMARTBASE
#if 0
TStructure.c

Implementation of the Structure class which stores a variable length set of 
(symbol,value) pairs stored as a linear array. Adopted from the Scheme dialect, 
it is one of the main mechanisms for variable binding in SmartLisp.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "tstruct.h"
#include "futil3.h"
#include "fpred2.h"
#include "fmake.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TStructure_Init

Initialize the TStructure class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TStructure_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TStructure_Initialized) 
	return;

gCP->TStructure_Initialized    = TRUE;

TObject_Init(gCP,gTP);

/*  Initialize the new types for this class. */
FSmartbase_NewType (gCP,
					gTP,
					 TYSTRUCTURE,
                    "Structure",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&FMake_Structure,
                    &TStructure_Mark,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &TStructure_SetIV1,
                    &TStructure_SetIV2,
                    &FObject_SetIV3Never,
                    &TStructure_GetIV1,
                    &TStructure_GetIV2,
                    &FObject_GetIV3Never,
                    &TStructure_Map,
                    &TStructure_Mapc,
                    &TObject_PrintObj,
                    &TStructure_Load,
					&TStructure_Save,
					&TStructure_ComputeSize,
					&TStructure_Copy,
					&TStructure_Doomed);

/*  Initialize and perm the special indexing keywords for this class. */
gCP->TStructure_key = TSymbol_MakeUnique(gCP,gTP,"key");
gCP->TStructure_value = TSymbol_MakeUnique(gCP,gTP,"value");
FObject_Perm(gCP,gTP,(TObject*)gCP->TStructure_key,TRUE);
FObject_Perm(gCP,gTP,(TObject*)gCP->TStructure_value,TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TStructure_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TStructure_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 cn;
StartFrame
DeclareOBJ(TStructure,it);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TStructure_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TStructure*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        asObject(itTval) = (TObject*)it;
        asTag(itTval) = it->itsObjectType;
        TStructure_SetMaxIndex(gCP,gTP,*itTval, TStructureOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        
        it->itsMethods = (TSymbol*)TObject_CheckRegistration(gCP,gTP,TStructureOnDiskPtr(aHMemory,0)->itsMethods);
        it->itsCdr = TObject_LoadTval(gCP,gTP,TStructureOnDiskPtr(aHMemory,0)->itsCdr);
        
		/* Load the itemArray containing the structure bindings from the disk stream */
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
			atHMBind(it->itsDictionaryArray,cn).Key = TObject_CheckRegistration(gCP,gTP,TStructureOnDiskPtr(aHMemory,0)->itsItemArray[cn].Key);
			atHMBind(it->itsDictionaryArray,cn).Value = TObject_LoadTval(gCP,gTP,TStructureOnDiskPtr(aHMemory,0)->itsItemArray[cn].Value);
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
TStructure_PrintStructure

Convert a Structure object into an ascii string and append it to an output buffer. 
Following the extended tradition, the Structure object is displayed as a series of 
elements as follows: 

#endif

TVAL TStructure_PrintStructure(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theEnv, LpNUM size, LpCHAR buf,
								BOLE PrintingBinding, NUM PrintIndex )
{
NUM                     indexOf;
NUM                     startIndex;
NUM                     endIndex;
StartFrame
DeclareTVAL(ec);
DeclareTVAL(tmpTval);
EndFrame

/*  Quit if the output string is already too long */

if ((*size) + 4 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
/*  Show Structure prefix */

buf[*size]      = '#';
buf[++(*size)]  = '{';
buf[++(*size)]  = 0;

/*  Show all or part of the Structure's itsDictionaryArray */

if(PrintingBinding == FALSE)
    {
    startIndex = 0;
    endIndex = theEnv->itsMaxItemIndex;
    }
else
    {
    startIndex = PrintIndex;
    endIndex = startIndex + 1;
    }

for(indexOf = startIndex; indexOf < endIndex; indexOf++)
    {
    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
    
    asObject(tmpTval) = atHMBind(theEnv->itsDictionaryArray,indexOf).Key;
    asTag(tmpTval) = asObject(tmpTval)->itsObjectType;
    
    *ec = FConio_sprintn(gCP,gTP,buf,size,*tmpTval);
    _TObject_ErrorChk(*ec);

    buf[*size]  = ':';
    buf[++(*size)]  = 0;
    
    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[*size]      = ' ';
    buf[++(*size)]  = 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,atHMBind(theEnv->itsDictionaryArray,indexOf).Value);
    _TObject_ErrorChk(*ec);
    if (indexOf != theEnv->itsMaxItemIndex - 1)
        {
        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ' ';
        buf[++(*size)]  = 0;
        }
    }


/*  Show the Structure's cdr if it is not void and we are not printing a single binding */

if (!PrintingBinding && asTag(&theEnv->itsCdr))
    {
    if ((*size) + 4 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[*size]      = ' ';
    buf[++(*size)]  = '.';
    buf[++(*size)]  = ' ';
    buf[++(*size)]  = 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,theEnv->itsCdr);
    _TObject_ErrorChk(*ec);
    }


/*  Show Structure suffix */

if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = '}';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TStructure_BSearchValue

Search a TStructure examining the indicated data. Search is on the Value field.

#endif

TVAL TStructure_BSearchValue(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TVAL keyData, COMPARE* compareCode)
{
register NUM    low, mid, high, compare = 0;
StartFrame
DeclareTVAL(retTval);
EndFrame

low = 0;
mid = 0;
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
    }   
else
    {
    FrameExit(*retTval);
    }
    
while ( low <= high )
    {   
    mid = (low + high) >> 1;

    *retTval = FPredicate2_FullCompare(gCP, gTP, keyData, atHMBind(theSpace->itsDictionaryArray,mid).Value);
    compare = asInt(retTval);

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
TStructure_MakeUniqueValue

Maintains a numerically sorted TStructure for later bsearch. Sort is on the Value field.
Returns a tval containing the index of the found or created item.

#endif

TVAL TStructure_MakeUniqueValue(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TObject* theObject, TVAL keyData)
{
COMPARE     compareCode;
StartFrame
DeclareTVAL(spaceTval);
DeclareTVAL(objectTval);
DeclareTVAL(indexTval);
EndFrame

/*  Init the search space array */

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

asObject(spaceTval) = (TObject*)theSpace;
asTag(spaceTval) = theSpace->itsObjectType;
asObject(objectTval) = theObject;
asTag(objectTval) = theObject->itsObjectType;

if ( theSpace->itsMaxItemIndex > 0 ){
    *indexTval = TStructure_BSearchValue(gCP,gTP,theSpace, keyData, &compareCode);
      
    if ( compareCode < 0 )
        {
        TStructure_Insert(gCP, gTP, *spaceTval, *indexTval,*objectTval,keyData);
        }
    else 
    if ( compareCode > 0 )
        {
        asInt(indexTval)++;
        TStructure_Insert(gCP, gTP, *spaceTval, *indexTval, *objectTval,keyData);
        }
    else 
        {
        /* collision */
        
        }
    }
else
    {
    asInt(indexTval) = 0;
    TStructure_AddNewValue(gCP,gTP,*spaceTval,*objectTval,keyData);
    }
    
FrameExit(*indexTval);
    
}

/*--------------------------------------------------------------------------------------- */
#if 0
TStructure_BSearchKey

Search the Structure bindings for the given object key.
Returns a TVAL containing the index for the closest match found, maybe an exact match.

#endif

TVAL TStructure_BSearchKey(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TObject* anObject, COMPARE* compareCode)
{
register NUM    low, mid, high, compare = 0;
HMBind          itsDictionaryArray;
StartFrame
DeclareTVAL(retTval);
DeclareTVAL(cmpTval);
DeclareTVALArray(item,2);

EndFrame

low = 0;
mid = 0;
*compareCode = -1;
asTag(retTval) = TYNUM;
asInt(retTval) = mid;

item[0].u.Object = anObject;
item[0].Tag = item[0].u.Object->itsObjectType;

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

if(theSpace->itsMaxItemIndex != 0)
    {
    high = theSpace->itsMaxItemIndex - 1;
    itsDictionaryArray = theSpace->itsDictionaryArray;
    }   
else
    {
    FrameExit(*retTval);
    }
        
while ( low <= high )
    {   
    mid = (low + high) >> 1;
    
    item[1].u.Object = atHMBind(itsDictionaryArray,mid).Key;
    item[1].Tag = item[1].u.Object->itsObjectType;
    *cmpTval = FPredicate2_Compare(gCP,gTP,2,&item[0]);
    compare = cmpTval->u.Int;

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
TStructure_SearchKey

Search the Structure bindings for the given object key.
Returns a TVAL containing the index for the closest match found, maybe an exact match.

#endif

TVAL TStructure_SearchKey(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* self, TObject* anObject, COMPARE* compareCode)
{
register NUM    indexOf;
TVAL			retValue;



gTP = gTP; // NOOP to hide unused parameter warning message
/*  We search only for object keys */

for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    if (atHMBind(self->itsDictionaryArray,indexOf).Key == anObject)
		{
		*compareCode = 0;
		retValue.Tag = TYNUM;
		retValue.u.Int = indexOf;
		retValue.DeclaredType = atHMBind(self->itsDictionaryArray,indexOf).Value.DeclaredType;
        return(retValue);
		}
    }
    
/*  If the key does not exist, then we return void, */

*compareCode = -1;
return(gCP->Tval_VOID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TStructure_MakeUniqueKey

Maintains a numerically sorted TStructure for later bsearch. Sort is on the Key object.
Returns a tval containing the index of the found or created item.

#endif

TVAL TStructure_MakeUniqueKey(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TObject* keyObject, TVAL valueData)
{
COMPARE     compareCode;
StartFrame
DeclareTVAL(spaceTval);
DeclareTVAL(objectTval);
DeclareTVAL(indexTval);
EndFrame

/*  Init the search space array */

if( theSpace == NULL )
    {
    FrameExit(gCP->TObject_FALSE);
    }

asObject(spaceTval) = (TObject*)theSpace;
asTag(spaceTval) = theSpace->itsObjectType;
asObject(objectTval) = keyObject;
asTag(objectTval) = keyObject->itsObjectType;

if ( theSpace->itsMaxItemIndex > 0 ){
    *indexTval = TStructure_BSearchKey(gCP, gTP, theSpace, keyObject, &compareCode);
      
    if ( compareCode < 0 )
        {
        TStructure_Insert(gCP, gTP, *spaceTval, *indexTval,*objectTval,valueData);
        }
    else 
    if ( compareCode > 0 )
        {
        asInt(indexTval)++;
        TStructure_Insert(gCP, gTP, *spaceTval, *indexTval, *objectTval,valueData);
        }
    else 
        {
        /* collision */
        
        }
    }
else
    {
    indexTval->Tag = TYNUM;
    indexTval->u.Int = 0;
    TStructure_AddNewValue(gCP,gTP,*spaceTval, *objectTval, valueData);
    }
    
FrameExit(*indexTval);
    
}


/*  CConversion */

/*--------------------------------------------------------------------------------------- */
#if 0
Map

Make a copy of *self and call the given proc on each element, storing the result in place.

#endif

TVAL TStructure_Map(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM             index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TStructure,copyEnv);
DeclareOBJ(TStructure,self);
EndFrame

self = (TStructure*)asObject(&selfTval);

/* Make a copy of myself since map returns an object of the same type */

copyEnv = (TStructure*)TStructure_Copy(gCP,gTP,selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    *tmp = atHMBind(copyEnv->itsDictionaryArray,index).Value;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        atHMBind(copyEnv->itsDictionaryArray,index).Value = *ret;
    }
asTag(ret) = TYSTRUCTURE;
asObject(ret) = (TObject*)copyEnv;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mapc

Loop through self->itsDictionaryArray and call the specified function.

#endif

TVAL TStructure_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM         index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TStructure,copyEnv);
DeclareOBJ(TStructure,self);
EndFrame

self = (TStructure*)asObject(&selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    *tmp = atHMBind(self->itsDictionaryArray,index).Value;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        break;
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
IStructure

Initialize a TStructure object with an array of bindings.

#endif

TVAL    TStructure_IStructure(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[], TVAL newCdr)
{
NUM             argIndex;
NUM             bindIndex;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TStructure,self);
EndFrame

self = (TStructure*)asObject(&selfTval);

/*  We must have an even number of arguments. The first argument of each pair is */
/*  the symbol (key) and the second argument is the value (value). */
if ((argc % 2) != 0)
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Reshape the Structure's array to be the correct size. */
TStructure_SetMaxIndex(gCP,gTP,selfTval, argc/2);

/*  Add each remaining binding to the end of the Structure. */
argIndex = 0;
bindIndex = 0;
while (argIndex < argc)
    {
    /*  Add the binding to the Structure, and move to the next binding pair. */
    /*  Note:   Each binding must always begin with an object. */
    
    
    if (_TObject_TypeFlag(asTag(&argv[argIndex])) != _TObject_TfTOBJECT)
        FrameExit(gCP->TObject_ERROR_INVALID);
    atHMBind(self->itsDictionaryArray,bindIndex).Key = asObject(&argv[argIndex++]);
    atHMBind(self->itsDictionaryArray,bindIndex++).Value = argv[argIndex++];
    }

/*  Set the Structure's tail(cdr). */
self->itsCdr = newCdr;
/* Set the proper return value for a successful initialization. */

asTag(ret) = TYSTRUCTURE;
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

void    TStructure_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TStructure*   self = (TStructure*)asObject(&selfTval);
NUM             indexOf;

/*  Mark the Structure's items so they will not be garbage collected. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    {
    TObject_MarkObj(gCP,gTP,atHMBind(self->itsDictionaryArray,indexOf).Key);
    TObject_MarkTval(gCP,gTP,atHMBind(self->itsDictionaryArray,indexOf).Value);
    }

/*  Mark the Structure's tail so it will not be garbage collected. */

TObject_MarkObj(gCP,gTP,(TObject*)self->itsMethods);
TObject_MarkTval(gCP,gTP,self->itsCdr);

}

/*--------------------------------------------------------------------------------------- */
#if 0
Doomed

Garbage collection is about to delete this object. Dispose of the Structure data.

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

void    TStructure_Doomed(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
TStructure*   self   = (TStructure*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsDictionaryArray = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsDictionaryArray);
self->itsMaxItemIndex = 0;	
self->itsDictionaryArray = NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TStructure_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
TStructure*   self   = (TStructure*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/* Compute the size of the object header */
*aSize = SIZEOF_TObjectOnDisk;

/* Compute the size of the structure and all its bindings */
*aSize += ((NUM)&((TStructureOnDisk*)0)->itsItemArray[self->itsMaxItemIndex]);

ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TStructure_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory)
{
long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TStructure,self);
EndFrame

self   = (TStructure*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TStructureOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TStructureOnDiskPtr(aHMemory,theOffset)->itsMethods = TObject_RegisterObject(gCP,gTP,(TObject*)self->itsMethods);
TStructureOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);

/*  Save the structure binding (key,value) into a disk record */

for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
	TStructureOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn].Key = TObject_RegisterObject(gCP,gTP,atHMBind(self->itsDictionaryArray,cn).Key);
	TStructureOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn].Value = TObject_RegisterTval(gCP,gTP,atHMBind(self->itsDictionaryArray,cn).Value);
   }


FrameExit(aHMemory);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TStructure.

#endif

TObject*    TStructure_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TStructure,self);
DeclareOBJ(TStructure,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TStructure*)selfTval.u.Object;

theCopy = TStructure_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

TStructure_SetMaxIndex(gCP,gTP,*tmpTval, self->itsMaxItemIndex);

if ((self->itsMaxItemIndex != 0) && (self->itsDictionaryArray != NULL))
    {
    _FMemory_memcpy(&atHMBind(theCopy->itsDictionaryArray,0),&atHMBind(self->itsDictionaryArray,0),(LONG)(self->itsMaxItemIndex*sizeof(BIND)));
    }

theCopy->itsCdr = self->itsCdr;
theCopy->itsMethods = self->itsMethods;

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TStructure_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TStructure*   self = (TStructure*)asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

#endif

TVAL TStructure_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
NUM             cn;
StartFrame
DeclareOBJ(TStructure,self);
EndFrame

self = (TStructure*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TStructure_ImmediateSpace/(NUM)sizeof(BIND)))
	{
	if (self->itsDictionaryArray == NULL) 
		{
		for(cn = 0; cn < newRepeats; cn++)
			{
			((BIND*)self->itsImmediateSpace)[cn].Key = (TObject*)gCP->TLambda_nil;
			((BIND*)self->itsImmediateSpace)[cn].Value = gCP->Tval_VOID;
			}
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)BindArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(BIND));
		if ((self->itsDictionaryArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsDictionaryArray);
		for(cn = self->itsMaxItemIndex; cn < newRepeats; cn++)
			{
			((BIND*)self->itsImmediateSpace)[cn].Key = (TObject*)gCP->TLambda_nil;
			((BIND*)self->itsImmediateSpace)[cn].Value = gCP->Tval_VOID;
			}
		}
	self->itsDictionaryArray = (HMBind)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item Structure handle. */
if (self->itsDictionaryArray == NULL)
    {
    self->itsDictionaryArray = (FMBind**)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(BIND)),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    for(cn = 0; cn < newRepeats; cn++)
        {
        atHMBind(self->itsDictionaryArray,cn).Key = (TObject*)gCP->TLambda_nil;
        atHMBind(self->itsDictionaryArray,cn).Value = gCP->Tval_VOID;
        }
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsDictionaryArray = (HMBind)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(BIND)),TRUE);
	_FMemory_memcpy((char*)BindArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(BIND));
	for(cn = self->itsMaxItemIndex; cn < newRepeats; cn++)
		{
		atHMBind(self->itsDictionaryArray,cn).Key = (TObject*)gCP->TLambda_nil;
		atHMBind(self->itsDictionaryArray,cn).Value = gCP->Tval_VOID;
		}
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsDictionaryArray = (FMBind**)FMemory_Resize(gCP, gTP, (FMemory**)self->itsDictionaryArray,(LONG)(newRepeats*sizeof(BIND)));
	self->itsImmediatePtr = NULL;
    if (self->itsMaxItemIndex < newRepeats)
        {
        for(cn = self->itsMaxItemIndex; cn < newRepeats; cn++)
            {
            atHMBind(self->itsDictionaryArray,cn).Key = (TObject*)gCP->TLambda_nil;
            atHMBind(self->itsDictionaryArray,cn).Value = gCP->Tval_VOID;
            }
        }
    self->itsMaxItemIndex = newRepeats;
    }

FrameExit(gCP->TObject_OK);

}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

Note:   a Structure object may be indexed either by a key value or by
        a numeric value.

#endif

TVAL TStructure_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TStructure,self);
EndFrame

self = (TStructure*)asObject(&selfTval);

if (_TObject_TypeFlag(asTag(&index1)) == _TObject_TfTOBJECT)
    {
    /*  We accept object indices */
    
    for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
        {
        if (atHMBind(self->itsDictionaryArray,indexOf).Key == asObject(&index1))
            FrameExit(atHMBind(self->itsDictionaryArray,indexOf).Value);
        }
        
    /*  If the key does not exist, then user defined Structures return an error, */
    /*  and normal Structure objects return a value of #void. */
    
    if (self->itsMethods == NULL)
		{
        FrameExit(gCP->Tval_VOID);
		}
    else
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    }
else 
if (isNumIndex(&index1))
    {
    /*  We accept numeric indices. */
    indexOf = asNumIndex(&index1);

    /*  Make sure index is in range. */
    if (indexOf < 0)
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    /*  If the key does not exist, then user defined Structures return an error, */
    /*  and normal Structure objects return a value of #void. */
    
    if (self->itsMethods == NULL)
		{
		if (indexOf >= self->itsMaxItemIndex)
			FrameExit(gCP->Tval_VOID);
		}
    else
		{
		if (indexOf >= self->itsMaxItemIndex)
			FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
		}

    /*  Return the bound value. */
    FrameExit(atHMBind(self->itsDictionaryArray,indexOf).Value);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV2

Return the indexed value from the repeating portion of this object.

Note:   a Structure object may be indexed either by an object value or by
        a numeric value.

#endif

TVAL TStructure_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1, TVAL index2)
{
NUM             indexOf;
NUM             whichNdx;
COMPARE			compareCode;
StartFrame
DeclareOBJ(TStructure,self);
DeclareTVAL(retValue);
EndFrame

self = (TStructure*)asObject(&selfTval);

/* We accept the first index to be unsorted: */

if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"unsorted") == 0))
    {
    /* Reference the Structure in sequential fasion. */
	FrameExit(TStructure_GetIV1(gCP,gTP,selfTval,index2));
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"sorted") == 0))
    {
    /* Reference the Structure in binary unique sorted fasion. */
	if (_TObject_TypeFlag(asTag(&index2)) != _TObject_TfTOBJECT)  {FrameExit(gCP->TObject_ERROR_BADIDXORKEY);}
	*retValue = TStructure_BSearchKey(gCP,gTP,selfTval.u.Structure,index2.u.Object,&compareCode);
	ExitOnError(*retValue);
	if (compareCode == 0)
		{
		if (isNumIndex(retValue))
			{
			/*  We accept numeric indices. */
			indexOf = asNumIndex(retValue);

			/*  Make sure index is in range. */
			if (indexOf < 0)
				FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
			/*  Make sure index is in range. */
			if (indexOf >= self->itsMaxItemIndex)
				FrameExit(gCP->TObject_VOID);
    
			/*  Return the bound value. */
			FrameExit(atHMBind(self->itsDictionaryArray,indexOf).Value);
			}

		FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
		}
	else
		{
		FrameExit(gCP->Tval_VOID);
		}
	}
else
if (isNumIndex(&index1) && isNumIndex(&index2))
    {
    /*  We accept numeric indices. */
    indexOf = asNumIndex(&index1);
    whichNdx = asNumIndex(&index2);

    /*  Make sure index is in range. */
    if (!inRange(indexOf, 0,self->itsMaxItemIndex) || !inRange(whichNdx, 0,2))
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    /*  Return the appropriate bound value. */
    
    if(whichNdx != 0)
        *retValue = atHMBind(self->itsDictionaryArray,indexOf).Value;
    else
        {
        asTag(retValue) = atHMBind(self->itsDictionaryArray,indexOf).Key->itsObjectType;
        asObject(retValue) = atHMBind(self->itsDictionaryArray,indexOf).Key;
        }
    FrameExit(*retValue);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   a Structure object may be indexed either by a object value or by
        a numeric value.

#endif

TVAL TStructure_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TStructure,self);
DeclareOBJ(TStructure,aStructure);
DeclareTVAL(tmpIndex);
EndFrame

self = (TStructure*)asObject(&selfTval);

/*  Convert text and string keys to symbolic keys. */
/*  Note:   Symbolic keys guarantee uniqueness. */

if ((index1.Tag == TYTEXT) || (index1.Tag == TYSTRING) || (index1.Tag == TYQUOTEDSYMBOL) || (index1.Tag == TYSTRINGSUBSTR))
    {
    *tmpIndex = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,index1);
	index1 = *tmpIndex;
    }

/*  We accept object indices. */

if (_TObject_TypeFlag(asTag(&index1)) == _TObject_TfTOBJECT)
    {
    for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
        {
        if (atHMBind(self->itsDictionaryArray,indexOf).Key == index1.u.Object)
            {
            atHMBind(self->itsDictionaryArray,indexOf).Value = newValue;
            FrameExit(selfTval);
            }
        }
        
    /*  If the key does not exist, then user defined Structures return an error, */
    /*  and normal Structure objects add the new key to the end of the Structure. */
    
    if (self->itsMethods == NULL)
		{
		indexOf = self->itsMaxItemIndex;
		TStructure_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex + 1);
		atHMBind(self->itsDictionaryArray,indexOf).Key = index1.u.Object;
		atHMBind(self->itsDictionaryArray,indexOf).Value = newValue;
		FrameExit(selfTval);
		}
    else
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    }
else
/*  We accept numeric indices. */
if (isNumIndex(&index1))
    {
    indexOf = asNumIndex(&index1);
    /*  Make sure index is in range. */
    if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
        FrameExit(gCP->TObject_ERROR_INVALID);
    
    /*  Set the Structure binding element to the new value. */
    
    atHMBind(self->itsDictionaryArray,indexOf).Value = newValue;
    FrameExit(selfTval);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV2

Set the indexed value in the repeating portion of this object.

#endif

TVAL TStructure_SetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2, TVAL newValue)
{
NUM             indexOf;
NUM             whichNdx;
StartFrame
DeclareOBJ(TStructure,self);
DeclareOBJ(TStructure,aStructure);
DeclareTVAL(newKey);
DeclareTVAL(retValue);
EndFrame

self = (TStructure*)asObject(&selfTval);

/* We accept the first index to be last: */

if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL)) && (strcmp(SymbolArray(index1),"last") == 0))
    {
    /* add a new Binding */
	*newKey = index2;
	if ((newKey->Tag == TYTEXT) || 
		(newKey->Tag == TYSTRING) || 
		(newKey->Tag == TYQUOTEDSYMBOL) ||
		(newKey->Tag == TYSTRINGSUBSTR))
		{
		*newKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,index2);
		}

	if (_TObject_TypeFlag(newKey->Tag) != _TObject_TfTOBJECT)
		FrameExit(gCP->TObject_ERROR_INVALID);

    TStructure_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex + 1);
    atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Key = newKey->u.Object;
    atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Value = newValue;
	FrameExit(selfTval);
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"unsorted") == 0))
    {
    /* Reference the Structure in sequential fasion. */
	FrameExit(TStructure_SetIV1(gCP,gTP,selfTval,index2,newValue));
	}
else
if (((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))  && (strcmp(SymbolArray(index1),"sorted") == 0))
    {
    /* Reference the Structure in binary unique sorted fasion. */
	if (_TObject_TypeFlag(asTag(&index2)) != _TObject_TfTOBJECT)  {FrameExit(gCP->TObject_ERROR_BADIDXORKEY);}
	*retValue = TStructure_MakeUniqueKey(gCP,gTP,selfTval.u.Structure,index2.u.Object,newValue);
	ExitOnError(*retValue);
	FrameExit(selfTval);
	}
else
/*  We accept numeric indices. */
if (isNumIndex(&index1) && isNumIndex(&index2))
    {
    /*  We accept numeric indices. */
    indexOf = asNumIndex(&index1);
    whichNdx = asNumIndex(&index2);

    /*  Make sure index is in range. */

    if (!inRange(indexOf, 0,self->itsMaxItemIndex) || !inRange(whichNdx, 0,2))
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    /*  Set the Structure binding element to the new value. */
    if (whichNdx != 0)
        atHMBind(self->itsDictionaryArray,indexOf).Value = newValue;
    else
        {
        /*  Convert text and string keys to symbolic keys. */
        /*  Note:   Symbolic keys guarantee uniqueness. */
        
        if ((newValue.Tag == TYTEXT) || (newValue.Tag == TYSTRING) || (newValue.Tag == TYQUOTEDSYMBOL) || (newValue.Tag == TYSTRINGSUBSTR))
            {
            newValue = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,newValue);
            }

        if (_TObject_TypeFlag(asTag(&newValue)) == _TObject_TfTOBJECT)
            atHMBind(self->itsDictionaryArray,indexOf).Key = asObject(&newValue);
        else
            FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
        }
        
    asTag(retValue) = self->itsObjectType;
    asObject(retValue) = (TObject*)self;
    FrameExit(*retValue);
    }

FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}


/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note:   

#endif

TVAL TStructure_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newKey, TVAL newValue)
{
StartFrame
DeclareOBJ(TStructure,self);
DeclareTVAL(tmp);
EndFrame

self = (TStructure*)asObject(&selfTval);

/*  Convert text and string keys to symbolic keys. */
/*  Note:   Symbolic keys guarantee uniqueness. */

if ((newKey.Tag == TYTEXT) || (newKey.Tag == TYSTRING) || (newKey.Tag == TYQUOTEDSYMBOL) || (newKey.Tag == TYSTRINGSUBSTR))
    {
    newKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,newKey);
    }

if (_TObject_TypeFlag(asTag(&newKey)) != _TObject_TfTOBJECT)
    FrameExit(gCP->TObject_ERROR_INVALID);

/* Check to see if newKey already defined if not then add it */

*tmp = TStructure_isBound(gCP,gTP,selfTval, newKey);
if (tmp->Tag == TYBOLE)
    {    
    /* add a new Binding */
    TStructure_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex + 1);
    atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Key = asObject(&newKey);
    atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Value = newValue;
    }
else
    {
    /*  redefine an existing binding */
    FrameExit(TStructure_SetIV1(gCP,gTP,selfTval, newKey, newValue));
    }
    
FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetCdr

Return the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TStructure_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TStructure*   self = (TStructure*)asObject(&selfTval);
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

TVAL TStructure_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TStructure*   self = (TStructure*)asObject(&selfTval);
gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */

#if 0
Print

Convert a Structure object into an ascii string and append it to an output buffer. 
Following the extended tradition, the Structure object is displayed as a series of 
elements as follows: 

#endif

TVAL TStructure_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
TStructure*   self = (TStructure*)asObject(&selfTval); 
return TStructure_PrintStructure(gCP,gTP, self, size, buf, FALSE, 0);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Vector is resized.

#endif

TVAL TStructure_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{
register LpBIND         targetPtr;
register LpBIND         sourcePtr;
register LpBIND         haltPtr;
NUM                     deleteIndex;
StartFrame
DeclareOBJ(TStructure,self);
EndFrame

self = (TStructure*)asObject(&selfTval);

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    {
    deleteIndex = asNumIndex(&index);
    }
else
    {
    index =  TStructure_isBound(gCP,gTP,selfTval,index);
    if (isNumIndex(&index))
        {
        deleteIndex = asNumIndex(&index);
        }
    else
        {
        FrameExit(gCP->TObject_OK);
        }
    }
    
/*  Make sure TStructure index is in range. */

if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the TStructure down one position */

sourcePtr = &atHMBind(self->itsDictionaryArray,deleteIndex+1);
targetPtr = &atHMBind(self->itsDictionaryArray,deleteIndex);
haltPtr = &atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }
    
/*  Resize the TStructure down one position */

TStructure_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex-1);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the higher values are moved up one position and
        the TStructure is resized.

#endif

TVAL TStructure_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index, TVAL newObject, TVAL newValue)
{
register LpBIND         targetPtr;
register LpBIND         sourcePtr;
register LpBIND         insertPtr;
NUM                     insertIndex;
StartFrame
DeclareOBJ(TStructure,self);
EndFrame

self = (TStructure*)asObject(&selfTval);

/*  We only accept numeric indices. */
if ((_TObject_TypeFlag(asTag(&newObject)) == _TObject_TfTOBJECT) && (isNumIndex(&index)))
    insertIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure TStructure index is in range. */

if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Resize the TStructure up one position */

TStructure_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex+1);

/*  Move all of the remaining values in the TStructure up one position */

sourcePtr = &atHMBind(self->itsDictionaryArray,(self->itsMaxItemIndex-2));
targetPtr = &atHMBind(self->itsDictionaryArray,(self->itsMaxItemIndex-1));
insertPtr = &atHMBind(self->itsDictionaryArray,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
    
/*  Insert the new value in the TStructure at the specified position */

atHMBind(self->itsDictionaryArray,insertIndex).Value = newValue;
atHMBind(self->itsDictionaryArray,insertIndex).Key = asObject(&newObject);
    
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
isBound

Return the numeric indexed position of the specified key value, or return
false if the key is not present.

#endif

TVAL TStructure_isBound(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
TStructure*   self = (TStructure*)asObject(&selfTval);
NUM             indexOf;

if (_TObject_TypeFlag(asTag(&index1)) == _TObject_TfTOBJECT)
    {
    /*  We accept object indices */
    
    for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
        {
        if (atHMBind(self->itsDictionaryArray,indexOf).Key == asObject(&index1))
            return(TINT(indexOf));
        }
        
    /*  If the key does not exist, then return false. */
    
    return(gCP->Tval_FALSE);
    }

return(gCP->Tval_FALSE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TStructure_New

Create a new TStructure.

#endif

TStructure*   TStructure_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TStructure,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TStructure_Initialized) TStructure_Init(gCP,gTP);

self = (TStructure*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYSTRUCTURE;
self->itsMaxItemIndex = 0;
self->itsMethods = NULL;
self->itsCdr = gCP->Tval_VOID;
self->itsDictionaryArray = NULL;
self->itsImmediatePtr = NULL;
FrameExit(self);
}
