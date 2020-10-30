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

#define _C_TCONTINUATION
#define _SMARTBASE
#if 0
TContinuation.c

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tcontin.h"
#include "tlambda.h"
#include "fobject.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TContinuation_Init

Initialize the TContinuation class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TContinuation_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Don't initialize more than once. */
if (gCP->TContinuation_Initialized) return;
gCP->TContinuation_Initialized   = TRUE;
TObject_Init(gCP,gTP);

/*  Initialize the new type for this class. */

FSmartbase_NewType  (gCP,
					 gTP,
					 TYCONTINUATION,
                    (LpCHAR)"Continuation",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&TObject_NewNever,
                    &TContinuation_Mark,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapDefault,
                    &TObject_Mapc,
                    &TObject_PrintDefault,
                    &TContinuation_Load,
                    &TContinuation_Save,
					&TContinuation_ComputeSize,
					&TContinuation_Copy,
					&FObject_DoomNever);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TContinuation_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TContinuation_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory anHMemory, NUM theFileID, NUM bResolve)
{
StartFrame
DeclareOBJ(TContinuation,it);
DeclareTVAL(retTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TContinuation_New(gCP, gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TContinuation*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        it->itsResult = TObject_LoadTval(gCP,gTP,TContinuationOnDiskPtr(anHMemory,0)->itsResult);
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
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TContinuation_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TContinuation*      self = (TContinuation*)asObject(&selfTval);

/*  Mark the Continuation's itsResult so its won't be garbage collected. */

TObject_MarkTval(gCP,gTP,self->itsResult);

}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TContinuation_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;
*aSize += sizeof(TContinuationOnDisk);
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TContinuation_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory anHMemory)
{
long                    theOffset;
TContinuation*      self = (TContinuation*)asObject(&selfTval);

TObjectOnDiskPtr(anHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TContinuationOnDiskPtr(anHMemory,theOffset)->inScope = self->inScope;
TContinuationOnDiskPtr(anHMemory,theOffset)->itsResult = TObject_RegisterTval(gCP,gTP,self->itsResult);

return(anHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TContinuation.

#endif

TObject*    TContinuation_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TContinuation*      self = (TContinuation*)asObject(&selfTval);
StartFrame
DeclareOBJ(TContinuation,theCopy);
EndFrame

theCopy = TContinuation_New(gCP,gTP);

theCopy->inScope = self->inScope;
theCopy->itsResult = self->itsResult;

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Evaluate

Evaluate a Continuation object by placing its object id in the global Continuation
object and returning an error message so that current execution will halt until
the previous call_cc function captures the return code and resumes execution. 

#endif

TVAL    TContinuation_Evaluate(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM argc, TVAL argv[])
{
StartFrame
DeclareOBJ(TContinuation,self);
EndFrame

self = (TContinuation*)asObject(&selfTval);

/*  The current continuation must be nil and this continuation must be  */
/*  in scope or we have an error. */
/*  Note:   The Continuation expects one and only one argument. */

if ((gTP->TLambda_TheContinuation != NULL) || (self->inScope != TRUE))
     FrameExit(gCP->TObject_ERROR_BADCONTINUATION);
     
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

self->inScope = FALSE;

/*  Place self as the current continuation and return an error. */
/*  More:   The error message will halt execution and be passed up */
/*          the call chain until captured by the call_cc which created */
/*          self. Then execution will resume where it left off. */

self->itsResult = argv[0];
gTP->TLambda_TheContinuation = self;

FrameExit(gCP->TObject_ERROR_CONTINUATION);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TContinuation_New

Create a new TContinuation.

#endif

TContinuation*  TContinuation_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TContinuation,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TContinuation_Initialized) TContinuation_Init(gCP, gTP);

self = (TContinuation*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYCONTINUATION;
self->inScope = TRUE;
self->itsResult = gCP->Tval_VOID;
self->itsMaxItemIndex = 0;   
self->itsNilArray = (HMChar)&self->itsImmediatePtr;
self->itsImmediatePtr = NULL;
FrameExit(self);
}
