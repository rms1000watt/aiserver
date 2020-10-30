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

#define _C_TWORKSPACE
#define _SMARTBASE
#if 0
TWkSpace.c

Implementation of the Workspace class which stores a list of the active Smartviews. 
The manifest typing system together with C++ methods controls and size the data 
items and the array itself.

PARENT:             TObjVector

AUTHORS:            Michael F. Korns

#endif

#include "twkspace.h"
#include "fsmtbase.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TWorkspace_Init

Initialize the TWorkspace class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TWorkspace_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{

/*  Don't initialize more than once. */
if (gCP->TWorkspace_Initialized) return;
gCP->TWorkspace_Initialized  = TRUE;
TObject_Init(gCP,gTP);

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					 TYWORKSPACE,
					(LpCHAR)"Workspace",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TWorkspace_MakeWorkspace,
					&TObjVector_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TWorkspace_SetIV1,
					&TWorkspace_SetIV2,
					&FObject_SetIV3Never,
					&TWorkspace_GetIV1,
					&TWorkspace_GetIV2,
					&FObject_GetIV3Never,
					&TObject_MapDefault, 
					&TObject_Mapc,
					&TObject_PrintDefault,
					&TWorkspace_Load,
					&TObjVector_Save,
					&TObjVector_ComputeSize,
					&TObjVector_Copy,
					&TObjVector_Doomed);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TWorkspace_MakeWorkspace

The makeWorkspace Procedure creates a new Workspace. There are no arguments.  

#endif

TVAL TWorkspace_MakeWorkspace(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TWorkspace,workspace);
EndFrame

argv = argv; // NOOP to hide unused parameter warning message
/*  There must be no arguments. */

if (argc != 0 )
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Create a new Workspace. */

workspace = TWorkspace_New(gCP,gTP);
asTag(ret) = workspace->itsObjectType;
asObject(ret) = (TObject*)workspace;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TWorkspace_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TWorkspace_Load(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 cn;
NUM*                onDiskData;
StartFrame
DeclareOBJ(TWorkspace,it);
DeclareTVAL(retTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TWorkspace_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TWorkspace*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        FObject_SetMaxIndex(gCP,gTP,(TObject*)it,TObjVectorOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TObjVectorOnDiskPtr(aHMemory,0)->itsCdr);
                
        for(cn = 0, onDiskData = TObjVectorOnDiskData(aHMemory,0);cn < it->itsMaxItemIndex; cn++)
            {
            atHMObject(it->itsObjectArray,cn) = (TObject*)TObject_CheckRegistration(gCP,gTP,TObjVectorOnDiskPtr(aHMemory,0)->itsItemArray[cn]);
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
GetIV1

Return the indexed value from the Workspace object.

        [name:]         Returns the value of the named object.
        [integer]       Returns the value of the indexed object.

#endif

TVAL TWorkspace_GetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
StartFrame
DeclareTVAL(retValue);
EndFrame

selfTval = selfTval; // NOOP to hide unused parameter warning message
index1 = index1; // NOOP to hide unused parameter warning message
*retValue = TERROR( "!Indexing a workspace is not supported!" );
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV2

Return the indexed value from the Workspace object.

        [name:      value:]         Returns the value of the named object.
        [integer    value:]         Returns the value of the indexed object.
        [name:      viewer:]        Returns the viewer of the named object.
        [integer    viewer:]        Returns the viewer of the indexed object.
        [integer    name:]          Returns the name of the indexed object.

Note:   

#endif

TVAL TWorkspace_GetIV2(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2)
{
StartFrame
DeclareTVAL(retValue);
EndFrame

selfTval = selfTval; // NOOP to hide unused parameter warning message
index1 = index1; // NOOP to hide unused parameter warning message
index2 = index2; // NOOP to hide unused parameter warning message
*retValue = TERROR( "!Indexing a workspace is not supported!" );
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value from the Workspace object.

        [name:]         Sets the value of the named object.
        [integer]       Sets the value of the indexed object.

#endif

TVAL TWorkspace_SetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
StartFrame
DeclareOBJ(TWorkspace,self);
DeclareTVAL(index2);
DeclareTVAL(ret);
EndFrame

self = (TWorkspace*)asObject(&selfTval);

/*  Treat this as a request to set the value of the object. */

asTag(index2) = gCP->TSymbol_Value->itsObjectType;
asObject(index2) = (TObject*)gCP->TSymbol_Value;
*ret = TWorkspace_SetIV2(gCP,gTP,selfTval, index1,*index2,newValue);
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV2

Set the indexed value of the Workspace object.

        [name:      value:]         Sets the value of the named object.
        [integer    value:]         Sets the value of the indexed object.
        [name:      viewer:]        Sets the viewer of the named object.
        [integer    viewer:]        Sets the viewer of the indexed object.

#endif

TVAL TWorkspace_SetIV2(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2,TVAL newValue)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TWorkspace,self);
DeclareOBJ(TWorkspace,anObj);
DeclareTVAL(retValue);
EndFrame

newValue = newValue; // NOOP to hide unused parameter warning message
self = (TWorkspace*)asObject(&selfTval);

/*  The first argument must be either a symbolic or numeric index. */

if (asTag(&index1) == TYSYMBOL)
    {
	FrameExit(gCP->TObject_ERROR_BADINDEX);
    }

if (isNumIndex(&index1))
    {
    indexOf = asNumIndex(&index1);

    /*  Make sure index is in range. */
    
    if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
        FrameExit(gCP->TObject_ERROR_BADINDEX);
    
    anObj = (TWorkspace*)atHMObject(self->itsObjectArray,indexOf);
    asTag(retValue) = anObj->itsObjectType;
    asObject(retValue) = (TObject*)anObj;
    }
else
    FrameExit(gCP->TObject_ERROR_BADINDEX);
    
/*  Set either a value or the viewer. */
if  ((asTag(&index2) == TYSYMBOL) && (asSymbol(&index2) == gCP->TSymbol_Viewer))
    {
    FrameExit(gCP->TObject_ERROR_ACCESSDENIED);
    }
else
if  ((asTag(&index2) == TYSYMBOL) && (asSymbol(&index2) == gCP->TSymbol_Value))
    {
	FrameExit(gCP->TObject_ERROR_ACCESSDENIED);
    }

FrameExit(gCP->TObject_ERROR_BADINDEX);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FindIndexByObject

Return the index of the Smartview argument. 

Note:   

#endif

TVAL TWorkspace_FindIndexByObject(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TObject* anObj)
{
NUM                 index;
StartFrame
DeclareOBJ(TWorkspace,self);
DeclareTVAL(retValue);
EndFrame

self = (TWorkspace*)asObject(&selfTval);


/*  We search through the list of object's until we find a match. */

for (index = 0; index < self->itsMaxItemIndex; ++index)
    {
    if (anObj == atHMObject(self->itsObjectArray,index))
        {
        asTag(retValue) = TYNUM;
        asInt(retValue) = index;
        FrameExit(*retValue);
        }
    }

FrameExit(gCP->TObject_ERROR_BADINDEX);
}

/*--------------------------------------------------------------------------------------- */
#if 0
DeleteView

Delete the Smartview argument from the workspace. 

#endif

TVAL TWorkspace_DeleteView(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL viewId)
{
NUM                 intIndex;
StartFrame
DeclareOBJ(TWorkspace,self);
DeclareOBJ(TObject,anObj);
DeclareTVAL(WindowSym);
DeclareTVAL(argTval);
DeclareTVAL(retValue);
DeclareTVAL(index);
EndFrame

self = (TWorkspace*)asObject(&selfTval);

/*  If the viewId is a Symbol, we try to locate the View by name. */

if (asTag(&viewId) == TYSYMBOL)
    {
    *retValue = gCP->TObject_ERROR_BADINDEX;

    DeleteView:

    /*  Check to see if there is an Lambda to be shut down. */

    ExitOnError(*retValue);
    
    /*  Get the Workspace Lambda to be deleted. */
    
    *index = *retValue;
    intIndex = asNumIndex(index);
    anObj = atHMObject(self->itsObjectArray, intIndex);
    asTag(argTval) = anObj->itsObjectType;
    asObject(argTval) = anObj;
    
    /*  Delete the Lambda from the Workspace. */
    
    TObjVector_Delete(gCP,gTP,selfTval,*index);
    
    /*  Delete each different type of Workspace Lambda. */
    
	*WindowSym = TSYMBOL("window");
    goto CleanUp;
    }

/*  If the viewId is an integer, we delete the Smartview by index. */

if (asTag(&viewId) == TYNUM)
    {
    if(asInt(&viewId) >= 0 && asInt(&viewId) < self->itsMaxItemIndex)
        {
        *retValue = viewId;
        goto DeleteView;
        }
    }

/*  If the viewId is an object, we try to locate the Smartview by object id. */

if (_TObject_TypeFlag(asTag(&viewId)) == _TObject_TfTOBJECT )
    {
    *retValue = TWorkspace_FindIndexByObject(gCP,gTP,selfTval,asObject(&viewId));
    goto DeleteView;
    }

FrameExit(gCP->TObject_ERROR_BADINDEX);

CleanUp:
    

FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TWorkspace_New

Create a new TWorkspace.

#endif

TWorkspace* TWorkspace_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TWorkspace,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TWorkspace_Initialized) TWorkspace_Init(gCP,gTP);

self = (TWorkspace*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYWORKSPACE;
self->itsMaxItemIndex = 0;
self->itsObjectArray = NULL;
self->itsImmediatePtr = NULL;

FrameExit(self);
}
