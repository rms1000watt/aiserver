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

#define _C_TBRICK
#define _SMARTBASE

#if 0
TBrick.c

Implementation of the Brick class which stores a variable number of
fields in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
#include "ctype.h"		// tolower macro
#include "tobject.h"
#include "tbrick.h"
#include "tstruct.h"
#include "fconvert.h"
#include "fobject.h"
#include "tdiction.h"
#include "tdirect.h"
#include "fconio.h"
#include "tmatrix.h"
#include "tnummat.h"
#include "tcpx.h"
#include "tbytevec.h"
#include "tcpxvec.h"
#include "tshortvec.h"
#include "tlambda.h"
#include "fproc.h"
#include "tstring.h"


/*--------------------------------------------------------------------------------------- */
#if 0
TBrick_Init

Initialize the TBrick class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

TVAL	TBrick_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame

/*  Don't initialize more than once. */
if (gCP->TBrick_Initialized) FrameExit(gCP->Tval_TRUE);
gCP->TBrick_Initialized     = TRUE;

/*  Initialize the new types for this class. */
FSmartbase_NewType  (gCP,
					 gTP,
					TYBRICK,
					(LpCHAR)"Brick",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TBrick_MakeNew,
					&TBrick_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TBrick_SetIV1,
					&TBrick_SetIV2,
					&TBrick_SetIV3,
					&TBrick_GetIV1,
					&TBrick_GetIV2,
					&TBrick_GetIV3,
					&TObject_Map,
					&TObject_Mapc,
					&TObject_PrintObj,
					&TBrick_Load,
					&TBrick_Save,
					&TBrick_ComputeSize,
					&TBrick_Copy,
					&TBrick_Doomed);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYBRICKROW,
                    (LpCHAR)"BrickRow",
                    _TObject_TfIOBJECT,
                    sizeof(TVAL),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkTval,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &TBrickRowT_SetIV1,
                    &TBrickRowT_SetIV2,
                    &FObject_SetIV3Never,
                    &TBrickRowT_GetIV1,
                    &TBrickRowT_GetIV2,
                    &FObject_GetIV3Never,
                    &TObject_Map,
                    &TObject_Mapc,
                    &TBrickRowT_Print,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&TBrickRowT_ComputeSize,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYBRICKFIELD,
                    (LpCHAR)"BrickField",
                    _TObject_TfIOBJECT,
                    sizeof(TVAL),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkTval,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &TBrickFieldT_SetIV1,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &TBrickFieldT_GetIV1,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_Map,
                    &TObject_Mapc,
                    &TBrickFieldT_Print,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&TBrickFieldT_ComputeSize,
					&FObject_CopyNever,
					&FObject_DoomNever);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"symbolToTypeCode",(LpFUNC)&TBrick_SymbolToTypeCode);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"typeCode",(LpFUNC)&TBrick_TypeCode);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"deleteRows",(LpFUNC)&TBrick_DeleteRows);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"insertRows",(LpFUNC)&TBrick_InsertRows);
ExitOnError(*ret);

FrameExit(gCP->Tval_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TBrick_SymbolToTypeCode

This function accepts the symbolic name of a Smartbase engine built-in type and returns
the type code associated with the specified type name.

#endif

TVAL    TBrick_SymbolToTypeCode(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM         type;
LpCHAR      namePtr;
StartFrame
DeclareTVAL(ret);
EndFrame

/* Check the argument list for validity. */

if ((argc != 1) || ((argv[0].Tag != TYSTRING) && (argv[0].Tag != TYTEXT) && (argv[0].Tag != TYSYMBOL))) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Retrieve a pointer to the symbolic type name. */

namePtr = FSmartbase_ObjectPtr(gCP, gTP, &argv[0]);
if (namePtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Search through all the possible built-in type names. */

for (type = 0; type <= TYMAXVALIDTYPE; ++type)
	{ 	//TM add code to replace stricmp which is not always included in the run-time library.
		const char * s = (const char *)namePtr;
		const char * t = (const char *)_TObject_TypeName(type);
		for (; tolower(*s) == tolower(*t); s++, t++) //include ctype.h to get macro version.
		{	if (*s == '\0')
				goto FoundType;
		}
	//if (stricmp((const char *)namePtr,(const char *)_TObject_TypeName(type)) == 0) goto FoundType;
	}

*ret = TERROR("!symbolToTypeCode: no such built-in type name!");
FrameExit(*ret);

/*  Return the type code of the built-in type. */

FoundType:
ret->Tag = TYTYPE;
ret->u.Type = type;    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TBrick_TypeCode

This function accepts an integer or the symbolic name of a Smartbase engine 
built-in type and returns the type code associated with the specified type name.

#endif

TVAL    TBrick_TypeCode(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

/* Check the argument list for validity. */

if (argc != 1) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* If the type is an integer, convert it to a type code. */

if ((isNumIndex(&argv[0]) == TRUE) && (inRange(asNumIndex(&argv[0]),0,TYMAXVALIDTYPE)))
	{
	ret->Tag = TYTYPE;
	ret->u.Type = asNumIndex(&argv[0]);
	FrameExit(*ret);
	}

/*  Return the type code of the built-in type. */

*ret = TBrick_SymbolToTypeCode(gCP,gTP,argc,argv);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TBrick_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
StartFrame
DeclareOBJ(TBrick,self);
EndFrame

self     = (TBrick*)asObject(&selfTval);
*aSize = SIZEOF_TObjectOnDisk;

/* *********************************************************************** */
/* Note: Because the FieldList will NOT be loaded and therefore available  */
/*       to us upon load, we will have to store the field type and repeats */
/*       in the saved data stream. There is no other way to accomplish the */
/*       load operation because the FieldList will always load last.       */
/* *********************************************************************** */
*aSize += SIZEOF_TBrickOnDisk;
*aSize += (self->itsMaxItemIndex * sizeof(CHAR));
*aSize += (self->itsFieldCount * self->itsRowCount * (sizeof(NUM) + sizeof(CHAR)));
ALLIGNME(*aSize);
FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TBrick_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory)
{
long			theOffset;
NUM				cn;
NUM				rowCnt;
NUM				rowIndex;
NUM				repeatCnt;
NUM				repeatIndex;
NUM				fieldCnt;
NUM				fieldIndex;
LpCHAR			fieldArrayPtr;
LpBIND			bindPtr;
LpCHAR			chrArrayPtr;
LpSHORT			shortArrayPtr;
LpNUM32			longArrayPtr;
LpNUM			intArrayPtr;
LpFLOAT			floatArrayPtr;
LpREAL			realArrayPtr;
LpOBJ			objArrayPtr;
LpTVAL			wordArrayPtr;
StartFrame
DeclareOBJ(TBrick,self);
EndFrame

self     = (TBrick*)asObject(&selfTval);

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

TBrickOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TBrickOnDiskPtr(aHMemory,theOffset)->itsFieldCount = self->itsFieldCount;
TBrickOnDiskPtr(aHMemory,theOffset)->itsRowCount = self->itsRowCount;
TBrickOnDiskPtr(aHMemory,theOffset)->itsRowByteCount = self->itsRowByteCount;
TBrickOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TBrickOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);
TBrickOnDiskPtr(aHMemory,theOffset)->itsFieldList = TObject_RegisterTval(gCP,gTP,self->itsFieldList);

/*  Each Brick row is a repetition of the same field layout, and each      */
/*  row must be properly converted into disk objects in this record stream. */
       
cn = 0; 
rowCnt = self->itsRowCount;
for (rowIndex = 0; rowIndex < rowCnt; ++rowIndex)
	{
	bindPtr = BindArray(self->itsFieldList);
	fieldArrayPtr = asFieldArray(self)+(rowIndex*self->itsRowByteCount);
	fieldCnt = self->itsFieldCount;

	/* *********************************************************************** */
	/* Note: Because the FieldList will NOT be loaded and therefore available  */
	/*       to us upon load, we will have to store the field type and repeats */
	/*       in the saved data stream. There is no other way to accomplish the */
	/*       load operation because the FieldList will always load last.       */
	/* *********************************************************************** */
	for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		/* Use the Declared Type to determine marking behavior. */
		switch (bindPtr->Value.DeclaredType)
			{
			case TYBOLE:
				chrArrayPtr = (LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYBOLE;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = chrArrayPtr[repeatIndex];
					}
				break;

			case TYCHAR:
				chrArrayPtr = (LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYCHAR;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = chrArrayPtr[repeatIndex];
					}
				break;

			case TYDATE:
				realArrayPtr = (LpREAL)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYDATE;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpREAL)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = realArrayPtr[repeatIndex];cn+=sizeof(REAL);
					}
				break;

			case TYNUM:
			case TYCHARPOINTER:
			case TYFLOATPOINTER:
			case TYREALPOINTER:
			case TYJUMPPOINTER:
			case TYINTPOINTER:
			case TYSHORTPOINTER:
			case TYLONGPOINTER:
			case TYWORDPOINTER:
				intArrayPtr = (LpNUM)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYNUM;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = intArrayPtr[repeatIndex];cn+=sizeof(NUM);
					}
				break;

			case TYFLOAT:
				floatArrayPtr = (LpFLOAT)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYFLOAT;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpFLOAT)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = floatArrayPtr[repeatIndex];cn+=sizeof(FLOAT);
					}
				break;

			case TYMONEY:
				realArrayPtr = (LpREAL)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYREAL;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpREAL)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = realArrayPtr[repeatIndex];cn+=sizeof(REAL);
					}
				break;

			case TYREAL:
				realArrayPtr = (LpREAL)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYREAL;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpREAL)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = realArrayPtr[repeatIndex];cn+=sizeof(REAL);
					}
				break;

			case TYOBJ:
				objArrayPtr = (LpOBJ)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYOBJ;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = (NUM)TObject_RegisterObject(gCP,gTP,(TObject*)objArrayPtr[repeatIndex]);cn+=sizeof(NUM);
					}
				break;

			case TYSHORT:
				shortArrayPtr = (LpSHORT)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYSHORT;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpSHORT)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = shortArrayPtr[repeatIndex];cn+=sizeof(SHORT);
					}
				break;

			case TYLONG:
				longArrayPtr = (LpNUM32)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYLONG;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpNUM32)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = longArrayPtr[repeatIndex];cn+=sizeof(NUM32);
					}
				break;

			case TYTVAL:
				wordArrayPtr = (LpTVAL)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn++] = TYTVAL;
				*((LpNUM)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = repeatCnt;cn+=sizeof(NUM);
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					*((LpTVAL)&TBrickOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn]) = TObject_RegisterTval(gCP,gTP,wordArrayPtr[repeatIndex]);cn+=sizeof(TVAL);
					}
				break;		

			default:
				break;
			}

		++bindPtr;
		}

	}

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TBrick_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TBrick_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory anHMemory, NUM theFileID, NUM bResolve)
{
NUM				cn;
NUM				rowCnt;
NUM				rowIndex;
NUM				repeatCnt;
NUM				repeatIndex;
NUM				fieldCnt;
NUM				fieldIndex;
LpCHAR			fieldArrayPtr;
LpCHAR			chrArrayPtr;
LpSHORT			shortArrayPtr;
LpNUM32			longArrayPtr;
LpNUM			intArrayPtr;
LpFLOAT			floatArrayPtr;
LpREAL			realArrayPtr;
LpOBJ			objArrayPtr;
LpTVAL			wordArrayPtr;
CHAR			type;
StartFrame
DeclareOBJ(TBrick,it);
DeclareTVAL(retTval);
DeclareTVAL(itTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TBrick_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TBrick*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        itTval->u.Object = (TObject*)it;
        itTval->Tag = it->itsObjectType;
        
        TBrick_SetMaxIndex(gCP, gTP, *itTval, TBrickOnDiskPtr(anHMemory,0)->itsMaxItemIndex);

        it->itsFieldCount = TBrickOnDiskPtr(anHMemory,0)->itsFieldCount;
        it->itsRowCount = TBrickOnDiskPtr(anHMemory,0)->itsRowCount;
        it->itsRowByteCount = TBrickOnDiskPtr(anHMemory,0)->itsRowByteCount;
        
        it->itsCdr = TObject_LoadTval(gCP,gTP,TBrickOnDiskPtr(anHMemory,0)->itsCdr);
        it->itsFieldList = TObject_LoadTval(gCP,gTP,TBrickOnDiskPtr(anHMemory,0)->itsFieldList);
                
		cn = 0;
		rowCnt = it->itsRowCount;
		for (rowIndex = 0; rowIndex < rowCnt; ++rowIndex)
			{
			/* *********************************************************************** */
			/* Note: Because the FieldList will NOT be loaded and therefore available  */
			/*       to us upon load, we will have to store the field type and repeats */
			/*       in the saved data stream. There is no other way to accomplish the */
			/*       load operation because the FieldList will always load last.       */
			/* *********************************************************************** */
			fieldArrayPtr = asFieldArray(it)+(rowIndex*it->itsRowByteCount);
			fieldCnt = it->itsFieldCount;

			for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
				{
				type = TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn++];
				repeatCnt = *((LpNUM)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]);cn+=sizeof(NUM);
				/* Use the Declared Type to determine marking behavior. */
				switch (type)
					{
					case TYBOLE:
						chrArrayPtr = (LpCHAR)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							chrArrayPtr[repeatIndex] = TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn++];
							}
						fieldArrayPtr += (repeatCnt*sizeof(CHAR));
						break;

					case TYCHAR:
						chrArrayPtr = (LpCHAR)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							chrArrayPtr[repeatIndex] = TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn++];
							}
						fieldArrayPtr += (repeatCnt*sizeof(CHAR));
						break;

					case TYDATE:
						realArrayPtr = (LpREAL)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							realArrayPtr[repeatIndex] = *((LpREAL)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]);cn+=sizeof(REAL);
							}
						fieldArrayPtr += (repeatCnt*sizeof(REAL));
						break;

					case TYNUM:
					case TYCHARPOINTER:
					case TYFLOATPOINTER:
					case TYREALPOINTER:
					case TYJUMPPOINTER:
					case TYINTPOINTER:
					case TYSHORTPOINTER:
					case TYLONGPOINTER:
					case TYWORDPOINTER:
						intArrayPtr = (LpNUM)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							intArrayPtr[repeatIndex] = *((LpNUM)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]);cn+=sizeof(NUM);
							}
						fieldArrayPtr += (repeatCnt*sizeof(NUM));
						break;

					case TYFLOAT:
						floatArrayPtr = (LpFLOAT)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							floatArrayPtr[repeatIndex] = *((LpFLOAT)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]);cn+=sizeof(FLOAT);
							}
						fieldArrayPtr += (repeatCnt*sizeof(FLOAT));
						break;

					case TYMONEY:
						realArrayPtr = (LpREAL)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							realArrayPtr[repeatIndex] = *((LpREAL)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]);cn+=sizeof(REAL);
							}
						fieldArrayPtr += (repeatCnt*sizeof(REAL));
						break;

					case TYREAL:
						realArrayPtr = (LpREAL)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							realArrayPtr[repeatIndex] = *((LpREAL)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]);cn+=sizeof(REAL);
							}
						fieldArrayPtr += (repeatCnt*sizeof(REAL));
						break;

					case TYOBJ:
						objArrayPtr = (LpOBJ)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							objArrayPtr[repeatIndex] = (NUM)TObject_CheckRegistration(gCP,gTP,*((LpNUM)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]));cn+=sizeof(OBJ);
							}
						fieldArrayPtr += (repeatCnt*sizeof(OBJ));
						break;

					case TYSHORT:
						shortArrayPtr = (LpSHORT)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							shortArrayPtr[repeatIndex] = *((LpSHORT)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]);cn+=sizeof(SHORT);
							}
						fieldArrayPtr += (repeatCnt*sizeof(SHORT));
						break;

					case TYLONG:
						longArrayPtr = (LpNUM32)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							longArrayPtr[repeatIndex] = *((LpNUM32)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]);cn+=sizeof(NUM32);
							}
						fieldArrayPtr += (repeatCnt*sizeof(NUM32));
						break;

					case TYTVAL:
						wordArrayPtr = (LpTVAL)fieldArrayPtr;
						for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
							{
							wordArrayPtr[repeatIndex] = TObject_LoadTval(gCP,gTP,*((LpTVAL)&TBrickOnDiskPtr(anHMemory,0)->itsItemArray[cn]));cn+=sizeof(TVAL);
							}
						fieldArrayPtr += (repeatCnt*sizeof(TVAL));
						break;		

					default:
						break;
					}
				}
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
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TBrick_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
NUM				rowCnt;
NUM				rowIndex;
NUM				repeatCnt;
NUM				repeatIndex;
NUM				fieldCnt;
NUM				fieldIndex;
LpCHAR			fieldArrayPtr;
LpBIND			bindPtr;
LpOBJ			objArrayPtr;
LpTVAL			wordArrayPtr;
StartFrame
DeclareOBJ(TBrick,self);
EndFrame


self     = selfTval.u.Brick;

/*  Mark the Brick's Lisp tail(cdr) and field list so they won't be garbage collected. */

TObject_MarkTval(gCP,gTP,self->itsFieldList);
TObject_MarkTval(gCP,gTP,self->itsCdr);

rowCnt = self->itsRowCount;
for (rowIndex = 0; rowIndex < rowCnt; ++rowIndex)
	{
	/*  Use the Declared Type to determine marking behavior. */
	/*  Mark the Brick's field list so they won't be garbage collected. */

	if (self->itsFieldList.Tag != TYSTRUCTURE) goto Last;
	if (self->itsFieldList.u.Structure->itsMaxItemIndex <= 0) goto Last;
	bindPtr = BindArray(self->itsFieldList);
	if (bindPtr == NULL) goto Last;
	fieldArrayPtr = asFieldArray(self) + (rowIndex*self->itsRowByteCount);
	if (fieldArrayPtr == NULL) goto Last;
	fieldCnt = self->itsFieldCount;

	for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		/* Use the Declared Type to determine marking behavior. */
		switch (bindPtr->Value.DeclaredType)
			{
			case TYBOLE:
				break;

			case TYCHAR:
				break;

            case TYDATE:
                break;

			case TYNUM:
			case TYCHARPOINTER:
			case TYFLOATPOINTER:
			case TYREALPOINTER:
			case TYJUMPPOINTER:
			case TYINTPOINTER:
			case TYSHORTPOINTER:
			case TYLONGPOINTER:
			case TYWORDPOINTER:
				break;

			case TYFLOAT:
				break;

            case TYMONEY:
                break;

			case TYREAL:
				break;

			case TYOBJ:
				objArrayPtr = (LpOBJ)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					TObject_MarkObj(gCP,gTP,(TObject*)objArrayPtr[repeatIndex]);
					}
				break;

			case TYLONG:
				break;

			case TYSHORT:
				break;

			case TYTVAL:
				wordArrayPtr = (LpTVAL)(fieldArrayPtr+bindPtr->Value.Offset);
				repeatCnt = bindPtr->Value.Modifier;
				for (repeatIndex = 0; repeatIndex < repeatCnt; ++repeatIndex)
					{
					TObject_MarkTval(gCP,gTP,wordArrayPtr[repeatIndex]);
					}
				break;		

			default:
				break;
			}

		++bindPtr;
		}
	}


/*  We are finished marking this object. */

Last:
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
void    TBrick_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TBrick*    self     = (TBrick*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsFieldArray = NULL;
	self->itsImmediatePtr = NULL;
	self->itsFieldList.Tag = TYVOID;
	self->itsCdr.Tag = TYVOID;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsFieldArray);
self->itsMaxItemIndex = 0;																				 
self->itsFieldArray = NULL;
self->itsImmediatePtr = NULL;
self->itsFieldList.Tag = TYVOID;
self->itsCdr.Tag = TYVOID;
}


/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TBrick.

#endif

TObject*    TBrick_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TBrick,self);
DeclareOBJ(TBrick,theCopy);
DeclareTVAL(copyTval);
EndFrame

self = (TBrick*)selfTval.u.Object;

theCopy = TBrick_New(gCP,gTP);
copyTval->u.Object = (TObject*)theCopy;
copyTval->Tag = theCopy->itsObjectType;

TBrick_SetMaxIndex(gCP,gTP,*copyTval, self->itsMaxItemIndex);

theCopy->itsFieldList = self->itsFieldList;
theCopy->itsCdr = self->itsCdr;
theCopy->itsFieldCount = self->itsFieldCount;
theCopy->itsRowCount = self->itsRowCount;
theCopy->itsRowByteCount = self->itsRowByteCount;

if (self->itsFieldArray != NULL)
    {
    _FMemory_memcpy((LpCHAR)&atHMChar(theCopy->itsFieldArray,0),(LpCHAR)&atHMChar(self->itsFieldArray,0),(LONG)(self->itsMaxItemIndex*sizeof(CHAR)));
    }

FrameExit((TObject*)theCopy);
}


/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TBrick_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TBrick,self);
EndFrame

self     = (TBrick*)asObject(&selfTval);
FrameExit(self->itsMaxItemIndex);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note:   Rember the repeating portion of this object is measured in characters.

#endif

TVAL TBrick_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
StartFrame
DeclareOBJ(TBrick,self);
EndFrame

self     = (TBrick*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)_TBrick_ImmediateSpace)
	{
	if (self->itsFieldArray == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(CHAR));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,FieldArray(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(CHAR));
		if ((self->itsFieldArray != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsFieldArray);
		}
	self->itsFieldArray = (HMChar)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
/*  Note: We always allocate a hidden byte at the end */
/*        of the byte array in which we place a zero. */
/*        this prevents Bricks, treated as strings */
/*        from overrunning memory. */
if (self->itsFieldArray == NULL)
    {
    self->itsFieldArray = (HMChar)FMemory_New(gCP, gTP, (LONG)((newRepeats*sizeof(CHAR))+1),TRUE);
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsFieldArray = (HMChar)FMemory_New(gCP, gTP, (LONG)((newRepeats*sizeof(CHAR))+1),TRUE);
	_FMemory_memcpy(FieldArray(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(CHAR));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
    {
    self->itsFieldArray = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)self->itsFieldArray,(LONG)((newRepeats*sizeof(CHAR))+1));
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

TVAL TBrick_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM				fieldCnt;
NUM				fieldIndex;
LpCHAR			fieldArrayPtr;
LpBIND			bindPtr;

StartFrame
DeclareOBJ(TBrick,self);
DeclareTVAL(ret);
EndFrame

self = selfTval.u.Brick;
fieldCnt = self->itsFieldList.u.Structure->itsMaxItemIndex;

/*  We accept numeric indices. */
if (isNumIndex(&index1))
	{
	/* Depending on the no. of rows in the record, the numeric index will be handled accordingly */
	fieldIndex = asNumIndex(&index1);
	
	/*  Make sure array index is in range. */
	if (self->itsRowCount > 1)
	    {
	    /* fieldIndex is the Row Index */
	    if ((fieldIndex < 0) || (fieldIndex >= self->itsRowCount)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	    }
	else
	    {
	    /* fieldIndex is the Field Index */
	    if ((fieldIndex < 0) || (fieldIndex >= fieldCnt)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	    }
	}
else
/*  We accept symbolic indices. */
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	{
	/* Memoizing failed, so we must perform a search. */
	bindPtr = BindArray(self->itsFieldList);
	for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
    /* Field name not found */
	FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
	}
else
/*  We accept the elipses. */
if (index1.Tag == TYELLIPSES)
	{
    *ret = self->itsFieldList;
	FrameExit(*ret);
	}
else
/*  We accept FieldList requests. */
if (((index1.Tag == TYTEXT) && (strcmp(index1.u.Text,"FieldList") == 0)) ||
    ((index1.Tag == TYSTRING) && (strcmp(CharArray(index1),"FieldList") == 0)) ||
    ((index1.Tag == TYSTRINGSUBSTR) && (substrrcmp(TStringSubstringT_GetStringPtr(gCP, gTP, index1),SubLen(index1),"FieldList") == 0)))
	{
    *ret = self->itsFieldList;
	FrameExit(*ret);
	}
else
/*  We accept FieldCount requests. */
if (((index1.Tag == TYTEXT) && (strcmp(index1.u.Text,"FieldCount") == 0)) ||
    ((index1.Tag == TYSTRING) && (strcmp(CharArray(index1),"FieldCount") == 0)) ||
    ((index1.Tag == TYSTRINGSUBSTR) && (substrrcmp(TStringSubstringT_GetStringPtr(gCP, gTP, index1),SubLen(index1),"FieldCount") == 0)))
	{
    ret->u.Int = self->itsFieldCount;
    ret->Tag = TYNUM;
	FrameExit(*ret);
	}
else
/*  We accept RowCount requests. */
if (((index1.Tag == TYTEXT) && (strcmp(index1.u.Text,"RowCount") == 0)) ||
    ((index1.Tag == TYSTRING) && (strcmp(CharArray(index1),"RowCount") == 0)) ||
    ((index1.Tag == TYSTRINGSUBSTR) && (substrrcmp(TStringSubstringT_GetStringPtr(gCP, gTP, index1),SubLen(index1),"RowCount") == 0)))
	{
    ret->u.Int = self->itsRowCount;
    ret->Tag = TYNUM;
	FrameExit(*ret);
	}
else
/*  We accept ByteCount requests. */
if (((index1.Tag == TYTEXT) && (strcmp(index1.u.Text,"ByteCount") == 0)) ||
    ((index1.Tag == TYSTRING) && (strcmp(CharArray(index1),"ByteCount") == 0)) ||
    ((index1.Tag == TYSTRINGSUBSTR) && (substrrcmp(TStringSubstringT_GetStringPtr(gCP, gTP, index1),SubLen(index1),"ByteCount") == 0)))
	{
    ret->u.Int = self->itsMaxItemIndex;
    ret->Tag = TYNUM;
	FrameExit(*ret);
	}
else
/*  We accept RowByteCount requests. */
if (((index1.Tag == TYTEXT) && (strcmp(index1.u.Text,"RowByteCount") == 0)) ||
    ((index1.Tag == TYSTRING) && (strcmp(CharArray(index1),"RowByteCount") == 0)) ||
    ((index1.Tag == TYSTRINGSUBSTR) && (substrrcmp(TStringSubstringT_GetStringPtr(gCP, gTP, index1),SubLen(index1),"RowByteCount") == 0)))
	{
    ret->u.Int = self->itsRowByteCount;
    ret->Tag = TYNUM;
	FrameExit(*ret);
	}
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

if (self->itsRowCount > 1)
    {
    /* Brick size is greater than 1, return a BrickRow */
    ret->Tag = TYBRICKROW;
    asObjIdx(ret) = self->itsObjectIndex;
    asRowIdx(ret) = fieldIndex;
    }
else
    {
    /* Brick size is 1 */
    FieldNameFound:

    /* Use the Declared Type to return the proper value. */
    bindPtr = &BindArray(self->itsFieldList)[fieldIndex];
    
    /* NOTE: There is a special handling For the CHARACTER type */
    if (bindPtr->Value.Modifier > 1 && bindPtr->Value.DeclaredType != TYCHAR)
        {
        /* Repeat count is greater than 1, return a BrickField */
        ret->Tag = TYBRICKFIELD;
        asObjIdx(ret) = self->itsObjectIndex;
        asRowIdx(ret) = 0;
        asFldIdx(ret) = fieldIndex;
        }
    else
        {
        /* Repeat count is 1 */
        
        fieldArrayPtr = asFieldArray(self) + bindPtr->Value.Offset;
        
        switch (bindPtr->Value.DeclaredType)
            {
            case TYBOLE:
                ret->Tag = TYBOLE;
                ret->u.Bool = *((LpCHAR)fieldArrayPtr);
                break;

            case TYCHAR:
                if (bindPtr->Value.Modifier > 1)
                    {
                    ret->Tag = TYSTRINGSUBSTR;
                    asObjIdx(ret) = self->itsObjectIndex;
                    asSubOff(ret) = bindPtr->Value.Offset;
                    /* The presence of null-terminator is not assured */
                    if (fieldArrayPtr[bindPtr->Value.Modifier - 1] != 0)
                        asSubLen(ret) = bindPtr->Value.Modifier;
                    else
                        asSubLen(ret) = strlen((char*)fieldArrayPtr);
                    }
                else
                    {
                    ret->Tag = TYCHAR;
                    ret->u.Char = fieldArrayPtr[0];
                    }
                break;

            case TYDATE:
                ret->Tag = TYDATE;
                ret->u.Real = *((LpREAL)fieldArrayPtr);
                break;

            case TYNUM:
            case TYCHARPOINTER:
            case TYFLOATPOINTER:
            case TYREALPOINTER:
            case TYJUMPPOINTER:
            case TYINTPOINTER:
            case TYLONGPOINTER:
            case TYSHORTPOINTER:
            case TYWORDPOINTER:
                ret->Tag = TYNUM;
                ret->u.Int = *((LpNUM)fieldArrayPtr);
                break;

            case TYFLOAT:
                ret->Tag = TYREAL;
                ret->u.Real = *((LpFLOAT)fieldArrayPtr);
                break;

            case TYMONEY:
                ret->Tag = TYMONEY;
                ret->u.Real = *((LpREAL)fieldArrayPtr);
                break;

            case TYREAL:
                ret->Tag = TYREAL;
                ret->u.Real = *((LpREAL)fieldArrayPtr);
                break;

            case TYOBJ:
                ret->u.Object = *((TObject**)fieldArrayPtr);
                ret->Tag = (ret->u.Object == NULL) ? TYVOID : ret->u.Object->itsObjectType;
                break;

            case TYLONG:
                ret->Tag = TYNUM;
                ret->u.Int = *((LpNUM32)fieldArrayPtr);
                break;

            case TYSHORT:
                ret->Tag = TYNUM;
                ret->u.Int = *((LpSHORT)fieldArrayPtr);
                break;

            case TYTVAL:
                *ret = *((LpTVAL)fieldArrayPtr);
                break;		

            default:
                *ret = gCP->TObject_ERROR_BADCELL;
                break;
            } /* switch */
        } /* Repeat count is 1 */
    } /* Brick size is 1 */

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV2

Return the indexed value from the repeating portion of this object.

#endif

TVAL TBrick_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2)
{
NUM				repeatIndex;
NUM				fieldCnt;
NUM				fieldIndex;
LpCHAR			fieldArrayPtr;
LpBIND			bindPtr;
StartFrame
DeclareOBJ(TBrick,self);
DeclareTVAL(ret);
EndFrame

self     = selfTval.u.Brick;

/*  *************************  */
/*  Process the first index.   */
/*  *************************  */
if (isNumIndex(&index1))
	/*  We accept numeric indices. */
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure array index is in range. */
	if (fieldIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	if (fieldIndex >= self->itsFieldList.u.Brick->itsMaxItemIndex) FrameExit(gCP->TObject_VOID);
	}
else
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	fieldCnt = self->itsFieldList.u.Brick->itsMaxItemIndex;
	bindPtr = BindArray(self->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  *************************  */
/*  Process the second index.  */
/*  *************************  */
if (isNumIndex(&index2))
	/*  We accept numeric indices. */
	{
    repeatIndex = asNumIndex(&index2);

	/* Use the Declared Type to return the proper value. */
	bindPtr = &BindArray(self->itsFieldList)[fieldIndex];
	fieldArrayPtr = asFieldArray(self);
	switch (bindPtr->Value.DeclaredType)
		{
		case TYBOLE:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYBOLE;
			ret->u.Bool = ((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYCHAR:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYCHAR;
			ret->u.Char = ((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

        case TYDATE:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
            ret->Tag = TYDATE;
		    ret->u.Real = ((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
		    break;

		case TYNUM:
		case TYCHARPOINTER:
		case TYFLOATPOINTER:
		case TYREALPOINTER:
		case TYJUMPPOINTER:
		case TYINTPOINTER:
		case TYLONGPOINTER:
		case TYSHORTPOINTER:
		case TYWORDPOINTER:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpNUM)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYFLOAT:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYREAL;
			ret->u.Real = ((LpFLOAT)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYMONEY:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYMONEY;
			ret->u.Real = ((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYREAL:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYREAL;
			ret->u.Real = ((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYOBJ:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->u.Object = ((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			ret->Tag = (ret->u.Object == NULL) ? TYVOID : ret->u.Object->itsObjectType;
			break;

		case TYSHORT:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpSHORT)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYLONG:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpNUM32)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYTVAL:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			*ret = ((LpTVAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex];
			break;

		default:
			break;
		}

	FrameExit(*ret);
	}
else
if ((index2.Tag == TYSYMBOL) || (index2.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	bindPtr = &BindArray(self->itsFieldList)[fieldIndex];
	fieldArrayPtr = asFieldArray(self);
	if (strcmp(SymbolArray(index2),"type") == 0)
		/*  We accept symbolic indices. */
		{
		ret->u.Symbol = TSymbol_MakeUnique(gCP,gTP,_TObject_TypeName(bindPtr->Value.DeclaredType));
		ret->Tag = TYSYMBOL;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"repeats") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = bindPtr->Value.Modifier;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"offset") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = bindPtr->Value.Offset;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"pointer") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = (NUM)(fieldArrayPtr+bindPtr->Value.Offset);
		FrameExit(*ret);
		} 
	else
		/*  Everything else is an error. */
		FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}

/*  Everything else is an error. */
FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV3

Return the indexed value from the repeating portion of this object.

#endif

TVAL TBrick_GetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL index3)
{
NUM				repeatIndex;
NUM				fieldCnt;
NUM				fieldIndex;
NUM				rowIndex;
LpCHAR			fieldArrayPtr;
LpBIND			bindPtr;
StartFrame
DeclareOBJ(TBrick,self);
DeclareTVAL(ret);
EndFrame

self     = selfTval.u.Brick;

/*  *************************  */
/*  Process the third index.   */
/*  *************************  */
if (isNumIndex(&index3))
	/*  We accept numeric indices. */
	{
    rowIndex = asNumIndex(&index3);

	/*  Make sure the row index is in range. */
	if (rowIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	if (rowIndex >= self->itsRowCount) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  *************************  */
/*  Process the first index.   */
/*  *************************  */
if (isNumIndex(&index1))
	/*  We accept numeric indices. */
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure field index is in range. */
	if (fieldIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	if (fieldIndex >= self->itsFieldList.u.Brick->itsMaxItemIndex) FrameExit(gCP->TObject_VOID);
	}
else
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	fieldCnt = self->itsFieldList.u.Brick->itsMaxItemIndex;
	bindPtr = BindArray(self->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  *************************  */
/*  Process the second index.  */
/*  *************************  */
if (isNumIndex(&index2))
	/*  We accept numeric indices. */
	{
    repeatIndex = asNumIndex(&index2);

	/* Use the Declared Type to return the proper value. */
	bindPtr = &BindArray(self->itsFieldList)[fieldIndex];
	fieldArrayPtr = asFieldArray(self) + (rowIndex*self->itsRowByteCount) + bindPtr->Value.Offset;
	switch (bindPtr->Value.DeclaredType)
		{
		case TYBOLE:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYBOLE;
			ret->u.Bool = ((LpCHAR)(fieldArrayPtr))[repeatIndex];
			break;

		case TYCHAR:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYCHAR;
			ret->u.Char = ((LpCHAR)(fieldArrayPtr))[repeatIndex];
			break;

        case TYDATE:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
            ret->Tag = TYDATE;
		    ret->u.Real = ((LpREAL)(fieldArrayPtr))[repeatIndex];
		    break;

		case TYNUM:
		case TYCHARPOINTER:
		case TYFLOATPOINTER:
		case TYREALPOINTER:
		case TYJUMPPOINTER:
		case TYINTPOINTER:
		case TYSHORTPOINTER:
		case TYLONGPOINTER:
		case TYWORDPOINTER:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpNUM)(fieldArrayPtr))[repeatIndex];
			break;

		case TYFLOAT:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYREAL;
			ret->u.Real = ((LpFLOAT)(fieldArrayPtr))[repeatIndex];
			break;

		case TYMONEY:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYMONEY;
			ret->u.Real = ((LpREAL)(fieldArrayPtr))[repeatIndex];
			break;

		case TYREAL:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYREAL;
			ret->u.Real = ((LpREAL)(fieldArrayPtr))[repeatIndex];
			break;

		case TYOBJ:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->u.Object = ((TObject**)(fieldArrayPtr))[repeatIndex];
			ret->Tag = (ret->u.Object == NULL) ? TYVOID : ret->u.Object->itsObjectType;
			break;

		case TYSHORT:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpSHORT)(fieldArrayPtr))[repeatIndex];
			break;

		case TYLONG:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpNUM32)(fieldArrayPtr))[repeatIndex];
			break;

		case TYTVAL:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			*ret = ((LpTVAL)(fieldArrayPtr))[repeatIndex];
			break;

		default:
			break;
		}

	FrameExit(*ret);
	}
else
if ((index2.Tag == TYSYMBOL) || (index2.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	bindPtr = &BindArray(self->itsFieldList)[fieldIndex];
	fieldArrayPtr = asFieldArray(self)+(rowIndex*self->itsRowByteCount);
	if (strcmp(SymbolArray(index2),"type") == 0)
		/*  We accept symbolic indices. */
		{
		ret->u.Symbol = TSymbol_MakeUnique(gCP,gTP,_TObject_TypeName(bindPtr->Value.DeclaredType));
		ret->Tag = TYSYMBOL;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"repeats") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = bindPtr->Value.Modifier;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"offset") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = (rowIndex*self->itsRowByteCount)+bindPtr->Value.Offset;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"pointer") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = (NUM)(fieldArrayPtr+bindPtr->Value.Offset);
		FrameExit(*ret);
		} 
	else
		/*  Everything else is an error. */
		FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}

/*  Everything else is an error. */
FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif
TVAL TBrick_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM				fieldCnt;
NUM				fieldIndex;
NUM				lengthOf;
LpCHAR			fieldArrayPtr;
LpCHAR          temp;
LpBIND			bindPtr;

StartFrame
DeclareOBJ(TBrick,self);
EndFrame

self     = selfTval.u.Brick;

/*  We accept numeric indices. */
if (isNumIndex(&index1))
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure array index is in range. */
	if (fieldIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	if (fieldIndex >= self->itsFieldList.u.Brick->itsMaxItemIndex) FrameExit(gCP->TObject_VOID);
	}
else
/*  We accept symbolic indices. */
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	{
	fieldCnt = self->itsFieldList.u.Brick->itsMaxItemIndex;
	bindPtr = BindArray(self->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
	}
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    

/* Use the Declared Type to return the proper value. */
bindPtr = &BindArray(self->itsFieldList)[fieldIndex];
fieldArrayPtr = asFieldArray(self);
switch (bindPtr->Value.DeclaredType)
	{
	case TYBOLE:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag != TYBOLE) &&
			(newValue.Tag != TYCHAR) &&
			(newValue.Tag != TYNUM) &&
			(newValue.Tag != TYCHARPOINTER) &&
			(newValue.Tag != TYFLOATPOINTER) &&
			(newValue.Tag != TYJUMPPOINTER) &&
			(newValue.Tag != TYINTPOINTER) &&
			(newValue.Tag != TYLONGPOINTER) &&
			(newValue.Tag != TYSHORTPOINTER) &&
			(newValue.Tag != TYWORDPOINTER) &&
			(newValue.Tag != TYPOINTER))
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		*((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset)) = (CHAR)newValue.u.Int;
		break;

	case TYCHAR:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag == TYBOLE) ||
			(newValue.Tag == TYCHAR) ||
			(newValue.Tag == TYNUM) ||
			(newValue.Tag == TYCHARPOINTER) ||
			(newValue.Tag == TYFLOATPOINTER) ||
			(newValue.Tag == TYJUMPPOINTER) ||
			(newValue.Tag == TYINTPOINTER) ||
			(newValue.Tag == TYSHORTPOINTER) ||
			(newValue.Tag == TYLONGPOINTER) ||
			(newValue.Tag == TYWORDPOINTER) ||
			(newValue.Tag == TYPOINTER))
			{
			((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0] = (CHAR)newValue.u.Int;
			if (bindPtr->Value.Modifier > 1) ((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[1] = 0;
			}
		else
		if (newValue.Tag == TYSTRING) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = strlen(CharArray(newValue))))
				{
				strncpy(&((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0],CharArray(newValue),lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
				}
			}
		else
		if (newValue.Tag == TYSYMBOL) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = strlen(CharArray(newValue))))
				{
				strncpy(&((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0],SymbolArray(newValue),lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
				}
			}
		else
		if (newValue.Tag == TYBYTEVECTOR) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = newValue.u.ByteVector->itsMaxItemIndex))
				{
				FMemory_memcpy(gCP,gTP,(LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset),(LpCHAR)ByteArray(newValue),lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
				}
			}
		else
		if (newValue.Tag == TYTEXT) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = strlen(newValue.u.Text)))
				{
				strncpy(&((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0],newValue.u.Text,lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
				}
			}
		else
		if (newValue.Tag == TYSTRINGSUBSTR)
			{
			if (bindPtr->Value.Modifier >= (lengthOf = SubLen(newValue)))
				{
				temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
				if (temp == NULL)
				    FrameExit(gCP->TObject_ERROR_INVALID);
				
				strncpy(&((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0],temp,lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
				}
			}
		else		
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		break;

    case TYDATE:
	    if (newValue.Tag == TYVOID)
			newValue.u.Real = 0;
        else
		if ((newValue.Tag != TYREAL) &&
			(newValue.Tag != TYDATE) &&
			(newValue.Tag != TYMONEY))
			{
            FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
            }
        *((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset)) = (REAL)newValue.u.Real;
        break;

	case TYNUM:
	case TYCHARPOINTER:
	case TYFLOATPOINTER:
	case TYREALPOINTER:
	case TYJUMPPOINTER:
	case TYINTPOINTER:
	case TYLONGPOINTER:
	case TYSHORTPOINTER:
	case TYWORDPOINTER:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag != TYBOLE) &&
			(newValue.Tag != TYCHAR) &&
			(newValue.Tag != TYNUM) &&
			(newValue.Tag != TYCHARPOINTER) &&
			(newValue.Tag != TYFLOATPOINTER) &&
			(newValue.Tag != TYJUMPPOINTER) &&
			(newValue.Tag != TYINTPOINTER) &&
			(newValue.Tag != TYSHORTPOINTER) &&
			(newValue.Tag != TYLONGPOINTER) &&
			(newValue.Tag != TYWORDPOINTER) &&
			(newValue.Tag != TYPOINTER))
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		*((LpNUM)(fieldArrayPtr+bindPtr->Value.Offset)) = (NUM)newValue.u.Int;
		break;

	case TYFLOAT:
	    if (newValue.Tag == TYVOID)
			newValue.u.Real = 0;
		else
		if ((newValue.Tag != TYREAL) &&
			(newValue.Tag != TYDATE) &&
			(newValue.Tag != TYMONEY))
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		*((LpFLOAT)(fieldArrayPtr+bindPtr->Value.Offset)) = (FLOAT)newValue.u.Real;
		break;

	case TYMONEY:
	    if (newValue.Tag == TYVOID)
			newValue.u.Real = 0;
		else
		if ((newValue.Tag != TYREAL) &&
			(newValue.Tag != TYDATE) &&
			(newValue.Tag != TYMONEY))
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		*((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset)) = (REAL)newValue.u.Real;
		break;

	case TYREAL:
	    if (newValue.Tag == TYVOID)
			newValue.u.Real = 0;
		else
		if ((newValue.Tag != TYREAL) &&
			(newValue.Tag != TYDATE) &&
			(newValue.Tag != TYMONEY))
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		*((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset)) = (REAL)newValue.u.Real;
		break;

	case TYOBJ:
		if (newValue.Tag == TYVOID)
			{
			*((TObject**)(fieldArrayPtr+bindPtr->Value.Offset)) = NULL;
			}
		else
		if (newValue.Tag == TYTEXT)
			{
			*((TObject**)(fieldArrayPtr+bindPtr->Value.Offset)) = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)&newValue.u.Text[0]);
			}
		else
		if (newValue.Tag == TYSTRINGSUBSTR)
			{
			temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
			if (temp == NULL)
			    FrameExit(gCP->TObject_ERROR_INVALID);

			*((TObject**)(fieldArrayPtr+bindPtr->Value.Offset)) = (TObject*)TString_SubString_MakeUnique(gCP,gTP,temp,0,SubLen(newValue));
    		}
		else		
		if ((_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT) || (_TObject_TypeFlag(newValue.u.Object->itsObjectType) != _TObject_TfTOBJECT))
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		else
			{
			*((TObject**)(fieldArrayPtr+bindPtr->Value.Offset)) = (TObject*)newValue.u.Object;
			}
		break;

	case TYSHORT:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag != TYBOLE) &&
			(newValue.Tag != TYCHAR) &&
			(newValue.Tag != TYSHORT) &&
			(newValue.Tag != TYNUM) &&
			(newValue.Tag != TYCHARPOINTER) &&
			(newValue.Tag != TYFLOATPOINTER) &&
			(newValue.Tag != TYJUMPPOINTER) &&
			(newValue.Tag != TYINTPOINTER) &&
			(newValue.Tag != TYSHORTPOINTER) &&
			(newValue.Tag != TYLONGPOINTER) &&
			(newValue.Tag != TYWORDPOINTER) &&
			(newValue.Tag != TYPOINTER))
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		*((LpSHORT)(fieldArrayPtr+bindPtr->Value.Offset)) = (SHORT)newValue.u.Int;
		break;

	case TYLONG:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag != TYBOLE) &&
			(newValue.Tag != TYCHAR) &&
			(newValue.Tag != TYLONG) &&
			(newValue.Tag != TYNUM) &&
			(newValue.Tag != TYCHARPOINTER) &&
			(newValue.Tag != TYFLOATPOINTER) &&
			(newValue.Tag != TYJUMPPOINTER) &&
			(newValue.Tag != TYINTPOINTER) &&
			(newValue.Tag != TYLONGPOINTER) &&
			(newValue.Tag != TYSHORTPOINTER) &&
			(newValue.Tag != TYWORDPOINTER) &&
			(newValue.Tag != TYPOINTER))
			{
			FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
			}
		*((LpNUM32)(fieldArrayPtr+bindPtr->Value.Offset)) = (NUM32)newValue.u.Int;
		break;

	case TYTVAL:
		*((LpTVAL)(fieldArrayPtr+bindPtr->Value.Offset)) = newValue;
		break;		

	default:
		FrameExit(gCP->TObject_ERROR_BADIDXORKEY);		
		break;
	}

FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV2

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TBrick_SetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL newValue)
{
NUM				repeatIndex;
NUM				fieldCnt;
NUM				fieldIndex;
LpCHAR			fieldArrayPtr;
LpBIND			bindPtr;
LpCHAR          temp;

StartFrame
DeclareOBJ(TBrick,self);
EndFrame

self     = selfTval.u.Brick;

/*  *************************  */
/*  Process the first index.   */
/*  *************************  */
if (isNumIndex(&index1))
	/*  We accept numeric indices. */
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure array index is in range. */
	if (fieldIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	if (fieldIndex >= self->itsFieldList.u.Brick->itsMaxItemIndex) FrameExit(gCP->TObject_VOID);
	}
else
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	fieldCnt = self->itsFieldList.u.Brick->itsMaxItemIndex;
	bindPtr = BindArray(self->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  *************************  */
/*  Process the second index.  */
/*  *************************  */
if (isNumIndex(&index2))
	/*  We accept numeric indices. */
	{
    repeatIndex = asNumIndex(&index2);

	/* Use the Declared Type to return the proper value. */
	bindPtr = &BindArray(self->itsFieldList)[fieldIndex];
	fieldArrayPtr = asFieldArray(self);
	switch (bindPtr->Value.DeclaredType)
		{
		case TYBOLE:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (CHAR)newValue.u.Int;
			break;

		case TYCHAR:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (CHAR)newValue.u.Int;
			break;

        case TYDATE:
	        if (newValue.Tag == TYVOID)
			    newValue.u.Real = 0;
            else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
                FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
                }
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
            ((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
            break;

		case TYNUM:
		case TYCHARPOINTER:
		case TYFLOATPOINTER:
		case TYREALPOINTER:
		case TYJUMPPOINTER:
		case TYINTPOINTER:
		case TYSHORTPOINTER:
		case TYLONGPOINTER:
		case TYWORDPOINTER:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpNUM)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (NUM)newValue.u.Int;
			break;

		case TYFLOAT:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpFLOAT)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (FLOAT)newValue.u.Real;
			break;

		case TYMONEY:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
			break;

		case TYREAL:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
			break;

		case TYOBJ:
			if (newValue.Tag == TYVOID)
				{
				if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
				((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = NULL;
				}
			else
			if (newValue.Tag == TYTEXT)
				{
				if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
				((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)&newValue.u.Text[0]);
				}
			else
			if (newValue.Tag == TYSTRINGSUBSTR)
				{
				if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
				temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
				if (temp == NULL)
				    FrameExit(gCP->TObject_ERROR_INVALID);
				((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)TString_SubString_MakeUnique(gCP,gTP,temp,0,SubLen(newValue));
				}
			else			
			if ((_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT) || (_TObject_TypeFlag(newValue.u.Object->itsObjectType) != _TObject_TfTOBJECT))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			else
				{
				if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
				((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)newValue.u.Object;
				}
			break;

		case TYSHORT:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYSHORT) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpSHORT)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (SHORT)newValue.u.Int;
			break;

		case TYLONG:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYLONG) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpNUM32)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (NUM32)newValue.u.Int;
			break;

		case TYTVAL:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpTVAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = newValue;
			break;

		default:
			FrameExit(gCP->TObject_ERROR_BADIDXORKEY);		
			break;
		}

	FrameExit(selfTval);
	}

/*  Everything else is an error. */
FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV3

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TBrick_SetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL index3,TVAL newValue)
{
NUM				repeatIndex;
NUM				fieldCnt;
NUM				rowIndex;
NUM				fieldIndex;
NUM				lengthOf;
LpCHAR			fieldArrayPtr;
LpBIND			bindPtr;
LpCHAR          temp;

StartFrame
DeclareOBJ(TBrick,self);
EndFrame

self     = selfTval.u.Brick;

/*  *************************  */
/*  Process the third index.   */
/*  *************************  */
if (isNumIndex(&index3))
	/*  We accept numeric indices. */
	{
    rowIndex = asNumIndex(&index3);

	/*  Make sure the row index is in range. */
	if (rowIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	if (rowIndex >= self->itsRowCount) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  *************************  */
/*  Process the first index.   */
/*  *************************  */
if (isNumIndex(&index1))
	/*  We accept numeric indices. */
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure field index is in range. */
	if (fieldIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	if (fieldIndex >= self->itsFieldList.u.Brick->itsMaxItemIndex) FrameExit(gCP->TObject_VOID);
	}
else
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	fieldCnt = self->itsFieldList.u.Brick->itsMaxItemIndex;
	bindPtr = BindArray(self->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  *************************  */
/*  Process the second index.  */
/*  *************************  */
if (isNumIndex(&index2))
	/*  We accept numeric indices. */
	{
    repeatIndex = asNumIndex(&index2);

	/* Use the Declared Type to return the proper value. */
	bindPtr = &BindArray(self->itsFieldList)[fieldIndex];
	fieldArrayPtr = asFieldArray(self)+(rowIndex*self->itsRowByteCount);
	switch (bindPtr->Value.DeclaredType)
		{
		case TYBOLE:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (CHAR)newValue.u.Int;
			break;

		case TYCHAR:
			if (repeatIndex > 0)
				{
				if (newValue.Tag == TYVOID)
					newValue.u.Int = 0;
				else
				if ((newValue.Tag != TYBOLE) &&
					(newValue.Tag != TYCHAR) &&
					(newValue.Tag != TYNUM) &&
					(newValue.Tag != TYCHARPOINTER) &&
					(newValue.Tag != TYFLOATPOINTER) &&
					(newValue.Tag != TYJUMPPOINTER) &&
					(newValue.Tag != TYINTPOINTER) &&
					(newValue.Tag != TYSHORTPOINTER) &&
					(newValue.Tag != TYLONGPOINTER) &&
					(newValue.Tag != TYWORDPOINTER) &&
					(newValue.Tag != TYPOINTER))
					{
					FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
					}
				if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
				((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (CHAR)newValue.u.Int;
				}
			else
				{
				if (newValue.Tag == TYVOID)
					newValue.u.Int = 0;
				else
				if ((newValue.Tag == TYBOLE) ||
					(newValue.Tag == TYCHAR) ||
					(newValue.Tag == TYNUM) ||
					(newValue.Tag == TYCHARPOINTER) ||
					(newValue.Tag == TYFLOATPOINTER) ||
					(newValue.Tag == TYJUMPPOINTER) ||
					(newValue.Tag == TYINTPOINTER) ||
					(newValue.Tag == TYSHORTPOINTER) ||
					(newValue.Tag == TYLONGPOINTER) ||
					(newValue.Tag == TYWORDPOINTER) ||
					(newValue.Tag == TYPOINTER))
					{
					((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0] = (CHAR)newValue.u.Int;
					// if (bindPtr->Value.Modifier > 1) ((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[1] = 0;
					}
				else
				if (newValue.Tag == TYSTRING) 
					{
					if (bindPtr->Value.Modifier >= (lengthOf = strlen(CharArray(newValue))))
						{
						strncpy(&((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0],CharArray(newValue),lengthOf);
						if (bindPtr->Value.Modifier > lengthOf)
						    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
						}
					else
						{
						FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
						}
					}
				else
				if (newValue.Tag == TYSYMBOL) 
					{
					if (bindPtr->Value.Modifier >= (lengthOf = strlen(CharArray(newValue))))
						{
						strncpy(&((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0],SymbolArray(newValue),lengthOf);
						if (bindPtr->Value.Modifier > lengthOf)
						    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
						}
					else
						{
						FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
						}
					}
				else
				if (newValue.Tag == TYBYTEVECTOR) 
					{
					if (bindPtr->Value.Modifier >= (lengthOf = newValue.u.ByteVector->itsMaxItemIndex))
						{
						FMemory_memcpy(gCP,gTP,(LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset),(LpCHAR)ByteArray(newValue),lengthOf);
						if (bindPtr->Value.Modifier > lengthOf)
						    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
						}
					else
						{
						FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
						}
					}
				else
				if (newValue.Tag == TYTEXT) 
					{
					if (bindPtr->Value.Modifier >= (lengthOf = strlen(newValue.u.Text)))
						{
						strncpy(&((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0],newValue.u.Text,lengthOf);
						if (bindPtr->Value.Modifier > lengthOf)
						    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
						}
					else
						{
						FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
						}
					}
				else
				if (newValue.Tag == TYSTRINGSUBSTR) 
					{
					if (bindPtr->Value.Modifier >= (lengthOf = SubLen(newValue)))
						{
						temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
						if (temp == NULL)
						    FrameExit(gCP->TObject_ERROR_INVALID);

						strncpy(&((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[0],temp,lengthOf);
						if (bindPtr->Value.Modifier > lengthOf)
						    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
						}
					else
						{
						FrameExit(TERROR("!Brick.set: character data too long for this data type!"));
						}
					}					
				else
					{
					FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
					}
				}
			break;

        case TYDATE:
	        if (newValue.Tag == TYVOID)
			    newValue.u.Real = 0;
            else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
                FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
                }
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
            ((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
            break;

		case TYNUM:
		case TYCHARPOINTER:
		case TYFLOATPOINTER:
		case TYREALPOINTER:
		case TYJUMPPOINTER:
		case TYINTPOINTER:
		case TYSHORTPOINTER:
		case TYLONGPOINTER:
		case TYWORDPOINTER:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpNUM)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (NUM)newValue.u.Int;
			break;

		case TYFLOAT:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpFLOAT)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (FLOAT)newValue.u.Real;
			break;

		case TYMONEY:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
			break;

		case TYREAL:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
			break;

		case TYOBJ:
			if (newValue.Tag == TYVOID)
				{
				if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
				((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = NULL;
				}
			else
			if (newValue.Tag == TYTEXT)
				{
				if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
				((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)&newValue.u.Text[0]);
				}
			else
		    if (newValue.Tag == TYSTRINGSUBSTR)
		        {
			    if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
		        temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
		        if (temp == NULL)
		            FrameExit(gCP->TObject_ERROR_INVALID);
			    ((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)TString_SubString_MakeUnique(gCP,gTP,temp,0,SubLen(newValue));
		        }
		    else			
			if ((_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT) || (_TObject_TypeFlag(newValue.u.Object->itsObjectType) != _TObject_TfTOBJECT))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			else
				{
				if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
				((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)newValue.u.Object;
				}
			break;

		case TYSHORT:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYSHORT) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpSHORT)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (SHORT)newValue.u.Int;
			break;

		case TYLONG:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYLONG) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!Brick.set: no automatic type conversion for this data type!"));
				}
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpNUM32)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (NUM32)newValue.u.Int;
			break;

		case TYTVAL:
			if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			((LpTVAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = newValue;
			break;

		default:
			FrameExit(gCP->TObject_ERROR_BADIDXORKEY);		
			break;
		}

	FrameExit(selfTval);
	}

/*  Everything else is an error. */
FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetCdr

Set the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TBrick_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TBrick*    self     = (TBrick*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TBrick_New

Create a new TBrick.

#endif

TBrick* TBrick_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TBrick,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TBrick_Initialized) TBrick_Init(gCP,gTP);

self = (TBrick*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYBRICK;
self->itsMaxItemIndex = 0;
self->itsFieldArray = NULL;
self->itsFieldCount = NIL;
self->itsRowCount = NIL;
self->itsRowByteCount = NIL;
self->itsImmediatePtr = NULL;
self->itsFieldList = gCP->Tval_VOID;
self->itsCdr = gCP->Tval_VOID;
FrameExit(self);
}


/*--------------------------------------------------------------------------------------- */
/*

TBrick_MakeNew

Return a Brick object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note1:   The valid arguments are as follows. 
            
        (new Brick: {RowCount} FieldListStructure)
        (new Brick: {RowCount} FieldListVector)
        (new Brick: {RowCount} Name:Type:repeats ... Name:Type:repeats) 
        
Note2:   A proper Field List Structure will have the following format.

		The number of bindings will be equal to the number of fields defined.
		The Key of each binding will be the symbolic field name.
		The Value of each binding will be the symbolic name of the field type.
		The Value's DeclaredType element will be the integer field type. 
		The Value's Modifier element will be the number of field repeats. 
		The Value's Offset element will be the field offset into the FieldArray (byte array).
		
Note3:	Any one Brick may NOT exceed 65,535 bytes in length. 
            
Note4:   The valid symbolic types are as follows:

		Boolean					TYBOLE
		Character			    TYCHAR
		CharPointer				TYCHARPOINTER
		Date                    TYDATE
		Money                   TYMONEY
		Float					TYFLOAT
		FloatPointer			TYFLOATPOINTER
		Integer					TYNUM
		IntPointer				TYINTPOINTER
		JumpPointer				TYJUMPPOINTER
		Long					TYLONG
		LongPointer				TYLONGPOINTER
		Number					TYREAL
		NumPointer				TYNUMPOINTER
		Object					TYOBJ
		Short					TYSHORT
		ShortPointer			TYSHORTPOINTER
		Word			        TYTVAL
		WordPointer				TYWORDPOINTER

*/

TVAL TBrick_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM						recordByteSize;
NUM						rowCount;
NUM						rowByteCount;
NUM						fieldCount;
NUM						fieldIndex;
NUM						fieldStart;
NUM						wordCnt;
LpTVAL					wordPtr;
LpBIND					bindPtr;
StartFrame
DeclareTVAL(fieldList);
DeclareTVAL(record);
DeclareTVAL(ret);
DeclareOBJ(TBrick,self);
EndFrame
 
/*  The first argument may be the (optional) row count. */
if (argc < 1) goto BadFields;
if ((argc == 1) && (argv[0].Tag == TYBRICK))
    {
	/*  Manage a an existing record used as a template. */
	*record = FObject_Copy(gCP,gTP,argv[0]);
    FrameExit(*record);
    }
else
if (isNumIndex(&argv[0]) && (asNumIndex(&argv[0]) > 0)) 
	{
	rowCount = asNumIndex(&argv[0]);
	fieldStart = 1;
	}
else
	{
	rowCount = 1;
	fieldStart = 0;
	}
*ret = gCP->Tval_VOID;
    
/*  Manage each of the input options. */
    
if ((argc == (fieldStart+1)) && (argv[fieldStart].Tag == TYSTRUCTURE))
    {
	/*  Manage a single field list structure. */
	*fieldList = argv[fieldStart];
    goto ComputeBrickSize;
    }
else
if ((argc == (fieldStart+1)) && (argv[fieldStart].Tag == TYVECTOR))
    {
	/*  Manage a single field list vector. */
    wordCnt = argv[fieldStart].u.Vector->itsMaxItemIndex;
	if ((wordCnt <= 0) || ((wordCnt % 3) != 0)) 
		{
		BadFields:
		FrameExit(TERROR("!new: invalid arguments for Brick field list!"));
		}
	wordPtr = TvalArray(argv[fieldStart]);
    }
else
if ((argc >= (fieldStart+3)) && (((argc-fieldStart) % 3) == 0))
    {
	/*  Manage field list definitions in the argument vector. */
	wordCnt = argc-fieldStart;
	wordPtr = &argv[fieldStart];
	}
else
	goto BadFields;

/* Construct a new field list structure. */
fieldCount = wordCnt / 3;
fieldList->u.Structure = TStructure_New(gCP,gTP);
fieldList->Tag = fieldList->u.Structure->itsObjectType;
*ret = TStructure_SetMaxIndex(gCP,gTP,*fieldList,fieldCount);
ExitOnError(*ret);
bindPtr = BindArray(*fieldList);

/* Fill in the fields in the new field list structure. */
for (fieldIndex = 0; fieldIndex < fieldCount; ++fieldIndex)
	{
	/* Save the field name in the Structure Key element for this field binding. */
	if ((wordPtr->Tag != TYSYMBOL) && ((wordPtr->Tag != TYQUOTEDSYMBOL))) goto BadFields;
	bindPtr->Key = (wordPtr++)->u.Object;

	/* Save the type name in the Structure Value.u.Symbol element for this field binding. */
	if (((wordPtr)->Tag != TYSYMBOL) && ((wordPtr->Tag != TYQUOTEDSYMBOL))) goto BadFields;
	bindPtr->Value.u.Symbol = (wordPtr++)->u.Symbol;
	bindPtr->Value.Tag = bindPtr->Value.u.Object->itsObjectType;

	/* Save the field repeats in the Structure Value.Modifier element for this field binding. */
	if (!isNumIndex(wordPtr)) goto BadFields;
	bindPtr->Value.Modifier = asNumIndex(wordPtr);
	++wordPtr; 

	/* Save the field type code in the Structure Value.DeclaredType element for this field binding. */
	*ret = TBrick_SymbolToTypeCode(gCP,gTP,1,&bindPtr->Value);
	ExitOnError(*ret);
	bindPtr->Value.DeclaredType = ret->u.Type;
	++bindPtr;
	}


/* Compute the record byte array size from the field list structure. */
ComputeBrickSize:
recordByteSize = 0;
fieldCount = fieldList->u.Structure->itsMaxItemIndex;
bindPtr = BindArray(*fieldList);

/* Validate the fields in the field list structure. */
for (fieldIndex = 0; fieldIndex < fieldCount; ++fieldIndex)
	{
	/* Use the Declared Type to determine the field size. */
	switch (bindPtr->Value.DeclaredType)
		{
		case TYBOLE:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += bindPtr->Value.Modifier;
			break;

		case TYCHAR:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += bindPtr->Value.Modifier;
			break;

        case TYDATE:
            bindPtr->Value.Offset = recordByteSize;
            recordByteSize += (bindPtr->Value.Modifier*sizeof(REAL));
            break;

		case TYNUM:
		case TYCHARPOINTER:
		case TYFLOATPOINTER:
		case TYREALPOINTER:
		case TYJUMPPOINTER:
		case TYINTPOINTER:
		case TYSHORTPOINTER:
		case TYLONGPOINTER:
		case TYWORDPOINTER:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += (bindPtr->Value.Modifier*sizeof(NUM));
			break;

		case TYFLOAT:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += (bindPtr->Value.Modifier*sizeof(FLOAT));
			break;

		case TYMONEY:
		    bindPtr->Value.Offset = recordByteSize;
		    recordByteSize += (bindPtr->Value.Modifier*sizeof(REAL));
		    break;

		case TYREAL:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += (bindPtr->Value.Modifier*sizeof(REAL));
			break;

		case TYOBJ:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += (bindPtr->Value.Modifier*sizeof(OBJ));
			break;

		case TYLONG:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += (bindPtr->Value.Modifier*sizeof(NUM32));
			break;

		case TYSHORT:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += (bindPtr->Value.Modifier*sizeof(SHORT));
			break;

		case TYTVAL:
			bindPtr->Value.Offset = recordByteSize;
			recordByteSize += (bindPtr->Value.Modifier*sizeof(TVAL));
			break;		

		default:
			goto BadFields;
			break;
		}

	/* Perform field validity checks and move to next field definition. */

	if (bindPtr->Key->itsObjectType != TYSYMBOL) goto BadFields;
	if (bindPtr->Value.Tag != TYSYMBOL) goto BadFields;
	if (bindPtr->Value.Modifier <= 0) goto BadFields;
	if ((recordByteSize < 0) || (recordByteSize > 65564)) goto BadFields;
	++bindPtr;
	}

/* Create the Brick object. */   
self = TBrick_New(gCP,gTP);
record->u.Brick = self;
record->Tag = self->itsObjectType;
self->itsFieldCount = fieldCount;
self->itsRowByteCount = rowByteCount = recordByteSize;
self->itsRowCount = rowCount;
TBrick_SetMaxIndex(gCP,gTP,*record,(rowCount*recordByteSize));
self->itsFieldList = *fieldList;
self->itsCdr = gCP->Tval_VOID;
    
FrameExit(*record);
}

/*--------------------------------------------------------------------------------------- */
#if 0

TBrick_DeleteRows

The deleteRows Function deletes the specified number of old rows including the specified row.

Note1:  The valid arguments are as follows. 
            
        (deleteRows  aBrick  deleteThisRow  rowCount)
        
#endif

TVAL TBrick_DeleteRows(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     startRow;
NUM                     remainingRow;
NUM                     rowCount;
NUM                     n;
NUM                     N;
LpCHAR					destPtr;
LpCHAR					fromPtr;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TBrick,self);
EndFrame
 
/*  The first argument must be the Brick object. */
if ((argc != 3) || (argv[0].Tag != TYBRICK))
	{
	BadFields:
	FrameExit(TERROR("!deleteRows: invalid arguments for Brick field list!"));
	}
self = argv[0].u.Brick;
*ret = gCP->Tval_VOID;
    
/*  The second argument must be the starting row. */
if (!isNumIndex(&argv[1])) goto BadFields;
startRow = asNumIndex(&argv[1]);
if ((startRow < 0) || (startRow >= self->itsRowCount)) goto BadFields;
    
/*  The third argument must be the row count. */
if (!isNumIndex(&argv[2])) goto BadFields;
rowCount = asNumIndex(&argv[2]);
if ((startRow + rowCount) > self->itsRowCount) rowCount = self->itsRowCount - startRow;
    
/*  Move upper remaining rows down (if necessary). */
    
if ((startRow + rowCount) < self->itsRowCount)
    {
	remainingRow = startRow + rowCount;
	fromPtr = (LpCHAR)((NUM)asFieldArray(self) + (NUM)(remainingRow * self->itsRowByteCount));
	destPtr = (LpCHAR)((NUM)asFieldArray(self) + (NUM)(startRow * self->itsRowByteCount));
	N = self->itsRowByteCount * (self->itsRowCount - remainingRow);
	for (n = 0; n < N; ++n )
		{
		destPtr[n] = fromPtr[n];
		}
    }

/* Adjust the Brick object. */ 
  
self->itsRowCount -= rowCount;
TBrick_SetMaxIndex(gCP,gTP,argv[0],(self->itsRowCount*self->itsRowByteCount));
FrameExit(argv[0]);
}


/*--------------------------------------------------------------------------------------- */
#if 0

TBrick_InsertRows

The insertRows Function inserts the specified number of new rows just before the specified row.

Note1:  The valid arguments are as follows. 
            
        (insertRows  aBrick  beforeThisRow  rowCount)
        
#endif

TVAL TBrick_InsertRows(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     startRow;
NUM                     rowCount;
NUM                     oldRowCount;
NUM                     n;
NUM                     N;
LpCHAR					destPtr;
LpCHAR					fromPtr;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TBrick,self);
EndFrame
 
/*  The first argument must be the Brick object. */
if ((argc != 3) || (argv[0].Tag != TYBRICK))
	{
	BadFields:
	FrameExit(TERROR("!insertRows: invalid arguments for Brick field list!"));
	}
self = argv[0].u.Brick;
*ret = gCP->Tval_VOID;
    
/*  The second argument must be the starting row. */
if (!isNumIndex(&argv[1])) goto BadFields;
startRow = asNumIndex(&argv[1]);
if ((startRow < 0) || (startRow > self->itsRowCount)) goto BadFields;
    
/*  The third argument must be the row count. */
if (!isNumIndex(&argv[2])) goto BadFields;
rowCount = asNumIndex(&argv[2]);
    
/* Add space for the additional rows to the Brick object. */ 

oldRowCount = self->itsRowCount;
self->itsRowCount += rowCount;
TBrick_SetMaxIndex(gCP,gTP,argv[0],(self->itsRowCount*self->itsRowByteCount));

/*  Move existing rows up to make room for the inserted rows (if necessary). */

if (startRow < oldRowCount)
	{    
	fromPtr = (LpCHAR)((NUM)asFieldArray(self) + (NUM)(startRow * self->itsRowByteCount));
	destPtr = (LpCHAR)((NUM)asFieldArray(self) + (NUM)((startRow + rowCount) * self->itsRowByteCount));
	N = self->itsRowByteCount * (oldRowCount - startRow);
	for (n = 0; n < N; ++n )
		{
		destPtr[n] = fromPtr[n];
		}
	}

/*  Clear newly inserted rows. */

destPtr = (LpCHAR)((NUM)asFieldArray(self) + (NUM)(startRow * self->itsRowByteCount));
N = self->itsRowByteCount * rowCount;
for (n = 0; n < N; ++n )
	{
	destPtr[n] = 0;
	}

/* Return the Brick object. */ 
FrameExit(argv[0]);
}

/*--------------------------------------------------------------------------------------- */

#if 0
Print

Convert a Brick object into an ascii string and append it to an output buffer. 
Following the extended tradition, the Structure object is displayed as a series of 
elements as follows: 

#endif

TVAL TBrick_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
TBrick*   self = selfTval.u.Brick; 
return TBrick_PrintBrick(gCP,gTP, self, size, buf, FALSE, 0);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TBrick_PrintBrick

Convert a Brick object into an ascii string and append it to an output buffer.
The Brick object is displayed as follows:

#(rec(FieldName:FieldType:Repeat FieldName:FieldType:Repeat ...)| FieldName:RepeatIndex:Value FieldName:RepeatIndex:Value ...)
or

#(rec(FieldName:FieldType:1 FieldName:FieldType:1 ...)| FieldName:Value FieldName:Value ...)
or

#(rec(RowCount FieldName:FieldType:Repeat FieldName:FieldType:1 ...)| [0](FieldName:RepeatIndex:Value FieldName:Value ...) [RowIndex](FieldName:RepeatIndex:Value FieldName:Value ...) ...)

Notes:
For single row records, the RowCount will not be displayed.
For single row records, the [RowIndex](...) notation will not be used.
For single repeat fields, the RepeatIndex will not be displayed.

#endif

TVAL TBrick_PrintBrick(LpXCONTEXT gCP,LpTHREAD gTP,TBrick* theBrick, LpNUM size, LpCHAR buf, BOLE PrintingBinding, NUM PrintIndex )
{
NUM                     indexOf;
NUM                     startIndex;
NUM                     endIndex;
TStructure*				fieldList = theBrick->itsFieldList.u.Structure;
LpBIND					bindPtr = NULL;

LpCHAR                  fieldArrayPtr;
NUM                     rowIndex;
NUM                     rowCnt;
NUM                     fieldIndex;
NUM                     fieldCnt;
NUM                     repeatCnt;
NUM                     repeatIndex;

CHAR                    charValue;

StartFrame
DeclareTVAL(ec);
DeclareTVAL(val);
DeclareTVAL(field);
EndFrame

/*  Quit if the output string is already too long */

if ((*size) + 7 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
/*  Show Brick prefix */

buf[*size]      = '#';
buf[++(*size)]  = '(';
buf[++(*size)]  = 'b';
buf[++(*size)]  = 'r';
buf[++(*size)]  = 'k';
buf[++(*size)]  = '(';
buf[++(*size)]  = 0;

/*  Show all or part of the Brick's field list */

if(PrintingBinding == FALSE)
    {
    startIndex = 0;
    endIndex = fieldList->itsMaxItemIndex;
    }
else
    {
    startIndex = PrintIndex;
    endIndex = startIndex + 1;
    }

/* Show record row count */
if (theBrick->itsRowCount > 1)
    {
    *ec = FConio_sprintn(gCP,gTP,buf,size,TINT(theBrick->itsRowCount));
    ExitOnError(*ec);

    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
    buf[*size]      = ' ';
    buf[++(*size)]  = 0;
    }

bindPtr = (LpBIND)*(fieldList->itsDictionaryArray);

for(indexOf = startIndex; indexOf < endIndex; indexOf++)
    {
    asObject(val) = bindPtr->Key;
    asTag(val) = asObject(val)->itsObjectType;
    
    /* Brick: Field Name */
    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
    ExitOnError(*ec);

    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);

    buf[*size]  = ':';
    buf[++(*size)]  = 0;
    
    /* Brick: Field Type */
    *ec = FConio_sprintn(gCP,gTP,buf,size,bindPtr->Value);
    ExitOnError(*ec);

    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);

    buf[*size]  = ':';
    buf[++(*size)]  = 0;

    /* Brick: Repeats */
    *ec = FConio_sprintn(gCP,gTP,buf,size,TINT(bindPtr->Value.Modifier));
    ExitOnError(*ec);

    if (indexOf != fieldList->itsMaxItemIndex - 1)
        {
        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);

        buf[*size]      = ' ';
        buf[++(*size)]  = 0;
        }

    ++bindPtr;
    }

/* Show Brick field list suffix */

if ((*size) + 3 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

buf[*size]      = ')';
buf[++(*size)]  = '|';
buf[++(*size)]  = 0;

rowCnt = theBrick->itsRowCount;
fieldCnt = theBrick->itsFieldCount;

for(rowIndex = 0; rowIndex < rowCnt; rowIndex++)
    {
    /* move to the beginning of the row data */
    fieldArrayPtr = asFieldArray(theBrick) + (rowIndex*theBrick->itsRowByteCount);

    if (rowCnt > 1)
        {
        if ((*size) + 3 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ' ';
        buf[++(*size)]  = '[';
        buf[++(*size)]  = 0;

        *ec = FConio_sprintn(gCP,gTP,buf,size,TINT(rowIndex));
        ExitOnError(*ec);

        if ((*size) + 3 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ']';
        buf[++(*size)]  = '(';
        buf[++(*size)]  = 0;
        }
    else
        {
        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ' ';
        buf[++(*size)]  = 0;
        }

    for(fieldIndex = 0; fieldIndex < fieldCnt; fieldIndex++)
        {
        /* get pointer to the field information */
        bindPtr = &BindArray(theBrick->itsFieldList)[fieldIndex];
        
        /* get the number of repeats */
        repeatCnt = bindPtr->Value.Modifier;

        /* special handling for repeating characters */
        if (bindPtr->Value.DeclaredType == TYCHAR)
            {
            /* display the field name */
            asObject(field) = bindPtr->Key;
            asTag(field) = asObject(field)->itsObjectType;
            *ec = FConio_sprintn(gCP,gTP,buf,size, *field);
            ExitOnError(*ec);

            if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                FrameExit(gCP->TObject_FALSE);
            buf[*size]      = ':';
            buf[++(*size)]  = 0;

            if (repeatCnt > 1)
                {
				val->u.String = TString_SubString_MakeUnique(gCP,gTP,(LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset),0,bindPtr->Value.Modifier);
				val->Tag = TYSTRING;
                *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                ExitOnError(*ec);
                }
            else
                {
                charValue = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[0];
                /* *ec = FConio_sprintn(gCP,gTP,buf,size, TCHAR(charValue)); */
                *ec = FConio_sprintn(gCP,gTP,buf,size, TCHAR((NUM)charValue));
                ExitOnError(*ec);
                }
            } /* special handling for character type */
        else
            {
            /* handle repeating items */
            for(repeatIndex = 0; repeatIndex < repeatCnt; repeatIndex++)
                {
                /* display the field name */
                asObject(field) = bindPtr->Key;
                asTag(field) = asObject(field)->itsObjectType;
                *ec = FConio_sprintn(gCP,gTP,buf,size, *field);
                ExitOnError(*ec);

                if (repeatCnt > 1)
                    {
                    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                        FrameExit(gCP->TObject_FALSE);
                    buf[*size]      = '[';
                    buf[++(*size)]  = 0;

                    *ec = FConio_sprintn(gCP,gTP,buf,size, TINT(repeatIndex));
                    ExitOnError(*ec);

                    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                        FrameExit(gCP->TObject_FALSE);
                    buf[*size]      = ']';
                    buf[++(*size)]  = 0;
                    }

                if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                    FrameExit(gCP->TObject_FALSE);
                buf[*size]      = ':';
                buf[++(*size)]  = 0;

	            switch (bindPtr->Value.DeclaredType)
		            {
		            case TYBOLE:
                        charValue = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, TBOOL(charValue));
                        ExitOnError(*ec);
			            break;

                    case TYDATE:
                        asTag(val) = TYDATE;
                        asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
                        break;

		            case TYNUM:
		            case TYCHARPOINTER:
		            case TYFLOATPOINTER:
		            case TYREALPOINTER:
		            case TYJUMPPOINTER:
		            case TYINTPOINTER:
		            case TYSHORTPOINTER:
		            case TYLONGPOINTER:
		            case TYWORDPOINTER:
                        asTag(val) = TYNUM;
                        asInt(val) = ((LpNUM)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
			            break;

		            case TYFLOAT:
                        asTag(val) = TYREAL;
                        asReal(val) = ((LpFLOAT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
                        break;

		            case TYMONEY:
                        asTag(val) = TYMONEY;
                        asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
                        break;

		            case TYREAL:
                        asTag(val) = TYREAL;
                        asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
                        break;

		            case TYOBJ:
                        asObject(val) = ((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        asTag(val) = (asObject(val) == NULL) ? TYVOID : asObject(val)->itsObjectType;
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
		                break;

		            case TYSHORT:
                        asTag(val) = TYNUM;
                        asInt(val) = ((LpSHORT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
			            break;

		            case TYLONG:
                        asTag(val) = TYNUM;
                        asInt(val) = ((LpNUM32)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
			            break;

		            case TYTVAL:
                        asTag(val) = TYTVAL;
                        *val = ((LpTVAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                        *ec = FConio_sprintn(gCP,gTP,buf,size, *val);
                        ExitOnError(*ec);
			            break;
		            }

                if (repeatIndex != repeatCnt - 1)
                    {
                    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                        FrameExit(gCP->TObject_FALSE);
                    buf[*size]      = ' ';
                    buf[++(*size)]  = 0;
                    }
                } /* repeating index */
            } /* handling for non-character types */

        if (fieldIndex != fieldCnt - 1)
            {
            if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                FrameExit(gCP->TObject_FALSE);

            buf[*size]      = ' ';
            buf[++(*size)]  = 0;
            }
        } /* handling for fields */

    if (rowCnt > 1)
        {
        /* Show Brick Row suffix */
        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ')';
        buf[++(*size)]  = 0;
        }
    } /* handling for rows */

/*  Show Brick suffix */

if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = ')';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void TBrickField_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk + SIZEOF_TBrickFieldOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void TBrickFieldT_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk + SIZEOF_TBrickFieldOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TBrickFieldT_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM         repeatIndex = 0;
NUM         repeatCnt = 0;
LpBIND      bindPtr = NIL;
LpCHAR      fieldArrayPtr = NIL;
TBrick*    recordObj = NIL;

StartFrame
DeclareTVAL(ret);
EndFrame

/* Get pointer to Brick object */
recordObj = (TBrick*)_TObject_MainObjectList(ObjIdx(selfTval));

/* Make sure the object is valid */
if ((recordObj == NIL) || (recordObj->itsObjectType != TYBRICK))
    FrameExit(gCP->TObject_ERROR_INVALID);

bindPtr = &BindArray(recordObj->itsFieldList)[FldIdx(selfTval)];
repeatCnt = bindPtr->Value.Modifier;
fieldArrayPtr = asFieldArray(recordObj) + (RowIdx(selfTval) * recordObj->itsRowByteCount);

/* ***************** */
/* Process 1st Index */
/* ***************** */
if (isNumIndex(&index1))
	/*  We accept numeric indices. */
	{
    repeatIndex = asNumIndex(&index1);

	/*  Make sure array index is in range. */
	if (repeatIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    if (repeatIndex >= repeatCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	if (strcmp(SymbolArray(index1),"type") == 0)
		/*  We accept symbolic indices. */
		{
		ret->u.Symbol = TSymbol_MakeUnique(gCP,gTP,_TObject_TypeName(bindPtr->Value.DeclaredType));
		ret->Tag = TYSYMBOL;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index1),"repeats") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = bindPtr->Value.Modifier;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index1),"offset") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = bindPtr->Value.Offset;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index1),"pointer") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = (NUM)(fieldArrayPtr + bindPtr->Value.Offset);
		FrameExit(*ret);
		} 
	else
		/*  Everything else is an error. */
		FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

switch (bindPtr->Value.DeclaredType)
	{
	case TYBOLE:
		ret->Tag = TYBOLE;
		ret->u.Bool = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		break;

    case TYDATE:
        ret->Tag = TYDATE;
	    ret->u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
	    break;

	case TYNUM:
	case TYCHARPOINTER:
	case TYFLOATPOINTER:
	case TYREALPOINTER:
	case TYJUMPPOINTER:
	case TYINTPOINTER:
	case TYLONGPOINTER:
	case TYSHORTPOINTER:
	case TYWORDPOINTER:
		ret->Tag = TYNUM;
		ret->u.Int = ((LpNUM)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		break;

	case TYFLOAT:
		ret->Tag = TYREAL;
		ret->u.Real = ((LpFLOAT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		break;

	case TYMONEY:
		ret->Tag = TYMONEY;
		ret->u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		break;

	case TYREAL:
		ret->Tag = TYREAL;
		ret->u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		break;

	case TYOBJ:
		ret->u.Object = ((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		ret->Tag = (ret->u.Object == NULL) ? TYVOID : ret->u.Object->itsObjectType;
		break;

	case TYSHORT:
		ret->Tag = TYNUM;
		ret->u.Int = ((LpSHORT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		break;

	case TYLONG:
		ret->Tag = TYNUM;
		ret->u.Int = ((LpNUM32)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		break;

	case TYTVAL:
		*ret = ((LpTVAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		break;

	default:
        *ret = gCP->TObject_ERROR_BADCELL;
		break;
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TBrickFieldT_Print

Convert a BrickFieldT object into an ascii string and append it to an output buffer. 
Following the extended tradition, the BrickField object is displayed as a series of 
elements as follows: 

#endif

TVAL TBrickFieldT_Print(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
return TBrickFieldT_PrintBrickField(gCP, gTP, selfTval, size, buf, FALSE, 0);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TBrickFieldT_PrintBrickField

Convert a BrickFieldT object into an ascii string and append it to an output buffer. 
Following the extended tradition, the BrickRow object is displayed as a series of 
elements as follows: 

#endif

TVAL TBrickFieldT_PrintBrickField(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf, BOLE PrintingBinding, NUM PrintIndex)
{
NUM         repeatCnt = 0;
NUM         repeatIndex = 0;
LpBIND      bindPtr = NIL;
LpCHAR      fieldArrayPtr = NIL;
TBrick*    recordObj = NIL;

StartFrame
DeclareTVAL(ec);
DeclareTVAL(val);
EndFrame

PrintIndex = PrintIndex; // NOOP to hide unused parameter warning message
PrintingBinding = PrintingBinding; // NOOP to hide unused parameter warning message
recordObj = (TBrick*)_TObject_MainObjectList(ObjIdx(selfTval));

/* Make sure the object is valid */
if ((recordObj == NIL) || (recordObj->itsObjectType != TYBRICK))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Quit if the output string is already too long */

if ((*size) + 10 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
/*  Show Brick Field prefix */

buf[*size]      = '{';
buf[++(*size)]  = 'B';
buf[++(*size)]  = 'r';
buf[++(*size)]  = 'i';
buf[++(*size)]  = 'c';
buf[++(*size)]  = 'k';
buf[++(*size)]  = 'F';
buf[++(*size)]  = 'i';
buf[++(*size)]  = 'e';
buf[++(*size)]  = 'l';
buf[++(*size)]  = 'd';
buf[++(*size)]  = '|';
buf[++(*size)]  = ' ';
buf[++(*size)]  = 0;

/* Move pointer to start of Brick Row data */
fieldArrayPtr = asFieldArray(recordObj) + (RowIdx(selfTval) * recordObj->itsRowByteCount);

/* get pointer to the field information */
bindPtr = &BindArray(recordObj->itsFieldList)[FldIdx(selfTval)];
  
/* get the number of repeats */
repeatCnt = bindPtr->Value.Modifier;
    
/* handle repeating items */
for(repeatIndex = 0; repeatIndex < repeatCnt; repeatIndex++)
    {
#if 0
    /* display the field name */
    asObject(field) = bindPtr->Key;
    asTag(field) = asObject(field)->itsObjectType;
    *ec = FConio_sprintn(gCP,gTP,buf,size,*field);
    ExitOnError(*ec);
#endif

    /* display index */
    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
    buf[*size]      = '[';
    buf[++(*size)]  = 0;

    *ec = FConio_sprintn(gCP,gTP,buf,size,TINT(repeatIndex));
    ExitOnError(*ec);

    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
    buf[*size]      = ']';
    buf[++(*size)]  = 0;

    if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
    buf[*size]      = ':';
    buf[++(*size)]  = 0;

    switch (bindPtr->Value.DeclaredType)
        {
        case TYBOLE:
            val->u.Bool = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            val->Tag = TYBOLE;
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYDATE:
            asTag(val) = TYDATE;
            asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYNUM:
        case TYCHARPOINTER:
        case TYFLOATPOINTER:
        case TYREALPOINTER:
        case TYJUMPPOINTER:
        case TYINTPOINTER:
        case TYSHORTPOINTER:
        case TYLONGPOINTER:
        case TYWORDPOINTER:
            asTag(val) = TYNUM;
            asInt(val) = ((LpNUM)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYFLOAT:
            asTag(val) = TYREAL;
            asReal(val) = ((LpFLOAT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYMONEY:
            asTag(val) = TYMONEY;
            asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYREAL:
            asTag(val) = TYREAL;
            asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYOBJ:
            asObject(val) = ((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            asTag(val) = (asObject(val) == NULL) ? TYVOID : asObject(val)->itsObjectType;
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYSHORT:
            asTag(val) = TYNUM;
            asInt(val) = ((LpSHORT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYLONG:
            asTag(val) = TYNUM;
            asInt(val) = ((LpNUM32)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;

        case TYTVAL:
            asTag(val) = TYTVAL;
            *val = ((LpTVAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            break;
        }

    if (repeatIndex != repeatCnt - 1)
        {
        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ' ';
        buf[++(*size)]  = 0;
        }
    }

/*  Show Brick Field suffix */

if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = '}';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TBrickFieldT_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM         repeatIndex = 0;
LpCHAR      fieldArrayPtr = NIL;
LpBIND      bindPtr = NIL;
LpCHAR      temp;

StartFrame
DeclareOBJ(TBrick,recordObj);
EndFrame

/* Get pointer to Brick object */
recordObj = (TBrick*)_TObject_MainObjectList(ObjIdx(selfTval));

/* Make sure the object is valid */
if ((recordObj == NIL) || (recordObj->itsObjectType != TYBRICK))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  *************************  */
/*  Process the first index.   */
/*  *************************  */
if (isNumIndex(&index1))
	/*  We accept numeric indices. */
	{
    repeatIndex = asNumIndex(&index1);

    /* Use the Declared Type to return the proper value. */
    bindPtr = &BindArray(recordObj->itsFieldList)[FldIdx(selfTval)];
    fieldArrayPtr = asFieldArray(recordObj) + (RowIdx(selfTval) * recordObj->itsRowByteCount);
    
	/*  Make sure array index is in range. */
	if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

    switch (bindPtr->Value.DeclaredType)
	    {
	    case TYBOLE:
		    if (newValue.Tag == TYVOID)
			    newValue.u.Int = 0;
		    else
		    if ((newValue.Tag != TYBOLE) &&
			    (newValue.Tag != TYCHAR) &&
			    (newValue.Tag != TYNUM) &&
			    (newValue.Tag != TYCHARPOINTER) &&
			    (newValue.Tag != TYFLOATPOINTER) &&
			    (newValue.Tag != TYJUMPPOINTER) &&
			    (newValue.Tag != TYINTPOINTER) &&
			    (newValue.Tag != TYSHORTPOINTER) &&
			    (newValue.Tag != TYLONGPOINTER) &&
			    (newValue.Tag != TYWORDPOINTER) &&
			    (newValue.Tag != TYPOINTER))
			    {
			    FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
			    }
		    ((LpCHAR)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (CHAR)newValue.u.Int;
		    break;

        case TYDATE:
            if (newValue.Tag == TYVOID)
		        newValue.u.Real = 0;
            else
		    if ((newValue.Tag != TYREAL) &&
			    (newValue.Tag != TYDATE) &&
			    (newValue.Tag != TYMONEY))
			    {
                FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
                }
            ((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
            break;

	    case TYNUM:
	    case TYCHARPOINTER:
	    case TYFLOATPOINTER:
	    case TYREALPOINTER:
	    case TYJUMPPOINTER:
	    case TYINTPOINTER:
	    case TYSHORTPOINTER:
	    case TYLONGPOINTER:
	    case TYWORDPOINTER:
		    if (newValue.Tag == TYVOID)
			    newValue.u.Int = 0;
		    else
		    if ((newValue.Tag != TYBOLE) &&
			    (newValue.Tag != TYCHAR) &&
			    (newValue.Tag != TYNUM) &&
			    (newValue.Tag != TYCHARPOINTER) &&
			    (newValue.Tag != TYFLOATPOINTER) &&
			    (newValue.Tag != TYJUMPPOINTER) &&
			    (newValue.Tag != TYINTPOINTER) &&
			    (newValue.Tag != TYSHORTPOINTER) &&
			    (newValue.Tag != TYLONGPOINTER) &&
			    (newValue.Tag != TYWORDPOINTER) &&
			    (newValue.Tag != TYPOINTER))
			    {
			    FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
			    }
		    ((LpNUM)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (NUM)newValue.u.Int;
		    break;

	    case TYFLOAT:
		    if (newValue.Tag == TYVOID)
			    newValue.u.Real = 0;
		    else
		    if ((newValue.Tag != TYREAL) &&
			    (newValue.Tag != TYDATE) &&
			    (newValue.Tag != TYMONEY))
			    {
			    FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
			    }
		    ((LpFLOAT)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (FLOAT)newValue.u.Real;
		    break;

	    case TYMONEY:
		    if (newValue.Tag == TYVOID)
			    newValue.u.Real = 0;
		    else
		    if ((newValue.Tag != TYREAL) &&
			    (newValue.Tag != TYDATE) &&
			    (newValue.Tag != TYMONEY))
			    {
			    FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
			    }
		    ((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
		    break;

	    case TYREAL:
		    if (newValue.Tag == TYVOID)
			    newValue.u.Real = 0;
		    else
		    if ((newValue.Tag != TYREAL) &&
			    (newValue.Tag != TYDATE) &&
			    (newValue.Tag != TYMONEY))
			    {
			    FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
			    }
		    ((LpREAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
		    break;

	    case TYOBJ:
		    if (newValue.Tag == TYVOID)
			    {
			    ((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = NULL;
			    }
		    else
		    if (newValue.Tag == TYTEXT)
			    {
			    ((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)&newValue.u.Text[0]);
			    }
		    else
		    if (newValue.Tag == TYSTRINGSUBSTR)
		        {
		        temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
		        if (temp == NULL)
		            FrameExit(gCP->TObject_ERROR_INVALID);
			    ((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)TString_SubString_MakeUnique(gCP,gTP,temp,0,SubLen(newValue));
		        }
		    else
		    if ((_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT) || (_TObject_TypeFlag(newValue.u.Object->itsObjectType) != _TObject_TfTOBJECT))
			    {
			    FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
			    }
		    else
			    {
			    ((TObject**)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (TObject*)newValue.u.Object;
			    }
		    break;

	    case TYSHORT:
		    if (newValue.Tag == TYVOID)
			    newValue.u.Int = 0;
		    else
		    if ((newValue.Tag != TYBOLE) &&
			    (newValue.Tag != TYCHAR) &&
			    (newValue.Tag != TYSHORT) &&
			    (newValue.Tag != TYNUM) &&
			    (newValue.Tag != TYCHARPOINTER) &&
			    (newValue.Tag != TYFLOATPOINTER) &&
			    (newValue.Tag != TYJUMPPOINTER) &&
			    (newValue.Tag != TYINTPOINTER) &&
			    (newValue.Tag != TYSHORTPOINTER) &&
			    (newValue.Tag != TYLONGPOINTER) &&
			    (newValue.Tag != TYWORDPOINTER) &&
			    (newValue.Tag != TYPOINTER))
			    {
			    FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
			    }
		    ((LpSHORT)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (SHORT)newValue.u.Int;
		    break;

	    case TYLONG:
		    if (newValue.Tag == TYVOID)
			    newValue.u.Int = 0;
		    else
		    if ((newValue.Tag != TYBOLE) &&
			    (newValue.Tag != TYCHAR) &&
			    (newValue.Tag != TYLONG) &&
			    (newValue.Tag != TYNUM) &&
			    (newValue.Tag != TYCHARPOINTER) &&
			    (newValue.Tag != TYFLOATPOINTER) &&
			    (newValue.Tag != TYJUMPPOINTER) &&
			    (newValue.Tag != TYINTPOINTER) &&
			    (newValue.Tag != TYSHORTPOINTER) &&
			    (newValue.Tag != TYLONGPOINTER) &&
			    (newValue.Tag != TYWORDPOINTER) &&
			    (newValue.Tag != TYPOINTER))
			    {
			    FrameExit(TERROR("!BrickField.set: no automatic type conversion for this data type!"));
			    }
		    ((LpNUM32)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = (NUM32)newValue.u.Int;
		    break;

	    case TYTVAL:
		    ((LpTVAL)(fieldArrayPtr+bindPtr->Value.Offset))[repeatIndex] = newValue;
		    break;

	    default:
		    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);		
		    break;
	    }
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void TBrickRow_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk + SIZEOF_TBrickRowOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void TBrickRowT_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk + SIZEOF_TBrickRowOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

(setq rowField recordRow[0])

#endif

TVAL TBrickRowT_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM         fieldIndex = 0;
NUM         fieldCnt = 0;
LpCHAR      fieldArrayPtr = NIL;
LpBIND      bindPtr = NIL;

StartFrame
DeclareTVAL(ret);
DeclareOBJ(TBrick,recordObj);
EndFrame

/* Get pointer to Brick object */
recordObj = (TBrick*)_TObject_MainObjectList(ObjIdx(selfTval));

/* Make sure the object is valid */
if ((recordObj == NIL) || (recordObj->itsObjectType != TYBRICK))
    FrameExit(gCP->TObject_ERROR_INVALID);

fieldCnt = recordObj->itsFieldList.u.Structure->itsMaxItemIndex;

/* ***************** */
/* Process 1st Index */
/* ***************** */
if (isNumIndex(&index1))
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure array index is in range. */
	if (fieldIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
/*  We accept the elipses. */
if (index1.Tag == TYELLIPSES)
	{
    *ret = recordObj->itsFieldList;
	FrameExit(*ret);
	}
else
/*  We accept FieldList requests. */
if (((index1.Tag == TYTEXT) && (strcmp(index1.u.Text,"FieldList") == 0)) ||
    ((index1.Tag == TYSTRING) && (strcmp(CharArray(index1),"FieldList") == 0)) ||
    ((index1.Tag == TYSTRINGSUBSTR) && (substrrcmp(TStringSubstringT_GetStringPtr(gCP, gTP, index1),SubLen(index1),"FieldList")) == 0))
	{
    *ret = recordObj->itsFieldList;
	FrameExit(*ret);
	}
else
/*  We accept FieldCount requests. */
if (((index1.Tag == TYTEXT) && (strcmp(index1.u.Text,"FieldCount") == 0)) || 
    ((index1.Tag == TYSTRING) && (strcmp(CharArray(index1),"FieldCount") == 0)) ||
    ((index1.Tag == TYSTRINGSUBSTR) && (substrrcmp(TStringSubstringT_GetStringPtr(gCP, gTP, index1),SubLen(index1),"FieldCount")) == 0))
	{
    ret->u.Int = recordObj->itsFieldCount;
    ret->Tag = TYNUM;
	FrameExit(*ret);
	}
else
/*  We accept ByteCount requests. */
if (((index1.Tag == TYTEXT) && (strcmp(index1.u.Text,"ByteCount") == 0)) ||
    ((index1.Tag == TYSTRING) && (strcmp(CharArray(index1),"ByteCount") == 0)) ||
    ((index1.Tag == TYSTRINGSUBSTR) && (substrrcmp(TStringSubstringT_GetStringPtr(gCP, gTP, index1),SubLen(index1),"ByteCount")) == 0))
	{
    ret->u.Int = recordObj->itsRowByteCount;
    ret->Tag = TYNUM;
	FrameExit(*ret);
	}
else
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	bindPtr = BindArray(recordObj->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
	}
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* If the no. of repeats are more than 1, we will return a TBrickField object */

bindPtr = &BindArray(recordObj->itsFieldList)[fieldIndex];

if (bindPtr->Value.Modifier > 1 && bindPtr->Value.DeclaredType != TYCHAR)
    {
    /* If the DeclaredType if TYCHAR, we will return a TYSTRINGSUBSTR */
    /* Otherwise, we return a TYBRICKFIELD */
    
    ret->Tag = TYBRICKFIELD;
    asObjIdx(ret) = ObjIdx(selfTval);   /* Brick object stack index */
    asRowIdx(ret) = RowIdx(selfTval);   /* Row index */
    asFldIdx(ret) = fieldIndex;         /* Field index */
    }
else
    {
	fieldArrayPtr = asFieldArray(recordObj) + bindPtr->Value.Offset;
    switch (bindPtr->Value.DeclaredType)
	    {
	    case TYBOLE:
		    ret->Tag = TYBOLE;
		    ret->u.Bool = *((LpCHAR)fieldArrayPtr);
		    break;

        case TYCHAR:
            if (bindPtr->Value.Modifier > 1)
                {
                ret->Tag = TYSTRINGSUBSTR;
                asObjIdx(ret) = ObjIdx(selfTval);
                asSubOff(ret) = bindPtr->Value.Offset;
                if (fieldArrayPtr[bindPtr->Value.Modifier - 1] != 0)
                    asSubLen(ret) = bindPtr->Value.Modifier;
                else
                    asSubLen(ret) = strlen((char*)fieldArrayPtr);
                }
            else
                {
                ret->Tag = TYCHAR;
                ret->u.Char = fieldArrayPtr[0];
                }
            break;

        case TYDATE:
            ret->Tag = TYDATE;
		    ret->u.Real = *((LpREAL)fieldArrayPtr);
		    break;

	    case TYNUM:
	    case TYCHARPOINTER:
	    case TYFLOATPOINTER:
	    case TYREALPOINTER:
	    case TYJUMPPOINTER:
	    case TYINTPOINTER:
	    case TYLONGPOINTER:
	    case TYSHORTPOINTER:
	    case TYWORDPOINTER:
		    ret->Tag = TYNUM;
		    ret->u.Int = *((LpNUM)fieldArrayPtr);
		    break;

	    case TYFLOAT:
		    ret->Tag = TYREAL;
		    ret->u.Real = *((LpFLOAT)fieldArrayPtr);
		    break;

	    case TYMONEY:
		    ret->Tag = TYMONEY;
		    ret->u.Real = *((LpREAL)fieldArrayPtr);
		    break;

	    case TYREAL:
		    ret->Tag = TYREAL;
		    ret->u.Real = *((LpREAL)fieldArrayPtr);
		    break;

	    case TYOBJ:
		    ret->u.Object = *((TObject**)fieldArrayPtr);
		    ret->Tag = (ret->u.Object == NULL) ? TYVOID : ret->u.Object->itsObjectType;
		    break;

	    case TYLONG:
		    ret->Tag = TYNUM;
		    ret->u.Int = *((LpNUM32)fieldArrayPtr);
		    break;

	    case TYSHORT:
		    ret->Tag = TYNUM;
		    ret->u.Int = *((LpSHORT)fieldArrayPtr);
		    break;

	    case TYTVAL:
		    *ret = *((LpTVAL)fieldArrayPtr);
		    break;		

	    default:
		    *ret = gCP->TObject_ERROR_BADCELL;
		    break;
	    }
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV2

Return the indexed value from the repeating portion of this object.

#endif

TVAL TBrickRowT_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2)
{
NUM         fieldIndex = 0;
NUM         fieldCnt = 0;
NUM         repeatIndex = 0;
LpBIND      bindPtr = NIL;
LpCHAR      fieldArrayPtr = NIL;

StartFrame
DeclareTVAL(ret);
DeclareOBJ(TBrick,recordObj);
EndFrame

/* Get pointer to Brick object */
recordObj = (TBrick*)_TObject_MainObjectList(ObjIdx(selfTval));

/* Make sure the object is valid */
if ((recordObj == NIL) || (recordObj->itsObjectType != TYBRICK))
    FrameExit(gCP->TObject_ERROR_INVALID);

/* Get field count */
fieldCnt = recordObj->itsFieldList.u.Structure->itsMaxItemIndex;

/* ***************** */
/* Process 1st Index */
/* ***************** */
if (isNumIndex(&index1))
	/*  We accept numeric indices. */
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure array index is in range. */
	if (fieldIndex < 0) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	bindPtr = BindArray(recordObj->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (fieldIndex >= fieldCnt) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* ***************** */
/* Process 2nd Index */
/* ***************** */
if (isNumIndex(&index2))
	/*  We accept numeric indices. */
	{
    repeatIndex = asNumIndex(&index2);

	/* Use the Declared Type to return the proper value. */
    bindPtr = &BindArray(recordObj->itsFieldList)[fieldIndex];

    /* Get pointer to field data */
    fieldArrayPtr = (LpCHAR) *(recordObj->itsFieldArray) + (RowIdx(selfTval) * recordObj->itsRowByteCount);

	switch (bindPtr->Value.DeclaredType)
		{
		case TYBOLE:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYBOLE;
			ret->u.Bool = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYCHAR:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
		    ret->Tag = TYCHAR;
		    ret->u.Char = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

        case TYDATE:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
            ret->Tag = TYDATE;
		    ret->u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
		    break;

		case TYNUM:
		case TYCHARPOINTER:
		case TYFLOATPOINTER:
		case TYREALPOINTER:
		case TYJUMPPOINTER:
		case TYINTPOINTER:
		case TYLONGPOINTER:
		case TYSHORTPOINTER:
		case TYWORDPOINTER:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpNUM)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYFLOAT:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYREAL;
			ret->u.Real = ((LpFLOAT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYMONEY:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYMONEY;
			ret->u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYREAL:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYREAL;
			ret->u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYOBJ:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->u.Object = ((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			ret->Tag = (ret->u.Object == NULL) ? TYVOID : ret->u.Object->itsObjectType;
			break;

		case TYSHORT:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpSHORT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYLONG:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			ret->Tag = TYNUM;
			ret->u.Int = ((LpNUM32)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

		case TYTVAL:
            if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY); 
			*ret = ((LpTVAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
			break;

		default:
		    *ret = gCP->TObject_ERROR_BADCELL;
		    break;
		}

	FrameExit(*ret);
	}
else
if ((index2.Tag == TYSYMBOL) || (index2.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
	bindPtr = &BindArray(recordObj->itsFieldList)[fieldIndex];
	fieldArrayPtr = (LpCHAR) *(recordObj->itsFieldArray) + (RowIdx(selfTval) * recordObj->itsRowByteCount);

	if (strcmp(SymbolArray(index2),"type") == 0)
		/*  We accept symbolic indices. */
		{
		ret->u.Symbol = TSymbol_MakeUnique(gCP,gTP,_TObject_TypeName(bindPtr->Value.DeclaredType));
		ret->Tag = TYSYMBOL;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"repeats") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = bindPtr->Value.Modifier;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"offset") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = bindPtr->Value.Offset;
		FrameExit(*ret);
		}
	else 
	if (strcmp(SymbolArray(index2),"pointer") == 0)
		/*  We accept symbolic indices. */
		{
		ret->Tag = TYNUM;
		ret->u.Int = (NUM)(fieldArrayPtr + bindPtr->Value.Offset);
		FrameExit(*ret);
		} 
	else
		/*  Everything else is an error. */
		FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}

/*  Everything else is an error. */
FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}

/*--------------------------------------------------------------------------------------- */

#if 0
Print

Convert a BrickRowT object into an ascii string and append it to an output buffer. 
Following the extended tradition, the BrickRow object is displayed as a series of 
elements as follows: 

#endif

TVAL TBrickRowT_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
return TBrickRowT_PrintBrickRow(gCP,gTP, selfTval, size, buf, FALSE, 0);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TBrickRowT_PrintBrickRow

Convert a BrickRowT object into an ascii string and append it to an output buffer. 
Following the extended tradition, the BrickRowT object is displayed as a series of 
elements as follows: 

#endif

TVAL TBrickRowT_PrintBrickRow(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf, BOLE PrintingBinding, NUM PrintIndex)
{
LpBIND      bindPtr = NIL;
LpCHAR      fieldArrayPtr = NIL;

NUM         fieldIndex = 0;
NUM         repeatCnt = 0;
NUM         repeatIndex = 0;
NUM         fieldCnt = 0;

StartFrame
DeclareTVAL(ec);
DeclareTVAL(val);
DeclareTVAL(field);
DeclareOBJ(TBrick,recordObj);
EndFrame

PrintIndex = PrintIndex; // NOOP to hide unused parameter warning message
PrintingBinding = PrintingBinding; // NOOP to hide unused parameter warning message
recordObj = (TBrick*)_TObject_MainObjectList(ObjIdx(selfTval));

/* Make sure the object is valid */
if ((recordObj == NIL) || (recordObj->itsObjectType != TYBRICK))
    FrameExit(gCP->TObject_ERROR_INVALID);
    
fieldCnt = recordObj->itsFieldList.u.Structure->itsMaxItemIndex;

/*  Quit if the output string is already too long */

if ((*size) + 8 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
/*  Show Brick Row prefix */

buf[*size]      = '{';
buf[++(*size)]  = 'B';
buf[++(*size)]  = 'r';
buf[++(*size)]  = 'i';
buf[++(*size)]  = 'c';
buf[++(*size)]  = 'k';
buf[++(*size)]  = 'R';
buf[++(*size)]  = 'o';
buf[++(*size)]  = 'w';
buf[++(*size)]  = '|';
buf[++(*size)]  = ' ';
buf[++(*size)]  = 0;

/* Move pointer to start of Brick Row data */
fieldArrayPtr = asFieldArray(recordObj) + (RowIdx(selfTval) * recordObj->itsRowByteCount);

for(fieldIndex = 0; fieldIndex < fieldCnt; fieldIndex++)
    {
    /* get pointer to the field information */
    bindPtr = &BindArray(recordObj->itsFieldList)[fieldIndex];
   
    /* get the number of repeats */
    repeatCnt = bindPtr->Value.Modifier;

    /* special handling for repeating characters */
    if (bindPtr->Value.DeclaredType == TYCHAR)
        {
        /* display the field name */
        asObject(field) = bindPtr->Key;
        asTag(field) = asObject(field)->itsObjectType;
        *ec = FConio_sprintn(gCP,gTP,buf,size,*field);
        ExitOnError(*ec);

        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
        buf[*size]      = ':';
        buf[++(*size)]  = 0;

        if (repeatCnt > 1)
            {
			val->u.String = TString_SubString_MakeUnique(gCP,gTP,(LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset),0,bindPtr->Value.Modifier);
			val->Tag = TYSTRING;
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            }
        else
            {
            val->u.Char = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[0];
            val->Tag = TYCHAR;
            *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
            ExitOnError(*ec);
            }
        } /* special handling for character type */
    else
        {
        /* handle repeating items */
        for(repeatIndex = 0; repeatIndex < repeatCnt; repeatIndex++)
            {
            /* display the field name */
            asObject(field) = bindPtr->Key;
            asTag(field) = asObject(field)->itsObjectType;
            *ec = FConio_sprintn(gCP,gTP,buf,size,*field);
            ExitOnError(*ec);

            if (repeatCnt > 1)
                {
                if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                    FrameExit(gCP->TObject_FALSE);
                buf[*size]      = '[';
                buf[++(*size)]  = 0;

                *ec = FConio_sprintn(gCP,gTP,buf,size,TINT(repeatIndex));
                ExitOnError(*ec);

                if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                    FrameExit(gCP->TObject_FALSE);
                buf[*size]      = ']';
                buf[++(*size)]  = 0;
                }

            if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                FrameExit(gCP->TObject_FALSE);
            buf[*size]      = ':';
            buf[++(*size)]  = 0;

            switch (bindPtr->Value.DeclaredType)
	            {
	            case TYBOLE:
	                val->u.Bool = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
	                val->Tag = TYBOLE;
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
		            break;

                case TYDATE:
                    asTag(val) = TYDATE;
                    asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
                    break;

	            case TYNUM:
	            case TYCHARPOINTER:
	            case TYFLOATPOINTER:
	            case TYREALPOINTER:
	            case TYJUMPPOINTER:
	            case TYINTPOINTER:
	            case TYSHORTPOINTER:
	            case TYLONGPOINTER:
	            case TYWORDPOINTER:
                    asTag(val) = TYNUM;
                    asInt(val) = ((LpNUM)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
		            break;

	            case TYFLOAT:
                    asTag(val) = TYREAL;
                    asReal(val) = ((LpFLOAT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
                    break;

	            case TYMONEY:
                    asTag(val) = TYMONEY;
                    asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
                    break;

	            case TYREAL:
                    asTag(val) = TYREAL;
                    asReal(val) = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
                    break;

	            case TYOBJ:
                    asObject(val) = ((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    asTag(val) = (asObject(val) == NULL) ? TYVOID : asObject(val)->itsObjectType;
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
	                break;

	            case TYSHORT:
                    asTag(val) = TYNUM;
                    asInt(val) = ((LpSHORT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
		            break;

	            case TYLONG:
                    asTag(val) = TYNUM;
                    asInt(val) = ((LpNUM32)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
		            break;

	            case TYTVAL:
                    asTag(val) = TYTVAL;
                    *val = ((LpTVAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex];
                    *ec = FConio_sprintn(gCP,gTP,buf,size,*val);
                    ExitOnError(*ec);
		            break;
	            }

            if (repeatIndex != repeatCnt - 1)
                {
                if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
                    FrameExit(gCP->TObject_FALSE);
                buf[*size]      = ' ';
                buf[++(*size)]  = 0;
                }
            } /* repeating index */
        } /* handling for non-character types */

    if (fieldIndex != fieldCnt - 1)
        {
        if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);

        buf[*size]      = ' ';
        buf[++(*size)]  = 0;
        }
    } /* handling for fields */

/*  Show Brick Row suffix */

if ((*size) + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[*size]      = '}';
buf[++(*size)]  = 0;

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TBrickRowT_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM         fieldCnt = 0;
NUM         fieldIndex = 0;
NUM         lengthOf = 0;
LpCHAR      fieldArrayPtr = NIL;
LpBIND      bindPtr = NIL;
LpCHAR      temp = NIL;

StartFrame
DeclareOBJ(TBrick,recordObj);
EndFrame

/* Get pointer to Brick object */
recordObj = (TBrick*)_TObject_MainObjectList(ObjIdx(selfTval));

/* Make sure the object is valid */
if ((recordObj == NIL) || (recordObj->itsObjectType != TYBRICK))
    FrameExit(gCP->TObject_ERROR_INVALID);

/* Get field count */
fieldCnt = recordObj->itsFieldList.u.Structure->itsMaxItemIndex;

/*  We accept numeric indices. */
if (isNumIndex(&index1))
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure array index is in range. */
	if (!inRange(fieldIndex,0,fieldCnt)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
/*  We accept symbolic indices. */
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	{
	bindPtr = BindArray(recordObj->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (!inRange(fieldIndex,0,fieldCnt)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* Use the Declared Type to return the proper value. */
bindPtr = &BindArray(recordObj->itsFieldList)[fieldIndex];
fieldArrayPtr = asFieldArray(recordObj) + (RowIdx(selfTval) * recordObj->itsRowByteCount);
switch (bindPtr->Value.DeclaredType)
	{
	case TYBOLE:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag != TYBOLE) &&
			(newValue.Tag != TYCHAR) &&
			(newValue.Tag != TYNUM) &&
			(newValue.Tag != TYCHARPOINTER) &&
			(newValue.Tag != TYFLOATPOINTER) &&
			(newValue.Tag != TYJUMPPOINTER) &&
			(newValue.Tag != TYINTPOINTER) &&
			(newValue.Tag != TYLONGPOINTER) &&
			(newValue.Tag != TYSHORTPOINTER) &&
			(newValue.Tag != TYWORDPOINTER) &&
			(newValue.Tag != TYPOINTER))
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		*((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset)) = (CHAR)newValue.u.Int;
		break;

	case TYCHAR:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag == TYBOLE) ||
			(newValue.Tag == TYCHAR) ||
			(newValue.Tag == TYNUM) ||
			(newValue.Tag == TYCHARPOINTER) ||
			(newValue.Tag == TYFLOATPOINTER) ||
			(newValue.Tag == TYJUMPPOINTER) ||
			(newValue.Tag == TYINTPOINTER) ||
			(newValue.Tag == TYSHORTPOINTER) ||
			(newValue.Tag == TYLONGPOINTER) ||
			(newValue.Tag == TYWORDPOINTER) ||
			(newValue.Tag == TYPOINTER))
			{
			((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[0] = (CHAR)newValue.u.Int;
			if (bindPtr->Value.Modifier > 1) ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[1] = 0;
			}
		else
		if (newValue.Tag == TYSTRING) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = strlen(CharArray(newValue))))
				{
				strncpy(&((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[0],CharArray(newValue),lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!BrickRow.set: character data too long for this data type!"));
				}
			}
		else
		if (newValue.Tag == TYSYMBOL) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = strlen(CharArray(newValue))))
				{
				strncpy(&((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[0],SymbolArray(newValue),lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!BrickRow.set: character data too long for this data type!"));
				}
			}
		else
		if (newValue.Tag == TYBYTEVECTOR) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = newValue.u.ByteVector->itsMaxItemIndex))
				{
				FMemory_memcpy(gCP,gTP,(LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset),(LpCHAR)ByteArray(newValue),lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!BrickRow.set: character data too long for this data type!"));
				}
			}
		else
		if (newValue.Tag == TYTEXT) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = strlen(newValue.u.Text)))
				{
				strncpy(&((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[0],newValue.u.Text,lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!BrickRow.set: character data too long for this data type!"));
				}
			}
		else
		if (newValue.Tag == TYSTRINGSUBSTR) 
			{
			if (bindPtr->Value.Modifier >= (lengthOf = SubLen(newValue)))
				{
				temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
				if (temp == NULL)
				    FrameExit(gCP->TObject_ERROR_INVALID);
				strncpy(&((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[0],temp,lengthOf);
				if (bindPtr->Value.Modifier > lengthOf)
				    memset((fieldArrayPtr + bindPtr->Value.Offset + lengthOf), 0, (bindPtr->Value.Modifier - lengthOf));
				}
			else
				{
				FrameExit(TERROR("!BrickRow.set: character data too long for this data type!"));
				}
			}
		else		
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		break;

    case TYDATE:
	    if (newValue.Tag == TYVOID)
			newValue.u.Real = 0;
        else
		if ((newValue.Tag != TYREAL) &&
			(newValue.Tag != TYDATE) &&
			(newValue.Tag != TYMONEY))
			{
            FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
            }
        *((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset)) = (REAL)newValue.u.Real;
        break;

	case TYNUM:
	case TYCHARPOINTER:
	case TYFLOATPOINTER:
	case TYREALPOINTER:
	case TYJUMPPOINTER:
	case TYINTPOINTER:
	case TYLONGPOINTER:
	case TYSHORTPOINTER:
	case TYWORDPOINTER:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag != TYBOLE) &&
			(newValue.Tag != TYCHAR) &&
			(newValue.Tag != TYNUM) &&
			(newValue.Tag != TYCHARPOINTER) &&
			(newValue.Tag != TYFLOATPOINTER) &&
			(newValue.Tag != TYJUMPPOINTER) &&
			(newValue.Tag != TYINTPOINTER) &&
			(newValue.Tag != TYSHORTPOINTER) &&
			(newValue.Tag != TYLONGPOINTER) &&
			(newValue.Tag != TYWORDPOINTER) &&
			(newValue.Tag != TYPOINTER))
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		*((LpNUM)(fieldArrayPtr + bindPtr->Value.Offset)) = (NUM)newValue.u.Int;
		break;

	case TYFLOAT:
	    if (newValue.Tag == TYVOID)
			newValue.u.Real = 0;
		else
		if ((newValue.Tag != TYREAL) &&
			(newValue.Tag != TYDATE) &&
			(newValue.Tag != TYMONEY))
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		*((LpFLOAT)(fieldArrayPtr + bindPtr->Value.Offset)) = (FLOAT)newValue.u.Real;
		break;

	case TYMONEY:
	    if (newValue.Tag == TYVOID)
			newValue.u.Real = 0;
		else
		if ((newValue.Tag != TYREAL) &&
			(newValue.Tag != TYDATE) &&
			(newValue.Tag != TYMONEY))
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		*((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset)) = (REAL)newValue.u.Real;
		break;

	case TYREAL:
	    if (newValue.Tag == TYVOID)
			newValue.u.Real = 0;
		else
		if ((newValue.Tag != TYREAL) &&
			(newValue.Tag != TYDATE) &&
			(newValue.Tag != TYMONEY))
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		*((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset)) = (REAL)newValue.u.Real;
		break;

	case TYOBJ:
		if (newValue.Tag == TYVOID)
			{
			*((TObject**)(fieldArrayPtr+bindPtr->Value.Offset)) = NULL;
			}
		else
		if (newValue.Tag == TYTEXT)
			{
			*((TObject**)(fieldArrayPtr + bindPtr->Value.Offset)) = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)&newValue.u.Text[0]);
			}
		else
		if (newValue.Tag == TYSTRINGSUBSTR)
		    {
			temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
			if (temp == NULL)
			    FrameExit(gCP->TObject_ERROR_INVALID);		    
    		*((TObject**)(fieldArrayPtr + bindPtr->Value.Offset)) = (TObject*)TString_SubString_MakeUnique(gCP,gTP,temp,0,SubLen(newValue));
		    }
		else
		if ((_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT) || (_TObject_TypeFlag(newValue.u.Object->itsObjectType) != _TObject_TfTOBJECT))
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		else
			{
			*((TObject**)(fieldArrayPtr + bindPtr->Value.Offset)) = (TObject*)newValue.u.Object;
			}
		break;

	case TYSHORT:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag != TYBOLE) &&
			(newValue.Tag != TYCHAR) &&
			(newValue.Tag != TYSHORT) &&
			(newValue.Tag != TYNUM) &&
			(newValue.Tag != TYCHARPOINTER) &&
			(newValue.Tag != TYFLOATPOINTER) &&
			(newValue.Tag != TYJUMPPOINTER) &&
			(newValue.Tag != TYINTPOINTER) &&
			(newValue.Tag != TYSHORTPOINTER) &&
			(newValue.Tag != TYLONGPOINTER) &&
			(newValue.Tag != TYWORDPOINTER) &&
			(newValue.Tag != TYPOINTER))
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		*((LpSHORT)(fieldArrayPtr + bindPtr->Value.Offset)) = (SHORT)newValue.u.Int;
		break;

	case TYLONG:
	    if (newValue.Tag == TYVOID)
			newValue.u.Int = 0;
		else
		if ((newValue.Tag != TYBOLE) &&
			(newValue.Tag != TYCHAR) &&
			(newValue.Tag != TYLONG) &&
			(newValue.Tag != TYNUM) &&
			(newValue.Tag != TYCHARPOINTER) &&
			(newValue.Tag != TYFLOATPOINTER) &&
			(newValue.Tag != TYJUMPPOINTER) &&
			(newValue.Tag != TYINTPOINTER) &&
			(newValue.Tag != TYLONGPOINTER) &&
			(newValue.Tag != TYSHORTPOINTER) &&
			(newValue.Tag != TYWORDPOINTER) &&
			(newValue.Tag != TYPOINTER))
			{
			FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
			}
		*((LpNUM32)(fieldArrayPtr + bindPtr->Value.Offset)) = (NUM32)newValue.u.Int;
		break;

	case TYTVAL:
		*((LpTVAL)(fieldArrayPtr + bindPtr->Value.Offset)) = newValue;
		break;		

	default:
		FrameExit(gCP->TObject_ERROR_BADIDXORKEY);		
		break;
	}

FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV2

Set the indexed value in the repeating portion of this object.

Note:   

#endif

TVAL TBrickRowT_SetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL newValue)
{
NUM         repeatIndex = 0;
NUM         fieldCnt = 0;
NUM         fieldIndex = 0;
LpCHAR      fieldArrayPtr = NIL;
LpBIND      bindPtr = NIL;
LpCHAR      temp = NIL;

StartFrame
DeclareOBJ(TBrick,recordObj);
EndFrame

/* Get pointer to Brick object */
recordObj = (TBrick*)_TObject_MainObjectList(ObjIdx(selfTval));

/* Make sure the object is valid */
if ((recordObj == NIL) || (recordObj->itsObjectType != TYBRICK))
    FrameExit(gCP->TObject_ERROR_INVALID);

/* Get field count */
fieldCnt = recordObj->itsFieldList.u.Structure->itsMaxItemIndex;

/*  *************************  */
/*  Process the first index.   */
/*  *************************  */
if (isNumIndex(&index1))
	/*  We accept numeric indices. */
	{
    fieldIndex = asNumIndex(&index1);

	/*  Make sure array index is in range. */
	if (!inRange(fieldIndex,0,fieldCnt)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
	/*  We accept symbolic indices. */
	{
    bindPtr = BindArray(recordObj->itsFieldList);
    for (fieldIndex = 0; fieldIndex < fieldCnt; ++fieldIndex)
		{
		if (bindPtr[fieldIndex].Key == index1.u.Object) goto FieldNameFound;
		}
	FieldNameFound:
	if (!inRange(fieldIndex,0,fieldCnt)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
	/*  Everything else is an error. */
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  *************************  */
/*  Process the second index.  */
/*  *************************  */
if (isNumIndex(&index2))
	/*  We accept numeric indices. */
	{
    repeatIndex = asNumIndex(&index2);

	/* Use the Declared Type to return the proper value. */
    bindPtr = &BindArray(recordObj->itsFieldList)[fieldIndex];
    fieldArrayPtr = asFieldArray(recordObj) + (RowIdx(selfTval) * recordObj->itsRowByteCount);
    
	/*  Make sure array index is in range. */
	if (!inRange(repeatIndex,0,bindPtr->Value.Modifier)) FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
	switch (bindPtr->Value.DeclaredType)
		{
		case TYBOLE:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (CHAR)newValue.u.Int;
			break;

		case TYCHAR:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (CHAR)newValue.u.Int;
			break;

        case TYDATE:
	        if (newValue.Tag == TYVOID)
			    newValue.u.Real = 0;
            else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
                FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
                }
            ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
            break;

		case TYNUM:
		case TYCHARPOINTER:
		case TYFLOATPOINTER:
		case TYREALPOINTER:
		case TYJUMPPOINTER:
		case TYINTPOINTER:
		case TYSHORTPOINTER:
		case TYLONGPOINTER:
		case TYWORDPOINTER:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			((LpNUM)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (NUM)newValue.u.Int;
			break;

		case TYFLOAT:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			((LpFLOAT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (FLOAT)newValue.u.Real;
			break;

		case TYMONEY:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
			break;

		case TYREAL:
			if (newValue.Tag == TYVOID)
				newValue.u.Real = 0;
			else
			if ((newValue.Tag != TYREAL) &&
				(newValue.Tag != TYDATE) &&
				(newValue.Tag != TYMONEY))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (REAL)newValue.u.Real;
			break;

		case TYOBJ:
			if (newValue.Tag == TYVOID)
				{
				((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = NULL;
				}
			else
			if (newValue.Tag == TYTEXT)
				{
				((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (TObject*)TString_MakeUnique(gCP,gTP,(LpCHAR)&newValue.u.Text[0]);
				}
			else
			if (newValue.Tag == TYSTRINGSUBSTR)
			    {
			    temp = TStringSubstringT_GetStringPtr(gCP, gTP, newValue);
			    if (temp == NULL)
			        FrameExit(gCP->TObject_ERROR_INVALID);			    
    			((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (TObject*)TString_SubString_MakeUnique(gCP,gTP,temp,0,SubLen(newValue));
			    }
			else
			if ((_TObject_TypeFlag(newValue.Tag) != _TObject_TfTOBJECT) || (_TObject_TypeFlag(newValue.u.Object->itsObjectType) != _TObject_TfTOBJECT))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			else
				{
				((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (TObject*)newValue.u.Object;
				}
			break;

		case TYSHORT:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYSHORT) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			((LpSHORT)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (SHORT)newValue.u.Int;
			break;

		case TYLONG:
			if (newValue.Tag == TYVOID)
				newValue.u.Int = 0;
			else
			if ((newValue.Tag != TYBOLE) &&
				(newValue.Tag != TYCHAR) &&
				(newValue.Tag != TYLONG) &&
				(newValue.Tag != TYNUM) &&
				(newValue.Tag != TYCHARPOINTER) &&
				(newValue.Tag != TYFLOATPOINTER) &&
				(newValue.Tag != TYJUMPPOINTER) &&
				(newValue.Tag != TYINTPOINTER) &&
				(newValue.Tag != TYSHORTPOINTER) &&
				(newValue.Tag != TYLONGPOINTER) &&
				(newValue.Tag != TYWORDPOINTER) &&
				(newValue.Tag != TYPOINTER))
				{
				FrameExit(TERROR("!BrickRow.set: no automatic type conversion for this data type!"));
				}
			((LpNUM32)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = (NUM32)newValue.u.Int;
			break;

		case TYTVAL:
			((LpTVAL)(fieldArrayPtr + bindPtr->Value.Offset))[repeatIndex] = newValue;
			break;

		default:
			FrameExit(gCP->TObject_ERROR_BADIDXORKEY);		
			break;
		}

	FrameExit(selfTval);
	}

/*  Everything else is an error. */
FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
}
