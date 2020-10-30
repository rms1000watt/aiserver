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

#define _C_FCONVERT
#define _SMARTBASE 

#if 0
FConvert.c

This source file contains some of the cProcedures which implement the conversion 
functions supported by the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  
    
#endif

#include "fconvert.h"
#include "fcompile.h"
#include "fobject.h"
#include "fmake.h"
#include "tdiction.h"
#include "tdirect.h"
#include "fconio.h"
#include "futil2.h"
#include "tmatrix.h"
#include "tnummat.h"
#include "tcpx.h"
#include "tcpxvec.h"
#include "tshortvec.h"
#include "tlongvec.h"
#include "tstring.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_Init

Initialize the conversion portion of the SmartLisp function library.  

#endif

TVAL FConvert_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,savSymbol);
DeclareTVAL(ec);
DeclareOBJArray(TSymbol,tstSymbol,3);
EndFrame

if(gCP->FConvert_Initialized) 
    FrameExit(gCP->TObject_OK)

gCP->FConvert_Initialized = 1;

/* Register the SmartLisp cProcedures contained in this package */

FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"money",(LpFUNC)&FConvert_ToMoney);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"integer",(LpFUNC)&FConvert_ToInteger);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"number",(LpFUNC)&FConvert_ToNumber);
ExitOnError(*ec);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charToNumber");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringToNumber");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);

FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"character",(LpFUNC)&FConvert_ToCharacter);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"boolean",(LpFUNC)&FConvert_ToBoolean);
ExitOnError(*ec);

FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"string",(LpFUNC)&FConvert_ToString);
ExitOnError(*ec);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"numberToString");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol,savSymbol->itsGlobalValue);

FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"downcase",(LpFUNC)&FConvert_Downcase);
ExitOnError(*ec);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charDowncase");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringDowncase");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);

FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"upcase",(LpFUNC)&FConvert_Upcase);
ExitOnError(*ec);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charUpcase");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringUpcase");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"objectToList",(LpFUNC)&FConvert_ObjToList);
ExitOnError(*ec);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"vectorToList");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"objectToVector",(LpFUNC)&FConvert_ObjToVector);
ExitOnError(*ec);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"listToVector");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"objectToNumVector",(LpFUNC)&FConvert_ObjToNumVector);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"objectToCpxVector",(LpFUNC)&FConvert_ObjToCpxVector);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"objectToMatrix",(LpFUNC)&FConvert_ObjToMatrix);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"objectToNumMatrix",(LpFUNC)&FConvert_ObjToNumMatrix);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"objectToDictionary",(LpFUNC)&FConvert_ObjToDictionary);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"objectToDirectory",(LpFUNC)&FConvert_ObjToDirectory);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"objectToStructure",(LpFUNC)&FConvert_ObjToStructure);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"objectToStructure"), aSymbol->itsGlobalValue);

FrameExit(gCP->TObject_OK)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ToDate

The FConvert_ToDate function coerces the argument into a Date.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_ToDate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Coerce the argument to a number.  */

*err = FObject_DateAnyCnv(gCP,gTP,TYDATE, argv[0]);
ExitOnError( *err);
*ret = *err;
    
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ToMoney

The FConvert_ToMoney function coerces the argument into a Money.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_ToMoney(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Coerce the argument to a number.  */

*err = FObject_MoneyAnyCnv(gCP,gTP,TYMONEY, argv[0]);
ExitOnError( *err);
*ret = *err;
    
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ToNumber

The FConvert_ToNumber function coerces the argument into a Number.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_ToNumber(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Coerce the argument to a number.  */

*ret = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
ExitOnError(*ret);
    
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ToInteger

The FConvert_ToInteger function coerces the argument into an Integer.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_ToInteger(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Coerce the argument to a number.  */

*err = FObject_IntAnyCnv(gCP,gTP,TYREAL, argv[0]);
ExitOnError( *err);
*ret = *err;
    
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ToCharacter

The FConvert_ToCharacter function coerces the argument into a Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_ToCharacter(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Coerce the argument to a boolean.  */

*err = FObject_CharAnyCnv(gCP,gTP,TYCHAR, argv[0]);
ExitOnError( *err);
*ret = *err;
    
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ToBoolean

The FConvert_ToBoolean function coerces the argument into a Boolean.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_ToBoolean(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Coerce the argument to a boolean.  */

*err = FObject_BoolAnyCnv(gCP,gTP,TYBOLE, argv[0]);
ExitOnError( *err);
*ret = *err;
    
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ToString

The FConvert_ToString function coerces the argument into a String.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_ToString(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
HMChar              theData = NULL;
LpCHAR              buf;
LpCHAR              stringPtr;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  Check the number and type of arguments */

if ((argc == 2) &&
	(argv[1].Tag == TYBOLE) &&
	(argv[1].u.Bool == TRUE))
	{
	if (argv[0].Tag == TYBYTEVECTOR)
		{
		stringPtr = (LpCHAR)FSmartbase_VectorPtr(gCP,gTP,argv[0]);
		*ret = TObject_CnvFromText(gCP,gTP,stringPtr);
		}
	else
		{
		/*  Produce a deep string conversion.*/
		theData = (HMChar)FMemory_New(gCP, gTP, (NUM)_FSmartbase_MAXBUFFERLEN+2,TRUE);
		buf = &atHMChar(theData,0);
		FConio_sprint(gCP,gTP,_FSmartbase_MAXBUFFERLEN,buf,argv[0]);
		*ret = TObject_CnvFromText(gCP, gTP, buf);
		FMemory_Free(gCP, gTP, (HMemory)theData);
		}
    FrameExit(*ret)
	}

else
if (argc != 1) 
	{
	*ret = TERROR("string: Missing arguments");
	FrameExit(*ret)
	}
else
	{
	*ret = gCP->Tval_VOID;
	}


/*  Handle an invalid argument which is already in error.  */

if (!_VALIDTVAL(argv[0]))
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    }
else

/*  Handle an argument which is already a string.  */

if (argv[0].Tag == TYSTRING)
    {
    *ret = argv[0];
    }
else

/*  Handle an argument which is text.  */

if (argv[0].Tag == TYTEXT)
    {
    *ret = argv[0];
    }
else

/*  Handle an argument which is character.  */

if (argv[0].Tag == TYCHAR)
    {
    argv[0].u.Text[1] = 0;
    *ret = argv[0];
	ret->Tag = TYTEXT;
    }
else

/*  Handle an argument which is a byte vector.  */

if (argv[0].Tag == TYBYTEVECTOR)
    {
    stringPtr = (LpCHAR)FSmartbase_VectorPtr(gCP,gTP,argv[0]);
    *ret = TObject_CnvFromText(gCP, gTP, stringPtr);
    }
else

/*  Handling an argument which is a substring.  */

if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    stringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (stringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        
    *ret = FSmartbase_CnvFromSubstring(gCP, gTP, stringPtr, 0, SubLen(argv[0]));
    }
else

/*  Coerce the argument to a string.  */
    {
	theData = (HMChar)FMemory_New(gCP, gTP, (NUM)_FSmartbase_MAXBUFFERLEN+2,TRUE);
	buf = &atHMChar(theData,0);
    *err = TObject_CnvToText(gCP,gTP,buf, _FSmartbase_MAXBUFFERLEN, argv[0]);
    *ret = TObject_CnvFromText(gCP, gTP, buf);
    if (theData != NULL) FMemory_Free(gCP, gTP, (HMemory)theData);
	ExitOnError(*err);  
    }
    
FrameExit(*ret)
}


/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_Downcase

The FConvert_Downcase function converts upper case characters to lower case characters
in Character or String objects.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_Downcase(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
LpCHAR                  sp;
NUM                     lengthOf;
NUM                     indexOf;
BOLE					returnSymbolSW = FALSE;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Initialization   */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Compute pointer to and length of character string. */

if (((asTag(&argv[0]) == TYSYMBOL) || (asTag(&argv[0]) == TYQUOTEDSYMBOL)) && (asObj(&argv[0]) != NIL))
    {
	returnSymbolSW = TRUE;
    *ret = FMake_String(gCP,gTP,1,&argv[0]);
	ExitOnError(*ret);
    sp = FObject_GetStringPtr(gCP, gTP, asObject(ret));
    lengthOf = strlen((char*)sp);
    }
else
if (asTag(&argv[0]) == TYCHAR)
    {
    *ret = argv[0];
    sp = (char *)&asChar(ret);
    lengthOf = 1;
    }
else
if (asTag(&argv[0]) == TYTEXT)
    {
    *ret = argv[0];
    sp = &asText(ret)[0];
    lengthOf = strlen((char*)sp);
    }
else
if ((asTag(&argv[0]) == TYSTRINGSUBSTR))
    {
    sp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
       
    *ret = FSmartbase_CnvFromSubstring(gCP, gTP, sp, 0, SubLen(argv[0]));
    ExitOnError(*ret);
    
    sp = FObject_GetStringPtr(gCP, gTP, asObject(ret));
    lengthOf = strlen((char*)sp);    
    }
else
if ((asTag(&argv[0]) == TYSTRING) && (asObj(&argv[0]) != NIL))
    {
    *ret = FUtil2_Copy(gCP,gTP,1,&argv[0]);
	ExitOnError(*ret);
    sp = FObject_GetStringPtr(gCP,gTP,asObject(ret));
    lengthOf = strlen((char*)sp);
    }
else
if ((asTag(&argv[0]) == TYBYTEVECTOR) && (asObj(&argv[0]) != NIL))
    {
    *ret = FUtil2_Copy(gCP,gTP,1,&argv[0]);
	ExitOnError(*ret);
    sp = (LpCHAR)ByteArray(*ret);
    lengthOf = ByteVector(*ret)->itsMaxItemIndex; 
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
    

/*  Convert all upper case characters into lower case characters. */

for(indexOf = 0; indexOf < lengthOf; indexOf++)
    {
    if (ISXUPPER((NUM)sp[indexOf]))
        sp[indexOf] += 32; 
    }

/*  Convert the result into a Symbol (if necessary). */

if (returnSymbolSW == TRUE)
	{
    *ret = FMake_Symbol(gCP,gTP,1,ret);
	ExitOnError(*ret);
	ret->QuoteCnt = argv[0].QuoteCnt;
	if (ret->QuoteCnt > 0) ret->Tag = TYQUOTEDSYMBOL;
	}

FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_Upcase

The FConvert_Upcase function lower case characters to upper case characters
in Character or String objects.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FConvert_Upcase(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
LpCHAR                  sp;
NUM                     lengthOf;
NUM                     indexOf;
BOLE					returnSymbolSW = FALSE;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Initialization   */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Compute pointer to and length of character string. */

if (((asTag(&argv[0]) == TYSYMBOL) || (asTag(&argv[0]) == TYQUOTEDSYMBOL)) && (asObj(&argv[0]) != NIL))
    {
	returnSymbolSW = TRUE;
    *ret = FMake_String(gCP,gTP,1,&argv[0]);
	ExitOnError(*ret);
    sp = FObject_GetStringPtr(gCP, gTP, asObject(ret));
    lengthOf = strlen((char*)sp);
    }
else
if (asTag(&argv[0]) == TYCHAR)
    {
    *ret = argv[0];
    sp = (char *)&asChar(ret);
    lengthOf = 1;
    }
else
if (asTag(&argv[0]) == TYTEXT)
    {
    *ret = argv[0];
    sp = &asText(ret)[0];
    lengthOf = strlen((char*)sp);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
       
    *ret = FSmartbase_CnvFromSubstring(gCP, gTP, sp, 0, SubLen(argv[0]));
    ExitOnError(*ret);
    
    sp = FObject_GetStringPtr(gCP,gTP,asObject(ret));
    lengthOf = strlen((char*)sp);    
    }
else
if ((asTag(&argv[0]) == TYSTRING) && (asObj(&argv[0]) != NIL))
    {
    *ret = FUtil2_Copy(gCP,gTP,1,&argv[0]);
	ExitOnError(*ret);
    sp = FObject_GetStringPtr(gCP, gTP, asObject(ret));
    lengthOf = strlen((char*)sp);
    }
else
if ((asTag(&argv[0]) == TYBYTEVECTOR) && (asObj(&argv[0]) != NIL))
    {
    *ret = FUtil2_Copy(gCP,gTP,1,&argv[0]);
	ExitOnError(*ret);
    sp = (LpCHAR)ByteArray(*ret);
    lengthOf = ByteVector(*ret)->itsMaxItemIndex; 
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
    

/*  Convert all lower case characters into upper case characters. */

for(indexOf = 0; indexOf < lengthOf; indexOf++)
    {
    if (ISXLOWER((NUM)sp[indexOf]))
        sp[indexOf] -= 32; 
    }

/*  Convert the result into a Symbol (if necessary). */

if (returnSymbolSW == TRUE)
	{
    *ret = FMake_Symbol(gCP,gTP,1,ret);
	ExitOnError(*ret);
	ret->QuoteCnt = argv[0].QuoteCnt;
	if (ret->QuoteCnt > 0) ret->Tag = TYQUOTEDSYMBOL;
	}

FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ObjToList

The FConvert_ObjToList cProcedure converts the object {object} into a list 
returning the new list. The {object} may be either a List, aVector, 
a Range, a  Structure, or a singleton. 

Several examples follow.

    (objectToList  #(4.2  6  5.1))              =>  (4.2   6   5.1)

#endif

TVAL FConvert_ObjToList(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 size;
NUM                 indexOf;
StartFrame
DeclareOBJ(TPair,sp);
DeclareOBJ(TPair,pp);
DeclareOBJ(TVector,vp);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TCpxVector, vCpxp);
DeclareTVAL(ndx1);
DeclareTVAL(argr);
EndFrame

/*  There must be only one argument */

*argr = gCP->Tval_VOID;
if ((argc != 1) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Make the first Pair in the new list */

sp = TPair_New(gCP,gTP);
asObject(argr) = (TObject*)sp;
asTag(argr) = TYPAIR;

switch(asTag(&argv[0]))
	{

	case TYPAIR:
		/*  If the argument is a list, do dotcdr processing if required. */

		pp = (TPair*)asObject(&argv[0]);
		if(asTag(&pp->itsCar) == TYSYMBOL && asSymbol(&pp->itsCar) == gCP->TLambda_dotcdr)
			{
			/*  error... */
			FrameExit(gCP->TObject_ERROR_SYNTAX)
			}
		else
			{
			while((asTag(&pp->itsCdr) == TYPAIR) && (asPair(&pp->itsCdr) != NIL))
				{
				if(asTag(&pp->itsCar) == TYSYMBOL && asSymbol(&pp->itsCar) == gCP->TLambda_dotcdr)
					{
					pp = asPair(&pp->itsCdr);
					sp->itsCdr = pp->itsCar;
					break;
					}
				else
					{
					sp = pp;
					pp = asPair(&pp->itsCdr);
					}
				}

			*argr = argv[0];
			FrameExit(*argr)
			}
	break;

	case TYVECTOR:
		vp = (TVector*)asObject(&argv[0]);
		size = vp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		sp->itsCar = atHMTval(vp->itsTvalArray,indexOf);    
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			sp->itsCar = atHMTval(vp->itsTvalArray,indexOf);
			}
		FrameExit(*argr)
	break;

	case TYMATRIX:
		mp = (TMatrix*)asObject(&argv[0]);
		size = mp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		sp->itsCar = atHMTval(mp->itsTvalMatrix,indexOf);    
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			sp->itsCar = atHMTval(mp->itsTvalMatrix,indexOf);
			}
		FrameExit(*argr)
	break;

	case TYNUMMATRIX:
		np = (TNumMatrix*)asObject(&argv[0]);
		size = np->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		sp->itsCar = TREAL(atHMReal(np->itsRealMatrix,indexOf));    
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			sp->itsCar = TREAL(atHMReal(np->itsRealMatrix,indexOf));
			}
		FrameExit(*argr)
	break;

	case TYBITVECTOR:
		vBitp = (TBitVector*)asObject(&argv[0]);
		size = vBitp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asTag(ndx1) = TYNUM;
		asInt(ndx1) = indexOf;  
		sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asInt(ndx1) = indexOf;  
			sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		FrameExit(*argr)
	break;

	case TYBYTEVECTOR:
		vBytep = (TByteVector*)asObject(&argv[0]);
		size = vBytep->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asTag(ndx1) = TYNUM;
		asInt(ndx1) = indexOf;  
		sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asInt(ndx1) = indexOf;  
			sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		FrameExit(*argr)
	break;

	case TYINTVECTOR:
		vIntp = (TIntVector*)asObject(&argv[0]);
		size = vIntp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asTag(ndx1) = TYNUM;
		asInt(ndx1) = indexOf;  
		sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asInt(ndx1) = indexOf;  
			sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		FrameExit(*argr)
	break;

	case TYLONGVECTOR:
		vLngp = (TLongVector*)asObject(&argv[0]);
		size = vLngp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asTag(ndx1) = TYNUM;
		asInt(ndx1) = indexOf;  
		sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asInt(ndx1) = indexOf;  
			sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		FrameExit(*argr)
	break;

	case TYSHORTVECTOR:
		vShtp = (TShtVector*)asObject(&argv[0]);
		size = vShtp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asTag(ndx1) = TYNUM;
		asInt(ndx1) = indexOf;  
		sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asInt(ndx1) = indexOf;  
			sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		FrameExit(*argr)
	break;

	case TYNUMVECTOR:
		vNump = (TNumVector*)asObject(&argv[0]);
		size = vNump->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asTag(ndx1) = TYNUM;
		asInt(ndx1) = indexOf;  
		sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asInt(ndx1) = indexOf;  
			sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		FrameExit(*argr)
	break;

	case TYFLTVECTOR:
		vFltp = (TFltVector*)asObject(&argv[0]);
		size = vFltp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asTag(ndx1) = TYNUM;
		asInt(ndx1) = indexOf;  
		sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asInt(ndx1) = indexOf;  
			sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		FrameExit(*argr)
	break;

	case TYOBJVECTOR:
		vObjp = (TObjVector*)asObject(&argv[0]);
		size = vObjp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asTag(ndx1) = TYNUM;
		asInt(ndx1) = indexOf;  
		sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asInt(ndx1) = indexOf;  
			sp->itsCar = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		FrameExit(*argr)
	break;

	case TYSTRUCTURE:
		ep = (TStructure*)asObject(&argv[0]);
		size = ep->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		asObject(&sp->itsCar) = atHMBind(ep->itsDictionaryArray,indexOf).Key;
		asTag(&sp->itsCar) = asObject(&sp->itsCar)->itsObjectType;
		asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
		asTag(&sp->itsCdr) = TYPAIR;
		sp = (TPair*)asObject(&sp->itsCdr);
		sp->itsCar = atHMBind(ep->itsDictionaryArray,indexOf).Value;
		while (++indexOf < size)
			{
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			asObject(&sp->itsCar) = atHMBind(ep->itsDictionaryArray,indexOf).Key;
			asTag(&sp->itsCar) = asObject(&sp->itsCar)->itsObjectType;
			asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			sp->itsCar = atHMBind(ep->itsDictionaryArray,indexOf).Value;
			}
		FrameExit(*argr)
	break;
	case TYCPXVECTOR:
		vCpxp = argv[0].u.CpxVector;
		size = vCpxp->itsMaxItemIndex;
		if (size <= 0) FrameExit(*argr)
		indexOf = 0;
		ndx1->Tag = TYNUM;
		ndx1->u.Int = indexOf;  
		sp->itsCar = TCpxVector_GetIV1(gCP,gTP, argv[0], *ndx1);
		while (++indexOf < size)
		{	asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
			asTag(&sp->itsCdr) = TYPAIR;
			sp = (TPair*)asObject(&sp->itsCdr);
			ndx1->u.Int = indexOf;  
			sp->itsCar = TCpxVector_GetIV1(gCP,gTP, argv[0], *ndx1);
		}
		FrameExit(*argr)
	break;
	default:
		/*  Convert anything else into a single element list. */
		sp->itsCar = argv[0];
		sp->itsCdr = gCP->Tval_VOID; 
		FrameExit(*argr)
	break;
	}
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ObjToVector

The FConvert_ObjToVector procedure converts an object to a vector. 

#endif

TVAL FConvert_ObjToVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
LpTVAL              rp;
NUM                 indexOf;
NUM                 targetIndex;
NUM                 sourceIndex;
NUM                 size, rank, col, row, plane;
StartFrame
DeclareOBJ(TPair,pp);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TVector,vp);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TCpxVector,vCpxp);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDirectory,xp);
DeclareTVAL(ret);
DeclareTVAL(aValue);
DeclareTVAL(ec);
DeclareTVAL(argr);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVAL(ndx3);
DeclareTVAL(tmp);
DeclareTVAL(tmp1);
DeclareTVAL(attb);
EndFrame
 
/*  Initialization */

*argr = gCP->Tval_VOID;

/*  Manage the conversion into an attributed vectors. */

if ((argc == 2) && (argv[0].Tag == TYVECTOR) && (Vector(argv[0])->itsAttributes != NULL))
	{
	switch (argv[1].Tag)
		{
		case TYVECTOR:
			if (Vector(argv[1])->itsAttributes == NULL) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

			*attb = TOBJ(Vector(argv[0])->itsAttributes);
			for (indexOf = 0; indexOf < ObjVector(*attb)->itsMaxItemIndex; ++indexOf)
				{
				*aValue = TVector_GetIV1(gCP,gTP,argv[1],TOBJ(ObjArray(*attb)[indexOf]));
				if (aValue->Tag == TYERROR) *aValue = gCP->Tval_VOID;
				TvalArray(argv[0])[indexOf] = *aValue;
				}

			FrameExit(argv[0])
		break;

		case TYSTRUCTURE:
			*attb = TOBJ(Vector(argv[0])->itsAttributes);
			for (indexOf = 0; indexOf < ObjVector(*attb)->itsMaxItemIndex; ++indexOf)
				{
				*aValue = TStructure_GetIV1(gCP,gTP,argv[1],TOBJ(ObjArray(*attb)[indexOf]));
				if (aValue->Tag == TYERROR) *aValue = gCP->Tval_VOID;
				TvalArray(argv[0])[indexOf] = *aValue;
				}

			FrameExit(argv[0])
		break;

		case TYDICTIONARY:
			*attb = TOBJ(Vector(argv[0])->itsAttributes);
			for (indexOf = 0; indexOf < ObjVector(*attb)->itsMaxItemIndex; ++indexOf)
				{
				*aValue = TDictionary_GetIV1(gCP,gTP,argv[1],TOBJ(ObjArray(*attb)[indexOf]));
				if (aValue->Tag == TYERROR) *aValue = gCP->Tval_VOID;
				TvalArray(argv[0])[indexOf] = *aValue;
				}
			FrameExit(argv[0])
		break;

		case TYDIRECTORY:
			*attb = TOBJ(Vector(argv[0])->itsAttributes);
			for (indexOf = 0; indexOf < ObjVector(*attb)->itsMaxItemIndex; ++indexOf)
				{
				*aValue = TDirectory_GetIV1(gCP,gTP,argv[1],TOBJ(ObjArray(*attb)[indexOf]));
				if (aValue->Tag == TYERROR) *aValue = gCP->Tval_VOID;
				TvalArray(argv[0])[indexOf] = *aValue;
				}

			FrameExit(argv[0])
		break;

		default:
			FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
		break;
		}
	}

/*  Manage the conversion of a single object into a Vector. */

if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
switch(asTag(&argv[0]))
{
case TYVECTOR:
	if (asObj(&argv[0]) != NIL)
		{
		*argr = argv[0];
		}
	else
		{
		/*  return an empty vector */
    
		asObject(argr) = (TObject*)TVector_New(gCP,gTP);
		asTag(argr) = TYVECTOR;
		}
	FrameExit(*argr)
break;

case TYMATRIX:
	mp = (TMatrix*)asObject(&argv[0]);
	if ((rank = mp->itsRank) == 1)
		{
		asObject(argr) = (TObject*)TVector_New(gCP,gTP);
		vp = (TVector*)asObject(argr);
		asTag(argr) = TYVECTOR;
		if (asObj(&argv[0]) != NIL)
			{
			size = mp->itsMaxItemIndex;
			TVector_SetMaxIndex(gCP,gTP,*argr,size);
			targetIndex = sourceIndex = 0;
			asTag(ndx1) = TYNUM;
			while (targetIndex < size)
				{
				asInt(ndx1) = sourceIndex++;
				atHMTval(vp->itsTvalArray,targetIndex++) = 
					(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
				}
			}
		}
	else if (rank == 2)
		{	// Create an object vector
		asObject(argr) = (TObject*)TObjVector_New(gCP,gTP);
		asTag(argr) = TYOBJVECTOR;
		size = mp->itsDimensions[0];
		TObjVector_SetMaxIndex(gCP,gTP,*argr,size);
		vObjp = (TObjVector*)asObject(argr);
		// Add one vector to object vector for each row in the matrix
		asTag(ndx1) = asTag(ndx2) = TYNUM;	// Tval to hold indices
		for (row = 0; row < size; ++row)
			{	
			asInt(ndx1) = row;
			asObject(tmp) = (TObject*)TVector_New(gCP,gTP);
			asTag(tmp) = TYVECTOR;
			TObjVector_SetIV1(gCP, gTP, *argr, *ndx1, *tmp);
			// Add each column of this row in matrix to the row vector
			for (col = 0; col < mp->itsDimensions[1]; ++col)
				{
				asInt(ndx2) = col;
				*aValue = TMatrix_GetIV2(gCP,gTP,argv[0],*ndx1,*ndx2);
				TVector_SetIV1(gCP, gTP, *tmp, *ndx2, *aValue);
				}
			}
		}
	else if (rank == 3)
		{	// Create an object vector
		asObject(argr) = (TObject*)TObjVector_New(gCP,gTP);
		asTag(argr) = TYOBJVECTOR;
		size = mp->itsDimensions[0];
		TObjVector_SetMaxIndex(gCP,gTP,*argr,size);
		vObjp = (TObjVector*)asObject(argr);
		// Add one object vector to object vector for each plane in the matrix
		asTag(ndx1) = asTag(ndx2) = asTag(ndx3) = TYNUM;
		for (plane = 0; plane < size; ++plane)
			{
			asInt(ndx1) = plane;
			asObject(tmp1) = (TObject*)TObjVector_New(gCP,gTP);
			asTag(tmp1) = TYOBJVECTOR;
			TObjVector_SetIV1(gCP, gTP, *argr, *ndx1, *tmp1);
			// Add each row of the matrix in this plane to a new row vector
			for (row = 0; row < mp->itsDimensions[1]; ++row)
				{	
				asInt(ndx2) = row;
				asObject(tmp) = (TObject*)TVector_New(gCP,gTP);
				asTag(tmp) = TYVECTOR;
				TObjVector_SetIV1(gCP, gTP, *tmp1, *ndx2, *tmp);
				// Add each column of the matrix in this row to the row vector
				for (col = 0; col < mp->itsDimensions[2]; ++col)
					{
					asInt(ndx3) = col;
					*aValue = TMatrix_GetIV3(gCP,gTP,argv[0],*ndx1,*ndx2,*ndx3);
					TVector_SetIV1(gCP, gTP, *tmp, *ndx3, *aValue);
					}
				}
			}
		}
	else
		*argr = TERROR("objectToVector: input matrix has incorrect rank");

	FrameExit(*argr)
break;

case TYNUMMATRIX:
	mp = (TMatrix*)asObject(&argv[0]);
	if ((rank = mp->itsRank) == 1)
		{
		asObject(argr) = (TObject*)TVector_New(gCP,gTP);
		vp = (TVector*) asObject(argr);
		asTag(argr) = TYVECTOR;
		if (asObj(&argv[0]) != NIL)
			{
			np = (TNumMatrix*)asObject(&argv[0]);
			size = np->itsMaxItemIndex;
			TVector_SetMaxIndex(gCP,gTP,*argr,size);
			targetIndex = sourceIndex = 0;
			asTag(ndx1) = TYNUM;
			while (targetIndex < size)
				{
				asInt(ndx1) = sourceIndex++;
				atHMTval(vp->itsTvalArray,targetIndex++) = 
					(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
				}
			}
		}
	else if (rank == 2)
		{	// Create an object vector
		asObject(argr) = (TObject*)TObjVector_New(gCP,gTP);
		asTag(argr) = TYOBJVECTOR;
		size = mp->itsDimensions[0];
		TObjVector_SetMaxIndex(gCP,gTP,*argr,size);
		vObjp = (TObjVector*)asObject(argr);
		// Add one number vector to object vector for each row in the matrix
		asTag(ndx1) = asTag(ndx2) = TYNUM;	// Tval to hold indices
		for (row = 0; row < size; ++row)
			{	
			asInt(ndx1) = row;
			asObject(tmp) = (TObject*)TNumVector_New(gCP,gTP);
			asTag(tmp) = TYNUMVECTOR;
			TObjVector_SetIV1(gCP, gTP, *argr, *ndx1, *tmp);
			// Add each column of this row in matrix to the row vector
			for (col = 0; col < mp->itsDimensions[1]; ++col)
				{
				asInt(ndx2) = col;
				*aValue = TNumMatrix_GetIV2(gCP,gTP,argv[0],*ndx1,*ndx2);
				TNumVector_SetIV1(gCP, gTP, *tmp, *ndx2, *aValue);
				}
			}
		}
	else if (rank == 3)
		{	// Create an object vector
		asObject(argr) = (TObject*)TObjVector_New(gCP,gTP);
		asTag(argr) = TYOBJVECTOR;
		size = mp->itsDimensions[0];
		TObjVector_SetMaxIndex(gCP,gTP,*argr,size);
		vObjp = (TObjVector*)asObject(argr);
		// Add one object vector to object vector for each plane in the matrix
		asTag(ndx1) = asTag(ndx2) = asTag(ndx3) = TYNUM;
		for (plane = 0; plane < size; ++plane)
			{
			asInt(ndx1) = plane;
			asObject(tmp1) = (TObject*)TObjVector_New(gCP,gTP);
			asTag(tmp1) = TYOBJVECTOR;
			TObjVector_SetIV1(gCP, gTP, *argr, *ndx1, *tmp1);
			// Add each row of the matrix in this plane to a new row vector
			for (row = 0; row < mp->itsDimensions[1]; ++row)
				{	
				asInt(ndx2) = row;
				asObject(tmp) = (TObject*)TNumVector_New(gCP,gTP);
				asTag(tmp) = TYNUMVECTOR;
				TObjVector_SetIV1(gCP, gTP, *tmp1, *ndx2, *tmp);
				// Add each column of the matrix in this row to the row vector
				for (col = 0; col < mp->itsDimensions[2]; ++col)
					{
					asInt(ndx3) = col;
					*aValue = TNumMatrix_GetIV3(gCP,gTP,argv[0],*ndx1,*ndx2,*ndx3);
					TNumVector_SetIV1(gCP, gTP, *tmp, *ndx3, *aValue);
					}
				}
			}
		}
	else
		*argr = TERROR("objectToVector: number matrix has incorrect rank");

	FrameExit(*argr)
break;

case TYBITVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*) asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		vBitp = (TBitVector*)asObject(&argv[0]);
		size = vBitp->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr,size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
			{
			asInt(ndx1) = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		}
	FrameExit(*argr)
break;

case TYBYTEVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		vBytep = (TByteVector*)asObject(&argv[0]);
		size = vBytep->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
			{
			asInt(ndx1) = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		}
	FrameExit(*argr)
break;

case TYINTVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		vIntp = (TIntVector*)asObject(&argv[0]);
		size = vIntp->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
			{
			asInt(ndx1) = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		}
	FrameExit(*argr)
break;

case TYLONGVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		vLngp = (TLongVector*)asObject(&argv[0]);
		size = vLngp->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
			{
			asInt(ndx1) = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		}
	FrameExit(*argr)
break;

case TYSHORTVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		vShtp = (TShtVector*)asObject(&argv[0]);
		size = vShtp->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
			{
			asInt(ndx1) = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		}
	FrameExit(*argr)
break;

case TYNUMVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		vNump = (TNumVector*)asObject(&argv[0]);
		size = vNump->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
			{
			asInt(ndx1) = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		}
	FrameExit(*argr)
break;

case TYFLTVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		vFltp = (TFltVector*)asObject(&argv[0]);
		size = vFltp->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
			{
			asInt(ndx1) = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		}
	FrameExit(*argr)
break;

case TYOBJVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		vObjp = (TObjVector*)asObject(&argv[0]);
		size = vObjp->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
			{
			asInt(ndx1) = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			}
		}
	FrameExit(*argr)
break;

case TYPAIR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		*ret = FUtil2_Length(gCP,gTP,(NUM)1,&argv[0]);
		ExitOnError(*ec);
		size = asInt(ret);
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		indexOf = 0;
		rp = &argv[0];
		while ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
			{
			pp = (TPair*)asObject(rp);
			rp = &pp->itsCdr;
			if(asTag(&pp->itsCar) == TYSYMBOL && asSymbol(&pp->itsCar) == gCP->TLambda_dotcdr)
				{
				if((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
					{
					pp = (TPair*)asObject(rp);
					vp->itsCdr = pp->itsCar;
					TVector_SetMaxIndex(gCP,gTP,*argr, indexOf);
					break;
					}
				else
					{
					/*  error... */
					}
				}
			atHMTval(vp->itsTvalArray,indexOf++) = pp->itsCar;
			}
		}
	FrameExit(*argr)
break;

case TYSTRUCTURE:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		ep = (TStructure*)asObject(&argv[0]);
		size = ep->itsMaxItemIndex * 2;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		while (targetIndex < size)
			{
			asObject(&atHMTval(vp->itsTvalArray,targetIndex)) = atHMBind(ep->itsDictionaryArray,sourceIndex).Key;
			asTag(&atHMTval(vp->itsTvalArray,targetIndex)) = asObject(&atHMTval(vp->itsTvalArray,targetIndex))->itsObjectType;
            ++targetIndex;
			atHMTval(vp->itsTvalArray,targetIndex) = atHMBind(ep->itsDictionaryArray,sourceIndex).Value;
            ++targetIndex;
            ++sourceIndex;
			}
		}
	FrameExit(*argr)
break;

case TYDICTIONARY:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		dp = (TDictionary*)asObject(&argv[0]);
		size = dp->itsMaxItemIndex * 2;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		while (targetIndex < size)
			{
			asObject(&atHMTval(vp->itsTvalArray,targetIndex)) = atHMBind(dp->itsDictionaryArray,sourceIndex).Key;
			asTag(&atHMTval(vp->itsTvalArray,targetIndex)) = asObject(&atHMTval(vp->itsTvalArray,targetIndex))->itsObjectType;
			++targetIndex;
			atHMTval(vp->itsTvalArray,targetIndex) = atHMBind(dp->itsDictionaryArray,sourceIndex).Value;
			++targetIndex;
			++sourceIndex;
			}
		}
	FrameExit(*argr)
break;

case TYDIRECTORY:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
		{
		xp = (TDirectory*)asObject(&argv[0]);
		size = xp->itsMaxItemIndex * 2;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		while (targetIndex < size)
			{
			atHMTval(vp->itsTvalArray,targetIndex++) = atHMPBind(xp->itsDirectoryArray,sourceIndex).Key;
			atHMTval(vp->itsTvalArray,targetIndex++) = atHMPBind(xp->itsDirectoryArray,sourceIndex++).Value;
			}
		}
	FrameExit(*argr)
break;
case TYCPXVECTOR:
	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	vp = (TVector*)asObject(argr);
	asTag(argr) = TYVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vCpxp = argv[0].u.CpxVector;
		size = vCpxp->itsMaxItemIndex;
		TVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	ndx1->u.Int = sourceIndex++;
			atHMTval(vp->itsTvalArray,targetIndex++) = 
				TCpxVector_GetIV1(gCP,gTP, argv[0], *ndx1);
		}
	}
	FrameExit(*argr)
break;

default:
	/*  Convert anything else to a singleton Vector. */

	asObject(argr) = (TObject*)TVector_New(gCP,gTP);
	asTag(argr) = TYVECTOR;
	vp = (TVector*)asObject(argr);
	TVector_SetMaxIndex(gCP,gTP,*argr,1);
	atHMTval(vp->itsTvalArray,0) = argv[0];
	FrameExit(*argr)
}
}
/*	--------------------------------------------------------------------------------------
FConvert_ObjToNumVector

The FConvert_ObjToNumVector procedure converts an object to a number vector. 
--------------------------------------------------------------------------------------- */
TVAL FConvert_ObjToNumVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
LpTVAL              rp;
NUM                 indexOf;
NUM                 targetIndex;
NUM                 sourceIndex;
NUM                 size;
StartFrame
DeclareOBJ(TPair,pp);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TNumVector,vp);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDirectory,xp);
DeclareOBJ(TCpxVector,vCpxp);
DeclareOBJ(TCpx, cp);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVAL(argr);
DeclareTVAL(ndx1);
DeclareTVAL(tmp);
EndFrame
 
//  Initialization
*argr = gCP->Tval_VOID;

//  Manage the conversion of a single object into a Vector.
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

switch(asTag(&argv[0]))
{case TYNUMVECTOR:
	if (asObj(&argv[0]) != NIL)
	{	*argr = argv[0];
	}
	else
	{	//  return an empty vector
		asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
		asTag(argr) = TYNUMVECTOR;
	}
	FrameExit(*argr)
	break;

case TYMATRIX:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*) asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	mp = (TMatrix*)asObject(&argv[0]);
		size = mp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr,size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
				
		}
	}
	FrameExit(*argr)
	break;

case TYNUMMATRIX:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*) asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	np = (TNumMatrix*)asObject(&argv[0]);
		size = np->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr,size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);					
		}
	}
	FrameExit(*argr)
	break;

case TYVECTOR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vNump = (TNumVector*)asObject(&argv[0]);
		size = vNump->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYBITVECTOR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*) asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vBitp = (TBitVector*)asObject(&argv[0]);
		size = vBitp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr,size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYBYTEVECTOR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vBytep = (TByteVector*)asObject(&argv[0]);
		size = vBytep->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYINTVECTOR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vIntp = (TIntVector*)asObject(&argv[0]);
		size = vIntp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYLONGVECTOR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vLngp = (TLongVector*)asObject(&argv[0]);
		size = vLngp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYSHORTVECTOR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vShtp = (TShtVector*)asObject(&argv[0]);
		size = vShtp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYFLTVECTOR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vFltp = (TFltVector*)asObject(&argv[0]);
		size = vFltp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYOBJVECTOR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vObjp = (TObjVector*)asObject(&argv[0]);
		size = vObjp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		asTag(ndx1) = TYNUM;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYPAIR:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	*ret = FUtil2_Length(gCP,gTP,(NUM)1,&argv[0]);
		ExitOnError(*ec);
		size = asInt(ret);
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		indexOf = 0;
		rp = &argv[0];
		while ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
		{	pp = (TPair*)asObject(rp);
			rp = &pp->itsCdr;
			if (asTag(&pp->itsCar) == TYSYMBOL && asSymbol(&pp->itsCar) == gCP->TLambda_dotcdr)
			{	if ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
				{	pp = (TPair*)asObject(rp);
					vp->itsCdr = pp->itsCar;
					TNumVector_SetMaxIndex(gCP,gTP,*argr, indexOf);
					break;
				}
				else
				{	//  error...
				}
			}
			*tmp = pp->itsCar;
			atHMReal(vp->itsRealArray,indexOf++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYSTRUCTURE:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	ep = (TStructure*)asObject(&argv[0]);
		size = ep->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		while (targetIndex < size)
		{	*tmp = atHMBind(ep->itsDictionaryArray,sourceIndex++).Value;
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);					
		}
	}
	FrameExit(*argr)
	break;

case TYDICTIONARY:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	dp = (TDictionary*)asObject(&argv[0]);
		size = dp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		while (targetIndex < size)
		{	*tmp = atHMBind(dp->itsDictionaryArray,sourceIndex++).Value;
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYDIRECTORY:
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	vp = (TNumVector*)asObject(argr);
	asTag(argr) = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	xp = (TDirectory*)asObject(&argv[0]);
		size = xp->itsMaxItemIndex;
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		while (targetIndex < size)
		{	*tmp = atHMPBind(xp->itsDirectoryArray,sourceIndex++).Value;
			atHMReal(vp->itsRealArray,targetIndex++) = asNumIndex(tmp);		
		}
	}
	FrameExit(*argr)
	break;
case TYCPXVECTOR:
	argr->u.NumVector = vp = TNumVector_New(gCP,gTP);
	argr->Tag = TYNUMVECTOR;
	if (asObj(&argv[0]) != NIL)
	{	vCpxp = argv[0].u.CpxVector;
		size = 2 * vCpxp->itsMaxItemIndex;		// 2 doubles per complex value
		TNumVector_SetMaxIndex(gCP,gTP,*argr, size);
		targetIndex = sourceIndex = 0;
		ndx1->Tag = TYNUM;
		while (targetIndex < size)
		{	ndx1->u.Int = sourceIndex++;
			*tmp = TCpxVector_GetIV1(gCP, gTP, argv[0], *ndx1);
			cp = tmp->u.Complex;
			atHMReal(vp->itsRealArray,targetIndex++) = cp->itsReal;
			atHMReal(vp->itsRealArray,targetIndex++) = cp->itsImag;
		}
	}
	FrameExit(*argr)
	break;
default:
	//  Convert anything else to a singleton number vector.
	asObject(argr) = (TObject*)TNumVector_New(gCP,gTP);
	asTag(argr) = TYNUMVECTOR;
	vp = (TNumVector*)asObject(argr);
	TNumVector_SetMaxIndex(gCP,gTP,*argr,1);
	atHMReal(vp->itsRealArray,0) = asNumIndex(&argv[0]);
	FrameExit(*argr)
	}
}

/*	--------------------------------------------------------------------------------------
FConvert_ObjToMatrix

The FConvert_ObjToMatrix procedure converts an object to a number vector. 
--------------------------------------------------------------------------------------- */
TVAL FConvert_ObjToMatrix(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
LpTVAL              rp;
NUM                 indexOf;
NUM                 targetIndex;
NUM                 sourceIndex;
NUM                 size, ncols, nrows, rank, col, row, plane, dim;
StartFrame
DeclareOBJ(TPair,pp);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TVector,vp);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDirectory,xp);
DeclareOBJ(TCpxVector,vCpxp);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVAL(argr);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVAL(ndx3);
DeclareTVAL(tmp);
DeclareTVAL(tmp1);
EndFrame
 
//  Initialization
*argr = gCP->Tval_VOID;
if (argc < 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
if (asObj(&argv[0]) == NIL) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
asTag(ndx1) = asTag(ndx2) = asTag(ndx3) = TYNUM;

// Determine matrix configuration from arguments
rank = plane = row = col = 0;
if (argc > 1)
{	rank = asNumIndex(&argv[1]);
	if (argc > 2)
		 plane = asNumIndex(&argv[2]);
	if (argc > 3)
		row = asNumIndex(&argv[3]);
	if (argc > 4)
		col = asNumIndex(&argv[4]);
}

//  Manage the conversion of a single object into a Matrix
switch(asTag(&argv[0]))
{   
case TYMATRIX:
	// If there are no resize arguments, then return the matrix sent.
	if (argc == 1)
		{
        *argr = argv[0];
		FrameExit(*argr);
		}
	else
	    // If there are resize arguments, then resize the matrix sent.
		{
		argr->u.Object = (TObject*)TMatrix_Copy(gCP,gTP,argv[0]);
		argr->Tag = TYMATRIX;
		argv[0] = *argr;
		*ec = TMatrix_Resize(gCP,gTP,argc,argv);
		ExitOnError(*ec);
		FrameExit(argv[0]);
		}
	break;

case TYNUMMATRIX:
	// Create a new matrix with the same configuration as the number matrix.
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	np = (TNumMatrix*)asObject(&argv[0]);
	size = np->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP,gTP,*argr,size);
	mp->itsRank = np->itsRank;
	mp->itsCdr = np->itsCdr;
	for (dim = 0; dim < np->itsRank; ++dim)
		mp->itsDimensions[dim] = np->itsDimensions[dim];
	// Copy all of the elements from the number matrix to the new matrix.
	for (row = 0; row < size; ++row)
	{	asInt(ndx1) = row;
		*tmp = TNumMatrix_GetIV1(gCP,gTP,argv[0],*ndx1);
		TMatrix_SetIV1(gCP, gTP, *argr, *ndx1, *tmp);
	}
	if (argc > 1)
	    // If there are resize arguments, then resize the new matrix.
		{
		argv[0] = *argr;
		*ec = TMatrix_Resize(gCP,gTP,argc,argv);
		ExitOnError(*ec);
		}
    FrameExit(*argr);
	break;

case TYVECTOR:
	// Create a new matrix and set configuration
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	asTag(argr) = TYMATRIX;
	mp = (TMatrix*)asObject(argr);
	vp = (TVector*)asObject(&argv[0]);
	size = vp->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP,gTP,*argr,size);
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;

	// Fill the new matrix tval array with elements from the vector
	targetIndex = sourceIndex = 0;
	for (row = 0; row < size; ++row)
	{	asInt(ndx1) = row;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
		atHMTval(mp->itsTvalMatrix, row) = *tmp;
	}
	FrameExit(*argr)
	break;

case TYBITVECTOR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	vBitp = (TBitVector*)asObject(&argv[0]);
	size = vBitp->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP,gTP,*argr,size);
	mp->itsDimensions[0] = size;
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		atHMTval(mp->itsTvalMatrix,targetIndex++) = 
			(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
	}
	FrameExit(*argr)
	break;

case TYBYTEVECTOR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	vBytep = (TByteVector*)asObject(&argv[0]);
	size = vBytep->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP, gTP, *argr, size);
	mp->itsDimensions[0] = size;
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		atHMTval(mp->itsTvalMatrix,targetIndex++) = 
			(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
	}
    FrameExit(*argr)
	break;

case TYINTVECTOR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	vIntp = (TIntVector*)asObject(&argv[0]);
	size = vIntp->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	mp->itsDimensions[0] = size;
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		atHMTval(mp->itsTvalMatrix,targetIndex++) = 
			(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
	}
	FrameExit(*argr)
	break;

case TYLONGVECTOR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	vLngp = (TLongVector*)asObject(&argv[0]);
	size = vLngp->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	mp->itsDimensions[0] = size;
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		atHMTval(mp->itsTvalMatrix,targetIndex++) = 
			(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
	}
	FrameExit(*argr)
	break;
case TYSHORTVECTOR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	vShtp = (TShtVector*)asObject(&argv[0]);
	size = vShtp->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	mp->itsDimensions[0] = size;
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		atHMTval(mp->itsTvalMatrix,targetIndex++) = 
			(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
	}
	FrameExit(*argr)
	break;

case TYNUMVECTOR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	vNump = (TNumVector*)asObject(&argv[0]);
	size = vNump->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	mp->itsDimensions[0] = size;
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		atHMTval(mp->itsTvalMatrix,targetIndex++) = 
			(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
	}
	FrameExit(*argr)
	break;

case TYFLTVECTOR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	vFltp = (TFltVector*)asObject(&argv[0]);
	size = vFltp->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	mp->itsDimensions[0] = size;
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		atHMTval(mp->itsTvalMatrix,targetIndex++) = 
			(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
	}
	FrameExit(*argr)
	break;

case TYOBJVECTOR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	vObjp = (TObjVector*)asObject(&argv[0]);
	size = vObjp->itsMaxItemIndex;
	asInt(ndx1) = 0;
	*tmp = TObjVector_GetIV1(gCP, gTP, argv[0], *ndx1);
	// Convert an object vector of vectors into a 2-dimensional matrix
	if ((tmp->Tag==TYVECTOR || tmp->Tag==TYNUMVECTOR) && (rank==2 || rank==0))
	{	// Set dimensions of new matrix using length of the first vector.
		vp = (TVector*)asObject(tmp);
		ncols = vp->itsMaxItemIndex;
		TMatrix_SetMaxIndex(gCP,gTP,*argr,size * ncols);  // Before setting rank!
		mp->itsRank = 2;
		mp->itsDimensions[0] = size;
		mp->itsDimensions[1] = ncols;
	
		// Fill the new matrix tval array with elements from each vector
		for (row = targetIndex = 0; row < size; ++row)
		{	asInt(ndx1) = row;
			*tmp = TObjVector_GetIV1(gCP, gTP, argv[0], *ndx1);
			for (col = 0; col < ncols; ++col, ++targetIndex)
			{	asInt(ndx2) = col;
				atHMTval(mp->itsTvalMatrix, targetIndex) =
					(*_TObject_TypeGetIV1(asTag(tmp)))(gCP,gTP,*tmp,*ndx2);
			}
		}		
	}
	// Convert an object vector of object vectors into a 3-dimensional matrix
	else if (tmp->Tag == TYOBJVECTOR && (rank == 3 || rank == 0))
	{	asInt(ndx2) = 0;
		*tmp1 = TObjVector_GetIV1(gCP, gTP, *tmp, *ndx2);
		if (tmp1->Tag != TYVECTOR && tmp1->Tag != TYNUMVECTOR)
			goto ELSE;
		// Set dimensions of new matrix using length of the first two vectors.
		vObjp = (TObjVector*)asObject(tmp);
		nrows = vObjp->itsMaxItemIndex;
		vp = (TVector*)asObject(tmp1);
		ncols = vp->itsMaxItemIndex;
		TMatrix_SetMaxIndex(gCP,gTP,*argr,size * nrows * ncols);  // Before setting rank!
		mp->itsRank = 3;
		mp->itsDimensions[0] = size;
		mp->itsDimensions[1] = nrows;
		mp->itsDimensions[2] = ncols;
		// Fill the new matrix tval array with elements from each vector
		for (plane = targetIndex = 0; plane < size; ++plane)
		{	asInt(ndx1) = plane;
			*tmp = TObjVector_GetIV1(gCP, gTP, argv[0], *ndx1);
			for (row = 0; row < size; ++row)
			{	asInt(ndx2) = row;
				*tmp1 = TObjVector_GetIV1(gCP, gTP, *tmp, *ndx2);
				for (col = 0; col < ncols; ++col, ++targetIndex)
				{	asInt(ndx3) = col;
					atHMTval(mp->itsTvalMatrix, targetIndex) =
						(*_TObject_TypeGetIV1(asTag(tmp1)))(gCP,gTP,*tmp1,*ndx3);
				}
			}
		}		
	}
	else
	{
ELSE:	TMatrix_SetMaxIndex(gCP, gTP, *argr, size);
		if (rank > 0) mp->itsRank = rank;
		mp->itsDimensions[0] = (plane > 0) ? plane : size;
		if (row > 0) mp->itsDimensions[1] = row;
		if (col > 0) mp->itsDimensions[2] = col;

		targetIndex = sourceIndex = 0;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			atHMTval(mp->itsTvalMatrix,targetIndex++) = 
				(*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		}
	}
	FrameExit(*argr)
	break;

case TYPAIR:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	*ret = FUtil2_Length(gCP,gTP,(NUM)1,&argv[0]);
	ExitOnError(*ec);
	size = asInt(ret);
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	indexOf = 0;
	rp = &argv[0];
	while ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
	{	pp = (TPair*)asObject(rp);
		rp = &pp->itsCdr;
		if (asTag(&pp->itsCar) == TYSYMBOL && asSymbol(&pp->itsCar) == gCP->TLambda_dotcdr)
		{	if ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
			{	pp = (TPair*)asObject(rp);
				mp->itsCdr = pp->itsCar;
				TMatrix_SetMaxIndex(gCP,gTP,*argr, indexOf);
				mp->itsDimensions[0] = size;
				break;
			}
			else
			{
				//  error..
			}
		}
		atHMTval(mp->itsTvalMatrix,indexOf++) = pp->itsCar;
	}

	FrameExit(*argr)
	break;

case TYSTRUCTURE:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	ep = (TStructure*)asObject(&argv[0]);
	size = ep->itsMaxItemIndex * 2;
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	mp->itsDimensions[0] = size;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asObject(&atHMTval(mp->itsTvalMatrix,targetIndex)) = atHMBind(ep->itsDictionaryArray,sourceIndex).Key;
		asTag(&atHMTval(mp->itsTvalMatrix,targetIndex)) = asObject(&atHMTval(mp->itsTvalMatrix,targetIndex))->itsObjectType;
        ++targetIndex;
		atHMTval(mp->itsTvalMatrix,targetIndex) = atHMBind(ep->itsDictionaryArray,sourceIndex).Value;
        ++targetIndex;
        ++sourceIndex;
	}

		FrameExit(*argr)
	break;

case TYDICTIONARY:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	dp = (TDictionary*)asObject(&argv[0]);
	size = dp->itsMaxItemIndex * 2;
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	mp->itsDimensions[0] = size;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asObject(&atHMTval(mp->itsTvalMatrix,targetIndex)) = atHMBind(dp->itsDictionaryArray,sourceIndex).Key;
		asTag(&atHMTval(mp->itsTvalMatrix,targetIndex)) = asObject(&atHMTval(mp->itsTvalMatrix,targetIndex))->itsObjectType;
		++targetIndex;
		atHMTval(mp->itsTvalMatrix,targetIndex) = atHMBind(dp->itsDictionaryArray,sourceIndex).Value;
        ++targetIndex;
        ++sourceIndex;
	}

	FrameExit(*argr)
	break;

case TYDIRECTORY:
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	mp = (TMatrix*)asObject(argr);
	asTag(argr) = TYMATRIX;
	xp = (TDirectory*)asObject(&argv[0]);
	size = xp->itsMaxItemIndex * 2;
	TMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	mp->itsDimensions[0] = size;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	atHMTval(mp->itsTvalMatrix,targetIndex++) = atHMPBind(xp->itsDirectoryArray,sourceIndex).Key;
		atHMTval(mp->itsTvalMatrix,targetIndex++) = atHMPBind(xp->itsDirectoryArray,sourceIndex++).Value;
	}

	FrameExit(*argr)
	break;
case TYCPXVECTOR:
	// Create a new matrix and set configuration
	argr->u.Matrix = mp = TMatrix_New(gCP,gTP);
	argr->Tag = TYMATRIX;
	vCpxp = argv[0].u.CpxVector;
	size = vCpxp->itsMaxItemIndex;
	TMatrix_SetMaxIndex(gCP, gTP, *argr, size);
	if (rank > 0) mp->itsRank = rank;
	mp->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) mp->itsDimensions[1] = row;
	if (col > 0) mp->itsDimensions[2] = col;

	// Fill the new matrix tval array with elements from the vector
	targetIndex = sourceIndex = 0;
	for (row = 0; row < size; ++row)
	{	ndx1->u.Int = row;
		*tmp = TCpxVector_GetIV1(gCP, gTP, argv[0], *ndx1);
		atHMTval(mp->itsTvalMatrix, row) = *tmp;
	}
	FrameExit(*argr)
	break;

default:
	//  Convert anything else to a singleton Matrix.
	asObject(argr) = (TObject*)TMatrix_New(gCP,gTP);
	asTag(argr) = TYMATRIX;
	mp = (TMatrix*)asObject(argr);
	TMatrix_SetMaxIndex(gCP,gTP,*argr,1);
	atHMTval(mp->itsTvalMatrix,0) = argv[0];
	FrameExit(*argr)
}
}

/*	--------------------------------------------------------------------------------------
FConvert_ObjToNumMatrix

The FConvert_ObjToNumMatrix procedure converts an object to a number matrix. 
--------------------------------------------------------------------------------------- */
TVAL FConvert_ObjToNumMatrix(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
LpTVAL              rp;
NUM                 indexOf;
NUM                 targetIndex;
NUM                 sourceIndex;
NUM                 size, ncols, nrows, rank, col, row, plane, dim;
StartFrame
DeclareOBJ(TPair,pp);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TVector,vp);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDirectory,xp);
DeclareOBJ(TCpxVector, vCpxp);
DeclareOBJ(TCpx, cp);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVAL(argr);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVAL(ndx3);
DeclareTVAL(tmp);
DeclareTVAL(tmp1);
DeclareTVAL(tmp2);
EndFrame
 
//  Initialization
*argr = gCP->Tval_VOID;
if (argc < 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
if (asObj(&argv[0]) == NIL) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
asTag(ndx1) = asTag(ndx2) = asTag(ndx3) = TYNUM;

// Determine matrix configuration from arguments
rank = plane = row = col = 0;
if (argc > 1)
{	rank = asNumIndex(&argv[1]);
	if (argc > 2)
		 plane = asNumIndex(&argv[2]);
	if (argc > 3)
		row = asNumIndex(&argv[3]);
	if (argc > 4)
		col = asNumIndex(&argv[4]);
}

//  Manage the conversion of a single object into a Matrix
switch(asTag(&argv[0]))
{   
case TYMATRIX:
	// Create a new matrix with the same configuration as the matrix.
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	mp = (TMatrix*)asObject(&argv[0]);
	size = mp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr,size);
	np->itsCdr = mp->itsCdr;
	np->itsRank = mp->itsRank;
	for (dim = 0; dim < mp->itsRank; ++dim)
		np->itsDimensions[dim] = mp->itsDimensions[dim];
	// Copy all of the elements from the number matrix to the new matrix.
	for (row = 0; row < size; ++row)
	{	asInt(ndx1) = row;
		*tmp = TMatrix_GetIV1(gCP,gTP,argv[0],*ndx1);
		atHMReal(np->itsRealMatrix,row) = (double)asNumIndex(tmp);
	}
	if (argc > 1)
	    // If there are resize arguments, then resize the new matrix.
		{
		argv[0] = *argr;
		*ec = TNumMatrix_Resize(gCP,gTP,argc,argv);
		ExitOnError(*ec);
		}
    FrameExit(*argr);
	break;

case TYNUMMATRIX:
	// If there are no resize arguments, then return the matrix sent.
	if (argc == 1)
		{
        *argr = argv[0];
		FrameExit(*argr);
		}
	else
	    // If there are resize arguments, then resize the matrix sent.
		{
		argr->u.Object = (TObject*)TNumMatrix_Copy(gCP,gTP,argv[0]);
		argr->Tag = TYNUMMATRIX;
		argv[0] = *argr;
		*ec = TNumMatrix_Resize(gCP,gTP,argc,argv);
		ExitOnError(*ec);
		FrameExit(argv[0]);
		}
	break;

case TYVECTOR:
	// Create a new matrix and set configuration
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	asTag(argr) = TYNUMMATRIX;
	np = (TNumMatrix*)asObject(argr);
	vp = (TVector*)asObject(&argv[0]);
	size = vp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr,size);
	if (rank > 0) np->itsRank = rank;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) np->itsDimensions[1] = row;
	if (col > 0) np->itsDimensions[2] = col;

	// Fill the new matrix tval array with elements from the vector
	targetIndex = sourceIndex = 0;
	for (row = 0; row < size; ++row)
	{	asInt(ndx1) = row;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0],*ndx1);
		atHMReal(np->itsRealMatrix, row) = (double)asNumIndex(tmp);
	}
	FrameExit(*argr)
	break;

case TYBITVECTOR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	vBitp = (TBitVector*)asObject(&argv[0]);
	size = vBitp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr,size);
	np->itsDimensions[0] = size;
	if (rank > 0) np->itsRank = rank;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) np->itsDimensions[1] = row;
	if (col > 0) np->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		atHMReal(np->itsRealMatrix,targetIndex++) = (double)asNumIndex(tmp);		
	}
	FrameExit(*argr)
	break;

case TYBYTEVECTOR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	vBytep = (TByteVector*)asObject(&argv[0]);
	size = vBytep->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP, gTP, *argr, size);
	np->itsDimensions[0] = size;
	if (rank > 0) np->itsRank = rank;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) np->itsDimensions[1] = row;
	if (col > 0) np->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		atHMReal(np->itsRealMatrix,targetIndex++) = (double)asNumIndex(tmp);
	}
    FrameExit(*argr)
	break;

case TYINTVECTOR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	vIntp = (TIntVector*)asObject(&argv[0]);
	size = vIntp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	np->itsDimensions[0] = size;
	if (rank > 0) np->itsRank = rank;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) np->itsDimensions[1] = row;
	if (col > 0) np->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		atHMReal(np->itsRealMatrix,targetIndex++) = (double)asNumIndex(tmp);
	}
	FrameExit(*argr)
	break;

case TYLONGVECTOR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	vLngp = (TLongVector*)asObject(&argv[0]);
	size = vLngp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	np->itsDimensions[0] = size;
	if (rank > 0) np->itsRank = rank;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) np->itsDimensions[1] = row;
	if (col > 0) np->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		atHMReal(np->itsRealMatrix,targetIndex++) = (double)asNumIndex(tmp);
	}
	FrameExit(*argr)
	break;

case TYSHORTVECTOR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	vShtp = (TShtVector*)asObject(&argv[0]);
	size = vShtp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	np->itsDimensions[0] = size;
	if (rank > 0) np->itsRank = rank;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) np->itsDimensions[1] = row;
	if (col > 0) np->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		atHMReal(np->itsRealMatrix,targetIndex++) = (double)asNumIndex(tmp);
	}
	FrameExit(*argr)
	break;

case TYNUMVECTOR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	vNump = (TNumVector*)asObject(&argv[0]);
	size = vNump->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	np->itsDimensions[0] = size;
	if (rank > 0) np->itsRank = rank;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) np->itsDimensions[1] = row;
	if (col > 0) np->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		atHMReal(np->itsRealMatrix,targetIndex++) = (double)asNumIndex(tmp);
	}
	FrameExit(*argr)
	break;

case TYFLTVECTOR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	vFltp = (TFltVector*)asObject(&argv[0]);
	size = vFltp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	np->itsDimensions[0] = size;
	if (rank > 0) np->itsRank = rank;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	if (row > 0) np->itsDimensions[1] = row;
	if (col > 0) np->itsDimensions[2] = col;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	asInt(ndx1) = sourceIndex++;
		*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
		atHMReal(np->itsRealMatrix,targetIndex++) = (double)asNumIndex(tmp);
	}
	FrameExit(*argr)
	break;

case TYOBJVECTOR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	vObjp = (TObjVector*)asObject(&argv[0]);
	size = vObjp->itsMaxItemIndex;
	asInt(ndx1) = 0;
	*tmp = TObjVector_GetIV1(gCP, gTP, argv[0], *ndx1);
	// Convert an object vector of vectors into a 2-dimensional matrix
	if ((tmp->Tag==TYVECTOR || tmp->Tag==TYNUMVECTOR) && (rank==2 || rank==0))
	{	// Set dimensions of new matrix using length of the first vector.
		vp = (TVector*)asObject(tmp);
		ncols = vp->itsMaxItemIndex;
		TNumMatrix_SetMaxIndex(gCP,gTP,*argr,size * ncols);  // Before setting rank!
		np->itsRank = 2;
		np->itsDimensions[0] = size;
		np->itsDimensions[1] = ncols;
	
		// Fill the new matrix tval array with elements from each vector
		for (row = targetIndex = 0; row < size; ++row)
		{	asInt(ndx1) = row;
			*tmp = TObjVector_GetIV1(gCP, gTP, argv[0], *ndx1);
			for (col = 0; col < ncols; ++col, ++targetIndex)
			{	asInt(ndx2) = col;
				*tmp1 = 
					(*_TObject_TypeGetIV1(asTag(tmp)))(gCP,gTP, *tmp, *ndx2);
				atHMReal(np->itsRealMatrix,targetIndex) = (double)asNumIndex(tmp1);
			}
		}		
	}
	// Convert an object vector of object vectors into a 3-dimensional matrix
	else if (tmp->Tag == TYOBJVECTOR && (rank == 3 || rank == 0))
	{	asInt(ndx2) = 0;
		*tmp1 = TObjVector_GetIV1(gCP, gTP, *tmp, *ndx2);
		if (tmp1->Tag != TYVECTOR && tmp1->Tag != TYNUMVECTOR)
			goto ELSE;
		// Set dimensions of new matrix using length of the first two vectors.
		vObjp = (TObjVector*)asObject(tmp);
		nrows = vObjp->itsMaxItemIndex;
		vp = (TVector*)asObject(tmp1);
		ncols = vp->itsMaxItemIndex;
		TNumMatrix_SetMaxIndex(gCP,gTP,*argr,size * nrows * ncols);  // Before setting rank!
		np->itsRank = 3;
		np->itsDimensions[0] = size;
		np->itsDimensions[1] = nrows;
		np->itsDimensions[2] = ncols;
		// Fill the new matrix tval array with elements from each vector
		for (plane = targetIndex = 0; plane < size; ++plane)
		{	asInt(ndx1) = plane;
			*tmp = TObjVector_GetIV1(gCP, gTP, argv[0], *ndx1);
			for (row = 0; row < size; ++row)
			{	asInt(ndx2) = row;
				*tmp1 = TObjVector_GetIV1(gCP, gTP, *tmp, *ndx2);
				for (col = 0; col < ncols; ++col, ++targetIndex)
				{	asInt(ndx3) = col;
					*tmp2 =
						(*_TObject_TypeGetIV1(asTag(tmp1)))(gCP,gTP,*tmp1,*ndx3);
					atHMReal(np->itsRealMatrix,targetIndex) = (double)asNumIndex(tmp2);	
				}
			}
		}		
	}
	else
	{
ELSE:	TNumMatrix_SetMaxIndex(gCP, gTP, *argr, size);
		if (rank > 0) np->itsRank = rank;
		np->itsDimensions[0] = (plane > 0) ? plane : size;
		if (row > 0) np->itsDimensions[1] = row;
		if (col > 0) np->itsDimensions[2] = col;

		targetIndex = sourceIndex = 0;
		while (targetIndex < size)
		{	asInt(ndx1) = sourceIndex++;
			*tmp = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
			atHMReal(np->itsRealMatrix,targetIndex++) = (double)asNumIndex(tmp);
		}
	}
	FrameExit(*argr)
	break;

case TYPAIR:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	*ret = FUtil2_Length(gCP,gTP,(NUM)1,&argv[0]);
	ExitOnError(*ec);
	size = asInt(ret);
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	indexOf = 0;
	rp = &argv[0];
	while ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
	{	pp = (TPair*)asObject(rp);
		rp = &pp->itsCdr;
		if (asTag(&pp->itsCar) == TYSYMBOL && asSymbol(&pp->itsCar) == gCP->TLambda_dotcdr)
		{	if ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
			{	pp = (TPair*)asObject(rp);
				np->itsCdr = pp->itsCar;
				TNumMatrix_SetMaxIndex(gCP,gTP,*argr, indexOf);
				np->itsDimensions[0] = size;
				break;
			}
			else
			{
				//  error..
			}
		}
		atHMReal(np->itsRealMatrix,indexOf++) = (double)asNumIndex(&(pp->itsCar));
	}
	FrameExit(*argr)
	break;

case TYSTRUCTURE:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	ep = (TStructure*)asObject(&argv[0]);
	size = ep->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	np->itsDimensions[0] = size;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	*tmp = atHMBind(ep->itsDictionaryArray,sourceIndex++).Value;
		atHMReal(np->itsRealMatrix,targetIndex++) = asNumIndex(tmp);	
	}

		FrameExit(*argr)
	break;

case TYDICTIONARY:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	dp = (TDictionary*)asObject(&argv[0]);
	size = dp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	np->itsDimensions[0] = size;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	*tmp = atHMBind(dp->itsDictionaryArray,sourceIndex++).Value;
		atHMReal(np->itsRealMatrix,targetIndex++) = asNumIndex(tmp);
	}
	FrameExit(*argr)
	break;

case TYDIRECTORY:
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	np = (TNumMatrix*)asObject(argr);
	asTag(argr) = TYNUMMATRIX;
	xp = (TDirectory*)asObject(&argv[0]);
	size = xp->itsMaxItemIndex;
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr, size);
	np->itsDimensions[0] = size;
	targetIndex = sourceIndex = 0;
	while (targetIndex < size)
	{	*tmp = atHMPBind(xp->itsDirectoryArray,sourceIndex++).Value;
		atHMReal(np->itsRealMatrix,targetIndex++) = asNumIndex(tmp);	
	}
	FrameExit(*argr)
	break;
case TYCPXVECTOR:
	// Create new num matrix with n rows and 2 cols, one for real, one for imag part 
	argr->u.NumMatrix = np = TNumMatrix_New(gCP,gTP);
	argr->Tag = TYNUMMATRIX;
	vCpxp = argv[0].u.CpxVector;
	size = 2 * vCpxp->itsMaxItemIndex;				// 2 doubles per complex
	TNumMatrix_SetMaxIndex(gCP, gTP, *argr, size);
	np->itsRank = (rank > 2) ? rank : 2;
	np->itsDimensions[0] = (plane > 0) ? plane : size;
	np->itsDimensions[1] = (row > 1) ? row : 1;
	np->itsDimensions[2] = (col > 0) ? col : 0;

	// Fill the new matrix real array with elements from the vector
	targetIndex = sourceIndex = 0;
	for (indexOf = 0; indexOf < size; ++indexOf)
	{	ndx1->u.Int = indexOf;
		*tmp = TCpxVector_GetIV1(gCP, gTP, argv[0], *ndx1);
		cp = tmp->u.Complex;
		atHMReal(np->itsRealMatrix, indexOf++) = cp->itsReal;
		atHMReal(np->itsRealMatrix, indexOf)   = cp->itsImag;
	}
	FrameExit(*argr)
	break;

default:
	//  Convert anything else to a singleton Matrix.
	asObject(argr) = (TObject*)TNumMatrix_New(gCP,gTP);
	asTag(argr) = TYNUMMATRIX;
	np = (TNumMatrix*)asObject(argr);
	TNumMatrix_SetMaxIndex(gCP,gTP,*argr,1);
	atHMReal(np->itsRealMatrix,0) = asNumIndex(&argv[0]);
	FrameExit(*argr)
}
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ObjToStructure
The FConvert_ObjToStructure procedure converts the argument(s) to a Structure.  If a second
argument is present,  the conversion may involve an update to the Structure 


(objectToStructure obj)
		   The FConvert_ObjToStructure procedure converts the object to a Structure. 

(objectToStructure {vector1} {vector2}
		  The I-th item of {vector1} paired with the I-th item in the {vector2} argument.
          to form a Structure. Each item in  {vector1} must be a symbol 
          and may contain more items than {vector2}.
          If {vector1} contains more items than {vector2} then the keys in the 
          structure will be assigned a value of #void. 

(objectToStructure {structure} {vector})
		  The {structure} argument is updated with the values from the {vector} argument. 
          The I-th item from the {vector}  argument is mapped to the I-th item in
          the target {structure}.  If {vector} contains more elements than {structure}, 
		  extra elements are ignored

(objectToStructure {structure1} {structure2})
(objectToStructure {structure}  {dictionary})
(objectToStructure {structure}  {directory})
	      The {structure1} argument is the target structure and will be updated 
          by the values in {structure2},{dictionary}, or {directory} only 
		  if the key from {structure2}, etc.  matches a key from {structure1}.
		  Keys that do not match are ignored.

(objectToStructure {dictionary1} {dictionary2})
(objectToStructure {directory1}  {directory2})
(objectToStructure {dictionary}  {directory})
(objectToStructure {directory}   {dictionary})
		   
	      The first argument is the target structure and will be updated 
          by the values in the second only if the keys match. If the key
          from the second argument do not match, it is added to the first
		  argument object.
#endif

TVAL FConvert_ObjToStructure(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
NUM                 envIndex;
NUM                 dirIndex;
NUM                 sizeKeys;
NUM                 sizeValues;
LpREAL				rp;
TVAL				newArgs[2];
TVAL				temp;
StartFrame
DeclareOBJ(TVector,vp);
DeclareOBJ(TVector,vp1);
DeclareOBJ(TVector,vp2);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TMatrix,mp1);
DeclareOBJ(TMatrix,mp2);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TNumMatrix,np1);
DeclareOBJ(TNumMatrix,np2);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TStructure,ep1);
DeclareOBJ(TStructure,ep2);
DeclareOBJ(TDirectory,dp);
DeclareOBJ(TDictionary,dc);
DeclareOBJ(TCpxVector,vCpxp);
DeclareOBJ(TCpx, cp);
DeclareTVAL(ret);
DeclareTVAL(tmpKey);
DeclareTVAL(values);
DeclareTVAL(keys);
DeclareTVAL(argTval);
DeclareTVAL(ec);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc > 2) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)


if (argc == 1)
    {
 /*  If the object is already a  Structure, then return it unchanged. */
   switch(asTag(&argv[0]))
        {
        case TYSTRUCTURE:
            *ret = argv[0];
            FrameExit(*ret)
        break;

        case TYVECTOR:
			if (Vector(argv[0])->itsAttributes != NULL)
				{
				newArgs[0] = TOBJ(Vector(argv[0])->itsAttributes);
				newArgs[1] = argv[0];
				*ret = FConvert_ObjToStructure(gCP,gTP,2,newArgs);
				FrameExit(*ret)
				}

        default:
            /*  Convert the object to a vector and init the Structure with that */
        
            *ret = FConvert_ObjToVector(gCP,gTP,argc, argv);
            if (asTag(ret) == TYVECTOR)
                {
                vp = (TVector*)asObject(ret);
                ep = TStructure_New(gCP,gTP);
            
                asObject(argTval) = (TObject*)ep;
                asTag(argTval) = ep->itsObjectType;
				if (vp->itsMaxItemIndex > 0)
					{
					*ret = TStructure_IStructure(gCP,gTP,*argTval, vp->itsMaxItemIndex, &atHMTval(vp->itsTvalArray,0), gCP->TObject_VOID);
					ExitOnError(*ret);
					}
                ep->itsCdr = vp->itsCdr;
                asTag(ret) = TYSTRUCTURE;
                asObject(ret) = (TObject*)ep;
                }

            FrameExit(*ret)
        break;
        }
	FrameExit(*ret)
    }


/* Handle the case when both arguments are vectors */
switch (argv[0].Tag)
	{
	case TYVECTOR:
		switch (argv[1].Tag)
			{
			case TYVECTOR:
				ep = TStructure_New(gCP,gTP);
				asTag(ret) = TYSTRUCTURE;
				asObject(ret) = (TObject*)ep;
				ep->itsCdr = gCP->Tval_VOID;
				/* get the size of vector 1 (the vector containing keys) */
				vp1 = (TVector*) asObject(&argv[0]);
				sizeKeys = vp1->itsMaxItemIndex;
 				/* get the size of vector 2 (the vector containing values) */
				vp2 = (TVector*) asObject(&argv[1]);
				sizeValues = vp2->itsMaxItemIndex;
				/* The length of the key vector will become the size of the Structure */
				TStructure_SetMaxIndex(gCP,gTP,*ret,sizeKeys);

				/* Fill the Structure with the keys and values from the 2 vectors */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Obtain the proposed key value. */
					*tmpKey = atHMTval(vp1->itsTvalArray,envIndex);

					/*  Convert text and string keys to symbolic keys. */
					/*  Note:   Symbolic keys guarantee uniqueness. */
					if ((tmpKey->Tag == TYTEXT) || (tmpKey->Tag == TYSTRING) || (tmpKey->Tag == TYQUOTEDSYMBOL) || (tmpKey->Tag == TYSTRINGSUBSTR))
						{
						*tmpKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,*tmpKey);
						}

					/*  We accept symbolic keys only, all others are errors. */
					if ((tmpKey->Tag != TYSYMBOL) && (tmpKey->Tag != TYQUOTEDSYMBOL))
						{
						FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
						}
					
					/* Save the key from the first vector argument */
					atHMBind(ep->itsDictionaryArray,envIndex).Key = asObject(tmpKey);
					/* Save the value from the second vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = atHMTval(vp2->itsTvalArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
			break;

			case TYMATRIX:
				ep = TStructure_New(gCP,gTP);
				asTag(ret) = TYSTRUCTURE;
				asObject(ret) = (TObject*)ep;
				ep->itsCdr = gCP->Tval_VOID;
				/* get the size of vector 1 (the vector containing keys) */
				vp1 = (TVector*) asObject(&argv[0]);
				sizeKeys = vp1->itsMaxItemIndex;
 				/* get the size of matrix 2 (the matrix containing values) */
				mp2 = (TMatrix*) asObject(&argv[1]);
				sizeValues = mp2->itsMaxItemIndex;
				/* The length of the key vector will become the size of the Structure */
				TStructure_SetMaxIndex(gCP,gTP,*ret,sizeKeys);

				/* Fill the Structure with the keys and values from the 2 vectors */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Obtain the proposed key value. */
					*tmpKey = atHMTval(vp1->itsTvalArray,envIndex);

					/*  Convert text and string keys to symbolic keys. */
					/*  Note:   Symbolic keys guarantee uniqueness. */
					if ((tmpKey->Tag == TYTEXT) || (tmpKey->Tag == TYSTRING) || (tmpKey->Tag == TYQUOTEDSYMBOL) || (tmpKey->Tag == TYSTRINGSUBSTR))
						{
						*tmpKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,*tmpKey);
						}

					/*  We accept symbolic keys only, all others are errors. */
					if ((tmpKey->Tag != TYSYMBOL) && (tmpKey->Tag != TYQUOTEDSYMBOL))
						{
						FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
						}
					
					/* Save the key from the first vector argument */
					atHMBind(ep->itsDictionaryArray,envIndex).Key = asObject(tmpKey);
					/* Save the value from the second vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = atHMTval(mp2->itsTvalMatrix,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
			break;

			case TYOBJVECTOR:
				ep = TStructure_New(gCP,gTP);
				asTag(ret) = TYSTRUCTURE;
				asObject(ret) = (TObject*)ep;
				ep->itsCdr = gCP->Tval_VOID;
				/* get the size of vector 1 (the vector containing keys) */
				*keys = argv[0];
				sizeKeys = FSmartbase_VectorLen(gCP,gTP,*keys);
 				/* get the size of vector 2 (the vector containing values) */
				*values = argv[1];
				sizeValues = FSmartbase_VectorLen(gCP,gTP,*values);
				/* The length of the key vector will become the size of the Structure */
				TStructure_SetMaxIndex(gCP,gTP,*ret,sizeKeys);

				/* Fill the Structure with the keys and values from the 2 vectors */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Obtain the proposed key value. */
					*tmpKey = FSmartbase_Ref(gCP,gTP,2,*keys,TINT(envIndex));

					/*  Convert text and string keys to symbolic keys. */
					/*  Note:   Symbolic keys guarantee uniqueness. */
					if ((tmpKey->Tag == TYTEXT) || (tmpKey->Tag == TYSTRING) || (tmpKey->Tag == TYQUOTEDSYMBOL) || (tmpKey->Tag == TYSTRINGSUBSTR))
						{
						*tmpKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,*tmpKey);
						}

					/*  We accept symbolic keys only, all others are errors. */
					if ((tmpKey->Tag != TYSYMBOL) && (tmpKey->Tag != TYQUOTEDSYMBOL))
						{
						FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
						}
					
					/* Save the key from the first vector argument */
					atHMBind(ep->itsDictionaryArray,envIndex).Key = asObject(tmpKey);
					/* Save the value from the second vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = FSmartbase_Ref(gCP,gTP,2,*values,TINT(envIndex));
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
			break;

			default:
				FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
			break;
			}

		FrameExit(*ret)
	break;

	case TYMATRIX:
		switch (argv[1].Tag)
			{
			case TYVECTOR:
				ep = TStructure_New(gCP,gTP);
				asTag(ret) = TYSTRUCTURE;
				asObject(ret) = (TObject*)ep;
				ep->itsCdr = gCP->Tval_VOID;
				/* get the size of matrix 1 (the matrix containing keys) */
				mp1 = (TMatrix*) asObject(&argv[0]);
				sizeKeys = mp1->itsMaxItemIndex;
 				/* get the size of vector 2 (the vector containing values) */
				vp2 = (TVector*) asObject(&argv[1]);
				sizeValues = vp2->itsMaxItemIndex;
				/* The length of the key vector will become the size of the Structure */
				TStructure_SetMaxIndex(gCP,gTP,*ret,sizeKeys);

				/* Fill the Structure with the keys and values from the 2 vectors */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Obtain the proposed key value. */
					*tmpKey = atHMTval(mp1->itsTvalMatrix,envIndex);

					/*  Convert text and string keys to symbolic keys. */
					/*  Note:   Symbolic keys guarantee uniqueness. */
					if ((tmpKey->Tag == TYTEXT) || (tmpKey->Tag == TYSTRING) || (tmpKey->Tag == TYQUOTEDSYMBOL) || (tmpKey->Tag == TYSTRINGSUBSTR))
						{
						*tmpKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,*tmpKey);
						}

					/*  We accept symbolic keys only, all others are errors. */
					if ((tmpKey->Tag != TYSYMBOL) && (tmpKey->Tag != TYQUOTEDSYMBOL))
						{
						FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
						}
					
					/* Save the key from the first vector argument */
					atHMBind(ep->itsDictionaryArray,envIndex).Key = asObject(tmpKey);
					/* Save the value from the second vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = atHMTval(vp2->itsTvalArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
			break;

			case TYMATRIX:
				ep = TStructure_New(gCP,gTP);
				asTag(ret) = TYSTRUCTURE;
				asObject(ret) = (TObject*)ep;
				ep->itsCdr = gCP->Tval_VOID;
				/* get the size of matrix 1 (the matrix containing keys) */
				mp1 = (TMatrix*) asObject(&argv[0]);
				sizeKeys = mp1->itsMaxItemIndex;
 				/* get the size of matrix 2 (the matrix containing values) */
				mp2 = (TMatrix*) asObject(&argv[1]);
				sizeValues = mp2->itsMaxItemIndex;
				/* The length of the key vector will become the size of the Structure */
				TStructure_SetMaxIndex(gCP,gTP,*ret,sizeKeys);

				/* Fill the Structure with the keys and values from the 2 vectors */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Obtain the proposed key value. */
					*tmpKey = atHMTval(mp1->itsTvalMatrix,envIndex);

					/*  Convert text and string keys to symbolic keys. */
					/*  Note:   Symbolic keys guarantee uniqueness. */
					if ((tmpKey->Tag == TYTEXT) || (tmpKey->Tag == TYSTRING) || (tmpKey->Tag == TYQUOTEDSYMBOL) || (tmpKey->Tag == TYSTRINGSUBSTR))
						{
						*tmpKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,*tmpKey);
						}

					/*  We accept symbolic keys only, all others are errors. */
					if ((tmpKey->Tag != TYSYMBOL) && (tmpKey->Tag != TYQUOTEDSYMBOL))
						{
						FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
						}
					
					/* Save the key from the first vector argument */
					atHMBind(ep->itsDictionaryArray,envIndex).Key = asObject(tmpKey);
					/* Save the value from the second vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = atHMTval(mp2->itsTvalMatrix,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
			break;

			case TYOBJVECTOR:
				ep = TStructure_New(gCP,gTP);
				asTag(ret) = TYSTRUCTURE;
				asObject(ret) = (TObject*)ep;
				ep->itsCdr = gCP->Tval_VOID;
				/* get the size of matrix 1 (the matrix containing keys) */
				*keys = argv[0];
				sizeKeys = FSmartbase_VectorLen(gCP,gTP,*keys);
 				/* get the size of vector 2 (the vector containing values) */
				*values = argv[1];
				sizeValues = FSmartbase_VectorLen(gCP,gTP,*values);
				/* The length of the key vector will become the size of the Structure */
				TStructure_SetMaxIndex(gCP,gTP,*ret,sizeKeys);

				/* Fill the Structure with the keys and values from the 2 vectors */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Obtain the proposed key value. */
					*tmpKey = FSmartbase_Ref(gCP,gTP,2,*keys,TINT(envIndex));

					/*  Convert text and string keys to symbolic keys. */
					/*  Note:   Symbolic keys guarantee uniqueness. */
					if ((tmpKey->Tag == TYTEXT) || (tmpKey->Tag == TYSTRING) || (tmpKey->Tag == TYQUOTEDSYMBOL) || (tmpKey->Tag == TYSTRINGSUBSTR))
						{
						*tmpKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,*tmpKey);
						}

					/*  We accept symbolic keys only, all others are errors. */
					if ((tmpKey->Tag != TYSYMBOL) && (tmpKey->Tag != TYQUOTEDSYMBOL))
						{
						FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
						}
					
					/* Save the key from the first vector argument */
					atHMBind(ep->itsDictionaryArray,envIndex).Key = asObject(tmpKey);
					/* Save the value from the second vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = FSmartbase_Ref(gCP,gTP,2,*values,TINT(envIndex));
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
			break;

			default:
				FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
			break;
			}

		FrameExit(*ret)
	break;

	case TYOBJVECTOR:
		ep = TStructure_New(gCP,gTP);
		asTag(ret) = TYSTRUCTURE;
		asObject(ret) = (TObject*)ep;
		ep->itsCdr = gCP->Tval_VOID;
		/* get the size of vector 1 (the vector containing keys) */
		*keys = argv[0];
		sizeKeys = FSmartbase_VectorLen(gCP,gTP,*keys);
 		/* get the size of vector 2 (the vector containing values) */
		*values = argv[1];
		sizeValues = FSmartbase_VectorLen(gCP,gTP,*values);
		/* The length of the key vector will become the size of the Structure */
		TStructure_SetMaxIndex(gCP,gTP,*ret,sizeKeys);

		/* Fill the Structure with the keys and values from the 2 vectors */
		for (envIndex = 0; envIndex < sizeKeys; envIndex++)
			{
			/* Obtain the proposed key value. */
			*tmpKey = FSmartbase_Ref(gCP,gTP,2,*keys,TINT(envIndex));
			temp = *tmpKey;

			/*  Convert text and string keys to symbolic keys. */
			/*  Note:   Symbolic keys guarantee uniqueness. */
			if ((tmpKey->Tag == TYTEXT) || (tmpKey->Tag == TYSTRING) || (tmpKey->Tag == TYQUOTEDSYMBOL) || (tmpKey->Tag == TYSTRINGSUBSTR))
				{
				*tmpKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,*tmpKey);
				}

			/*  We accept symbolic keys only, all others are errors. */
			if ((tmpKey->Tag != TYSYMBOL) && (tmpKey->Tag != TYQUOTEDSYMBOL))
				{
				FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
				}
			
			/* Save the key from the first vector argument */
			atHMBind(ep->itsDictionaryArray,envIndex).Key = asObject(tmpKey);
			/* Save the value from the second vector argument */
			if (envIndex < sizeValues)
				{
				atHMBind(ep->itsDictionaryArray,envIndex).Value = FSmartbase_Ref(gCP,gTP,2,*values,TINT(envIndex));
				}
			else
				{
				/* If there are more keys than values, assign the remaining keys with #void */
				atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
				}
			}
		FrameExit(*ret)
	break;
		
	/* Handle the case where the first argument is a structure */
	case TYSTRUCTURE:
		/* get the Structure object */
		ep =  (TStructure*) asObject(&argv[0]);
		/* Setup the return tval */
		asTag(ret) = TYSTRUCTURE;
		asObject(ret) = (TObject*)ep;
		/* get the size of the target Structure */
		sizeKeys = TStructure_GetMaxIndex(gCP,gTP,*ret);
		/* Check second argument */
		switch (argv[1].Tag)
			{
			case TYVECTOR:
 				/* get the size of the vector containing values */
				vp = (TVector*) asObject(&argv[1]);
				sizeValues = vp->itsMaxItemIndex;

				/* Map the values from the vector argument to the   */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = atHMTval(vp->itsTvalArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}

				FrameExit(*ret)
				break;

			case TYMATRIX:
 				/* get the size of the vector containing values */
				mp = (TMatrix*) asObject(&argv[1]);
				sizeValues = mp->itsMaxItemIndex;

				/* Map the values from the vector argument to the   */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = atHMTval(mp->itsTvalMatrix,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}

				FrameExit(*ret)
				break;

			case TYNUMMATRIX:
 				/* get the size of the vector containing values */
				np = (TNumMatrix*) asObject(&argv[1]);
				sizeValues = np->itsMaxItemIndex;

				/* Map the values from the vector argument to the   */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = TREAL(atHMReal(np->itsRealMatrix,envIndex));
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}

				FrameExit(*ret)
				break;

			case TYOBJVECTOR:
				/* get the size of Structure (the Structure contains the keys) */
				*keys = argv[0];
				sizeKeys = FSmartbase_VectorLen(gCP,gTP,*keys);
 				/* get the size of vector 2 (the vector containing values) */
				*values = argv[1];
				sizeValues = FSmartbase_VectorLen(gCP,gTP,*values);

				/* Fill the Structure with the keys and values from the 2 vectors */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value = FSmartbase_Ref(gCP,gTP,2,*values,TINT(envIndex));
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
			break;

			case TYBITVECTOR:
 				/* get the size of the bit vector containing values */
				vBitp = (TBitVector*)asObject(&argv[1]);
				sizeValues = vBitp->itsMaxItemIndex;

				/* Map the values from the vector argument to the  */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the bit vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value.Tag   = TYNUM;
						atHMBind(ep->itsDictionaryArray,envIndex).Value.u.Int = ((atHMChar(vBitp->itsBitArray,(NUM)(envIndex/8)) & gCP->TBitVector_OrMasks[(envIndex%8)]) != 0);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}

				FrameExit(*ret)
				break;
				

			case TYBYTEVECTOR:
 				/* get the size of the bit vector containing values */
				vBytep = (TByteVector*)asObject(&argv[1]);
				sizeValues = vBytep->itsMaxItemIndex;

				/* Map the values from the vector argument to the  */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the byte vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value.Tag   = TYNUM;
						atHMBind(ep->itsDictionaryArray,envIndex).Value.u.Int = atHMChar(vBytep->itsByteArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}

				FrameExit(*ret)
				break;

			case TYINTVECTOR:
				vIntp = (TIntVector*)asObject(&argv[1]);
				sizeValues = vIntp->itsMaxItemIndex;
				/* Map the values from the vector argument to the  */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the byte vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value.Tag   = TYNUM;
						atHMBind(ep->itsDictionaryArray,envIndex).Value.u.Int = atHMInt(vIntp->itsIntArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
				break;

			case TYLONGVECTOR:
				vLngp = (TLongVector*)asObject(&argv[1]);
				sizeValues = vLngp->itsMaxItemIndex;
				/* Map the values from the vector argument to the  */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the byte vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value.Tag   = TYNUM;
						atHMBind(ep->itsDictionaryArray,envIndex).Value.u.Int = atHMLong(vLngp->itsLongArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)

			case TYSHORTVECTOR:
				vShtp = (TShtVector*)asObject(&argv[1]);
				sizeValues = vShtp->itsMaxItemIndex;
				/* Map the values from the vector argument to the  */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the byte vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value.Tag   = TYNUM;
						atHMBind(ep->itsDictionaryArray,envIndex).Value.u.Int = atHMShort(vShtp->itsShortArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
				break;

			case TYNUMVECTOR:
				vNump = (TNumVector*)asObject(&argv[1]);
				sizeValues = vNump->itsMaxItemIndex;
				/* Map the values from the vector argument to the  */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the byte vector argument */
					if (envIndex < sizeValues)
						{
						atHMBind(ep->itsDictionaryArray,envIndex).Value.Tag   = TYREAL;
						atHMBind(ep->itsDictionaryArray,envIndex).Value.u.Real = atHMReal(vNump->itsRealArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}
				FrameExit(*ret)
				break;

			case TYFLTVECTOR:
  				/* get the size of the float vector containing values */
				vFltp = (TFltVector*)asObject(&argv[1]);
				sizeValues = vFltp->itsMaxItemIndex;
				/* Map the values from the vector argument to the  */
				/* Structure object based on matching positions  */
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
					{
					/* Save the value from the float vector argument */
					if (envIndex < sizeValues)
						{
						/*  Convert the nth real item into a tval. */
						atHMBind(ep->itsDictionaryArray,envIndex).Value.Tag   = TYREAL;
						atHMBind(ep->itsDictionaryArray,envIndex).Value.u.Real = atHMFloat(vFltp->itsFloatArray,envIndex);
						}
					else
						{
						/* If there are more keys than values, assign the remaining keys with #void */
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
						}
					}

				FrameExit(*ret)
				break;

			case TYSTRUCTURE:

				/* Handle the case where the target Structure values (structure1) */
				/* are updated by the values in the source Structure (structure2)  */
				/* if and only if the key in structure2 match the key in structure1 */

 				/* get the size of the source Structure containing the values */
				ep2 = (TStructure*) asObject(&argv[1]);
				sizeValues = ep2->itsMaxItemIndex;

				/* For every value in source Structure (ep2), find a key match in the  */
				/* target Structure (ep1). Any key in ep2 does not match in ep1 is ignored */
				for (envIndex = 0; envIndex < sizeValues; envIndex++)
					{
					*ec = TStructure_isBound(gCP,gTP,*ret,TOBJ(atHMBind(ep2->itsDictionaryArray,envIndex).Key));
					if (ec->Tag != TYBOLE)
						{
						*ec = TStructure_SetIV1(gCP,gTP,*ret, 
													TOBJ(atHMBind(ep2->itsDictionaryArray,envIndex).Key),
													atHMBind(ep2->itsDictionaryArray,envIndex).Value);
						}
					}
				FrameExit(*ret)
				break;

			case TYDICTIONARY:

				/* Handle the case where the target Structure values */
				/* are updated by the values in the source dictionary. Updates only	 */
				/* occur if the key in dictionary matches the key in Structure.      */

 				/* get the size of the source dictionary containing the values */
				dc = (TDictionary*) asObject(&argv[1]);
				sizeValues = dc->itsMaxItemIndex;

				/* For every key in source dictionary (dc), find a matching key in the */
				/* target Structure (ep) and update the value in the Structure    */
				/* with the value from the dictionary.  Any key in the dictionary that */
				/* does not match in the Structure is ignored */
				for (dirIndex = 0; dirIndex < sizeValues; dirIndex++)
					{
					*ec = TStructure_isBound(gCP,gTP,*ret,TOBJ(atHMBind(dc->itsDictionaryArray,dirIndex).Key));
					if (ec->Tag != TYBOLE)
						{
						*ec = TStructure_SetIV1(gCP,gTP,*ret,
												 TOBJ(atHMBind(dc->itsDictionaryArray,dirIndex).Key),
												 atHMBind(dc->itsDictionaryArray,dirIndex).Value);
						}
					}

				FrameExit(*ret)
				break;

			case TYDIRECTORY:

				/* Handle the case where the target Structure values (ep) */
				/* are updated by the values in the source directory (dp)	  */
				/* if and only if the key in directory match the key in ep */

				/* get the target Structure object */
				ep = (TStructure*) asObject(&argv[0]);
				/* Setup the return tval */
				asTag(ret) = TYSTRUCTURE;
				asObject(ret) = (TObject*)ep;
 				/* get the size of the source directory containing the values */
				dp = (TDirectory*) asObject(&argv[1]);
				sizeValues = dp->itsMaxItemIndex;

				/* For every key in source directory (dp), find a matching key in the */
				/* target Structure (ep) and update the value in the Structure    */
				/* with the value from the directory.  Any key in the directory that */
				/* does not match in the Structure is ignored */
				for (dirIndex = 0; dirIndex < sizeValues; dirIndex++)
					{
					*ec = TStructure_isBound(gCP,gTP,*ret,atHMPBind(dp->itsDirectoryArray,dirIndex).Key);
					if (ec->Tag != TYBOLE)
						{
						*ec = TStructure_SetIV1(gCP,gTP,*ret,
												 atHMPBind(dp->itsDirectoryArray,dirIndex).Key,
												 atHMPBind(dp->itsDirectoryArray,dirIndex).Value);
						}
					}
				FrameExit(*ret)
				break;

			case TYCPXVECTOR:
 				// Get the size of the complex vector containing values
				vCpxp = argv[1].u.CpxVector;
				sizeValues = vCpxp->itsMaxItemIndex;
				rp = (LpREAL)*vCpxp->itsCpxArray;

				// Copy each pair of values into a complex object.
				for (envIndex = 0; envIndex < sizeKeys; envIndex++)
				{	if (envIndex < sizeValues)
					{	atHMBind(ep->itsDictionaryArray,envIndex).Value.Tag   = TYCPX;
						atHMBind(ep->itsDictionaryArray,envIndex).Value.u.Complex = cp = TCpx_New(gCP, gTP);
						cp->itsReal = *rp++;
						cp->itsImag = *rp++;
					}
					else	// Assign the remaining keys with #void
						atHMBind(ep->itsDictionaryArray,envIndex).Value = gCP->Tval_VOID;
				}
				FrameExit(*ret)
			break;
			default:
				FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
			} // end switch
	break;


	/* Handle the case where the first argument is a dictionary or directory */
	case TYDICTIONARY:
	case TYDIRECTORY:
		/* create a new structure object */
		ep = TStructure_New(gCP,gTP);
		asTag(ret) = TYSTRUCTURE;
		asObject(ret) = (TObject*)ep;
		ep->itsCdr = gCP->Tval_VOID;

		/* Copy the elements of the Dictionary object  or	  */
		/* Directory object (arg[0]) to the result Structure */
		
		if (asTag(&argv[0]) == TYDICTIONARY)
			{
 			/* get the size of the source dictionary containing initial keys and values */
			dc = (TDictionary*) asObject(&argv[0]);
			sizeKeys = dc->itsMaxItemIndex;
			for (envIndex = 0; envIndex < sizeKeys; envIndex++)
				{
				*ec = TStructure_SetIV1(gCP,gTP,*ret,
										 TOBJ(atHMBind(dc->itsDictionaryArray,envIndex).Key),
										 atHMBind(dc->itsDictionaryArray,envIndex).Value);
				}
			}
		else
			{
 			/* get the size of the source directory containing initial keys and values */
			dp = (TDirectory*) asObject(&argv[0]);
			sizeValues = dp->itsMaxItemIndex;
			
			for (envIndex = 0; envIndex < sizeValues; envIndex++)
				{
				*ec = TStructure_SetIV1(gCP,gTP,*ret,
										 atHMPBind(dp->itsDirectoryArray,envIndex).Key,
										 atHMPBind(dp->itsDirectoryArray,envIndex).Value);
				}
			}

		/* Check second argument */
		switch (argv[1].Tag)
			{
			case TYDICTIONARY:

 				/* get the size of the source dictionary containing the values */
				dc = (TDictionary*) asObject(&argv[1]);
				sizeValues = dc->itsMaxItemIndex;

				/* Scan the source dictionary (dc) for keys that match in	*/
				/* the target Structure (*ret)								*/
				/* If a key is found, update the value in the Structure		*/
				/* If a key is not found, add the key and its value			*/
				for (dirIndex = 0; dirIndex < sizeValues; dirIndex++)
					{
					*ec = TStructure_SetIV1(gCP,gTP,*ret,
											 TOBJ(atHMBind(dc->itsDictionaryArray,dirIndex).Key),
											 atHMBind(dc->itsDictionaryArray,dirIndex).Value);
					}

				FrameExit(*ret)
				break;

			case TYDIRECTORY:

 				/* get the size of the source directory containing the values */
				dp = (TDirectory*) asObject(&argv[1]);
				sizeValues = dp->itsMaxItemIndex;
				/* Attempt to find a matching key in the target Structure      */
				/* If the key is found, update the value in the Structure      */
				/* If the key is not found, add the key and its value */
				for (dirIndex = 0; dirIndex < sizeValues; dirIndex++)
					{
					*ec = TStructure_SetIV1(gCP,gTP,*ret,
											 atHMPBind(dp->itsDirectoryArray,dirIndex).Key,
											 atHMPBind(dp->itsDirectoryArray,dirIndex).Value);
					}
				FrameExit(*ret)
				break;

			case TYSTRUCTURE:
 				/* get the size of the source Structure containing the values */
				ep2 = (TStructure*) asObject(&argv[1]);
				sizeValues = ep2->itsMaxItemIndex;

				/* Attempt to find a matching key in the target Structure (ep) */
				/* If the key is found, update the value in the Structure      */
				/* If the key is not found, add the key and its value */

				for (envIndex = 0; envIndex < sizeValues; envIndex++)
					{
					*ec = TStructure_SetIV1(gCP,gTP,*ret, 
											 TOBJ(atHMBind(ep2->itsDictionaryArray,envIndex).Key),
											 atHMBind(ep2->itsDictionaryArray,envIndex).Value);
					}
				FrameExit(*ret)
				break;

			default:
				FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
			break;
			} /* end switch */
	break;

	default:
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
	break;
	}

}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ObjToDictionary


The FConvert_ObjToDictionary converts the argument(s) to an Dictionary.If a second
argument is present,  the conversion may involve an update to the Dictionary. 


(objectToDictionary obj)
		   The FConvert_ObjToDictionary procedure converts the object to a Dictionary. 

(objectToDictionary {vector1} {vector2}
		  The I-th item of {vector1} paired with the I-th item in the {vector2} argument.
          to form a Dictionary. Each item in  {vector1} must be a symbol 
          and may contain more items than {vector2}.
          If {vector1} contains more items than {vector2} then the keys in the 
          structure will be assigned a value of #void. 

(objectToDictionary {structure} {vector})
		  The I-th element from the{structure} argument is updated with the 
		  values from the I-th element from the {vector} argument. 
          If {vector} contains more elements than {structure}, extra elements 
		  are ignored. The resulting structure is converted to a Dictionary and is
		  returned.
		  

(objectToDictionary {structure1} {structure2})
(objectToDictionary {structure}  {dictionary})
(objectToDictionary {structure}  {directory})

	      The {structure1} argument is updated by the values
          {structure2},{dictionary}, or {directory} only 
		  if the key from {structure2}, etc. matches a key from {structure1}.
		  Keys that do not match are ignored. The result structure is 
		  converted to a Dictionary and is returned.
		  
(objectToDictionary {dictionary1} {dictionary2})
(objectToDictionary {dictionary}  {directory})
(objectToDictionary {dictionary}  {structure})

	      The {dictionary} object and will be updated by the values in the second
          argument {dictionary2} {directory}, or {structure}
		  only if the keys match. If the key from the second argument do not match,
          it is added to the first argument object.
		  
(objectToDictionary {directory}   {dictionary})
(objectToDictionary {directory1}  {directory2})
(objectToDictionary {directory1}  {structure})
		   
	      The {directory} object and will be updated 
          by the values in the second argument {dictionary2} {directory}, 
		  or {structure}only if the keys match. If the key
          from the second argument do not match, it is added to the first
		  argument object. The result Directory is converted to a Dictionary
		  and the Dictionary is returned.

#endif

TVAL FConvert_ObjToDictionary(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM                 dicIndex;
NUM                 sizeKeys;
NUM                 sizeValues;
LpREAL				pp;			// -> complex pair
StartFrame
DeclareOBJ(TVector,vp);
DeclareOBJ(TVector,vp1);
DeclareOBJ(TVector,vp2);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TMatrix,mp1);
DeclareOBJ(TMatrix,mp2);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TNumMatrix,np1);
DeclareOBJ(TNumMatrix,np2);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TStructure,ep1);
DeclareOBJ(TStructure,ep2);
DeclareOBJ(TDirectory,dp);
DeclareOBJ(TDictionary,dc);
DeclareOBJ(TDictionary,dc2);
DeclareOBJ(TCpxVector, vCpxp);
DeclareOBJ(TCpx,cp);
DeclareTVAL(ret);
DeclareTVAL(tmpKey);
DeclareTVAL(tmpTval);
DeclareTVAL(argTval);
DeclareTVAL(ec);
EndFrame

 
/*  Initialization */

*ret = gCP->Tval_VOID;
if ((argc == 1) && (argv[0].Tag == TYPAIR) && (argv[0].u.Pair == NIL)) 
	{
	dc = TDictionary_New(gCP,gTP);
	ret->Tag = TYDICTIONARY;
	ret->u.Object = (TObject*)dc;
	FrameExit(*ret)
	}
else
if ((argc <= 0) || (argc > 2) || !_VALIDTVAL(argv[0])) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

if (argc == 1)
	{
	switch(asTag(&argv[0]))
		{
		/*  If the object is already a Dictionary, then return it unchanged. */
		case TYDICTIONARY:
			*ret = argv[0];
			FrameExit(*ret)
		break;

		case TYSTRUCTURE:
			dc = TDictionary_New(gCP,gTP);
			asTag(ret) = TYDICTIONARY;
			asObject(ret) = (TObject*)dc;
			/* get the size of the Structure */
			ep = (TStructure*)asObject(&argv[0]);
			sizeKeys = ep->itsMaxItemIndex;
			sizeValues = ep->itsMaxItemIndex;

			/* Fill the Dictionary with the keys and values from the Structure */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the key from the vector argument */
				/* Save the value from the vector argument */
				*tmpKey = TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key);
				*tmpTval = atHMBind(ep->itsDictionaryArray,dicIndex).Value;
				*ec = TDictionary_SetIV1(gCP,gTP,*ret,*tmpKey,*tmpTval);
				ExitOnError(*ec);
				}

            dc->itsCdr = ep->itsCdr;

			FrameExit(*ret)
		break;
    
		case TYDIRECTORY:
			dc = TDictionary_New(gCP,gTP);
			asTag(ret) = TYDICTIONARY;
			asObject(ret) = (TObject*)dc;
			/* get the size of the directory */
			dp = (TDirectory*)asObject(&argv[0]);
			sizeKeys = dp->itsMaxItemIndex;
			sizeValues = dp->itsMaxItemIndex;

			/* Fill the Dictionary with the keys and values from the Structure */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the key from the vector argument */
				/* Save the value from the vector argument */
				*tmpKey = atHMPBind(dp->itsDirectoryArray,dicIndex).Key;
				*tmpTval = atHMPBind(dp->itsDirectoryArray,dicIndex).Value;
				*ec = TDictionary_SetIV1(gCP,gTP,*ret,*tmpKey,*tmpTval);
				ExitOnError(*ec);
				}

            dc->itsCdr = dp->itsCdr;

			FrameExit(*ret)
		break;

		default:
			/*  Convert the object to a vector and init the Dictionary with that */
        
			*ret = FConvert_ObjToVector(gCP,gTP,argc, argv);
			if (asTag(ret) == TYVECTOR)
				{
				vp = (TVector*)asObject(ret);
				dc = TDictionary_New(gCP,gTP);
            
				asObject(argTval) = (TObject*)dc;
				asTag(argTval) = dc->itsObjectType;
				*ec = TDictionary_IDictionary(gCP,gTP,*argTval, vp->itsMaxItemIndex, &atHMTval(vp->itsTvalArray,0));
				ExitOnError(*ec);
				asTag(ret) = TYDICTIONARY;
				asObject(ret) = (TObject*)dc;
				}

            dc->itsCdr = vp->itsCdr;

			FrameExit(*ret)
		break;
		}
	}


/* Handle the case when both arguments are vectors */
if (asTag(&argv[0]) == TYVECTOR && asTag(&argv[1]) == TYVECTOR)
	{
	dc = TDictionary_New(gCP,gTP);
	asTag(ret) = TYDICTIONARY;
	asObject(ret) = (TObject*)dc;
	/* get the size of vector 1 (the vector containing keys) */
    vp1 = (TVector*) asObject(&argv[0]);
	sizeKeys = vp1->itsMaxItemIndex;
 	/* get the size of vector 2 (the vector containing values) */
    vp2 = (TVector*) asObject(&argv[1]);
	sizeValues = vp2->itsMaxItemIndex;

	/* Fill the Dictionary with the keys and values from the 2 vectors */
	for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
		{
		/* Obtain the proposed key value. */
        *tmpKey = atHMTval(vp1->itsTvalArray,dicIndex);

		if (dicIndex < sizeValues)
			{
			/* Save the key from the vector argument */
			/* Save the value from the vector argument */
			*ec = TDictionary_SetIV1(gCP,gTP,*ret,*tmpKey,atHMTval(vp2->itsTvalArray,dicIndex));
			ExitOnError(*ec);
			}
		else
			{
			/* If there are more keys than values, assign the  values as #void */
			*ec = TDictionary_AddNewValue(gCP,gTP,*ret,*tmpKey,gCP->Tval_VOID);
			ExitOnError(*ec);
			}
		}

	FrameExit(*ret)
	}


/* Handle the case where the first argument is a  Structure */
if (asTag(&argv[0]) == TYSTRUCTURE)
	{
	/* create the target Dictionary object */
	dc = TDictionary_New(gCP,gTP);
	asTag(ret) = TYDICTIONARY;
	asObject(ret) = (TObject*)dc;
	
 	/* get the number of elements in the Structure object */
	ep = (TStructure*) asObject(&argv[0]);
	sizeKeys = ep->itsMaxItemIndex;
	
	
	/* Check second argument */

	switch (argv[1].Tag)
		{
		case TYVECTOR:
 			/* get the size of the vector containing values */
			vp = (TVector*) asObject(&argv[1]);
			sizeValues = vp->itsMaxItemIndex;

			/* Map the values from the vector argument to the   */
			/* Dictonary object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the vector argument */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMTval(vp->itsTvalArray,dicIndex));
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}

			FrameExit(*ret)
			break;

		case TYMATRIX:
 			/* get the size of the matrix containing values */
			mp = (TMatrix*) asObject(&argv[1]);
			sizeValues = mp->itsMaxItemIndex;

			/* Map the values from the vector argument to the   */
			/* Dictonary object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the vector argument */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMTval(mp->itsTvalMatrix,dicIndex));
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}

			FrameExit(*ret)
			break;


		case TYNUMMATRIX:
 			/* get the size of the matrix containing values */
			np = (TNumMatrix*) asObject(&argv[1]);
			sizeValues = np->itsMaxItemIndex;

			/* Map the values from the vector argument to the   */
			/* Dictonary object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the vector argument */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   TREAL(atHMReal(np->itsRealMatrix,dicIndex)));
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}

			FrameExit(*ret)
			break;


		case TYBITVECTOR:
 			/* get the size of the bit vector containing values */
			vBitp = (TBitVector*)asObject(&argv[1]);
			sizeValues = vBitp->itsMaxItemIndex;

			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the value from the bit vector argument */
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the bit vector argument */
					tmpTval->Tag = TYNUM;
					tmpTval->u.Int = ((atHMChar(vBitp->itsBitArray,(NUM)(dicIndex/8)) & gCP->TBitVector_OrMasks[(dicIndex%8)]) != 0);
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key), 
										   *tmpTval);

					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}

			FrameExit(*ret)
			break;

		case TYBYTEVECTOR:
 			/* get the size of the bit vector containing values */
			vBytep = (TByteVector*)asObject(&argv[1]);
			sizeValues = vBytep->itsMaxItemIndex;

			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the value from the byte vector argument */
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the byte vector argument */
					tmpTval->Tag = TYNUM;
					tmpTval->u.Int = atHMChar(vBytep->itsByteArray,dicIndex);

					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key), 
										   *tmpTval);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}

			FrameExit(*ret)
			break;

		case TYINTVECTOR:
            vIntp = (TIntVector*)asObject(&argv[1]);
            sizeValues = vIntp->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the value from the int vector argument */
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the int vector argument */
					tmpTval->Tag   = TYNUM;
				    tmpTval->u.Int = atHMInt(vIntp->itsIntArray,dicIndex);
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key), 
										   *tmpTval);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}
			FrameExit(*ret)
			break;

		case TYLONGVECTOR:
            vLngp = (TLongVector*)asObject(&argv[1]);
            sizeValues = vLngp->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the value from the int vector argument */
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the int vector argument */
					tmpTval->Tag   = TYNUM;
				    tmpTval->u.Int = atHMLong(vLngp->itsLongArray,dicIndex);
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key), 
										   *tmpTval);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}
			FrameExit(*ret)
			break;

		case TYSHORTVECTOR:
            vShtp = (TShtVector*)asObject(&argv[1]);
            sizeValues = vShtp->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the value from the int vector argument */
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the int vector argument */
					tmpTval->Tag   = TYNUM;
				    tmpTval->u.Int = atHMShort(vShtp->itsShortArray,dicIndex);
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key), 
										   *tmpTval);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}
			FrameExit(*ret)
			break;

		case TYNUMVECTOR:
            vNump = (TNumVector*)asObject(&argv[1]);
            sizeValues = vNump->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the value from the byte vector argument */
				if (dicIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the num vector argument */
					tmpTval->Tag   = TYREAL;
				    tmpTval->u.Real = atHMReal(vNump->itsRealArray,dicIndex);
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key), 
										   *tmpTval);	
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}
			FrameExit(*ret)
			break;

		case TYFLTVECTOR:
  			/* get the size of the float vector containing values */
            vFltp = (TFltVector*)asObject(&argv[1]);
            sizeValues = vFltp->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
				{
				/* Save the value from the float vector argument */
				if (dicIndex < sizeValues)
					{

					/* Save the key from the Structure argument */
					/* Save the value from the float vector argument */
					tmpTval->Tag   = TYREAL;
				    tmpTval->u.Real = atHMFloat(vFltp->itsFloatArray,dicIndex);
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key), 
										   *tmpTval);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dicIndex).Value);
					}
				}

			FrameExit(*ret)
			break;

		case TYSTRUCTURE:
		case TYDICTIONARY:
		case TYDIRECTORY:

			/* Handle the case where both arguments are Structures.   */

			*tmpTval = FConvert_ObjToStructure(gCP,gTP,argc,argv);
			ExitOnError(*tmpTval);

			*tmpTval = FConvert_ObjToDictionary(gCP,gTP,1,tmpTval);
			FrameExit(*tmpTval)

			break;
		case TYCPXVECTOR:
            vCpxp = argv[1].u.CpxVector;
            sizeValues = vCpxp->itsMaxItemIndex;
			pp = (LpREAL)*vCpxp->itsCpxArray;
			// Map these values to the Structure object
			for (dicIndex = 0; dicIndex < sizeKeys; dicIndex++)
			{	if (dicIndex < sizeValues)
				{	// Set the key from Structure arg. and the value from cpx vector
					tmpTval->Tag   = TYCPX;
				    tmpTval->u.Complex = cp = TCpx_New(gCP, gTP);
					cp->itsReal = *pp++;
					cp->itsImag = *pp++;
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
						TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key), *tmpTval);	
				}
				else // If no more values, keep the same structure value
					*ec = TDictionary_SetIV1(gCP,gTP,*ret,
						TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
						atHMBind(ep->itsDictionaryArray,dicIndex).Value);
			}
			FrameExit(*ret)
			break;
		default:
			FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
		} // end switch
	} // end if

/* Handle the case where the first argument is a dictionary or directory */
if (asTag(&argv[0]) == TYDICTIONARY || asTag(&argv[0]) == TYDIRECTORY)
	{
	/* If argument1 is a Dictionary, its contents will be modified */
	/* by any keys matching from the argument2 object */
		
	if (asTag(&argv[0]) == TYDICTIONARY)
		{
		dc = (TDictionary*) asObject(&argv[0]);
		asTag(ret) = TYDICTIONARY;
		asObject(ret) = (TObject*)dc;
		sizeKeys = dc->itsMaxItemIndex;
		}
	else
		{
		/* If argument1 is a Directory, its contents will be copied to a Dictionary */
		/* create a new Dictionary object */
		dc = TDictionary_New(gCP,gTP);
		asTag(ret) = TYDICTIONARY;
		asObject(ret) = (TObject*)dc;

 		/* get the size of the target directory containing initial keys and values */
		dp = (TDirectory*) asObject(&argv[0]);
		/* Copy the elements of the Directory object (arg[0]) */
		/* to the result Dictionary */
		sizeValues = dp->itsMaxItemIndex;
		
		for (dicIndex = 0; dicIndex < sizeValues; dicIndex++)
			{
			*ec = TDictionary_SetIV1(gCP,gTP,*ret,
									 atHMPBind(dp->itsDirectoryArray,dicIndex).Key,
									 atHMPBind(dp->itsDirectoryArray,dicIndex).Value);
			}
		}

	/* Check second argument */
	switch (argv[1].Tag)
		{
		case TYDICTIONARY:

 			/* get the size of the source dictionary containing the values */
			dc = (TDictionary*) asObject(&argv[1]);
			sizeValues = dc->itsMaxItemIndex;

			/* Attempt to find a matching key in the target Structure (ep) */
			/* If the key is found, update the value in the Structure      */
			/* If the key is not found, add the key and its value */
			for (dicIndex = 0; dicIndex < sizeValues; dicIndex++)
				{
				*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										 TOBJ(atHMBind(dc->itsDictionaryArray,dicIndex).Key),
										 atHMBind(dc->itsDictionaryArray,dicIndex).Value);
				}

			FrameExit(*ret)
			break;

		case TYDIRECTORY:

 			/* get the size of the source directory containing the values */
			dp = (TDirectory*) asObject(&argv[1]);
			sizeValues = dp->itsMaxItemIndex;
			/* Attempt to find a matching key in the target Structure (ep) */
			/* If the key is found, update the value in the Structure      */
			/* If the key is not found, add the key and its value */
			for (dicIndex = 0; dicIndex < sizeValues; dicIndex++)
				{
				*ec = TDictionary_SetIV1(gCP,gTP,*ret,
										 atHMPBind(dp->itsDirectoryArray,dicIndex).Key,
										 atHMPBind(dp->itsDirectoryArray,dicIndex).Value);
				}
			FrameExit(*ret)
			break;

		case TYSTRUCTURE:
 			/* get the size of the source Structure containing the values */
			ep = (TStructure*) asObject(&argv[1]);
			sizeValues = ep->itsMaxItemIndex;

			/* Attempt to find a matching key in the target Structure (ep) */
			/* If the key is found, update the value in the Structure      */
			/* If the key is not found, add the key and its value */

			for (dicIndex = 0; dicIndex < sizeValues; dicIndex++)
				{
				*ec = TDictionary_SetIV1(gCP,gTP,*ret, 
										 TOBJ(atHMBind(ep->itsDictionaryArray,dicIndex).Key),
										 atHMBind(ep->itsDictionaryArray,dicIndex).Value);
				}
			FrameExit(*ret)
			break;

		default:
			FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

		} /* end switch */
	} /* end if */
else
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ObjToDirectory

The FConvert_ObjToDirectory converts the argument(s) to an Directory.If a second
argument is present, the two arguments will be either merged or joined depending on
the types passed. 


(objectToDirectory obj)
		   The FConvert_ObjToDirectory procedure converts the object to a Directory. 

(objectToDirectory {vector1} {vector2}
		  The I-th item of {vector1} paired with the I-th item in the {vector2} argument.
          to form a Directory. Each item in  {vector1} must be a symbol 
          and may contain more items than {vector2}.
          If {vector1} contains more items than {vector2} then the keys in the 
          structure will be assigned a value of #void. 

(objectToDirectory {structure} {vector})
		  The I-th element from the{structure} argument is updated with the 
		  values from the I-th element from the {vector} argument. 
          If {vector} contains more elements than {structure}, extra elements 
		  are ignored. The resulting structure is converted to a Directory and is
		  returned.
		  

(objectToDirectory {structure1} {structure2})
(objectToDirectory {structure}  {dictionary})
(objectToDirectory {structure}  {directory})

	      The {structure1} argument is updated by the values
          {structure2},{dictionary}, or {directory} only 
		  if the key from {structure2}, etc. matches a key from {structure1}.
		  Keys that do not match are ignored. The result structure is 
		  converted to a Directory and is returned.
		  
(objectToDirectory {dictionary1} {dictionary2})
(objectToDirectory {dictionary}  {directory})
(objectToDirectory {dictionary}  {structure})

	      The {dictionary} object and will be updated by the values in the second
          argument {dictionary2} {directory}, or {structure}
		  only if the keys match. If the key from the second argument do not match,
          it is added to the first argument object.
		  
(objectToDirectory {directory}   {dictionary})
(objectToDirectory {directory1}  {directory2})
(objectToDirectory {directory1}  {structure})
		   
	      The {directory} object and will be updated 
          by the values in the second argument {dictionary2}, {directory}, 
		  or {structure} only if the keys match. If the key
          from the second argument do not match, it is added to the first
		  argument object. The result Directory is converted to a Directory
		  and the Directory is returned.


#endif

TVAL FConvert_ObjToDirectory(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM                 dirIndex;
NUM                 sizeKeys;
NUM                 sizeValues;
LpREAL				pp;				// -> complex pair
StartFrame
DeclareOBJ(TVector,vp);
DeclareOBJ(TVector,vp1);
DeclareOBJ(TVector,vp2);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TMatrix,mp1);
DeclareOBJ(TMatrix,mp2);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TNumMatrix,np1);
DeclareOBJ(TNumMatrix,np2);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TStructure,ep1);
DeclareOBJ(TStructure,ep2);
DeclareOBJ(TDirectory,dp);
DeclareOBJ(TDictionary,dc);
DeclareOBJ(TDictionary,dc2);
DeclareOBJ(TCpxVector, vCpxp);
DeclareOBJ(TCpx, cp);
DeclareTVAL(ret);
DeclareTVAL(tmpTval);
DeclareTVAL(argTval);
DeclareTVAL(ec);
EndFrame
 
/*  Initialization */
*ret = gCP->Tval_VOID;
if (argc > 2) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

if (argc == 1)
	{
	switch(asTag(&argv[0]))
		{
	/*  If the object is already a Directory, then return it unchanged. */
		case TYDIRECTORY:
			*ret = argv[0];
			FrameExit(*ret)
		break;
    
		case TYSTRUCTURE:
			dp = TDirectory_New(gCP,gTP);
			asTag(ret) = TYDIRECTORY;
			asObject(ret) = (TObject*)dp;
			/* get the size of the Structure */
			ep = (TStructure*)asObject(&argv[0]);
			sizeKeys = ep->itsMaxItemIndex;
			sizeValues = ep->itsMaxItemIndex;

			/* Fill the Dictionary with the keys and values from the Structure */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				/* Save the key from the Structure argument */
				/* Save the value from the Structure argument */
				*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										atHMBind(ep->itsDictionaryArray,dirIndex).Value);
				ExitOnError(*ec);
				}

            dp->itsCdr = ep->itsCdr;

			FrameExit(*ret)
		break;
    
		case TYDICTIONARY:
			dp = TDirectory_New(gCP,gTP);
			asTag(ret) = TYDIRECTORY;
			asObject(ret) = (TObject*)dp;
			/* get the size of the dictionary */
			dc = (TDictionary*)asObject(&argv[0]);
			sizeKeys = dc->itsMaxItemIndex;
			sizeValues = dc->itsMaxItemIndex;

			/* Fill the Directory with the keys and values from the dictionary */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				/* Save the key from the dictionary argument */
				/* Save the value from the dictionary argument */
				*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										 TOBJ(atHMBind(dc->itsDictionaryArray,dirIndex).Key),
										 atHMBind(dc->itsDictionaryArray,dirIndex).Value);
				ExitOnError(*ec);
				}

            dp->itsCdr = dc->itsCdr;

			FrameExit(*ret)
		break;

		default:
			/*  Convert the object to a vector and init the Directory with that */
        
			*ret = FConvert_ObjToVector(gCP,gTP,argc, argv);
			if (asTag(ret) == TYVECTOR)
				{
				vp = (TVector*)asObject(ret);
				dp = TDirectory_New(gCP,gTP);
            
				asObject(argTval) = (TObject*)dp;
				asTag(argTval) = dp->itsObjectType;
				if (vp->itsMaxItemIndex > 0)
					{
					*ec = TDirectory_IDirectory(gCP, gTP, *argTval, vp->itsMaxItemIndex, &atHMTval(vp->itsTvalArray,0));
					ExitOnError(*ec);
					}
				asTag(ret) = TYDIRECTORY;
				asObject(ret) = (TObject*)dp;
				}

            dp->itsCdr = vp->itsCdr;

			FrameExit(*ret)
		break;
		}
	}

/* Handle the case when both arguments are vectors */
if (asTag(&argv[0]) == TYVECTOR && asTag(&argv[1]) == TYVECTOR)
	{
	dp = TDirectory_New(gCP,gTP);
	asTag(ret) = TYDIRECTORY;
	asObject(ret) = (TObject*)dp;
	/* get the size of vector 1 (the vector containing keys) */
    vp1 = (TVector*) asObject(&argv[0]);
	sizeKeys = vp1->itsMaxItemIndex;
 	/* get the size of vector 2 (the vector containing values) */
    vp2 = (TVector*) asObject(&argv[1]);
	sizeValues = vp2->itsMaxItemIndex;

	/* Fill the Directory with the keys and values from the 2 vectors */
	for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
		{
		/* Save the value from the second vector argument */
		if (dirIndex < sizeValues)
			{
			*ec = TDirectory_SetIV1(gCP,gTP,*ret,
								   atHMTval(vp1->itsTvalArray,dirIndex),
								   atHMTval(vp2->itsTvalArray,dirIndex));
			ExitOnError(*ec);
			}
		else
			{
			/* If there are more keys than values, assign the remaining keys with #void */
			*ec = TDirectory_AddNewValue(gCP,gTP,*ret,
								   atHMTval(vp1->itsTvalArray,dirIndex),
								   gCP->Tval_VOID);
			ExitOnError(*ec);
			}
		}

	FrameExit(*ret)
	}

/* Handle the case where the first argument is a  Structure */
if (asTag(&argv[0]) == TYSTRUCTURE)
	{
	/* create a new Directory object */
	dp = TDirectory_New(gCP,gTP);
	asTag(ret) = TYDIRECTORY;
	asObject(ret) = (TObject*)dp;
	
	
 	/* get the number of elements in the Structure object */
	ep = (TStructure*) asObject(&argv[0]);
	sizeKeys = ep->itsMaxItemIndex;
	
	
	/* Check second argument */

	switch (argv[1].Tag)
		{
		case TYVECTOR:
 			/* get the size of the vector containing values */
			vp = (TVector*) asObject(&argv[1]);
			sizeValues = vp->itsMaxItemIndex;

			/* Map the values from the vector argument to the   */
			/* Dictonary object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the vector argument */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMTval(vp->itsTvalArray,dirIndex));
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;

		case TYMATRIX:
 			/* get the size of the matrix containing values */
			mp = (TMatrix*) asObject(&argv[1]);
			sizeValues = mp->itsMaxItemIndex;

			/* Map the values from the vector argument to the   */
			/* Dictonary object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the vector argument */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMTval(mp->itsTvalMatrix,dirIndex));
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;

		case TYNUMMATRIX:
 			/* get the size of the matrix containing values */
			np = (TNumMatrix*) asObject(&argv[1]);
			sizeValues = np->itsMaxItemIndex;

			/* Map the values from the vector argument to the   */
			/* Dictonary object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the vector argument */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   TREAL(atHMReal(np->itsRealMatrix,dirIndex)));
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;

		case TYBITVECTOR:
 			/* get the size of the bit vector containing values */
			vBitp = (TBitVector*)asObject(&argv[1]);
			sizeValues = vBitp->itsMaxItemIndex;

			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the bit vector argument */
					tmpTval->Tag = TYNUM;
					tmpTval->u.Int = ((atHMChar(vBitp->itsBitArray,(NUM)(dirIndex/8)) & gCP->TBitVector_OrMasks[(dirIndex%8)]) != 0);
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key), 
										   *tmpTval);
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;			

		case TYBYTEVECTOR:
 			/* get the size of the bit vector containing values */
			vBytep = (TByteVector*)asObject(&argv[1]);
			sizeValues = vBytep->itsMaxItemIndex;

			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the byte vector argument */
					tmpTval->Tag = TYNUM;
					tmpTval->u.Int = atHMChar(vBytep->itsByteArray,dirIndex);

					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key), 
										   *tmpTval);
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;

		case TYINTVECTOR:
            vIntp = (TIntVector*)asObject(&argv[1]);
            sizeValues = vIntp->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the int vector argument */
					tmpTval->Tag   = TYNUM;
				    tmpTval->u.Int = atHMInt(vIntp->itsIntArray,dirIndex);

					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key), 
										   *tmpTval);
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;

		case TYLONGVECTOR:
            vLngp = (TLongVector*)asObject(&argv[1]);
            sizeValues = vLngp->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the int vector argument */
					tmpTval->Tag   = TYNUM;
				    tmpTval->u.Int = atHMLong(vLngp->itsLongArray,dirIndex);

					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key), 
										   *tmpTval);
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;

		case TYSHORTVECTOR:
            vShtp = (TShtVector*)asObject(&argv[1]);
            sizeValues = vShtp->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the int vector argument */
					tmpTval->Tag   = TYNUM;
				    tmpTval->u.Int = atHMShort(vShtp->itsShortArray,dirIndex);

					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key), 
										   *tmpTval);
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;

		case TYNUMVECTOR:
            vNump = (TNumVector*)asObject(&argv[1]);
            sizeValues = vNump->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the int vector argument */
					tmpTval->Tag   = TYREAL;
				    tmpTval->u.Real = atHMReal(vNump->itsRealArray,dirIndex);
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key), 
										   *tmpTval);
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}

			FrameExit(*ret)
			break;

		case TYFLTVECTOR:
  			/* get the size of the float vector containing values */
            vFltp = (TFltVector*)asObject(&argv[1]);
            sizeValues = vFltp->itsMaxItemIndex;
			/* Map the values from the vector argument to the  */
			/* Structure object based on matching positions  */
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
				{
				if (dirIndex < sizeValues)
					{
					/* Save the key from the Structure argument */
					/* Save the value from the int vector argument */
					tmpTval->Tag   = TYREAL;
				    tmpTval->u.Real = atHMFloat(vFltp->itsFloatArray,dirIndex);
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key), 
										   *tmpTval);
					ExitOnError(*ec);
					}
				else
					{
					/* If there are more Structure values than vector values,  */
					/* assign the Structure values as is */
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										   TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										   atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
					}
				}


			FrameExit(*ret)
			break;


		case TYSTRUCTURE:
		case TYDICTIONARY:
		case TYDIRECTORY:

			/* Handle the case where both arguments are Structures.   */

			*tmpTval = FConvert_ObjToStructure(gCP,gTP,argc,argv);
			ExitOnError(*tmpTval);

			*tmpTval = FConvert_ObjToDirectory(gCP,gTP,1,tmpTval);
			FrameExit(*tmpTval)

			break;
		case TYCPXVECTOR:
            vCpxp = argv[1].u.CpxVector;
            sizeValues = vCpxp->itsMaxItemIndex;
			pp = (LpREAL)*vCpxp->itsCpxArray;
			// Map the values from cpx vector argument to the Structure
			for (dirIndex = 0; dirIndex < sizeKeys; dirIndex++)
			{	if (dirIndex < sizeValues)
				{	// Return key from the Structure and complex value from vector
					tmpTval->Tag  = TYCPX;
				    tmpTval->u.Complex = cp = TCpx_New(gCP, gTP);
					cp->itsReal = *pp++;
					cp->itsImag = *pp++;
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
						TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key), *tmpTval);
					ExitOnError(*ec);
				}
				else	// Just return the next key and the next structure value
					*ec = TDirectory_SetIV1(gCP,gTP,*ret,
						TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
						atHMBind(ep->itsDictionaryArray,dirIndex).Value);
					ExitOnError(*ec);
			}
			FrameExit(*ret)
			break;
		default:
			FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
		} /* end switch */

	} /* end if */


/* Handle the case where the first argument is a dictionary or directory */
if (asTag(&argv[0]) == TYDICTIONARY || asTag(&argv[0]) == TYDIRECTORY)
	{
	/* If argument1 is a Directory, its contents will be modified */
	/* by any keys matching from the argument2 object */
		
	if (asTag(&argv[0]) == TYDIRECTORY)
		{
		dp = (TDirectory*) asObject(&argv[0]);
		asTag(ret) = TYDIRECTORY;
		asObject(ret) = (TObject*)dp;
		sizeKeys = dp->itsMaxItemIndex;
		}
	else
		{
		/* If argument1 is a Dictionary, its contents will be copied */
		/* to the target Directory. Create a new Directory object   */
		dp = TDirectory_New(gCP,gTP);
		asTag(ret) = TYDIRECTORY;
		asObject(ret) = (TObject*)dp;

 		/* get the size of the Dictionary containing initial keys and values */
		dc = (TDictionary*) asObject(&argv[0]);
		/* Copy the elements of the Dictionary object (arg[0]) */
		/* to the result Directory */
		sizeValues = dc->itsMaxItemIndex;
		
		for (dirIndex = 0; dirIndex < sizeValues; dirIndex++)
			{
			*ec = TDirectory_SetIV1(gCP,gTP,*ret,
									 TOBJ(atHMBind(dc->itsDictionaryArray,dirIndex).Key),
									 atHMBind(dc->itsDictionaryArray,dirIndex).Value);
			ExitOnError(*ec);
			}
		}

	/* Check second argument */
	switch (argv[1].Tag)
		{
		case TYDICTIONARY:

 			/* get the size of the source dictionary containing the values */
			dc = (TDictionary*) asObject(&argv[1]);
			sizeValues = dc->itsMaxItemIndex;

			/* Attempt to find a matching key in the target Structure (ep) */
			/* If the key is found, update the value in the Structure      */
			/* If the key is not found, add the key and its value */
			for (dirIndex = 0; dirIndex < sizeValues; dirIndex++)
				{
				*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										 TOBJ(atHMBind(dc->itsDictionaryArray,dirIndex).Key),
										 atHMBind(dc->itsDictionaryArray,dirIndex).Value);
				ExitOnError(*ec);
				}

			FrameExit(*ret)
			break;

		case TYDIRECTORY:

 			/* get the size of the source directory containing the values */
			dp = (TDirectory*) asObject(&argv[1]);
			sizeValues = dp->itsMaxItemIndex;
			/* Attempt to find a matching key in the target Structure (ep) */
			/* If the key is found, update the value in the Structure      */
			/* If the key is not found, add the key and its value */
			for (dirIndex = 0; dirIndex < sizeValues; dirIndex++)
				{
				*ec = TDirectory_SetIV1(gCP,gTP,*ret,
										 atHMPBind(dp->itsDirectoryArray,dirIndex).Key,
										 atHMPBind(dp->itsDirectoryArray,dirIndex).Value);
				ExitOnError(*ec);
				}
			FrameExit(*ret)
			break;

		case TYSTRUCTURE:
 			/* get the size of the source Structure containing the values */
			ep = (TStructure*) asObject(&argv[1]);
			sizeValues = ep->itsMaxItemIndex;

			/* Attempt to find a matching key in the target Structure (ep) */
			/* If the key is found, update the value in the Structure      */
			/* If the key is not found, add the key and its value */

			for (dirIndex = 0; dirIndex < sizeValues; dirIndex++)
				{
				*ec = TDirectory_SetIV1(gCP,gTP,*ret, 
										 TOBJ(atHMBind(ep->itsDictionaryArray,dirIndex).Key),
										 atHMBind(ep->itsDictionaryArray,dirIndex).Value);
				ExitOnError(*ec);
				}
			FrameExit(*ret)
			break;

		default:
			FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

		} /* end switch */
	} /* end if */
else
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)


FrameExit(*ret)
}

/*	----------------------------------------------------------------------------------------
FConvert_ToComplex

FConvert_ToComplex converts a native type (or string, complex) to a Complex number.
	------------------------------------------------------------------------------------- */
TVAL FConvert_ToComplex(LpXCONTEXT gCP, LpTHREAD gTP, TVAL arg)     
{
	char		*stgp;	// -> string
	REAL		value;	// Real value from conversion

	StartFrame
	DeclareTVAL(ret);
	DeclareTVAL(tmp);
	DeclareOBJ(TCpx, cp);
	EndFrame
 
	//  Initialization
	*ret = gCP->Tval_VOID;

	// Create empty Complex TVAL
	cp = TCpx_New(gCP,gTP);
	ret->u.Complex = cp;
	ret->Tag = TYCPX;

	switch(arg.Tag)
	{
	case TYVOID:
		break;

	case TYCHAR:
	case TYBOLE:
		cp->itsReal = (REAL)arg.u.Char;
		break;

	case TYCOMPARE:
	case TYTYPE:
		cp->itsReal = (REAL)arg.u.Short;
		break;

	case TYNUM:
	case TYPOINTER:
		cp->itsReal = (REAL)arg.u.Int;
		break;

	case TYREAL:
	case TYMONEY:
	case TYDATE:
		cp->itsReal = arg.u.Real;
		break;

	case TYTEXT:
	case TYSYMBOL:
	case TYSTRING:
	case TYERROR:
		stgp = FObject_GetStringPtr(gCP, gTP, arg.u.Object);
		if (stgp == NIL)
			break; 
		*tmp = TObject_ator(gCP, gTP, &value, stgp);
		ExitOnError(*tmp);
		cp->itsReal = value;
		break;
	
	case TYSTRINGSUBSTR:
        /* Get a pointer to our data. */
        stgp = TStringSubstringT_GetStringPtr(gCP, gTP, arg);
        if (stgp == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        
        /* copy our substring to a temporary buffer */
        strncpy(gTP->TempBuffer, stgp, SubLen(arg));
        gTP->TempBuffer[SubLen(arg)] = 0;
	    *tmp = TObject_ator(gCP, gTP, &value, gTP->TempBuffer);
	    
	    ExitOnError(*tmp);
	    cp->itsReal = value;
	    break;

	case TYCPX:
		cp->itsReal = arg.u.Complex->itsReal;
		cp->itsImag = arg.u.Complex->itsImag;
		break;

	default:
		FrameExit(gCP->TObject_ERROR_INVALID)
		break;
	}
	FrameExit(*ret)
}

/*	----------------------------------------------------------------------------------------
FConvert_ObjToComplex

FConvert_ObjToComplex converts an object to a Complex vector.
	------------------------------------------------------------------------------------- */
TVAL FConvert_ObjToComplex(LpXCONTEXT gCP, LpTHREAD gTP, LpNUM np,TVAL arg)     
{
	NUM	  i,size,n,nr;	// Loop index, array size, number of reals needed, number converted.

	StartFrame
	DeclareOBJ(TBitVector, bvp);
	DeclareOBJ(TByteVector,byp);
	DeclareOBJ(TCpx, cp);
	DeclareOBJ(TDictionary, dp);
	DeclareOBJ(TDirectory, drp);
	DeclareOBJ(TFltVector, fvp);
	DeclareOBJ(TIntVector, ivp);
	DeclareOBJ(TShtVector, shvp);
	DeclareOBJ(TLongVector, lngvp);
	DeclareOBJ(TMatrix, mp);
	DeclareOBJ(TNumMatrix, nmp);
	DeclareOBJ(TNumVector, nvp);
	DeclareOBJ(TObjVector, ovp);
	DeclareOBJ(TPair, pp);
	DeclareOBJ(TStructure, sp);
	DeclareOBJ(TVector, vp);
	DeclareTVAL(ndx);
	DeclareTVAL(tmp);
	DeclareTVAL(argr);
	EndFrame
 
	//  Initialization
	*argr = gCP->Tval_VOID;
	n = *np;				// Number of conversions requested
	*np = 0;				// No conversions yet
	ndx->Tag = TYNUM;

	switch(arg.Tag)
	{
	case TYBITVECTOR:
	case TYBYTEVECTOR:
	case TYFLTVECTOR:
	case TYINTVECTOR:
	case TYSHORTVECTOR:
	case TYLONGVECTOR:
	case TYNUMMATRIX:
	case TYNUMVECTOR:
		// Create empty Complex TVAL
		cp = TCpx_New(gCP,gTP);
		argr->u.Complex = cp;
		argr->Tag = TYCPX;
		break;
	}

	switch(arg.Tag)
	{
	case TYVOID:
	case TYCHAR:
	case TYBOLE:
	case TYNUM:
	case TYPOINTER:
	case TYREAL:
	case TYMONEY:
	case TYDATE:
	case TYTEXT:
	case TYSYMBOL:
	case TYSTRING:
	case TYSTRINGSUBSTR:
	case TYERROR:
	case TYCPX:
		*argr = FConvert_ToComplex(gCP, gTP, arg);
		*np = (arg.Tag == TYCPX) ? 2 : 1;
		break;

	// Objects in alphabetic order
	case TYBITVECTOR:
		if ((bvp = arg.u.BitVector) != NIL && bvp->itsBitArray != NIL)
		{	size = bvp->itsMaxItemIndex;
			if (size > 0)
			{	cp->itsReal = (REAL)(((*bvp->itsBitArray)->Char[0] & gCP->TBitVector_OrMasks[0]) != 0);
				*np = 1;
			}
			if (size > 1)
			{	cp->itsImag = (REAL)((atHMChar(bvp->itsBitArray,0) & gCP->TBitVector_OrMasks[1]) != 0);
				*np = 2;
			}
		}
		break;

	case TYBYTEVECTOR:
		if ((byp = arg.u.ByteVector) != NIL && byp->itsByteArray != NIL)
		{	size = byp->itsMaxItemIndex;
			if (size > 0)
			{	cp->itsReal = (REAL)((*byp->itsByteArray)->Char[0]);
				*np = 1;
			}
			if (size > 1)
			{	cp->itsImag = (REAL)(atHMChar(byp->itsByteArray,1));
				*np = 2;
			}
		}
		break;

	case TYDICTIONARY:		// Same as structure but sorted by key. Key is an object.
		if ((dp = arg.u.Dictionary) != NIL && dp->itsDictionaryArray != NIL)
		{	size = dp->itsMaxItemIndex;
			for (i = 0; i < size && n > 0; ++i, n -= nr)
			{	*tmp = atHMBind(dp->itsDictionaryArray, i).Value;
				nr = n;
				*tmp = FConvert_ObjToComplex(gCP, gTP, &nr, *tmp);
				if (nr > 0)			// Got something
				{	if (n == 1)		// Needed one more real and got it.
					{	argr->u.Complex->itsImag = tmp->u.Complex->itsReal;
						++*np;
					}
					else	// Needed 2 more and got at least one of them
					{	*argr = *tmp;
						*np += nr;
					}
				}
			}
		}
		break;

	case TYDIRECTORY:	// Same as dictionary, but key can be native type
		if ((drp = arg.u.Directory) != NIL && drp->itsDirectoryArray != NIL)
		{	size = drp->itsMaxItemIndex;
			for (i = 0; i < size && n > 0; ++i, n -= nr)
			{	*tmp = atHMPBind(drp->itsDirectoryArray, i).Value;
				nr = n;
				*tmp = FConvert_ObjToComplex(gCP, gTP, &nr, *tmp);
				if (nr > 0)			// Got something
				{	if (n == 1)		// Needed one more real and got it.
					{	argr->u.Complex->itsImag = tmp->u.Complex->itsReal;
						++*np;
					}
					else	// Needed 2 more and got at least one of them
					{	*argr = *tmp;
						*np += nr;
					}
				}
			}
		}
		break;

	case TYFLTVECTOR:
		if ((fvp = arg.u.FltVector) != NIL && fvp->itsFloatArray != NIL)
		{	size = fvp->itsMaxItemIndex;
			if (size > 0)
			{	cp->itsReal = (REAL)atHMFloat(fvp->itsFloatArray, 0);
				*np = 1;
			}
			if (size > 1)
			{	cp->itsImag = (REAL)atHMFloat(fvp->itsFloatArray, 1);
				++*np;
			}
		}
		break;

	case TYINTVECTOR:
		if ((ivp = arg.u.IntVector) != NIL && ivp->itsIntArray != NIL)
		{	size = ivp->itsMaxItemIndex;
			if (size > 0)
			{	cp->itsReal = (REAL)atHMInt(ivp->itsIntArray, 0);
				*np = 1;
			}
			if (size > 1)
			{	cp->itsImag = (REAL)atHMInt(ivp->itsIntArray, 1);
				++*np;
			}
		}
		break;

	case TYLONGVECTOR:
		if ((lngvp = arg.u.LongVector) != NIL && lngvp->itsLongArray != NIL)
		{	size = lngvp->itsMaxItemIndex;
			if (size > 0)
			{	cp->itsReal = (REAL)atHMLong(lngvp->itsLongArray, 0);
				*np = 1;
			}
			if (size > 1)
			{	cp->itsImag = (REAL)atHMLong(lngvp->itsLongArray, 1);
				++*np;
			}
		}
		break;

	case TYSHORTVECTOR:
		if ((shvp = arg.u.ShortVector) != NIL && shvp->itsShortArray != NIL)
		{	size = shvp->itsMaxItemIndex;
			if (size > 0)
			{	cp->itsReal = (REAL)atHMShort(shvp->itsShortArray, 0);
				*np = 1;
			}
			if (size > 1)
			{	cp->itsImag = (REAL)atHMShort(shvp->itsShortArray, 1);
				++*np;
			}
		}
		break;

	case TYMATRIX:
		if ((mp = arg.u.Matrix) != NIL && mp->itsTvalMatrix != NIL)
		{	size = mp->itsMaxItemIndex;
			for (i = 0; i < size && n > 0; ++i, n -= nr)
			{	*tmp = atHMTval(mp->itsTvalMatrix, i);
				nr = n;
				*tmp = FConvert_ObjToComplex(gCP, gTP, &nr, *tmp);
				if (nr > 0)			// Got something
				{	if (n == 1)		// Needed one more real and got it.
					{	argr->u.Complex->itsImag = tmp->u.Complex->itsReal;
						++*np;
					}
					else	// Needed 2 more and got at least one of them
					{	*argr = *tmp;
						*np += nr;
					}
				}
			}
		}
		break;

	case TYNUMMATRIX:
		if ((nmp = arg.u.NumMatrix) != NIL && nmp->itsRealMatrix != NIL)
		{	size = nmp->itsMaxItemIndex;
			if (size > 0)
			{	cp->itsReal = atHMReal(nmp->itsRealMatrix, 0);
				*np = 1;
			}
			if (size > 1)
			{	cp->itsImag = atHMReal(nmp->itsRealMatrix, 1);
				++*np;
			}
		}
		break;

	case TYNUMVECTOR:
		if ((nvp = arg.u.NumVector) != NIL && nvp->itsRealArray != NIL)
		{	size = nvp->itsMaxItemIndex;
			if (size > 0)
			{	cp->itsReal = atHMReal(nvp->itsRealArray, 0);
				*np = 1;
			}
			if (size > 1)
			{	cp->itsImag = atHMReal(nvp->itsRealArray, 1);
				++*np;
			}
			// atHMReal does either of the following:
			// value = *((REAL *)(*nvp->itsRealArray) + 1);
			// value = ((REAL *)(*nvp->itsRealArray))[1];
		}
		break;

	case TYOBJVECTOR:
		if ((ovp = arg.u.ObjVector) != NIL && ovp->itsObjectArray != NIL)
		{	size = ovp->itsMaxItemIndex;
			for (i = 0; i < size && n > 0; ++i, n -= nr)
			{	ndx->u.Int = i;
				*tmp = TObjVector_GetIV1(gCP, gTP, arg, *ndx);
				nr = n;
				*tmp = FConvert_ObjToComplex(gCP, gTP, &nr, *tmp);
				if (nr > 0)			// Got something
				{	if (n == 1)		// Needed one more real and got it.
					{	argr->u.Complex->itsImag = tmp->u.Complex->itsReal;
						++*np;
					}
					else	// Needed 2 more and got at least one of them
					{	*argr = *tmp;
						*np += nr;
					}
				}
			}
		}
		break;

	case TYPAIR:
		if ((pp = arg.u.Pair) != NIL)
		{	//  Coerce the first car to a Complex:
			nr = n;
			*argr = FConvert_ObjToComplex(gCP, gTP, &nr, pp->itsCar);
			cp = argr->u.Complex;
			*np = nr;
			n -= nr;
			if (n > 0)
			{	// Try converting the cdr
				nr = n;
				*tmp = FConvert_ObjToComplex(gCP, gTP, &nr, pp->itsCdr);
				if (nr > 0)
				{	if (n == 1)		// Needed one more real and got it.
					{	cp->itsImag = tmp->u.Complex->itsReal;
						++*np;
					}
					else	// Needed 2 more and got at least one of them
					{	*argr = *tmp;
						*np = nr;
					}
				}
			}
		}
		break;

	case TYSTRUCTURE:
		if ((sp = arg.u.Structure) != NIL && sp->itsDictionaryArray != NIL)
		{	size = sp->itsMaxItemIndex;
			for (i = 0; i < size && n > 0; ++i, n -= nr)
			{	*tmp = atHMBind(sp->itsDictionaryArray, i).Value;
				nr = n;
				*tmp = FConvert_ObjToComplex(gCP, gTP, &nr, *tmp);
				if (nr > 0)			// Got something
				{	if (n == 1)		// Needed one more real and got it.
					{	argr->u.Complex->itsImag = tmp->u.Complex->itsReal;
						++*np;
					}
					else	// Needed 2 more and got at least one of them
					{	*argr = *tmp;
						*np += nr;
					}
				}
			}
		}
		break;

	case TYVECTOR:
		if ((vp = arg.u.Vector) != NIL && vp->itsTvalArray != NIL)
		{	size = vp->itsMaxItemIndex;
			for (i = 0; i < size && n > 0; ++i, n -= nr)
			{	*tmp = atHMTval(vp->itsTvalArray, i);
				nr = n;
				*tmp = FConvert_ObjToComplex(gCP, gTP, &nr, *tmp);
				if (nr > 0)			// Got something
				{	if (n == 1)		// Needed one more real and got it.
					{	argr->u.Complex->itsImag = tmp->u.Complex->itsReal;
						++*np;
					}
					else	// Needed 2 more and got at least one of them
					{	*argr = *tmp;
						*np += nr;
					}
				}
			}
		}
		break;

	default:
		i = 0;		// testing
		//  Convert anything else to Complex with value 0.
		break;
	}
	FrameExit(*argr)
}


/*	--------------------------------------------------------------------------------------
FConvert_ObjToCpxVector

The FConvert_ObjToCpxVector procedure converts an object to a Complex vector. 
--------------------------------------------------------------------------------------- */
TVAL FConvert_ObjToCpxVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
LpTVAL              tp;				// -> next tval in array
LpBIND				bp;				// -> next bind structure in structure array
LpPBIND				pbp;			// -> next pbind structure in directory array
NUM                 si;			    // Source index
NUM                 ssz, dsz;		// Size of source array, destination array
NUM					*isp;			// -> pair of integers
NUM32				*ilongp;		// -> pair of integers
SHORT				*ishp;			// -> pair of integers
REAL				*sdp, *ddp;		// -> pair of doubles
float				*fdp;			// -> pair of floats
StartFrame
DeclareOBJ(TPair, pp);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TCpxVector, vp);
DeclareOBJ(TVector,vVecp);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TCpxVector,vCpxp);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDirectory,xp);
DeclareTVAL(ret);
DeclareTVAL(argr);
DeclareTVAL(ndx1);
DeclareTVAL(tmp);
EndFrame
 
//  Check inputs
*argr = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
if (argv[0].u.Obj == NIL) FrameExit(*argr)

switch(argv[0].Tag)
{
case TYNUMVECTOR:
	vNump = argv[0].u.NumVector;
	ssz = vNump->itsMaxItemIndex ;		// Number of input values
	sdp = (LpREAL)*vNump->itsRealArray;	// -> source array of doubles

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;				// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy each set of two doubles into a complex pair
	for (si  = 0; si < ssz; ++si)
		*ddp++ = *sdp++;

	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYMATRIX:
	mp = argv[0].u.Matrix;
	ssz = mp->itsMaxItemIndex;
	tp = (LpTVAL)*mp->itsTvalMatrix;// -> source array of tvals

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;			// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy next 2 values into a complex pair
	for (si  = 0; si < ssz; ++si)
	{	*tmp = *tp++;
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYNUMMATRIX:
	np = argv[0].u.NumMatrix;
	ssz = np->itsMaxItemIndex ;			// Number of input values
	sdp = (LpREAL)*np->itsRealMatrix;	// -> source array of doubles

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;				// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy each set of two doubles into a complex pair
	for (si  = 0; si < ssz; ++si)
		*ddp++ = *sdp++;

	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYVECTOR:
	vVecp = argv[0].u.Vector;
	ssz = vVecp->itsMaxItemIndex;
	tp = (LpTVAL)*vVecp->itsTvalArray;// -> source array of tvals

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;			// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy next 2 values into a complex pair
	for (si  = 0; si < ssz; ++si)
	{	*tmp = *tp++;
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYBITVECTOR:
	vBitp = argv[0].u.BitVector;
	ssz = vBitp->itsMaxItemIndex ;		// Number of input values
	ndx1->Tag = TYNUM;

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;				// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy each set of two doubles into a complex pair
	for (si  = 0; si < ssz; ++si)
	{	ndx1->u.Int = si;
		*tmp = TBitVector_GetIV1(gCP, gTP, argv[0], *ndx1);
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYBYTEVECTOR:
	vBytep = argv[0].u.ByteVector;
	ssz = vBytep->itsMaxItemIndex ;		// Number of input values
	ndx1->Tag = TYNUM;

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;				// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy each set of two bytes into a complex pair
	for (si  = 0; si < ssz; ++si)
	{	ndx1->u.Int = si;
		*tmp = TByteVector_GetIV1(gCP, gTP, argv[0], *ndx1);
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYINTVECTOR:
	vIntp = argv[0].u.IntVector;
	ssz = vIntp->itsMaxItemIndex ;		// Number of input values
	isp = (LpNUM)*vIntp->itsIntArray;	// -> source array of doubles

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;				// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy each set of two doubles into a complex pair
	for (si  = 0; si < ssz; ++si)
		*ddp++ = (REAL)*isp++;

	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYLONGVECTOR:
	vLngp = argv[0].u.LongVector;
	ssz = vLngp->itsMaxItemIndex ;			// Number of input values
	ilongp = (LpNUM32)*vLngp->itsLongArray;	// -> source array of doubles

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;				// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy each set of two doubles into a complex pair
	for (si  = 0; si < ssz; ++si)
		*ddp++ = (REAL)*ilongp++;

	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYSHORTVECTOR:
	vShtp = argv[0].u.ShortVector;
	ssz = vShtp->itsMaxItemIndex ;			// Number of input values
	ishp = (LpSHORT)*vShtp->itsShortArray;	// -> source array of doubles

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;				// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy each set of two doubles into a complex pair
	for (si  = 0; si < ssz; ++si)
		*ddp++ = (REAL)*ishp++;

	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYFLTVECTOR:
	vFltp = argv[0].u.FltVector;
	ssz = vFltp->itsMaxItemIndex ;			// Number of input values
	fdp = (float *)*vFltp->itsFloatArray;	// -> source array of floats

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;				// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy each set of two doubles into a complex pair
	for (si  = 0; si < ssz; ++si)
		*ddp++ = (REAL)*fdp++;

	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYOBJVECTOR:
	vObjp = argv[0].u.ObjVector;
	ssz = vObjp->itsMaxItemIndex;
	ndx1->Tag = TYNUM;

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;			// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy next 2 values into a complex pair
	for (si  = 0; si < ssz; ++si)
	{	ndx1->u.Int = si;
		*tmp = TObjVector_GetIV1(gCP, gTP, argv[0], *ndx1);;
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYPAIR:
	pp = argv[0].u.Pair;
	*tmp = FUtil2_Length(gCP, gTP, (NUM)1, &argv[0]);
	ExitOnError(*tmp);
	ssz = ret->u.Int;

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;			// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy next 2 values into a complex pair
	tp = argv;
	for (si = 0; tp->Tag == TYPAIR && tp->u.Pair != NIL; ++si)
	{	pp = tp->u.Pair;
		tp = &pp->itsCdr;
		if (tp->Tag == TYSYMBOL && pp->itsCar.u.Symbol == gCP->TLambda_dotcdr)
		{	if (tp->Tag == TYPAIR && tp->u.Pair != NIL)
			{	pp = tp->u.Pair;
				vp->itsCdr = pp->itsCar;
				TCpxVector_SetMaxIndex(gCP, gTP, *argr, si);
			}
			break;
		}
		*tmp = pp->itsCar;
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYSTRUCTURE:
	ep = argv[0].u.Structure;
	ssz = ep->itsMaxItemIndex;
	bp = (LpBIND)*ep->itsDictionaryArray;// -> source array

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;			// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy next 2 values into a complex pair
	for (si  = 0; si < ssz; ++si, ++bp)
	{	*tmp = bp->Value;
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYDICTIONARY:
	dp = argv[0].u.Dictionary;
	ssz = dp->itsMaxItemIndex;
	bp = (LpBIND)*ep->itsDictionaryArray;// -> source structures

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;			// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy next 2 values into a complex pair
	for (si  = 0; si < ssz; ++si)
	{	*tmp = bp++->Value;
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;


case TYDIRECTORY:
	xp = argv[0].u.Directory;
	ssz = xp->itsMaxItemIndex;
	pbp = (LpPBIND)*xp->itsDirectoryArray;// -> source structures

	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	dsz = (ssz + 1) / 2;			// Number of destination pairs
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, dsz);
	ddp = (LpREAL)*vp->itsCpxArray;
	// Copy next 2 values into a complex pair
	for (si  = 0; si < ssz; ++si)
	{	*tmp = pbp++->Value;
		*ddp++ = asNumIndex(tmp);
	}
	if (dsz * 2 < ssz)			// An odd number of input values
		*ddp = 0.0;
	break;

case TYCPXVECTOR:
	*ret = argv[0];
	break;

default:
	//  Convert anything else to a complex vector with 1 element
	argr->u.CpxVector = vp = TCpxVector_New(gCP, gTP);
	argr->Tag = TYCPXVECTOR;
	TCpxVector_SetMaxIndex(gCP, gTP, *argr, 1);
	ddp = (LpREAL)*vp->itsCpxArray;
	*ddp = asNumIndex(&argv[0]);
	break;
}
FrameExit(*argr)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConvert_ObjToDeclStructure

The FConvert_ObjToDeclStructure procedure converts the argument(s) to a Variable Declaration 
Structure.

#{decl| a Number:x (String:c "Hello")}

#endif

TVAL FConvert_ObjToDeclStructure(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareOBJ(TVector,compilerState);
EndFrame
 
/*  Argument checking */

if ((argc != 1) || (argv[0].Tag != TYPAIR)) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

/*  Initialization */

ret->u.Structure = TStructure_New(gCP,gTP);
ret->Tag = TYSTRUCTURE;
compilerState = NULL;

*ec = FCompile_Bind(gCP, gTP, compilerState, argv[0].u.Pair, ret->u.Structure);
ExitOnError(*ec);

FrameExit(*ret);
}
