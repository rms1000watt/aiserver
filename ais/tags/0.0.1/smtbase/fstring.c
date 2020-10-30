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
FString.c

Implementation of utility functions which are used in string and substring comparison and
manipulation.

PARENT:             None. 

AUTHORS:            Michael F. Korns

#endif

#include    "fstring.h"
#include    "tbytevec.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "fobject.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FString_Init

Initialize a portion of the the SmartLisp function library.  

#endif

TVAL FString_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
TSymbol* aSymbol  = (TSymbol*)_TObject_FrameTobj(&aSymbol);
EndFrame
 
if(gCP->FString_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FString_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"stringFill",(LpFUNC)&FString_Stringfill);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substring",(LpFUNC)&FString_Substring);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringFill",(LpFUNC)&FString_Substringfill);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringLT",(LpFUNC)&FString_SubstringLT);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringLE",(LpFUNC)&FString_SubstringLE);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringEQ",(LpFUNC)&FString_SubstringEQ);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringNE",(LpFUNC)&FString_SubstringNE);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringGT",(LpFUNC)&FString_SubstringGT);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringGE",(LpFUNC)&FString_SubstringGE);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringCiLT",(LpFUNC)&FString_SubstringCILT);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringCiLE",(LpFUNC)&FString_SubstringCILE);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringCiEQ",(LpFUNC)&FString_SubstringCIEQ);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringCiNE",(LpFUNC)&FString_SubstringCINE);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringCiGT",(LpFUNC)&FString_SubstringCIGT);
FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"substringCiGE",(LpFUNC)&FString_SubstringCIGE);

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_Stringfill

The FString_Stringfill cProcedure returns a string {string} with each character 
replaced with a copy of the specified fill character {fill}. 

Several examples follow.
    
    (define  Y  "My love")          =>  "My love"
    (stringFill  Y  #\x)          =>  "xxxxxxx"

#endif

TVAL FString_Stringfill(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
HMChar                  copyHandle;
LpCHAR                  sp;
NUM                     size;
NUM                     indexOf;
CHAR                    fillChar;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  There must be exactly two arguments. */

*ret = argv[0];
if ((argc != 2) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Convert the fill argument into a character. */

*tmp = FObject_CharAnyCnv(gCP,gTP,TYCHAR, argv[1]);

if(isERROR(tmp))
    {
    FrameExit( *tmp);
    }
else
    fillChar = asChar(tmp);

if (asTag(ret) == TYTEXT)
    {
    
    /*  Get a pointer to a TEXT argument. */
    
    sp = &asText(ret)[0];
    size = strlen((char *)(sp));
    for(indexOf = 0;indexOf < size;indexOf++)
        {
        sp[indexOf] = fillChar;
        }
    FrameExit( *ret);
    }
else
if ((asTag(ret) == TYSTRING) || (asTag(ret) == TYBYTEVECTOR))
    {
    if (asTag(ret) == TYBYTEVECTOR)
        {
        copyHandle = (HMChar)FMemory_Copy(gCP, gTP, (HMemory)((TByteVector*)asObject(ret))->itsByteArray);
        size = ((TByteVector*)asObject(ret))->itsMaxItemIndex;
        }
    else
        {
        copyHandle = (HMChar)FMemory_Copy(gCP, gTP, (HMemory)asString(ret)->itsCString);
        size = asString(ret)->itsMaxItemIndex;
        }
    copyHandle = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)copyHandle,(LONG)(size+1));
    
    /*  Get a pointer to a STRING argument. */
    
    sp = &atHMChar(copyHandle,0);
    for(indexOf = 0;indexOf < size;indexOf++)
        {
        sp[indexOf] = fillChar;
        }
        
    sp[size] = '\0';
        
    *ret = gCP->Tval_VOID;
    asObject(ret) = (TObject*)TString_MakeUnique(gCP,gTP,sp);
    asTag(ret) = TYSTRING;
    
    FMemory_Free(gCP, gTP, (HMemory)copyHandle);
    
    FrameExit( *ret);
    }

FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_Substringfill

The substringFill  cProcedure returns a string {string} with 
each character replaced with a copy of the specified fill 
character {fill}. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (substringFill  Y  0  1  #\x)         =>  "xx love"
    (substringFill  Y  3  7  #\x)         =>  "My xxxx"

#endif

TVAL FString_Substringfill(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])

{
LpCHAR                  sp;
LpCHAR                  tp;
NUM                     size;
NUM                     begIndex;
NUM                     endIndex;
CHAR                    fill;
HMChar                  copyHandle = NULL;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */

*ret = argv[0];
if ((argc != 4) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(ret) == TYTEXT)
    {
    sp = &asText(ret)[0];
    size = strlen((char*)sp);
    }
else
if (asTag(ret) == TYSTRING)
    {
    copyHandle = (HMChar)FMemory_Copy(gCP, gTP, (HMemory)asString(ret)->itsCString);
    
    /*  Get a pointer to a STRING argument. */
    
    sp = &atHMChar((HMemory)copyHandle,0);
    size = strlen((char*)sp);
    }
else
if (asTag(ret) == TYBYTEVECTOR)
    {
    copyHandle = (HMChar)FMemory_Copy(gCP, gTP, (HMemory)((TByteVector*)asObject(ret))->itsByteArray);
    
    /*  Get a pointer to a STRING argument. */
    
    sp = &atHMChar((HMemory)copyHandle,0);
    size = ((TByteVector*)asObject(ret))->itsMaxItemIndex;
    }
else
if (asTag(ret) == TYSTRINGSUBSTR)
    {
    tp = TStringSubstringT_GetStringPtr(gCP, gTP, *ret);
    if (tp == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    size = asSubLen(ret);
    copyHandle = (HMChar)FMemory_New(gCP, gTP, (size + 1), TRUE);
    sp = &atHMChar((HMemory)copyHandle,0);
    FMemory_memcpy(gCP, gTP, sp, tp, size);
    sp[size] = 0;
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex = asInt(err);
    }

if (begIndex >= size) begIndex = size - 1;
if (begIndex < 0) begIndex = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex = asInt(err);
    }

if (endIndex < begIndex) endIndex = begIndex;
if (endIndex >= size) endIndex = size - 1;
if (endIndex < 0) endIndex = 0;

/*  Find the fill character. */

if (asTag(&argv[3]) == TYCHAR)
    fill = asChar(&argv[3]);
else
    {
    *err = FObject_CharAnyCnv(gCP,gTP,TYCHAR, argv[3]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        fill = asChar(err);
    }

/*  Fill the substring with the fill character. */

while (begIndex <= endIndex)
    {
    sp[begIndex++] = fill;
    }

if (argv[0].Tag == TYCHAR)
	{
	*ret = argv[0];
	}
else
	{
	ret->u.Object = (TObject*)TString_MakeUnique(gCP,gTP,sp);
	ret->Tag = TYSTRING;
	}

if(copyHandle != NULL )
    {
    FMemory_Free(gCP, gTP, (HMemory)copyHandle);
    }
    
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_Substring

The substring  cProcedure creates and returns a string which is 
a substring of the argument {string}. 

Several examples follow.

    (define  Y  "My love")              =>  "My love"
    (substring  Y  3  6)                =>  "love"
    (substring  Y  0  1)                =>  "My"
    (substring  Y  0  34)               =>  "My love"
    (substring  Y  3 )                  =>  "love"

#endif

TVAL FString_Substring(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])

{
LpCHAR                  sp;
LpCHAR                  op;
NUM                     size;
NUM                     indexOf;
NUM                     begIndex;
NUM                     endIndex;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

*ret = gCP->Tval_VOID;
if ((argc < 1) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp = &asText(&argv[0])[0];
    size = strlen((char*)sp);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSYMBOL)
    {
    sp = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (argc >= 2)
    {
    if (asTag(&argv[1]) == TYREAL)
        begIndex = asReal(&argv[1]);
    else
    if (asTag(&argv[1]) == TYNUM)
        begIndex = asInt(&argv[1]);
    else
        {
        *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
        if(isERROR(err))
            {
            FrameExit( *err);
            }
        else
            begIndex = asInt(err);
        }
    }
else
    begIndex = 0;

if (begIndex >= size) begIndex = size - 1;
if (begIndex < 0) begIndex = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (argc >= 3)
    {
    if (asTag(&argv[2]) == TYREAL)
        endIndex = asReal(&argv[2]);
    else
    if (asTag(&argv[2]) == TYNUM)
        endIndex = asInt(&argv[2]);
    else
        {
        *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
        if(isERROR(err))
            {
            FrameExit( *err);
            }
        else
            endIndex = asInt(err);
        }
    }
else
    endIndex = size - 1;

if (endIndex < begIndex) endIndex = begIndex;
if (endIndex >= size) endIndex = size - 1;
if (endIndex < 0) endIndex = 0;

/*  Create a substring as the result. */
/*  Note:   If the substring is small enough, we can */
/*          create a TYTEXT. */

if ((endIndex - begIndex + 1) < (MAXTVALTEXTLEN - 1))
    {
    asTag(ret) = TYTEXT;
    op = &asText(ret)[0];
    indexOf = 0;
    while (begIndex <= endIndex)
        {
        op[indexOf++] = sp[begIndex++];
        }
    op[indexOf++] = 0;
    }
else
    {
    *ret = gCP->Tval_VOID;
    asObject(ret) = (TObject*)TString_SubString_MakeUnique(gCP,gTP,sp,begIndex,1 + endIndex - begIndex);
    asTag(ret) = TYSTRING;
    }

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringLT

These cProcedures perform a case-sensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringEQ  Y  0  1  X  5  6)         =>  true
    (substringLT  Y  3  X  0  3)            =>  true
    (substringLT  Y  3  3  X  3  3)         =>  true

#endif

TVAL FString_SubstringLT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])

{
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (c1 < c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    }

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringLE

These cProcedures perform a case-sensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringEQ  Y  0  1  X  5  6)         =>  true
    (substringLE  Y  3  X  0  3)            =>  true
    (substringLE  Y  3  3  X  3  3)         =>  true

#endif

TVAL FString_SubstringLE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])

{
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame

/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (c1 < c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringEQ

These cProcedures perform a case-sensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringEQ  Y  0  1  X  5  6)         =>  true
    (substringEQ  Y  3  X  0  3)            =>  true
    (substringEQ  Y  3  3  X  3  3)         =>  true

#endif

TVAL FString_SubstringEQ(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])

{
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM,argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. \(.*\) */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (c1 < c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringNE

These cProcedures perform a case-sensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringEQ  Y  0  1  X  5  6)         =>  true
    (substringNE  Y  3  X  0  3)            =>  true
    (substringNE  Y  3  3  X  3  3)         =>  true

#endif

TVAL FString_SubstringNE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])

{
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (c1 < c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringGT

These cProcedures perform a case-sensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringEQ  Y  0  1  X  5  6)         =>  true
    (substringGT  Y  3  X  0  3)            =>  true
    (substringGT  Y  3  3  X  3  3)         =>  true

#endif

TVAL FString_SubstringGT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])

{
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2; 
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame

/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM,argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (c1 < c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringGE

These cProcedures perform a case-sensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringEQ  Y  0  1  X  5  6)         =>  true
    (substringGE  Y  3  X  0  3)            =>  true
    (substringGE  Y  3  3  X  3  3)         =>  true

#endif

TVAL FString_SubstringGE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (c1 < c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringCILT

These cProcedures perform a case-insensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringCiEQ  Y  0  1  X  5  6)      =>  true
    (substringCiLT  Y  3  X  0  3)         =>  true
    (substringCiLT  Y  3  3  X  3  3)      =>  true

#endif

TVAL FString_SubstringCILT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (ISALPHA((NUM)c1) && ISALPHA((NUM)c2))
        {
        c1 |= 0x20; c2 |= 0x20;     
        }
        
    if (c1 < c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringCILE

These cProcedures perform a case-insensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringCiEQ  Y  0  1  X  5  6)      =>  true
    (substringCiLE  Y  3  X  0  3)         =>  true
    (substringCiLE  Y  3  3  X  3  3)      =>  true

#endif

TVAL FString_SubstringCILE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (ISALPHA((NUM)c1) && ISALPHA((NUM)c2))
        {
        c1 |= 0x20; c2 |= 0x20;     
        }
        
    if (c1 < c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringCIEQ

These cProcedures perform a case-insensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringCiEQ  Y  0  1  X  5  6)      =>  true
    (substringCiEQ  Y  3  X  0  3)         =>  true
    (substringCiEQ  Y  3  3  X  3  3)      =>  true

#endif

TVAL FString_SubstringCIEQ(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (ISALPHA((NUM)c1) && ISALPHA((NUM)c2))
        {
        c1 |= 0x20; c2 |= 0x20;     
        }
        
    if (c1 < c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringCINE

These cProcedures perform a case-insensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringCiEQ  Y  0  1  X  5  6)      =>  true
    (substringCiNE  Y  3  X  0  3)         =>  true
    (substringCiNE  Y  3  3  X  3  3)      =>  true

#endif

TVAL FString_SubstringCINE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (ISALPHA((NUM)c1) && ISALPHA((NUM)c2))
        {
        c1 |= 0x20; c2 |= 0x20;     
        }
        
    if (c1 < c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringCIGT

These cProcedures perform a case-insensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringCiEQ  Y  0  1  X  5  6)      =>  true
    (substringCiGT  Y  3  X  0  3)         =>  true
    (substringCiGT  Y  3  3  X  3  3)      =>  true

#endif

TVAL FString_SubstringCIGT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (ISALPHA((NUM)c1) && ISALPHA((NUM)c2))
        {
        c1 |= 0x20; c2 |= 0x20;     
        }
        
    if (c1 < c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FString_SubstringCIGE

These cProcedures perform a case-insensitive comparison between the 
two substrings {string1  start1  end1} and {string2  start2  end2}. 
If the comparison is true, true is returned; otherwise, false is 
returned. 

Several examples follow.

    (define  Y  "My love")                  =>  "My love"
    (define  X  "Love My Mom")              =>  "My love"
    (substringCiEQ  Y  0  1  X  5  6)      =>  true
    (substringCiGE  Y  3  X  0  3)         =>  true
    (substringCiGE  Y  3  3  X  3  3)      =>  true

#endif

TVAL FString_SubstringCIGE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
NUM                     size;
NUM                     begIndex1;
NUM                     endIndex1;
NUM                     begIndex2;
NUM                     endIndex2;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(err);
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 6) || (isNullTval(&argv[0]))) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT)
    {
    sp1 = &asText(&argv[0])[0];
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    sp1 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp1 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp1 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[0]));
    size = strlen((char*)sp1);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp1 = (char*)*((TByteVector*)asObject(&argv[0]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[0]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[1]) == TYREAL)
    begIndex1 = asReal(&argv[1]);
else
if (asTag(&argv[1]) == TYNUM)
    begIndex1 = asInt(&argv[1]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex1 = asInt(err);
    }

if (begIndex1 >= size) begIndex1 = size - 1;
if (begIndex1 < 0) begIndex1 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[2]) == TYREAL)
    endIndex1 = asReal(&argv[2]);
else
if (asTag(&argv[2]) == TYNUM)
    endIndex1 = asInt(&argv[2]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex1 = asInt(err);
    }

if (endIndex1 < begIndex1) endIndex1 = begIndex1;
if (endIndex1 >= size) endIndex1 = size - 1;
if (endIndex1 < 0) endIndex1 = 0;

/*  The fourth argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string, */
/*          and the length of the text string. */

if (isNullTval(&argv[3])) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[3]) == TYTEXT)
    {
    sp2 = &asText(&argv[3])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[3]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[3]);
    }
else
if (asTag(&argv[3]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[3]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[3]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[3]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[3]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Find the beginning substring index. */
/*  Note:   If omitted, it defaults to zero.     */

if (asTag(&argv[4]) == TYREAL)
    begIndex2 = asReal(&argv[4]);
else
if (asTag(&argv[4]) == TYNUM)
    begIndex2 = asInt(&argv[4]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[4]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        begIndex2 = asInt(err);
    }

if (begIndex2 >= size) begIndex2 = size - 1;
if (begIndex2 < 0) begIndex2 = 0;

/*  Find the ending substring index. */
/*  Note:   If omitted, it defaults to size-1.   */

if (asTag(&argv[5]) == TYREAL)
    endIndex2 = asReal(&argv[5]);
else
if (asTag(&argv[5]) == TYNUM)
    endIndex2 = asInt(&argv[5]);
else
    {
    *err = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[5]);
    if(isERROR(err))
        {
        FrameExit( *err);
        }
    else
        endIndex2 = asInt(err);
    }

if (endIndex2 < begIndex2) endIndex2 = begIndex2;
if (endIndex2 >= size) endIndex2 = size - 1;
if (endIndex2 < 0) endIndex2 = 0;

/*  Compare the two substrings and return the result. */

while(TRUE)
    {
    if (begIndex1 > endIndex1)
        c1 = 0;
    else
        c1 = sp1[begIndex1++];
        
    if (begIndex2 > endIndex2)
        c2 = 0;
    else
        c2 = sp2[begIndex2++];
        
    if (ISALPHA((NUM)c1) && ISALPHA((NUM)c2))
        {
        c1 |= 0x20; c2 |= 0x20;     
        }
        
    if (c1 < c2)
        {
        asBool(ret) = FALSE;
        FrameExit( *ret);
        }
    else
    if (c1 > c2)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    else
    if (c1 == 0)
        {
        asBool(ret) = TRUE;
        FrameExit( *ret);
        }
    }

FrameExit( gCP->TObject_TRUE);
}
