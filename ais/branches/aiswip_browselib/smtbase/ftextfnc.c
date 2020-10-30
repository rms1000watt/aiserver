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

#define _C_FTEXTFNC
#define _SMARTBASE

#if 0
FTextFnc.c

This source file contains some of the cProcedures which implement the math functions
supported by the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "ftextfnc.h"
#include    "fconio.h"
#include    "fconvert.h"
#include    "flisp.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tintvec.h"
#include    "tobjvec.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "fconvert.h"
#include    "fmath1.h"
#include    "ffloat.h"
#include    "fmake.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_Init

Initialize the text portion of the SmartLisp function library.  

#endif

TVAL FTextFnc_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TSymbol,aSymbol);
EndFrame
 
if(gCP->FTextfnc_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FTextfnc_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"char",(LpFUNC)&FTextFnc_char);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"clean",(LpFUNC)&FTextFnc_clean);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"code",(LpFUNC)&FTextFnc_code);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"find",(LpFUNC)&FTextFnc_find);
ExitOnError(*ret);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"search"),  aSymbol->itsGlobalValue);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"left",(LpFUNC)&FTextFnc_left);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"right",(LpFUNC)&FTextFnc_right);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fixed",(LpFUNC)&FTextFnc_fixed);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"dollar",(LpFUNC)&FTextFnc_dollar);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"text",(LpFUNC)&FTextFnc_text);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"mid",(LpFUNC)&FTextFnc_mid);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"replace",(LpFUNC)&FTextFnc_replace);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"rept",(LpFUNC)&FTextFnc_rept);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"substitute",(LpFUNC)&FTextFnc_substitute);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"trim",(LpFUNC)&FTextFnc_trim);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"stringToVector",(LpFUNC)&FTextFnc_StringToVector);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"stringToBVector",(LpFUNC)&FTextFnc_StringToBVector);
ExitOnError(*ret);

FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"stringCiLT",(LpFUNC)&FTextFnc_StringCILT);
ExitOnError(*ret);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"stringCiLE",(LpFUNC)&FTextFnc_StringCILE);
ExitOnError(*ret);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"stringCiEQ",(LpFUNC)&FTextFnc_StringCIEQ);
ExitOnError(*ret);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"stringCiNE",(LpFUNC)&FTextFnc_StringCINE);
ExitOnError(*ret);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"stringCiGT",(LpFUNC)&FTextFnc_StringCIGT);
ExitOnError(*ret);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"stringCiGE",(LpFUNC)&FTextFnc_StringCIGE);
ExitOnError(*ret);

FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"macroReplace",(LpFUNC)&FTextFnc_MacroReplace);
ExitOnError(*ret);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_trim

Remove all leading, trailing, extra spaces between words and return string.  All words are 
separated with only one space.

Note:   The trim function expects one argument.

For example 

    (trim "this is    a test  ") ==> "this is a test"

#endif

TVAL FTextFnc_trim(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
LpCHAR          stringPtr;
NUM             size;
LpCHAR          newPtr;
CHAR            c;
NUM             j;
NUM             i;
CHAR			cbuf[2];
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TString,ss);
EndFrame

/*  There must be one argument. */

if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

*ret = gCP->Tval_VOID;

/*  Get pointer to and size of the source string. */

if (argv[0].Tag == TYCHAR)
    {
	cbuf[0] = argv[0].u.Char;
	cbuf[1] = 0;
	stringPtr = &cbuf[0];
    size = 1;
    }
else
if (argv[0].Tag == TYTEXT)
    {
    stringPtr = &argv[0].u.Text[0];
    size = strlen(stringPtr);
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    stringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (stringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (argv[0].Tag == TYSTRING)
    {
    stringPtr = (char*)*((TString*)argv[0].u.Object)->itsCString;
    size = strlen(stringPtr);
    }
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    stringPtr = (char*)*((TSymbol*)argv[0].u.Object)->itsCString;
    size = strlen(stringPtr);
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    stringPtr = (char*)*((TByteVector*)argv[0].u.Object)->itsByteArray;
    size = ByteVector(argv[0])->itsMaxItemIndex; 
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Make a copy of the source string to trim as the result. */

ss = TString_SubString_MakeUnique(gCP, gTP, stringPtr, 0, size);
newPtr = (char*)*ss->itsCString;

/*  Remove blanks from the source as we copy to the result. */

newPtr[0] = 0;

/*  Drop any leading blanks from the source string. */

for (i = 0; stringPtr[i] && ISSPACE((NUM)stringPtr[i]) ; i++) {}

/*  Copy to the result buffer if I am not a space and I do not follow a space. */

for (j = 0; ((c = stringPtr[i]) != 0) && (i < size); i++)
    {
    if (!ISSPACE((NUM)c) || !ISSPACE((NUM)stringPtr[i-1]))
        newPtr[j++] = c;
    }

/*  Trim trailing spaces. */

if ((j != 0) && ISSPACE((NUM)newPtr[j-1]))
    {
    newPtr[j-1] = 0;
    j--;
    }
else
    newPtr[j] = 0;

/*  Produce a new result from the trimmed string. */

*ret = TObject_CnvFromText(gCP, gTP, newPtr);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_substitute

Substitues new_text for old_text in a text string. Use SUBSTITUE when you 
want to replace specific text in a text string; use REPLACE when you want 
to replace any text that occurs in a specific location in a text string.

(substitute text,old_text,new_text {,instance_num})

(NOTE: if instance_num is omitted all occurences of old_text is replaced)

For example:

    (substitute "the dog barked like a dog" "dog" "cat" 1) ==> "the cat barked like a dog"

#endif

TVAL FTextFnc_substitute(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM             count;
StartFrame
DeclareTVAL(tlength);
DeclareTVAL(klength);
DeclareTVAL(loc);
DeclareTVAL(start);
DeclareTVAL(append);
DeclareTVAL(find);
DeclareTVAL(mid);
DeclareTVAL(target);
DeclareTVAL(key);
DeclareTVAL(replacement);
DeclareTVAL(tmp);
DeclareTVAL(ret);
EndFrame


if (argc < 3)
	{
	*ret = TERROR("!substitute: Requires a minimum of 3 arguments!");
	FrameExit(*ret);
	}

if (argc > 4)
	{
	*ret = TERROR("!substitute: Received too many arguments!");
	FrameExit(*ret);
	}

if (argv[0].Tag != TYTEXT && argv[0].Tag != TYSTRINGSUBSTR &&
    argv[0].Tag != TYSTRING && argv[0].Tag != TYSYMBOL && argv[0].Tag != TYBYTEVECTOR)
	{
	*ret = TERROR("!substitute: Target must be a String, Substring, Text, Symbol, or Byte Vector!");
	FrameExit(*ret);
	}

if (argv[1].Tag != TYTEXT && argv[1].Tag != TYSTRINGSUBSTR && argv[1].Tag != TYSTRING &&
    argv[1].Tag != TYSYMBOL && argv[1].Tag != TYCHAR && argv[1].Tag != TYBYTEVECTOR)
	{
	*ret = TERROR("!substitute: Key must be a String, Substring, Text, Symbol, or Byte Vector!");
	FrameExit(*ret);
	}

if (argv[2].Tag != TYTEXT && argv[2].Tag != TYSTRINGSUBSTR && argv[2].Tag != TYSTRING &&
    argv[2].Tag != TYSYMBOL && argv[2].Tag != TYCHAR && argv[2].Tag != TYBYTEVECTOR)
	{
	*ret = TERROR("!substitute: Replacement must be a String, Substring, Text, Symbol, or Byte Vector!");
	FrameExit(*ret);
	}

if (argc == 4 && argv[3].Tag != TYNUM)	   
	{
	*ret = TERROR("!substitute: Optional repeat argument must be numeric!");
	FrameExit(*ret);
	}


/* Miscellaneous Initializations */

*target         = argv[0];
*key            = argv[1];
*replacement    = argv[2];
*append         = TGVALUE("append");
*find           = TGVALUE("find");
*mid            = TGVALUE("mid");



/*  Get the lengths of the key and the target */

klength->Tag = TYNUM;
switch (key->Tag)
	{
	case TYCHAR:
		klength->u.Int = 1;
		break;

	case TYTEXT:
		klength->u.Int = strlen(key->u.Text);
		break;

    case TYSTRINGSUBSTR:
        klength->u.Int = asSubLen(key);
        break;

	case TYSTRING:
	case TYSYMBOL:
    case TYQUOTEDSYMBOL:
		klength->u.Int = FSmartbase_StringLen(gCP,gTP,key);
		break;

  	case TYBYTEVECTOR:
		klength->u.Int = FSmartbase_StringLen(gCP,gTP,key);
		break;

	default:
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		break;
	}
if (klength->u.Int <= 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

tlength->Tag = TYNUM;
switch (target->Tag)
	{
	case TYCHAR:
		tlength->u.Int = 1;
		break;

	case TYTEXT:
		tlength->u.Int = strlen(target->u.Text);
		break;
	
    case TYSTRINGSUBSTR:
        tlength->u.Int = asSubLen(target);
        break;

	case TYSTRING:
	case TYSYMBOL:
    case TYQUOTEDSYMBOL:
		tlength->u.Int = FSmartbase_StringLen(gCP,gTP,target);
		break;
  	case TYBYTEVECTOR:
		tlength->u.Int = FSmartbase_StringLen(gCP,gTP,target);
		break;

	default:
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		break;
	}

/*  Get the repetition factor. */

if (argc == 4)
    {
    *ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[3]);
    ExitOnError(*ret);
    count = ret->u.Int;
    }
else
    count = -1;

/*  Replace the key in the target with the replacement (count) times. */

*ret = TSTRING("");
*start = TINT(0);
*loc = TINT(0);
while (loc->Tag == TYNUM)
    {
    /*  If there are no more substitutions append the remainder of the target.   */
    if (count == 0)
        goto AppendRemainder;
        
    /*  Locate the next key instance within the target.  */
    *loc = FSmartbase_Eval(gCP,gTP,*find,3,*key,*target,*start);
    ExitOnError(*loc);

    /*  Replace the key instance with the replacement within the target.     */
    if (loc->Tag == TYNUM)
        {
        *tmp = FSmartbase_Eval(gCP,gTP,*mid,3,*target,*start,TINT(loc->u.Int - start->u.Int));
        ExitOnError(*tmp);
        *ret = FSmartbase_Eval(gCP,gTP,*append,2,*ret,*tmp);
        ExitOnError(*ret);
        *ret = FSmartbase_Eval(gCP,gTP,*append,2,*ret,*replacement);
        ExitOnError(*ret);
        start->u.Int = loc->u.Int + klength->u.Int;
        --count;
        }
    else
        {
        AppendRemainder:
        *tmp = FSmartbase_Eval(gCP,gTP,*mid,3,*target,*start,TINT(tlength->u.Int - start->u.Int));
        ExitOnError(*tmp);
        *ret = FSmartbase_Eval(gCP,gTP,*append,2,*ret,*tmp);
        ExitOnError(*ret);
        goto Last;
        }
    }   
    
/*  Return the result. */

Last:
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_rept

Returns a string value where the given string is repeated N number of times.

Note:   This function accepts exactly two arguments.

Usage:  (rept text_string, number_of_repeats)
EX:     (rept "*",4) = "****"

#endif

TVAL FTextFnc_rept(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM             n;
NUM             i;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(append);
EndFrame

/*  There must be two arguments. */


if (argc < 2)
	{
	*ret = TERROR("!rept: Requires a minimum of 3 arguments!");
	FrameExit(*ret);
	}

if (argc > 2) 
	{
	*ret = TERROR("!rept: Received too many arguments!");
	FrameExit(*ret);
	}

if (argv[0].Tag != TYTEXT && argv[0].Tag != TYSTRINGSUBSTR && argv[0].Tag != TYSTRING &&
    argv[0].Tag != TYSYMBOL && argv[0].Tag != TYBYTEVECTOR)
	{
	*ret = TERROR("!rept: Target must be a String, Substring, Text, or Byte Vector!");
	FrameExit(*ret);
	}

if (argv[1].Tag != TYNUM)	   
	{
	*ret = TERROR("!rept: Repeat argument must be numeric!");
	FrameExit(*ret);
	}

*ret = gCP->Tval_VOID;

/*  Get the repetition factor. */

*ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
ExitOnError(*ret);
n = ret->u.Int;

/*  Append the source object to itself n times. */

ret->Tag = TYTEXT;
ret->u.Text[0] = 0;
*append = TGVALUE("append");
for (i = 0; i < n; ++i)
    {
    *ret = FSmartbase_Eval(gCP,gTP,*append,2,*ret,argv[0]);
    ExitOnError(*ret);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_replace

Replaces a subset in a string with another set of characters and returns the new string.

    (replace Source_string, Start_position, Number_chars, Replacement_string)
    
    Source_string is the original string.
    Start_position specifies the position of the first character to replace.
    Number_chars is the length of the substring being replaced.
        If Number_chars is zero, the newsubstring is inserted before start_position.
        No overwrite of the original string occurs.
    Replacement_string is the string that will overwrite the old substring.

    ex: (replace "12-345",3,3,"789") = "12-789"

#endif

TVAL FTextFnc_replace(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(tlength);
DeclareTVAL(rlength);
DeclareTVAL(start);
DeclareTVAL(left);
DeclareTVAL(mid);
DeclareTVAL(target);
DeclareTVAL(length);
DeclareTVAL(append);
DeclareTVAL(replacement);
DeclareTVAL(tmp);
DeclareTVAL(ret);
EndFrame



/* Check if arguments are valid. */

if (argc < 4) 
	{
	*ret = TERROR("!replace: Missing Arguments!");
	FrameExit(*ret);
	}

if (argc > 4) 
	{
	*ret = TERROR("!replace: Received too many arguments!");
	FrameExit(*ret);
	}

if (argv[0].Tag != TYTEXT && argv[0].Tag != TYSTRING && argv[0].Tag != TYSTRINGSUBSTR &&
    argv[0].Tag != TYSYMBOL && argv[0].Tag != TYBYTEVECTOR)
	{
	*ret = TERROR("!replace: Target must be a String, Substring, Text, or Byte Vector!");
	FrameExit(*ret);
	}

if (argv[1].Tag != TYNUM)
	{
	*ret = TERROR("!replace: Start Position must be a number!");
	FrameExit(*ret);
	}

if (argv[2].Tag != TYNUM)
	{
	*ret = TERROR("!replace: Length must be a number!");
	FrameExit(*ret);
	}

if (argv[3].Tag != TYTEXT && argv[3].Tag != TYSTRINGSUBSTR && argv[3].Tag != TYSTRING &&
    argv[3].Tag != TYSYMBOL && argv[3].Tag != TYCHAR && argv[3].Tag != TYBYTEVECTOR)
	{
	*ret = TERROR("!replace: Replacement text must be a String, Substring, Text, or Byte Vector!");
	FrameExit(*ret);
	}


/* Miscellaneous Initializations */

*target         = argv[0];
*replacement    = argv[3];
*append         = TGVALUE("append");
*mid            = TGVALUE("mid");
*length         = TGVALUE("length");
*left           = TGVALUE("left");

/*  Find the length of the target. */

*tlength = FSmartbase_Eval(gCP,gTP,*length,1,*target);
ExitOnError(*tlength);

/*  Check the start and replacement length factors. */

*start = FObject_IntAnyCnv(gCP,gTP,TYNUM,argv[1]);
ExitOnError(*ret);
*rlength = FObject_IntAnyCnv(gCP,gTP,TYNUM,argv[2]);
ExitOnError(*ret);

/*  Replace the key in the target with the replacement item. */

*ret = TSTRING("");
*ret = FSmartbase_Eval(gCP,gTP,*left,2,*target,*start);
ExitOnError(*ret);
*ret = FSmartbase_Eval(gCP,gTP,*append,2,*ret,*replacement);
ExitOnError(*ret);

start->u.Int += rlength->u.Int;
*tmp = FSmartbase_Eval(gCP,gTP,*mid,3,*target,*start,TINT(tlength->u.Int - start->u.Int));
ExitOnError(*tmp);
*ret = FSmartbase_Eval(gCP,gTP,*append,2,*ret,*tmp);
ExitOnError(*ret);
    
/*  Return the result. */

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_mid

Return a subset of a text string.  Start_position specifies the position of the first
character to extract.  Number_chars defines the number of characters to extract.

(mid "123456789" 3 6) ==> 

#endif

TVAL FTextFnc_mid(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
CHAR                    tmp;
LpCHAR                  sp;
LpCHAR                  bp;
NUM                     lengthOf;
NUM                     size;
NUM                     start;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  There must be exactly three arguments. */

if (argc != 3) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Get pointer to the source string. */

if (argv[0].Tag == TYTEXT)
    {
    sp = &argv[0].u.Text[0];
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    sp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    goto StartConvert;
    }
else
if (argv[0].Tag == TYSTRING)
    {
    sp = (char*)*((TString*)argv[0].u.Object)->itsCString;
    }
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    sp = (char*)*((TSymbol*)argv[0].u.Object)->itsCString;
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    sp = (char*)*((TByteVector*)argv[0].u.Object)->itsByteArray;
    size = ByteVector(argv[0])->itsMaxItemIndex;
    goto StartConvert; 
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

size = strlen((char*)sp);

/*  Convert the starting position to NUM. */

StartConvert:
*ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
_TObject_ErrorChk(*ret);
start = asInt(ret);

if (start < 0)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Read third argument-- the desired size of string. */

*ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
_TObject_ErrorChk(*ret);
lengthOf = asInt(ret);
    
if (lengthOf < 0)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  make sure starting position doesn't exceed the actual length of string. */

if (start > size) 
    start = size;

/*  truncate lengthOf to its max legal value */

lengthOf = min(lengthOf, size - start);

bp = sp + start;
tmp = bp[lengthOf];
bp[lengthOf] = 0;

/*  Return the resulting string. */

*ret = TObject_CnvFromText(gCP, gTP, bp);
bp[lengthOf] = tmp;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_dollar

Converts a number to text using currency format, with the decimals rounded 
to the specified place. The format used is $#,##0.00; ($#,##0.00).

Usage:  (dollar  number, {round_decimals})
EX:     (dollar 1234.3,2) ==> "$1,234.30"

#endif

TVAL FTextFnc_dollar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 places;
NUM                 maxLen;             
NUM                 cn;             
NUM                 fmtcn;  
CHAR                xlFmt[128];
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
DeclareTVALArray(prmv,2);

EndFrame

if ((argc < 1) || (argc > 2)) 
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto BadCleanUp;
    }
    
if(argc == 2)
    {
    /*  Get the second arg as an int */
    
    *ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(ret))
        goto BadCleanUp;
        
    places = asInt(ret);
    
    strcpy((char*)xlFmt, "$(#.");
    
    fmtcn = strlen((char*)xlFmt);
    maxLen  = sizeof(xlFmt) - fmtcn - 1;
    maxLen = min(places, maxLen);
    
    for(cn = 0; cn < maxLen; cn++)
        {
        xlFmt[fmtcn++] = '#';
        }
    xlFmt[fmtcn++] = ')';
    xlFmt[fmtcn] = 0;
    }
else
    sprintf((char*)xlFmt, "$(#.#0)");
    
prmv[0] = argv[0];

prmv[1] = gCP->Tval_VOID;
asObject(&prmv[1]) = (TObject*)TString_MakeUnique(gCP,gTP,xlFmt);
asTag(&prmv[1]) = TYSTRING;

*ret = FTextFnc_text(gCP,gTP,2, &prmv[0]);
if(isERROR(ret))
    goto BadCleanUp;
else
    *argr = *ret;
    
FrameExit( *argr);

BadCleanUp:

if(!isERROR(ret))
    *ret = gCP->TObject_ERROR_INVALID;
    
FrameExit( *ret );
}
/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_fixed

Rounds a number to a specified number of digits, formats it with commas, and returns
the value as text.  If the decimal accuracy is not specified, the default is 2.

Usage:  (fixed  number, {round_decimals})
EX:     (fixed 1234.3 2) ==> "1,234.30"

#endif

TVAL FTextFnc_fixed(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 places;
NUM                 maxLen;             
NUM                 cn;             
NUM                 fmtcn;  
CHAR                xlFmt[128];
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
DeclareTVALArray(prmv,2);

EndFrame

if ((argc < 1) || (argc > 2)) 
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto BadCleanUp;
    }
    
if(argc == 2)
    {
    /*  Get the second arg as an int */
    
    *ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    if(isERROR(ret))
        goto BadCleanUp;
        
    places = asInt(ret);
    
    strcpy((char*)xlFmt, "#.");
    
    fmtcn = strlen((char*)xlFmt);
    maxLen  = sizeof(xlFmt) - fmtcn - 1;
    maxLen = min(places, maxLen);
    
    for(cn = 0; cn < maxLen; cn++)
        {
        xlFmt[fmtcn++] = '#';
        }
    xlFmt[fmtcn] = 0;
    }
else
    sprintf((char*)xlFmt, "#.#0");
    
prmv[0] = argv[0];

prmv[1] = gCP->Tval_VOID;
asObject(&prmv[1]) = (TObject*)TString_MakeUnique(gCP,gTP,xlFmt);
asTag(&prmv[1]) = TYSTRING;

*ret = FTextFnc_text(gCP,gTP,2, &prmv[0]);
if(isERROR(ret))
    goto BadCleanUp;
else
    *argr = *ret;
    
FrameExit( *argr);

BadCleanUp:

if(!isERROR(ret))
    *ret = gCP->TObject_ERROR_INVALID;
    
FrameExit( *ret );
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_text

This procedure is responsible for formatting numbers as text.

Format rules

    $ as a prefix
    % as a postfix
    . indicates decimal precision is specified
    () indicates that negative numbers are printed in parenthesis.
    , indicates that number before decimal is to be printed with commas.


#endif

TVAL FTextFnc_text(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
CHAR        xlFmt[128];
CHAR        prefix[8];
CHAR        postfix[8];
CHAR        preDecimal[512];
CHAR        postDecimal[512];
LpCHAR      tmpP;
REAL        tmpReal;
REAL        floorValue;
REAL        sign;
REAL        realValue;
NUM         places;
NUM         cn;
NUM         outcn;
NUM         decimalcn;
CHAR        buff[512];
StartFrame
DeclareTVAL(ret);
EndFrame

if(argc != 2)
    goto BadCleanUp;

prefix[0] = postfix[0] = preDecimal[0] = postDecimal[0] = 0;

/*  Get the fmt string */

*ret = TObject_CnvToText(gCP, gTP, xlFmt, sizeof(xlFmt) - 1, argv[1]);
if(isERROR(ret))
    goto BadCleanUp;

/*  Extract information on the data argument. */

*ret = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
if(isERROR(ret))
    goto BadCleanUp;

realValue = asReal(ret);
if( realValue < 0.0)
    {
    sign = -1;
    realValue *= sign;
    }
else
    sign = 1;

/*  Extract information on the format to be employed. */

cn = 0;
if(sign < 0.0)
    {
    if(strchr((char*)xlFmt, '(' ) )
        prefix[cn++] = '(';
    else
        prefix[cn++] = '-';
    }
if(strchr((char*)xlFmt, '$' ) )
    prefix[cn++] = '$';
prefix[cn] = 0;

cn = 0;
if(strchr((char*)xlFmt, '%' ) )
    {
    postfix[cn++] = '%';
    realValue *= 100.0;
    }
if(sign < 0.0)
    {
    if(strchr((char*)xlFmt, ')' ) )
        postfix[cn++] = ')';
    }
postfix[cn] = 0;

floorValue = floor(realValue);

if ((tmpP = (LpCHAR)strchr((char*)xlFmt, '.' )) != NULL)
    {
    places = 0;
    tmpP++;
    while(*tmpP && (*tmpP == '#' || *tmpP == '0'))
        {
        tmpP++;
        places++;
        }
    
    /*  Now round to the correct number of decimal places. */
    
    tmpReal = realValue - floorValue;
    
    sprintf((char*)&buff[0], "%.20f", tmpReal);
    strncpy((char*)&postDecimal[0], (char*)&buff[1], places+1);
    postDecimal[places+1] = 0;
    }
else
    {
    postDecimal[0] = 0;
    }
    
sprintf((char*)preDecimal, "%.0f", floorValue);

/*  Now add all the pieces together to make up the result. */

strcpy((char*)buff, (char*)prefix);

if(strchr((char*)xlFmt, ',' ) != NULL)
    {
    outcn = strlen((char*)buff);
    decimalcn = 0;
    for(cn = strlen((char*)preDecimal); cn > 0; --cn)
        {
        buff[outcn++] = preDecimal[decimalcn++];
        if((cn - 1) && !((cn - 1) % 3))
            {
            buff[outcn++] = ',';
            }
        }
    buff[outcn] = 0;
    }
else
    {
    strcat((char*)buff, (char*)preDecimal);
    }

strcat((char*)buff, (char*)postDecimal);
strcat((char*)buff, (char*)postfix);

*ret = FSmartbase_CnvFromText(gCP,gTP,buff);
FrameExit(*ret);

BadCleanUp:

if(!isERROR(ret))
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_right

Return the N Rightmost characters in a string.

Usage:  (right text_string, number_chars)
ex:     (right "12345",2) ==>"45"

#endif

TVAL FTextFnc_right(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
CHAR                    tmp;
LpCHAR                  sp;
LpCHAR                  bp;
NUM                     lengthOf;
NUM                     size;
NUM                     start;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  There must be exactly two arguments. */

if (argc != 2) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Get pointer to the source string. */

if (argv[0].Tag == TYTEXT)
    {
    sp = &argv[0].u.Text[0];
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    sp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    goto StartConvert;
    }
else
if (argv[0].Tag == TYSTRING)
    {
    sp = (char*)*((TString*)argv[0].u.Object)->itsCString;
    }
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    sp = (char*)*((TSymbol*)argv[0].u.Object)->itsCString;
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    sp = (char*)*((TByteVector*)argv[0].u.Object)->itsByteArray;
    size = ByteVector(argv[0])->itsMaxItemIndex;
    goto StartConvert; 
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

size = strlen((char*)sp);

/*  Read second argument-- the desired size of string. */

StartConvert:
*ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
_TObject_ErrorChk(*ret);
lengthOf = asInt(ret);
    
if (lengthOf < 0)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Compute the starting position. */

if (lengthOf > size) 
    lengthOf = size;
start = size - lengthOf;

/*  Save the temporary character. */

bp = sp + start;
tmp = bp[lengthOf];
bp[lengthOf] = 0;

/*  Return the resulting string. */

*ret = TObject_CnvFromText(gCP, gTP, bp);
bp[lengthOf] = tmp;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_left

Return the N leftmost characters in a string.

#endif

TVAL FTextFnc_left(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
CHAR                    tmp;
LpCHAR                  sp;
LpCHAR                  bp;
NUM                     lengthOf;
NUM                     size;
NUM                     start;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  There must be exactly two arguments. */

if (argc != 2) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Get pointer to the source string. */

if (argv[0].Tag == TYTEXT)
    {
    sp = &argv[0].u.Text[0];
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    sp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (sp == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    goto StartConvert;
    }
else
if (argv[0].Tag == TYSTRING)
    {
    sp = (char*)*((TString*)argv[0].u.Object)->itsCString;
    }
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    sp = (char*)*((TSymbol*)argv[0].u.Object)->itsCString;
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    sp = (char*)*((TByteVector*)argv[0].u.Object)->itsByteArray;
    size = ByteVector(argv[0])->itsMaxItemIndex;
    goto StartConvert; 
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

size = strlen((char*)sp);

/*  Convert the starting position to NUM. */

StartConvert:
start = 0;

/*  Read second argument-- the desired size of string. */

*ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
_TObject_ErrorChk(*ret);
lengthOf = asInt(ret);
    
if (lengthOf < 0)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  make sure starting position doesn't exceed the actual length of string. */

if (start > size) 
    start = size;

/*  truncate lengthOf to its max legal value */

lengthOf = min(lengthOf, size - start);

bp = sp + start;
tmp = bp[lengthOf];
bp[lengthOf] = 0;

/*  Return the resulting string. */

*ret = TObject_CnvFromText(gCP, gTP, bp);
bp[lengthOf] = tmp;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_find

If the first argument is a string, return the index position of the first occurance 
of a match string within a target string. There may be an optional starting point.

	(setq fpos (find "find me" "Here is the find me substring" startpos))


If the first argument is a Vector of strings, return the index position of the first occurance 
of a match string within the target string. The cdr of the vector of strings contains the index 
of the match string which was found. A maximum of 256 match strings may be con tained in the 
Vector of match strings. The match strings must be entered into the vector of strings in the 
search order required by the user. There may be an optional starting point.

	(setq fpos (find #("me" "him" "her") "Here is the find me substring" startpos))


#endif

TVAL FTextFnc_find(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
COMPARE                 code;
LpCHAR                  pMatch;
LpCHAR                  pTarget;
NUM                     nMatch;
NUM                     nTarget;
NUM                     start;
CHAR					CH;
CHAR					cbuf[2];
CHAR					c2buf[2];
NUM						matchCount;
NUM						matchIndex;
CHAR*					matchPtr;
NUM						matchSize;
NUM						StartIndex[256];	/* Start index of match strings by starting character code */
NUM						EndIndex[256];		/* End index of match strings by starting character code */
NUM						StringSize[256];	/* Size match strings (max 256 match strings) */
NUM						StringPtr[256];		/* Pointer of match strings (max 256 match strings) */

    
/*  There must be at least two and not more than three arguments. */

if ((argc < 2) || (argc > 3)) 
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
    
start = 0;

/*  Get optional starting point. */

if (argc == 2)
	{
	start = 0;
	}
else
	{
	if (isNumIndex(&argv[2]))
		start = asNumIndex(&argv[2]);
	else
		return(TERROR("!find: start position argument must be numeric!"));
	}

/*  Get pointer to the target string. */

if (argv[1].Tag == TYCHAR)
	{
	c2buf[0] = argv[1].u.Char;
	c2buf[1] = 0;
	pTarget = &c2buf[0];
	nTarget = 1;
	}
else
if (argv[1].Tag == TYSTRINGSUBSTR)
    {
    pTarget = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (pTarget == NULL) return(gCP->TObject_ERROR_INVALID);
    nTarget = SubLen(argv[1]);
    }
else
if (argv[1].Tag == TYTEXT)
	{
	pTarget = &argv[1].u.Text[0];
	nTarget = strlen(pTarget);
	}
else
if (argv[1].Tag == TYSTRING)
	{
	pTarget = (char*)CharArray(argv[1]);
	nTarget = strlen(pTarget);
	}
else
if ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL))
	{
	pTarget = (char*)SymbolArray(argv[1]);
	nTarget = strlen(pTarget);
	}
else
if (argv[1].Tag == TYBYTEVECTOR)
	{
	pTarget = (char*)ByteArray(argv[1]);
	nTarget = ByteVector(argv[1])->itsMaxItemIndex;
	}
else
	return(gCP->TObject_ERROR_INVALID_ARGLIST);

if ((start < 0) || (start > nTarget)) 
	{
	return(gCP->Tval_FALSE);
	}

/* Process the case where we are looking for a vector of possible match strings. */

if (argv[0].Tag == TYVECTOR)
	{
	/* Retrieve number of match strings */

	matchCount = Vector(argv[0])->itsMaxItemIndex;
	if (matchCount > 256)
		{
		return(TERROR("!find: only 256 match strings supported!"));
		}

	/* Retrieve a pointer to each match string and build the start and end */ 
	/* index of match strings array by starting character code plus save */
	/* the size of and a pointer to each match string. */

	for (matchIndex = 0; matchIndex < 256; ++matchIndex)
		{
		StartIndex[matchIndex] = -1;
		EndIndex[matchIndex] = -1;
		}
	nMatch = 0;

	for (matchIndex = 0; matchIndex < matchCount; ++matchIndex)
		{
		/* Retrieve a pointer to each match string and build the start and end */ 
		/* index of match strings array by starting character code plus save */
		/* the size of and a pointer to each match string. */

		if ((FSmartbase_StringPtr(gCP,gTP,&TvalArray(argv[0])[matchIndex],&matchPtr,&matchSize) == FALSE) || (matchSize == 0))
			{
			return(TERROR("!find: match vector contains invalid element!"));
			}
		
		/* Build the start and end index of match strings by starting character code */

		if (StartIndex[(NUM)matchPtr[0]] < 0)
			{
			StartIndex[(NUM)matchPtr[0]] = matchIndex;
			EndIndex[(NUM)matchPtr[0]] = matchIndex;
			}
		else
			{
			EndIndex[(NUM)matchPtr[0]] = matchIndex;
			}

		/* Save the size of each match string and a pointer to each match string. */

		StringSize[matchIndex] = matchSize;
		StringPtr[matchIndex] = (NUM)matchPtr;
		}


	/*  Find any one of the match strings within the target string. */

	while (start < nTarget)
		{
		CH = pTarget[start];
		if (StartIndex[(unsigned char)CH] >= 0)
			{
			for (matchIndex = StartIndex[(unsigned char)CH]; matchIndex <= EndIndex[(unsigned char)CH]; ++matchIndex)
				{
				if ((start + StringSize[matchIndex]) <= nTarget)
					{
					code = memcmp((void*)&pTarget[start],(void*)StringPtr[matchIndex],StringSize[matchIndex]);
					if (code == 0)
						{
						Vector(argv[0])->itsCdr = TINT(matchIndex);
						return(TINT(start));
						}
					}
				}
			}

		++start;
		}
	
	}
else
	{

	/*  Get pointer to the match string. */

	if (argv[0].Tag == TYCHAR)
		{
		cbuf[0] = argv[0].u.Char;
		cbuf[1] = 0;
		pMatch = &cbuf[0];
		nMatch = 1;
		}
	else
	if (argv[0].Tag == TYTEXT)
		{
		pMatch = &argv[0].u.Text[0];
		nMatch = strlen(pMatch);
		}
	else
	if (argv[0].Tag == TYSTRINGSUBSTR)
	    {
    	pMatch = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    	if (pMatch == NULL) return(gCP->TObject_ERROR_INVALID);
    	nMatch = SubLen(argv[0]);
	    }
	else
	if (argv[0].Tag == TYSTRING)
		{
		pMatch = (char*)CharArray(argv[0]);
		nMatch = strlen(pMatch);
		}
	else
	if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
		{
		pMatch = (char*)SymbolArray(argv[0]);
		nMatch = strlen(pMatch);
		}
	else
	if (argv[0].Tag == TYBYTEVECTOR)
		{
		pMatch = (char*)ByteArray(argv[0]);
		nMatch = ByteVector(argv[0])->itsMaxItemIndex;
		}
	else
		return(gCP->TObject_ERROR_INVALID_ARGLIST);

	/*  Do not attempt to match a null string  */

	if (nMatch == 0)
		{
		return(gCP->Tval_FALSE);
 		}


	/*  Find the match string within the target string. */
	while ((start + nMatch) <= nTarget)
		{
		code = memcmp((void*)&pTarget[start],(void*)pMatch,nMatch);
		if (code == 0)
			{
			return(TINT(start));
			}
		++start;
		}
	} 


return(gCP->Tval_FALSE); 
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_clean

Removes all nonprintable characters from text. Use CLEAN on text imported
from other applications which contains characters than may not print with 
user`s current operating system. E.g.; you can use CLEAN to remove some 
low-level computer code that is frequently at the beginning and end of data
files and cannot be printed.

#endif

TVAL FTextFnc_clean(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
LpCHAR          stringPtr;
NUM             size;
LpCHAR          newPtr;
CHAR            c;
NUM             j;
NUM             i;
CHAR			cbuf[2];
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TString,ss);
EndFrame

/*  There must be one argument. */

if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

*ret = gCP->Tval_VOID;

/*  Get pointer to and size of the source string. */

if (argv[0].Tag == TYCHAR)
    {
	cbuf[0] = argv[0].u.Char;
	cbuf[1] = 0;
	stringPtr = &cbuf[0];
    size = 1;
    }
else
if (argv[0].Tag == TYTEXT)
    {
    stringPtr = &argv[0].u.Text[0];
    size = strlen(stringPtr);
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    stringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (stringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (argv[0].Tag == TYSTRING)
    {
    stringPtr = (char*)*((TString*)argv[0].u.Object)->itsCString;
    size = strlen(stringPtr);
    }
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    stringPtr = (char*)*((TSymbol*)argv[0].u.Object)->itsCString;
    size = strlen(stringPtr);
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    stringPtr = (char*)*((TByteVector*)argv[0].u.Object)->itsByteArray;
    size = ByteVector(argv[0])->itsMaxItemIndex;
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Make a copy of the source string to trim as the result. */

ss = TString_SubString_MakeUnique(gCP, gTP, stringPtr, 0, size);
newPtr = (char*)*ss->itsCString;

/*  Remove blanks from the source as we copy to the result. */

newPtr[0] = 0;

/*  Copy to the result buffer if I am a printable character. */

for (j = 0, i = 0; ((c = stringPtr[i]) && (i < size)); i++)                   
    {
    if (ISPRINT(c))
        newPtr[j++] = c;
    }

newPtr[j] = 0;

/*  Produce a new result from the cleaned string. */

*ret = TObject_CnvFromText(gCP, gTP, newPtr);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_code

Accepts a string and returns the corresponding number for the first character 
in the string.

#endif

TVAL FTextFnc_code(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
LpCHAR          stringPtr;
CHAR			cbuf[2];
StartFrame
DeclareTVAL(ret);
EndFrame

/*  There must be one argument. */

if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

*ret = gCP->Tval_VOID;

/*  Get pointer to and size of the source string. */

if (argv[0].Tag == TYCHAR)
    {
	cbuf[0] = argv[0].u.Char;
	cbuf[1] = 0;
	stringPtr = &cbuf[0];
    }
else
if (argv[0].Tag == TYTEXT)
    {
    stringPtr = &argv[0].u.Text[0];
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    stringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (stringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    }
else
if (argv[0].Tag == TYSTRING)
    {
    stringPtr = (char*)*((TString*)argv[0].u.Object)->itsCString;
    }
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    stringPtr = (char*)*((TSymbol*)argv[0].u.Object)->itsCString;
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    stringPtr = (char*)*((TByteVector*)argv[0].u.Object)->itsByteArray;
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Return the first character of the string as an integer. */

*ret = TINT(stringPtr[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_char

Accepts an integer [1-255] and returns the corresponding character in the character set.
Returns the character in text form.  

#endif

TVAL FTextFnc_char(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

if (argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

*ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[0]);
if (ret->Tag != TYNUM) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

ret->Tag = TYCHAR;
ret->u.Char = ret->u.Int;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_StringToBVector

This procedure converts a string into a vector of the lines within the string. 
The key is used to break the original string up into separate lines. If the
optional third argument is present and true, any character in the second string
will cause a line break. 

Note:   The stringToBVector function expects either two arguments or three arguments.
        The break characters are appended to the end of each line. The lines are
        returned as byte vector objects, so the stringToBVector procedure will work
        with binary data.

For example 
    
    (stringToBVector "this is a test" " is ") ==> #("this is " "a test")

    (stringToBVector "this is a test" "ie" true) ==> #("thi" "s i" "s a te" "st")



#endif

TVAL FTextFnc_StringToBVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
LpCHAR          stringPtr;
NUM             size;
LpCHAR          keyPtr;
LpCHAR          locPtr;
NUM             ksize;
NUM             j;
NUM             i;
NUM             k;
NUM             n;
BOLE            anyCharWillDo = FALSE;
NUM             klen = 1;
CHAR			cbuf[2];
CHAR			c2buf[2];
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVAL(ec);
DeclareOBJ(TByteVector,bv);
EndFrame

/*  There must be two or three arguments. */

if (argc == 3)
    {
    if ((argv[2].Tag == TYBOLE) && (argv[2].u.Bool == TRUE)) anyCharWillDo = TRUE;
    }
else
if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

*ret = gCP->Tval_VOID;

/*  Get pointer to and size of the source string. */

if (argv[0].Tag == TYCHAR)
    {
	cbuf[0] = argv[0].u.Char;
	cbuf[1] = 0;
	stringPtr = &cbuf[0];
    size = 1;
    }
else
if (argv[0].Tag == TYTEXT)
    {
    stringPtr = &argv[0].u.Text[0];
    size = strlen(stringPtr);
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    stringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (stringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (argv[0].Tag == TYSTRING)
    {
    stringPtr = (char*)*((TString*)argv[0].u.Object)->itsCString;
    size = strlen(stringPtr);
    }
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    stringPtr = (char*)*((TSymbol*)argv[0].u.Object)->itsCString;
    size = strlen(stringPtr);
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    stringPtr = (char*)*((TByteVector*)argv[0].u.Object)->itsByteArray;
    size = ByteVector(argv[0])->itsMaxItemIndex;
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Get pointer to the key string. */

if (argv[1].Tag == TYCHAR)
    {
	c2buf[0] = argv[1].u.Char;
	c2buf[1] = 0;
	keyPtr = &c2buf[0];
    ksize = 1;
    }
else
if (argv[1].Tag == TYTEXT)
    {
    keyPtr = &argv[1].u.Text[0];
    ksize = strlen(keyPtr);
    }
else
if (argv[1].Tag == TYSTRINGSUBSTR)
    {
    keyPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (keyPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    ksize = SubLen(argv[1]);
    }
else
if (argv[1].Tag == TYSTRING)
    {
    keyPtr = (char*)*((TString*)argv[1].u.Object)->itsCString;
    ksize = strlen(keyPtr);
    }
else
if ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL))
    {
    keyPtr = (char*)*((TSymbol*)argv[1].u.Object)->itsCString;
    ksize = strlen(keyPtr);
    }
else
if (argv[1].Tag == TYBYTEVECTOR)
    {
    keyPtr = (char*)*((TByteVector*)argv[1].u.Object)->itsByteArray;
    ksize = ByteVector(argv[1])->itsMaxItemIndex;
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);



/*  Create the initial empty vector. */

*tmp = TSYMBOL("object");
*ret = FSmartbase_Eval(gCP,gTP,TGVALUE("makeVector"),2,*tmp,TINT(0));
ExitOnError(*ret);

/*  Do not try to match a null string, just return the original string in a byte vector */
if (ksize == 0)
	{
    bv = TByteVector_MakeUnique(gCP,gTP,stringPtr,0,size);
    tmp->Tag = bv->itsObjectType;
    tmp->u.Object = (TObject*)bv;
    *ec = TObjVector_AddNewValue(gCP,gTP,*ret,*tmp);
    ExitOnError(*ec);
	FrameExit(*ret);
	}

if (!anyCharWillDo) klen = ksize;

/*  Find the match string within the target string. */

i = 0;
while (i < size)
    {
    if (anyCharWillDo)
        {
        /* Search the remaining target binary string for any key character. */
        for (j = i; j < size; ++j)
           {
           for (k = 0; k < ksize; ++k)
              {
              /* Any char in the key will cause a match. */
              if (stringPtr[j] == keyPtr[k])
                {
                locPtr = &stringPtr[j];
                goto Found;
                }
              }
           }
        locPtr = &stringPtr[size-klen];
        }
    else
        {
        /* Search the remaining target binary string for the key string. */
        for (j = i; j < size; ++j)
           {
           /* Any char in the key will cause a match. */
           if (memcmp(&stringPtr[j],keyPtr,ksize) == 0)
             {
             locPtr = &stringPtr[j];
             goto Found;
             }
           }
        locPtr = &stringPtr[size-klen];
        }

    /*  Key string was not found. */
    if (TRUE) 
        {
        n = (locPtr - &stringPtr[i]) + klen;
        bv = TByteVector_MakeUnique(gCP,gTP,stringPtr,i,n);
        i = size;
        }
    else
    /*  Key string was found. */
        {
        Found:
        n = (locPtr - &stringPtr[i]) + klen;
        bv = TByteVector_MakeUnique(gCP,gTP,stringPtr,i,n);
        i = j + klen;
        }
        
    /*  Add the new line byte vector to the result vector. */
    tmp->Tag = bv->itsObjectType;
    tmp->u.Object = (TObject*)bv;
    *ec = TObjVector_AddNewValue(gCP,gTP,*ret,*tmp);
    ExitOnError(*ec);
    }
    
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_StringToVector

This procedure converts a string into a vector of the lines within the string. 
The key is used to break the original string up into separate lines. If the
optional third argument is present and true, any character in the second string
will cause a line break. If the optional fourth argument is present and true, 
adjacent characters in the second string will cause multiple line breaks.

Note:   The stringToVector function expects either two arguments or three arguments.

For example 
    
    (stringToVector "this is a test" " is ") ==> #("this" "a test")

    (stringToVector "this is a test" "ie" true) ==> #("th" "s " "s a t" "st")

    (stringToVector "F1~~F2~" "~" true true) ==> #("F1" "" "F2" "")

#endif

TVAL FTextFnc_StringToVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
LpCHAR          stringPtr;
NUM             size;
LpCHAR          keyPtr;
LpCHAR          locPtr;
NUM             ksize;
NUM             klen;
NUM             matchCount = 0;
NUM             matchIndex = 0;
NUM             k;
NUM             j;
NUM             i;
NUM             n;
NUM             N;
BOLE            anyCharWillDo = FALSE;
BOLE            multipleBreaks = FALSE;
BOLE            trailingBreak = FALSE;
CHAR			cbuf[2];
CHAR			c2buf[2];
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TString,ss);
EndFrame

/*  There must be two or three arguments. */

if (argc == 4)
    {
    if ((argv[2].Tag == TYBOLE) && (argv[2].u.Bool == TRUE)) anyCharWillDo = TRUE;
    if ((argv[3].Tag == TYBOLE) && (argv[3].u.Bool == TRUE) && (anyCharWillDo == TRUE)) multipleBreaks = TRUE;
    }
else
if (argc == 3)
    {
    if ((argv[2].Tag == TYBOLE) && (argv[2].u.Bool == TRUE)) anyCharWillDo = TRUE;
    }
else
if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

*ret = gCP->Tval_VOID;

/*  Get pointer to and size of the source string. */

if (argv[0].Tag == TYCHAR)
    {
	cbuf[0] = argv[0].u.Char;
	cbuf[1] = 0;
	stringPtr = &cbuf[0];
    size = 1;
    }
else
if (argv[0].Tag == TYTEXT)
    {
    stringPtr = &argv[0].u.Text[0];
    size = strlen(stringPtr);
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    stringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (stringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[0]);
    }
else
if (argv[0].Tag == TYSTRING)
    {
    stringPtr = (char*)*((TString*)argv[0].u.Object)->itsCString;
    size = strlen(stringPtr);
    }
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    stringPtr = (char*)*((TSymbol*)argv[0].u.Object)->itsCString;
    size = strlen(stringPtr);
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    stringPtr = (char*)*((TByteVector*)argv[0].u.Object)->itsByteArray;
    N = ByteVector(argv[0])->itsMaxItemIndex;
	n = 0; while ((stringPtr[n] != 0) && (n < N)) {++n;} size = n;
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Get pointer to the key string. */

if (argv[1].Tag == TYCHAR)
    {
	c2buf[0] = argv[1].u.Char;
	c2buf[1] = 0;
	keyPtr = &c2buf[0];
    ksize = 1;
    }
else
if (argv[1].Tag == TYTEXT)
    {
    keyPtr = &argv[1].u.Text[0];
    ksize = strlen(keyPtr);
    }
else
if (argv[1].Tag == TYSTRINGSUBSTR)
    {
    keyPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (keyPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    ksize = SubLen(argv[1]);
    }
else
if (argv[1].Tag == TYSTRING)
    {
    keyPtr = (char*)*((TString*)argv[1].u.Object)->itsCString;
    ksize = strlen(keyPtr);
    }
else
if ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL))
    {
    keyPtr = (char*)*((TSymbol*)argv[1].u.Object)->itsCString;
    ksize = strlen(keyPtr);
    }
else
if (argv[1].Tag == TYBYTEVECTOR)
    {
    keyPtr = (char*)*((TByteVector*)argv[1].u.Object)->itsByteArray;
    ksize = ByteVector(argv[1])->itsMaxItemIndex;
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

klen = ksize;
if (anyCharWillDo) ksize = 1;

/*  Find the count of matched strings within the target string. */
/*  Note:   We do this to avoid costly repeated upward reallocation */
/*          of the result vector (which is far more expensive than */
/*          performing a pre-search count). */

i = matchCount = 0;
while (i < size)
    {
    if (anyCharWillDo)
        {
        /* Any character in key string may match target substring. */
        j = i;
        locPtr = NULL;
        while (j < size)
            {
            k = 0;
            while (k < klen)
                {
                if (stringPtr[j] == keyPtr[k])
                    {
                    goto WeHaveAMatch1;
                    }
                else
                    {
                    ++k;
                    }
                }
            goto TryNextTargetChar1;

            WeHaveAMatch1:
            locPtr = (char*)&stringPtr[j];
			if ((multipleBreaks == TRUE) && ((j+1) == size))
				{
				++matchCount;
				trailingBreak = TRUE;
				}
            break;

            TryNextTargetChar1:
            ++j;
            }
        }
    else
        {
        /* locPtr = (LpCHAR)strstr((char*)&stringPtr[i],(char*)keyPtr); */
        j = i;
        locPtr = NULL;
        while (j <= (size - ksize))
            {
            k = 0;
            while (k < ksize)
                {
                if (stringPtr[j+k] != keyPtr[k])
                    {
                    goto TryNextTargetChar2;
                    }
                else
                    {
                    ++k;
                    }
                }
            locPtr = (char*)&stringPtr[j];
            break;

            TryNextTargetChar2:
            ++j;
            }
        }

    /*  Key string was not found. */
    if (locPtr == NULL)
        {
        i = size;
  
        /*  Add the new string to the count of matched strings. */
        ++matchCount;
        }
    else
    /*  Key string was found. */
        {
        j = (locPtr - stringPtr);
        if ((j-i) > 0)
            {
            /*  Add the new string to the count of matched strings. */
            ++matchCount;
            }
		else
        if (multipleBreaks == TRUE)
            {
            /*  Add the new string to the count of matched strings. */
            ++matchCount;
            }

        i = j + ksize;
        }
        
    }
    
/*  Create the initial empty vector. */

*ret = FSmartbase_Eval(gCP,gTP,TGVALUE("makeVector"),1,TINT(matchCount));
ExitOnError(*ret);
if (anyCharWillDo) ksize = 1;

/*  Find each match string within the target string and add them to the */
/*  result vector. We do this second pass to avoid costly reallocation. */

i = matchIndex = 0;
while (i < size)
    {
    if (anyCharWillDo)
        {
        /* Any character in key string may match target substring. */
        j = i;
        locPtr = NULL;
        while (j < size)
            {
            k = 0;
            while (k < klen)
                {
                if (stringPtr[j] == keyPtr[k])
                    {
                    goto WeHaveAMatch3;
                    }
                else
                    {
                    ++k;
                    }
                }
            goto TryNextTargetChar3;

            WeHaveAMatch3:
            locPtr = (char*)&stringPtr[j];
            break;

            TryNextTargetChar3:
            ++j;
            }
        }
    else
        {
        /* All characters in key string must match target substring. */
        j = i;
        locPtr = NULL;
        while (j <= (size - ksize))
            {
            k = 0;
            while (k < ksize)
                {
                if (stringPtr[j+k] != keyPtr[k])
                    {
                    goto TryNextTargetChar4;
                    }
                else
                    {
                    ++k;
                    }
                }
            locPtr = (char*)&stringPtr[j];
            break;

            TryNextTargetChar4:
            ++j;
            }
        }

    /*  Key string was not found. */
    if (locPtr == NULL) 
        {
        ss = TString_MakeUnique(gCP,gTP,&stringPtr[i]);
        i = size;
  
        /*  Add the new string to the result vector. */
        tmp->Tag = ss->itsObjectType;
        tmp->u.Object = (TObject*)ss;
        TvalArray(*ret)[matchIndex++] = *tmp;
        }
    else
    /*  Key string was found. */
        {
        j = (locPtr - stringPtr);
        if ((j-i) > 0)
            {
            ss = TString_SubString_MakeUnique(gCP,gTP,stringPtr,i,(j-i));

            /*  Add the new string to the result vector. */
            tmp->Tag = ss->itsObjectType;
            tmp->u.Object = (TObject*)ss;
            TvalArray(*ret)[matchIndex++] = *tmp;
            }
		else
        if (multipleBreaks == TRUE)
            {
            ss = TString_SubString_MakeUnique(gCP,gTP,stringPtr,i,(j-i));

            /*  Add the new string to the result vector. */
            tmp->Tag = ss->itsObjectType;
            tmp->u.Object = (TObject*)ss;
            TvalArray(*ret)[matchIndex++] = *tmp;
            }
        i = j + ksize;
        }
        
    }
    
if (trailingBreak == TRUE)
    {
    ss = TString_SubString_MakeUnique(gCP,gTP,stringPtr,0,0);

    /*  Add the new string to the result vector. */
    tmp->Tag = ss->itsObjectType;
    tmp->u.Object = (TObject*)ss;
    TvalArray(*ret)[matchIndex++] = *tmp;
    }
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_StringCILT

These cProcedures perform a case-insensitive comparison between the 
two strings {string1} and {string2}. If the comparison is true, then
true is returned; otherwise, false is returned. 

#endif

TVAL FTextFnc_StringCILT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
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
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 2) || (isNullTval(&argv[0])) || (isNullTval(&argv[1]))) 
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
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

begIndex1 = 0;
endIndex1 = size - 1;

/*  The second argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

if (asTag(&argv[1]) == TYTEXT)
    {
    sp2 = &asText(&argv[1])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[1]);
    }
else
if (asTag(&argv[1]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[1]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[1]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[1]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
begIndex2 = 0;
endIndex2 = size - 1;

/*  Compare the two strings and return the result. */

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

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FTextFnc_StringCILE

These cProcedures perform a case-insensitive comparison between the 
two strings {string1} and {string2}. If the comparison is true, then
true is returned; otherwise, false is returned. 

#endif

TVAL FTextFnc_StringCILE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
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
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 2) || (isNullTval(&argv[0])) || (isNullTval(&argv[1]))) 
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
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
    
begIndex1 = 0;
endIndex1 = size - 1;

/*  The second argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

if (asTag(&argv[1]) == TYTEXT)
    {
    sp2 = &asText(&argv[1])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[1]);
    }
else
if (asTag(&argv[1]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[1]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[1]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[1]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
begIndex2 = 0;
endIndex2 = size - 1;

/*  Compare the two strings and return the result. */

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
FTextFnc_StringCIEQ

These cProcedures perform a case-insensitive comparison between the 
two strings {string1} and {string2}. If the comparison is true, then
true is returned; otherwise, false is returned. 

#endif

TVAL FTextFnc_StringCIEQ(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
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
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 2) || (isNullTval(&argv[0])) || (isNullTval(&argv[1]))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
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

begIndex1 = 0;
endIndex1 = size - 1;

/*  The second argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

if (asTag(&argv[1]) == TYTEXT)
    {
    sp2 = &asText(&argv[1])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[1]);
    }
else
if (asTag(&argv[1]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[1]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[1]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[1]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

begIndex2 = 0;
endIndex2 = size - 1;    

/*  Compare the two strings and return the result. */

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
FTextFnc_StringCINE

These cProcedures perform a case-insensitive comparison between the 
two strings {string1} and {string2}. If the comparison is true, then
true is returned; otherwise, false is returned. 

#endif

TVAL FTextFnc_StringCINE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
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
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 2) || (isNullTval(&argv[0])) || (isNullTval(&argv[1]))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
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
    
begIndex1 = 0;
endIndex1 = size - 1; 

/*  The second argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

if (asTag(&argv[1]) == TYTEXT)
    {
    sp2 = &asText(&argv[1])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[1]);
    }
else
if (asTag(&argv[1]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[1]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[1]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[1]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
begIndex2 = 0;
endIndex2 = size - 1; 

/*  Compare the two strings and return the result. */

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
FTextFnc_StringCIGT

These cProcedures perform a case-insensitive comparison between the 
two strings {string1} and {string2}. If the comparison is true, then
true is returned; otherwise, false is returned. 

#endif

TVAL FTextFnc_StringCIGT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
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
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 2) || (isNullTval(&argv[0])) || (isNullTval(&argv[1]))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
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
    
begIndex1 = 0;
endIndex1 = size - 1; 

/*  The second argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

if (asTag(&argv[1]) == TYTEXT)
    {
    sp2 = &asText(&argv[1])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[1]);
    }
else
if (asTag(&argv[1]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[1]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[1]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[1]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

begIndex2 = 0;
endIndex2 = size - 1; 

/*  Compare the two strings and return the result. */

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
FTextFnc_StringCIGE

These cProcedures perform a case-insensitive comparison between the 
two strings {string1} and {string2}. If the comparison is true, then
true is returned; otherwise, false is returned. 

#endif

TVAL FTextFnc_StringCIGE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
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
EndFrame
 
/*  The first argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 2) || (isNullTval(&argv[0])) || (isNullTval(&argv[1]))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
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
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

begIndex1 = 0;
endIndex1 = size - 1; 

/*  The second argument must be a TYSTRING or a TYTEXT. */
/*  Note:   We must obtain a pointer to the text string. */

if (asTag(&argv[1]) == TYTEXT)
    {
    sp2 = &asText(&argv[1])[0];
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYSTRINGSUBSTR)
    {
    sp2 = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
    if (sp2 == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(argv[1]);
    }
else
if (asTag(&argv[1]) == TYSTRING)
    {
    sp2 = FObject_GetStringPtr(gCP, gTP, asObject(&argv[1]));
    size = strlen((char*)sp2);
    }
else
if (asTag(&argv[1]) == TYBYTEVECTOR)
    {
    sp2 = (char*)*((TByteVector*)asObject(&argv[1]))->itsByteArray;
    size = ((TByteVector*)asObject(&argv[1]))->itsMaxItemIndex;
    }
else
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

begIndex2 = 0;
endIndex2 = size - 1; 

/*  Compare the two strings and return the result. */

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

/*--------------------------------------------------------------------------------------- */
/*
FTextFnc_MacroReplace

This cProcedure supports C-like argument substitution macros. The function
accepts any number of arguments, but the last argument must be a macro String
template containing arguments %1, %2, etc. for substitution. 

(macroReplace %1 %2 {(+= %1 %2)})

The CMacro function performs the argument substitution and then
returns the substituted macro template as a String. 

(macroReplace %1 %2 '(+= %1 %2))

The CMacro function performs the argument substitution and then
returns the substituted macro template as a List. 

The original Lisp version of this function is as follows:

(defun macroReplace(...)
   regs:(n N)
   vars:(x form (pairSW false))
   (setq N (- (argCount) 1))
   (setq form (argFetch N))
   (if (isPair form) (setq pairSW true))
   (if (not (isString form)) (setq form (string form true)))
   (loop for n from 0 until N do
      (setq x (string (argFetch n) true))
	  (if (= pairSW false)
		(setq form (substitute form (append "%" (+ n 1)) x))
		(setq form (substitute form (append "|%" (+ n 1) "|") x))
		) 
      ) ; end loop
   (if (= pairSW true) (lisp form)[0] form))

*/


TVAL FTextFnc_MacroReplace(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
BOLE                    pairSW = FALSE;
NUM                     n;
NUM                     lastArg;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(arg);
DeclareTVAL(tmp);
DeclareTVALArray(prmv,3);

EndFrame
 
/*  The last argument must be a TYSTRING or a TYTEXT. */
/*  Note: If not, it must be converted to a String.   */

lastArg = argc-1;
if (lastArg < 0) {FrameExit(TERROR("!macroReplace: must have at least one argument!"));}
if (lastArg > 8) {FrameExit(TERROR("!macroReplace: can only have max ten arguments!"));}
if ((argv[lastArg].Tag==TYPAIR)||(argv[lastArg].Tag==TYQUOTEDPAIR)) pairSW = TRUE;

if ((argv[lastArg].Tag!=TYSTRING) && (argv[lastArg].Tag!=TYTEXT))
	{
	prmv[0] = argv[lastArg];
	prmv[1] = gCP->Tval_TRUE;
	*ret = FConvert_ToString(gCP, gTP, 2, &prmv[0]);
	ExitOnError(*ret);
	}
else
	{
	*ret = argv[lastArg];
	}


/*  Copy characters from the macro string template to the output buffer.   */
/*  Note: Any argument references must be replaced with their arguments.   */

for (n = 0;n < lastArg; ++n) 
	{
	/* Substitute each argument for %n in the macro string template. */
	arg->Tag = TYTEXT;
	if (pairSW == FALSE)
		{
		arg->u.Text[0] = '%';
		arg->u.Text[1] = 49+n;
		arg->u.Text[2] = 0;
		}
	else
		{
		arg->u.Text[0] = '|';
		arg->u.Text[1] = '%';
		arg->u.Text[2] = 49+n;
		arg->u.Text[3] = '|';
		arg->u.Text[4] = 0;
		}
	prmv[0] = argv[n];
	prmv[1] = gCP->Tval_TRUE;
	*tmp = FConvert_ToString(gCP, gTP, 2, &prmv[0]);
	ExitOnError(*ret);
	
	prmv[0] = *ret;
	prmv[1] = *arg;
	prmv[2] = *tmp;
	*ret = FTextFnc_substitute(gCP, gTP, 3, &prmv[0]);
	ExitOnError(*ret);
	}

/* Return the finished macro template as a String or as a List. */
if (pairSW == TRUE)
	{
	*ret = FConio_Parse(gCP, gTP, 1, ret);
	}

FrameExit(*ret);
}
