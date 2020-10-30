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

#define _C_FMAKE
#define _SMARTBASE

#if 0
FMake.c

This source file contains some of the cProcedures which implement the make functions
supported by the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

#endif
#include    "fmake.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tdiction.h"
#include    "tdirect.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tintvec.h"
#include    "tpcodvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tfltvec.h"
#include    "tobjvec.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "fmath1.h"
#include    "fmath2.h"
#include    "futil2.h"
#include    "futil3.h"
#include    "fpred2.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include	"tcpxvec.h"
#include	"tshortvec.h"
#include	"tlongvec.h"

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Init

Initialize the make portion of the SmartLisp function library.  

#endif

TVAL FMake_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FMake_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FMake_Initialized = TRUE;
    
/* Register the special types created in this package */

TBitVector_Init(gCP,gTP);
TCpxVector_Init(gCP, gTP);
TIntVector_Init(gCP, gTP);
TByteVector_Init(gCP, gTP);
TNumVector_Init(gCP, gTP);
TFltVector_Init(gCP, gTP);
TObjVector_Init(gCP, gTP);
    
/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"list",(LpFUNC)&FMake_List);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"quote",(LpFUNC)&FMake_Quote);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeQuotedList",(LpFUNC)&FMake_QuotedList);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"pair",(LpFUNC)&FMake_Pair);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeStructure",(LpFUNC)&FMake_Structure);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"makeStructure"), aSymbol->itsGlobalValue);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeDictionary",(LpFUNC)&FMake_Dictionary);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeString",(LpFUNC)&FMake_String);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeSubstring",(LpFUNC)&FMake_SubstringT);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeSymbol",(LpFUNC)&FMake_Symbol);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"symbol"), aSymbol->itsGlobalValue);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeQuotedSymbol",(LpFUNC)&FMake_QuotedSymbol);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeVector",(LpFUNC)&FMake_Vector);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeMatrix",(LpFUNC)&FMake_Matrix);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeObject",(LpFUNC)&FMake_Object);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"resize",(LpFUNC)&FMake_Resize);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"binarySearch",(LpFUNC)&FMake_BinarySearch);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"binaryInsert",(LpFUNC)&FMake_BinaryInsert);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"uniqueInsert",(LpFUNC)&FMake_UniqueInsert);
ExitOnError( *ec);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_List

Return a list of Pair objects (a list) with the specified initial values 
(argv[0] thru argv[argc-1]).  

Note:   If no arguments are specified, return TYVOID (the empty list).
        If the special character '.' is encountered, terminate the Pair chain. 
            
        (list)                  =>      () 
        (list 5)                =>      (5) 
        (list 5 1)              =>      (5 1) 
        (list 5 1 2 3)          =>      (5 1 2 3)
        (list 1 . 2)            =>      (1 . 2) 
        (list 1 2 . 3)          =>      (1 2 . 3) 
  
#endif

TVAL FMake_List(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])  
{
NUM                     argIndex;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,sp);
EndFrame
 
/*  Is this the empty list ? */

*ret = gCP->Tval_VOID;
if (argc == 0) FrameExit( *ret);

/*  Get the first Pair in the list */

sp = TPair_New(gCP,gTP);
asObject(ret) = (TObject*)sp;
asTag(ret) = TYPAIR;

/*  If we find the special character '.', terminate the Pair chain. */

if ((asTag(&argv[0]) != TYPCODE) || (asShort(&argv[0]) != PERIODTOK))
    {
    sp->itsCar = argv[0];
    }
else
    {
    if (argc > 1)
        {
        sp->itsCdr = argv[1];
        }
    FrameExit( *ret);
    }

/*  Link each remaining argument to the end of the Pair chain. */

argIndex = 1;
while (argIndex < argc)
    {
    
    /*  If we find the special character '.', terminate the Pair chain. */
    
    if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
        {
        if (argc > ++argIndex)
            {
            sp->itsCdr = argv[argIndex++];
            }
        FrameExit( *ret);
        }
        
    asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
    asTag(&sp->itsCdr) = TYPAIR;
    sp = asPair(&sp->itsCdr);
    sp->itsCar = argv[argIndex++];
    }
 
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Quote

Quote should never be compiled. It is a Lisp parser operator only. 
  
#endif

TVAL FMake_Quote(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])  
{
StartFrame
DeclareTVAL(err);
EndFrame
 
argc = argc; // NOOP to hide unused parameter warning message
argv = argv; // NOOP to hide unused parameter warning message
*err = TERROR("!quote: attempt to execute Lisp lexical form!");
FrameExit( *err);
}

/*--------------------------------------------------------------------------------------- */
/*

FMake_QuotedList

Return a quoted list of Pair objects (a list) with the specified initial values 
(argv[0] thru argv[argc-1]).  

Note:   If no arguments are specified, return TYVOID (the empty list).
        If the special character '.' is encountered, terminate the Pair chain. 
            
        (makeQuotedList 5)                =>      '(5) 
        (makeQuotedList 5 1)              =>      '(5 1) 
        (makeQuotedList 5 1 2 3)          =>      '(5 1 2 3)
        (makeQuotedList 1 . 2)            =>      '(1 . 2) 
        (makeQuotedList 1 2 . 3)          =>      '(1 2 . 3) 
  
*/

TVAL FMake_QuotedList(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])  
{
NUM                     argIndex;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,sp);
EndFrame
 
/*  Is this the empty list ? */

*ret = gCP->Tval_VOID;
if (argc == 0) FrameExit( *ret);

/*  Get the first Pair in the list */

sp = TPair_New(gCP,gTP);
asObject(ret) = (TObject*)sp;
asTag(ret) = TYPAIR;

/*  If we find the special character '.', terminate the Pair chain. */

if ((asTag(&argv[0]) != TYPCODE) || (asShort(&argv[0]) != PERIODTOK))
    {
    sp->itsCar = argv[0];
    }
else
    {
    if (argc > 1)
        {
        sp->itsCdr = argv[1];
        }
    FrameExit( *ret);
    }

/*  Link each remaining argument to the end of the Pair chain. */

argIndex = 1;
while (argIndex < argc)
    {
    
    /*  If we find the special character '.', terminate the Pair chain. */
    
    if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
        {
        if (argc > ++argIndex)
            {
            sp->itsCdr = argv[argIndex++];
            }
        FrameExit( *ret);
        }
        
    asObject(&sp->itsCdr) = (TObject*)TPair_New(gCP,gTP);
    asTag(&sp->itsCdr) = TYPAIR;
    sp = asPair(&sp->itsCdr);
    sp->itsCar = argv[argIndex++];
    }
 
/*  Make sure we return a quoted Pair. */     

if (ret->Tag == TYPAIR)
	{
	ret->Tag = TYQUOTEDPAIR;
	asQuoteCnt(ret) = 1;
	}

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMake_String

Return a String objectwith the specified initial value.

Note:   (makeString  MySymbol:)		=>  "MySymbol"
        (makeString  "xyz")			=>  "xyz"
        (makeString  ptr start end)	=>  "xyz"

  
#endif

TVAL FMake_String(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
LpCHAR temp = NIL;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TString,sp);
EndFrame

/*  We must have exactly one argument. */

*ret = gCP->Tval_VOID;

if ((argc == 3) && (argv[0].Tag == TYNUM) && (argv[1].Tag == TYNUM) && (argv[2].Tag == TYNUM))
	{
	sp = TString_SubString_MakeUnique(gCP,gTP,(LpCHAR)argv[0].u.Pointer,argv[1].u.Int,(argv[2].u.Int-argv[1].u.Int));
	}
else
if (argc != 1 )
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYTEXT)
    {
    sp = TString_MakeUnique(gCP,gTP,(LpCHAR)&asText(&argv[0]));
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    temp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (temp == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);
    sp = TString_SubString_MakeUnique(gCP, gTP, temp, 0, SubLen(argv[0]));
    }
else
if (argv[0].Tag == TYSTRING)
    {
    sp = TString_MakeUnique(gCP,gTP,&atHMChar((LpCHAR)asString(&argv[0])->itsCString,0));
    }
else 
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    sp = TString_MakeUnique(gCP,gTP,&atHMChar((LpCHAR)asSymbol(&argv[0])->itsCString,0));
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

ret->Tag = TYSTRING;
ret->u.String = sp;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMake_SubstringT

Return a Substring from a repeating object (String, Symbol, ByteVector)

Note:   (makeSubstringT MySymbol: offset length) =>  "MySymbol"
        (makeSubstringT "xyz" offset length) =>  "xyz"
        (makeSubstringT myRecord offset length) =>   "xyz"
        (makeSubstringT myByteVector offset length) => "xyz"
  
#endif

TVAL FMake_SubstringT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     offset = 0;
NUM     length = 0;
NUM     maxLength = 0;

StartFrame
DeclareTVAL(ret);
EndFrame

*ret = gCP->Tval_VOID;

/* This function accepts 3 arguments: variable, offset, and the length */
if ((argc != 3) || (argv[1].Tag != TYNUM) || (argv[2].Tag != TYNUM))
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYSTRING)
    {
    maxLength = argv[0].u.String->itsMaxItemIndex;
    }
else 
if (argv[0].Tag == TYSYMBOL || (argv[0].Tag == TYQUOTEDSYMBOL))
    {
    maxLength = argv[0].u.Symbol->itsMaxItemIndex;
    }
else 
if (argv[0].Tag == TYBYTEVECTOR)
    {
    maxLength = argv[0].u.ByteVector->itsMaxItemIndex + 1;
    }
else
    FrameExit(TERROR("!Unsupported type for Substring creation!"));
    
offset = argv[1].u.Int;
length = argv[2].u.Int;

/* Check if offset and length values are valid */
/* Make sure the substring won't exceed our maximum length */
if ((offset < 0) || (length < 1) || ((offset + length) >= maxLength))
    FrameExit(TERROR("!Substring length is invalid!"));

ret->Tag = TYSTRINGSUBSTR;
asObjIdx(ret) = argv[0].u.Object->itsObjectIndex;
asSubOff(ret) = offset;
asSubLen(ret) = length;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Symbol

Return a Symbol objectwith the specified initial value.

Note:   (makeSymbol  "MySymbol")	=>  MySymbol:
        (makeSymbol  "xyz")			=>  xyz:
        (makeSymbol  ptr start end)	=>  "xyz"

  
#endif

TVAL FMake_Symbol(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
LpCHAR temp = NULL;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TSymbol,sp);
EndFrame

/*  We must have exactly one argument. */

*ret = gCP->Tval_VOID;

if ((argc == 3) && (argv[0].Tag == TYNUM) && (argv[1].Tag == TYNUM) && (argv[2].Tag == TYNUM))
	{
	sp = TSymbol_SubString_MakeUnique(gCP,gTP,(LpCHAR)argv[0].u.Pointer,argv[1].u.Int,(argv[2].u.Int-argv[1].u.Int));
	}
else
if (argc != 1 )
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (asTag(&argv[0]) == TYTEXT)
    {
    sp = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)&asText(&argv[0]));
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    temp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (temp == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);
    sp = TSymbol_SubString_MakeUnique(gCP, gTP, temp, 0, SubLen(argv[0]));
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    sp = TSymbol_MakeUnique(gCP,gTP,&atHMChar(asString(&argv[0])->itsCString,0));
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR)
    {
    sp = TSymbol_MakeUnique(gCP,gTP,ByteArray(argv[0]));
    }
else
if ((asTag(&argv[0]) == TYSYMBOL) || (asTag(&argv[0]) == TYQUOTEDSYMBOL))
    {
    sp = TSymbol_MakeUnique(gCP,gTP,&atHMChar(asSymbol(&argv[0])->itsCString,0));
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

asTag(ret) = TYSYMBOL;
asObject(ret) = (TObject*)sp;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_QuotedSymbol

Return a quoted Symbol object with the specified initial value.

Note:   (makeQuotedSymbol  "MySymbol")    =>  ''MySymbol
        (makeQuotedSymbol  "xyz")     =>  ''xyz

  
#endif

TVAL FMake_QuotedSymbol(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
EndFrame

/*  We must first create a symbol object. */

*ret = FMake_Symbol(gCP,gTP,argc,argv);
ExitOnError(*ret);

/*  Next we increment the quote count. */

if (ret->Tag == TYSYMBOL)
    {
    ret->Tag = TYQUOTEDSYMBOL;
    asQuoteCnt(ret) = 1;
    }
else
    {
    ret->Tag = TYQUOTEDSYMBOL;
    ++asQuoteCnt(ret);
    }

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Pair

Return a Pair object (a list) with the specified initial values 
(argv[0] thru argv[1]).  

Note:   (pair 0 4)              =>      (0 . 4) 
        [[0 4]]                 =>      (0 . 4) 
  
#endif

TVAL FMake_Pair(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,sp);
EndFrame

/*  We must have exactly two arguments. */

*ret = gCP->Tval_VOID;
if (argc != 2) 
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Create the Pair with the two arguments specified. */

sp = TPair_New(gCP,gTP);

asTag(ret) = TYPAIR;
asObject(ret) = (TObject*)sp;
TPair_IPair(gCP, gTP, *ret, argv[0], argv[1]);

FrameExit( *ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Object

Check to see if the integer oid passed is valid. If it is then construct a TVAL which contains
its type and offset and return it.
  
#endif

TVAL FMake_Object(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                 theOID;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TObject,op);
EndFrame

/*  We must have exactly two arguments. */

*ret = gCP->Tval_VOID;
if (argc != 1 || asTag(&argv[0]) != TYNUM) 
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

theOID = asInt(&argv[0]);

if(inRange(theOID, 1, gCP->TObject_MaxObjectCount))
    {
    op = _TObject_ObjectByIndex(theOID);
    asTag(ret) = op->itsObjectType;
    asObject(ret) = op;
    }

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Structure

Return a Structure object with the specified initial bindings and optional 
cdr value.  

Note:   If no arguments are specified, return (the empty Structure).
        If the special character '.' is encountered, assign the cdr value. 
            
        (makeStructure X: 5 Y: 6)        =>      #{X: 5  Y: 6}
        (makeStructure X: 5 Y: 6 . 6)    =>      #{X: 5  Y: 6 . 6}
        (makeStructure )                 =>      #{}
        (makeStructure . 5)              =>      #{ . 5}
  
#endif

TVAL FMake_Structure(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
NUM                     argIndex;
NUM                     maxIndex;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TStructure,sp);
EndFrame

/*  Create the empty Structure */

maxIndex = argc;
*ret = gCP->Tval_VOID;
sp = TStructure_New(gCP,gTP);
asTag(ret) = TYSTRUCTURE;
asObject(ret) = (TObject*)sp;
if (argc == 0) FrameExit(*ret);

/*  Scan for the tail, if any. */

argIndex = 0;
while ((argIndex + 1) < argc)
    {
    
    /*  If we find the special symbol '.', set the Structure's cdr. */
    
    if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
        {
        sp->itsCdr = argv[argIndex+1];
        FrameExit( *ret);
        }
    TStructure_AddNewValue(gCP,gTP,*ret,argv[argIndex],argv[argIndex+1]);
    argIndex += 2;
    }

/*  Bindings must always come in pairs. */

if (argIndex < argc) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Dictionary

Return a Dictionary object with the specified initial bindings.  

Note:   If no arguments are specified, return (the empty Dictionary).
            
        (makeDictionary X: 5 Y: 6 . 67)
        (makeDictionary )
  
#endif

TVAL FMake_Dictionary(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     argIndex;
NUM                     maxIndex;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareOBJ(TDictionary,sp);
EndFrame

/*  Create the empty Structure */

maxIndex = argc;
*ret = gCP->Tval_VOID;
sp = TDictionary_New(gCP,gTP);
asTag(ret) = TYDICTIONARY;
asObject(ret) = (TObject*)sp;
if (argc == 0) FrameExit(*ret);

/*  Scan for the tail, if any. */

argIndex = 0;
while ((argIndex + 1) < argc)
    {
    
    /*  If we find the special symbol '.', set the Structure's cdr. */
    
    if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
        {
        sp->itsCdr = argv[argIndex+1];
        FrameExit( *ret);
        }
    *ec = TDictionary_SetIV1(gCP,gTP,*ret,argv[argIndex],argv[argIndex+1]);
	ExitOnError(*ec);
    argIndex += 2;
    }

/*  Bindings must always come in pairs. */

if (argIndex < argc) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMake_Resize

The resize cProcedure resizes a vector, or Structure {obj} to the 
specified size {size} and fills any new space with  ().  If the object is downsized, 
the trailing values are dropped.  For example:

    (resize  #(A  A  A  A  A)  6)       =>  #(A  A  A  A  A  ())
    (resize  #(A  A  A  A  A)  4)       =>  #(A  A  A  A)
    (resize  #{A:  1  B:  2}  3)        =>  #{A:  1  B:  2  ()  ()}
    (resize  #{A:  1  B:  2}  1)        =>  #{A:  1}
    (resize  #[A:  1  B:  2]  3)        =>  #[A:  1  B:  2  ()  ()]
    (resize  #[A:  1  B:  2]  1)        =>  #[A:  1]

  
#endif

TVAL FMake_Resize(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM			newSize;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  We must have exactly two arguments, an Object Pointer and a numeric size. */

*ret = gCP->Tval_VOID;
if ((argc < 2) || (!isNumIndex(&argv[1])) || (asNumIndex(&argv[1]) < 0))
	{
	FrameExit(TERROR("!resize: invalid arguments!"));
	}
newSize = asNumIndex(&argv[1]);

switch(asTag(&argv[0]))
    {
    case TYDIRECTORY:
        *ret = TDirectory_SetMaxIndex(gCP,gTP,argv[0],newSize);
        if(!isERROR(ret)) *ret = argv[0];
    break;

    case TYDICTIONARY:
        *ret = TDictionary_SetMaxIndex(gCP,gTP,argv[0],newSize);
        if(!isERROR(ret)) *ret = argv[0];
    break;

    case TYMATRIX:
        *ret = TMatrix_Resize(gCP,gTP,argc,argv);
        if(!isERROR(ret)) *ret = argv[0];
    break;

    case TYNUMMATRIX:
        *ret = TNumMatrix_Resize(gCP,gTP,argc,argv);
        if(!isERROR(ret)) *ret = argv[0];
    break;

    case TYBYTEVECTOR:
    case TYSTRING:
    case TYBITVECTOR:
    case TYVECTOR:
    case TYSTRUCTURE:
    case TYINTVECTOR:
    case TYSHORTVECTOR:
    case TYLONGVECTOR:
    case TYNUMVECTOR:
    case TYFLTVECTOR:
    case TYOBJVECTOR:
	case TYCPXVECTOR:
		if (argc != 2 || !isNumIndex(&argv[1] )) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
        *ret = FObject_SetMaxIndex(gCP,gTP,asObject(&argv[0]),(NUM)asNumIndex(&argv[1]));
        if(!isERROR(ret)) *ret = argv[0];
    break;
    
    case TYTEXT:
		FrameExit(TERROR("!resize: cannot resize Text please convert to a String type!"));
    break;
    
    default:
		FrameExit(TERROR("!resize: cannot resize the specified type!"));
    break;
    }
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMake_BinarySearch

The binarySearch Procedure searches any target for the presence of a key.  If the key
is found, an index of the matching element in the target if returned; otherwise FALSE
is returned.
                
For example:

    (binarySearch  #(1 3 5 7 9)  7)    =>  3
    (binarySearch  #(1 3 5 7 9)  0)    =>  false

Note:   If there is an optional third argument, and it is FALSE, then a full
        comparison is NOT performed; instead, only identical object keys found.

  
#endif

TVAL FMake_BinarySearch(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
BOLE        full = TRUE;
BOLE        aStruct = FALSE;
NUM         low;
NUM         mid;
NUM         high;
NUM         count;
NUM         compareCode;
NUM			indexOf;
StartFrame
DeclareTVAL(ec);
DeclareTVAL(ret);
DeclareTVAL(element);
DeclareTVALArray(parms,3);

EndFrame

/*  Check the arguments and validate them. */

if (argc == 3)
    {
    if ((argv[2].Tag == TYBOLE) && (argv[2].u.Bool == TRUE)) full = TRUE;
    if ((argv[2].Tag == TYBOLE) && (argv[2].u.Bool == FALSE)) full = FALSE;
    }
else
if (argc != 2)
    {
	*ret = TERROR("!binarySearch: Missing arguments!");
    FrameExit(*ret);
    }

/*  Check if the search Object is a Structure. */

if (argv[0].Tag == TYSTRUCTURE)
    {
	/* Perform a fast sequential search (object ids only) if Structure is small, */
	/*    and if the comparison switch is set to partial comparison only. */

	if ((Structure(argv[0])->itsMaxItemIndex <= 256) && (full == FALSE))
		{
		/* Only Object keys are allowed with the partial search option. */

		if (_TObject_TypeFlag(argv[1].Tag) != _TObject_TfTOBJECT)
			{
			*ret = TERROR("!binarySearch: Key argument must be an object!");
			FrameExit(*ret);
			}
		
		for (indexOf = 0; indexOf < Structure(argv[0])->itsMaxItemIndex; ++indexOf)
			{
			if (atHMBind(Structure(argv[0])->itsDictionaryArray,indexOf).Key == argv[1].u.Object)
				{
				ret->Tag = TYNUM;
				ret->u.Int = indexOf;
				FrameExit(*ret);
				}
			}

		FrameExit(gCP->Tval_FALSE);
		}


	/* Perform binary search (with full comparison) if Structure is too large, */
	/*    or if the comparison switch is set to full comparison required. */

    aStruct = TRUE;
    }

/*  Check if the search Object is a Dictionary. */

if (argv[0].Tag == TYDICTIONARY)
    {
	/* Perform a fast sequential search (object ids only) if Dictionary is small, */
	/*    and if the comparison switch is set to partial comparison only. */

	if ((Dictionary(argv[0])->itsMaxItemIndex <= 256) && (full == FALSE))
		{
		/* Only Object keys are allowed with the partial search option. */

		if (_TObject_TypeFlag(argv[1].Tag) != _TObject_TfTOBJECT)
			{
			*ret = TERROR("!binarySearch: Key argument must be an object!");
			FrameExit(*ret);
			}
		
		for (indexOf = 0; indexOf < Dictionary(argv[0])->itsMaxItemIndex; ++indexOf)
			{
			if (atHMBind(Dictionary(argv[0])->itsDictionaryArray,indexOf).Key == argv[1].u.Object)
				{
				ret->Tag = TYNUM;
				ret->u.Int = indexOf;
				FrameExit(*ret);
				}
			}

		FrameExit(gCP->Tval_FALSE);
		}


	/* Perform binary search (with full comparison) if Dictionary is too large, */
	/*    or if the comparison switch is set to full comparison required. */

    aStruct = TRUE;
    }

/*  Perform the binary search. */

full = TRUE;
low = 0;
mid = 0;
compareCode = -1;
parms[0] = argv[0];
parms[1].Tag = TYNUM;
parms[2].Tag = TYNUM;
parms[2].u.Int = 0;


/*  Get the length of the target. */
*ec = FSmartbase_Evalv(gCP,gTP,TGVALUE("length"),1,&argv[0]);
if (ec->Tag == TYNUM)
    {
    high = ec->u.Int - 1;
    count = high;
    }   
else
    {
	*ret = TERROR("!binarySearch: Object to be searched must be a Vector or Structure!");
    FrameExit(*ret);
    }
        
while ((low <= high) && (count-- >= 0))
    {
    /* Adjust the search index based upon the comparison results. */
      
    mid = (low + high) >> 1;
    parms[1].u.Int = mid;
    if (aStruct)
        *element = FUtil2_Ref(gCP,gTP,3,&parms[0]);
    else
        *element = FUtil2_Ref(gCP,gTP,2,&parms[0]);
    if (element->Tag == TYERROR) FrameExit(*element);
                                           
    /* Compare the key with the chosen element from the target. */

    if (full)
        *ec = FPredicate2_FullCompare(gCP,gTP,argv[1],*element);
    else
        *ec = FPredicate2_FullCompare(gCP,gTP,argv[1],*element);
    if (ec->Tag == TYERROR) FrameExit(*ec);
    compareCode = ec->u.Compare;

    if ( compareCode < 0 )
        {
        high = mid - 1;
        }
    else 
    if ( compareCode > 0 )
        {
         low = mid + 1;
        }
    else
        break;
   }
   
if (compareCode == 0)
    {
    ret->Tag = TYNUM;
    ret->u.Int = mid;
    }
else
    {
    ret->Tag = TYBOLE;
    ret->u.Bool = FALSE;
    }

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMake_BinaryInsert

The binaryInsert Procedure inserts any key value into a target in ascending sort order.  
An index of the key element in the target is returned.
        
        
For example:

    (binaryInsert  #(1 3 5 7 9)  7)    =>  3
    (binaryInsert  #(1 3 5 7 9)  4)    =>  2  .. the new target object is #(1 3 4 5 7 9)...

Note:   If there is an optional third argument, and it is TRUE, then a full
        comparison is performed.

  
#endif

TVAL FMake_BinaryInsert(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
BOLE        full = TRUE;
NUM         low;
NUM         mid;
NUM         high;
NUM         count;
NUM         compareCode;
StartFrame
DeclareTVAL(ec);
DeclareTVAL(ret);
DeclareTVAL(element);
DeclareTVALArray(refs,3);
DeclareTVALArray(parms,4);


EndFrame


/*  Check the arguments and validate them. */

if (argc == 3)
    {
    if ((argv[2].Tag == TYBOLE) && (argv[2].u.Bool == TRUE)) full = TRUE;
    }
else
if (argc != 2)
    {
	*ret = TERROR("!binaryInsert: Missing arguments!");
    FrameExit(*ret);
    }


/*  Perform the binary insert. */


refs[0] = parms[0] = argv[0];
refs[1].Tag = parms[1].Tag = TYNUM;
parms[2] = argv[1];
refs[2].Tag = TYNUM;
refs[2].u.Int = 0;
parms[3] = gCP->Tval_VOID;
parms[1].u.Int = mid = 0;
low = 0;
compareCode = -1;

/*  Get the length of the target. */

*ec = FUtil2_Length(gCP,gTP,1,&argv[0]);
if (ec->Tag == TYNUM)
    {
    high = ec->u.Int - 1;
    count = high;
    }   
else
    {
	*ret = TERROR("!binaryInsert: Target object must be a Vector or Structure!");
    FrameExit(*ret);
    }
        
while ((low <= high) && (count-- >= 0))
    {
    /* Adjust the search index based upon the comparison results. */
      
    mid = (low + high) >> 1;
    refs[1].u.Int = parms[1].u.Int = mid;

	if (argv[0].Tag == TYSTRUCTURE)
        *element = FUtil2_Ref(gCP,gTP,3,&refs[0]);
    else
        *element = FUtil2_Ref(gCP,gTP,2,&refs[0]);
   
	if (element->Tag == TYERROR) FrameExit(*element);
                                           
    /* Compare the key with the chosen element from the target. */

    if (full)
        *ec = FPredicate2_FullCompare(gCP,gTP,argv[1],*element);
    else
        *ec = FPredicate2_FullCompare(gCP,gTP,argv[1],*element);
    if (ec->Tag == TYERROR) FrameExit(*ec);
    compareCode = ec->u.Compare;

    if ( compareCode < 0 )
        {
        high = mid - 1;
        }
    else 
    if ( compareCode > 0 )
        {
         low = mid + 1;
        }
    else
        break;
   }
   
if (compareCode < 0)
    {
	if (argv[0].Tag == TYSTRUCTURE)
        *ec = FUtil3_VectorInsert(gCP,gTP,4,&parms[0]);
    else
        *ec = FUtil3_VectorInsert(gCP,gTP,3,&parms[0]);
    if (ec->Tag == TYERROR) FrameExit(*ec);
    *ret = parms[1];
    }
else
if (compareCode == 0)
    {
    ret->Tag = TYNUM;
    ret->u.Int = mid;
    }
else
    {
    ++parms[1].u.Int;
	if (argv[0].Tag == TYSTRUCTURE)
        *ec = FUtil3_VectorInsert(gCP,gTP,4,&parms[0]);
    else
        *ec = FUtil3_VectorInsert(gCP,gTP,3,&parms[0]);
    if (ec->Tag == TYERROR) FrameExit(*ec);
    *ret = parms[1];
    }

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMake_UniqueInsert

The uniqueInsert Procedure inserts any key value into a target (if one is not already 
present). An index of the key element in the target is returned.
        
        
For example:

    (uniqueInsert  #(1 3 5 7 9)  7)    =>  3
    (uniqueInsert  #(1 3 5 7 9)  4)    =>  5  .. the new target object is #(1 3 5 7 9 4)...


Note:   If there is an optional third argument, and it is TRUE, then a full
        comparison is performed.

#endif

TVAL FMake_UniqueInsert(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
BOLE        full = TRUE;
NUM         low;
NUM         mid;
NUM         high;
NUM         compareCode;
StartFrame
DeclareTVAL(ec);
DeclareTVAL(ret);
DeclareTVAL(element);
DeclareTVALArray(refs,3);
DeclareTVALArray(parms,4);


EndFrame


/*  Check the arguments and validate them. */

if (argc == 3)
    {
    if ((argv[2].Tag == TYBOLE) && (argv[2].u.Bool == TRUE)) full = TRUE;
    }
else
if (argc != 2)
    {
	*ret = TERROR("!uniqueInsert: Missing arguments!");
    FrameExit(*ret);
    }

if ((argv[0].Tag == TYSTRUCTURE) && (argv[1].Tag != TYSYMBOL))
    {
	*ret = TERROR("!uniqueInsert: Second argument must be a symbol!");
    FrameExit(*ret);
    }


/*  Perform the binary search. */

refs[0] = parms[0] = argv[0];
refs[1].Tag = parms[1].Tag = TYNUM;
parms[2] = argv[1];
refs[2].Tag = TYNUM;
refs[2].u.Int = 0;
parms[3] = gCP->Tval_VOID;
parms[1].u.Int = mid = 0;
low = 0;
compareCode = -1;

/*  Get the length of the target. */

*ec = FSmartbase_Evalv(gCP,gTP,TGVALUE("length"),1,&argv[0]);
if (ec->Tag == TYNUM)
    {
    high = ec->u.Int;
    }   
else
    {
	*ret = TERROR("!uniqueInsert: Target object must be a Vector or Structure!");
    FrameExit(*ret);
    }
        
while (mid < high)
    {
    /* Adjust the search index based upon the comparison results. */
      
    refs[1].u.Int = parms[1].u.Int = mid;
    if (argv[0].Tag == TYSTRUCTURE)
        *element = FUtil2_Ref(gCP,gTP,3,&refs[0]);
    else
        *element = FUtil2_Ref(gCP,gTP,2,&refs[0]);
    if (element->Tag == TYERROR) FrameExit(*element);
                                           
    /* Compare the key with the chosen element from the target. */

    if (full)
        *ec = FPredicate2_FullCompare(gCP,gTP,argv[1],*element);
    else
        *ec = FPredicate2_FullCompare(gCP,gTP,argv[1],*element);
    if (ec->Tag == TYERROR) FrameExit(*ec);
    compareCode = ec->u.Compare;

    if ( compareCode != 0 )
        {
        ++mid;
        }
    else
        break;
   }
   
if (compareCode == 0)
    {
    ret->Tag = TYNUM;
    ret->u.Int = mid;
    }
else
    {
    parms[1].u.Int = mid;
    if (argv[0].Tag == TYSTRUCTURE)
        *ec = FUtil3_VectorInsert(gCP,gTP,4,&parms[0]);
    else
        *ec = FUtil3_VectorInsert(gCP,gTP,3,&parms[0]);
    if (ec->Tag == TYERROR) FrameExit(*ec);
    *ret = parms[1];
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Vector

Return a Vector object of the specified size (argv[0]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Vector with zeros. 
        If too few arguments are specified, fill the Vector with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (makeVector 5)             =>      #(0 0 0 0 0) 
        (makeVector 5 1)           =>      #(1 1 1 1 1) 
        (makeVector 5 1 2 3)       =>      #(1 2 3 1 2) 
        (makeVector 5 1 2 3 . 6)   =>      #(1 2 3 1 2 . 6) 
        
        Also handle special vector requests such as:
  
        (makeVector Bit: 5)
        (makeVector Byte: 5 1)
        (makeVector Integer: 5 1)

#endif

TVAL FMake_Vector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM						size = 0;
NUM						sizeIndex = 1;
NUM                     vectorIndex = 0;
NUM                     startIndex = sizeIndex + 1;
NUM                     argIndex = sizeIndex + 1;
NUM                     cdrIndex = argc;
TVAL                    ivTval;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareOBJ(TVector,sp);
DeclareOBJ(TBitVector,bp);
DeclareOBJ(TIntVector,ip);
DeclareOBJ(TShtVector,shp);
DeclareOBJ(TLongVector,lhp);
DeclareOBJ(TPcodeVector,pp);
DeclareOBJ(TByteVector,cp);
DeclareOBJ(TNumVector,np);
DeclareOBJ(TFltVector,fp);
DeclareOBJ(TObjVector,ap);
DeclareOBJ(TCpxVector, xp);
EndFrame
 
/*  The first (or second if first is a symbol) argument should be the requested size. */

if (argc == 0)
    {
    size = 0;
	sizeIndex = 0;
	goto Normal;
    }

asTag(ndx) = TYNUM;

/*  Is this a request to construct an object vector? */

if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"object") == 0) || (strcmp(SymbolArray(argv[0]),"Object") == 0)))
    {
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
    ap = TObjVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)ap, size);
    
    /*  Initialize the Object Vector object */
    
    asObject(ret)   = (TObject*)ap;
    asTag(ret)		= TYOBJVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;   

    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)ap;
        asTag(&ivTval) = ap->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            
            asInt(ndx) = vectorIndex++;
        
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP, ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */        
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)ap, argv[cdrIndex]);
            }
        }
    }
else
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"number") == 0) || (strcmp(SymbolArray(argv[0]),"Number") == 0)))
    {   /*  This is a request to construct a number vector. */
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }    
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);        
    np = TNumVector_New(gCP, gTP);
    asObject(&ivTval) = (TObject*)np;
    asTag(&ivTval) = np->itsObjectType;
    TNumVector_SetMaxIndex(gCP, gTP, ivTval, size);
    
    /*  Initialize the Number Vector object */    
    asObject(ret)   = (TObject*)np;
    asTag(ret)		= TYNUMVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;

    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)np;
        asTag(&ivTval) = np->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];            
            asInt(ndx) = vectorIndex++;        
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP, ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)np, argv[cdrIndex]);
            }
        }
    }
else
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"float") == 0) || (strcmp(SymbolArray(argv[0]),"Float") == 0)))
    {
    /*  This is a request to construct a float vector. */
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);  
    fp = TFltVector_New(gCP,gTP);
    asObject(&ivTval) = (TObject*)fp;
    asTag(&ivTval) = fp->itsObjectType;
    TFltVector_SetMaxIndex(gCP,gTP,ivTval,size);
    
    /*  Initialize the Float Vector object */
    asObject(ret)   = (TObject*)fp;
    asTag(ret)		= TYFLTVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;

    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)fp;
        asTag(&ivTval) = fp->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            asInt(ndx) = vectorIndex++;
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP, ivTval, *ndx, *fill);
            }

        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)fp, argv[cdrIndex]);
            }
        }
    }
else
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"Character") == 0) || (strcmp(SymbolArray(argv[0]),"byte") == 0) || (strcmp(SymbolArray(argv[0]),"Byte") == 0)))
    { /*  This is a request to construct a byte vector. */
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)  
    cp = TByteVector_New(gCP,gTP);
    asObject(&ivTval) = (TObject*)cp;
    asTag(&ivTval) = cp->itsObjectType;
    TByteVector_SetMaxIndex(gCP,gTP,ivTval,size);

    /*  Initialize the Byte Vector object */
    asObject(ret)   = (TObject*)cp;
    asTag(ret)		= TYBYTEVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;

    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)cp;
        asTag(&ivTval) = cp->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            asInt(ndx) = vectorIndex++;
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP, ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)cp, argv[cdrIndex]);
            }
        }
    }
else
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"integer") == 0) || (strcmp(SymbolArray(argv[0]),"Integer") == 0)))
    {    /*  This is a request to construct an integer vector. */
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    ip = TIntVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)ip, size);

    /*  Initialize the Bit Vector object */
    asObject(ret)   = (TObject*)ip;
    asTag(ret)		= TYINTVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;

    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)ip;
        asTag(&ivTval) = ip->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            
            asInt(ndx) = vectorIndex++;
        
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP,  ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)ip, argv[cdrIndex]);
            }
        }
    }
else
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"short") == 0) || (strcmp(SymbolArray(argv[0]),"Short") == 0)))
    {    /*  This is a request to construct a short vector. */
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    shp = TShtVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)shp, size);

    /*  Initialize the Short Vector object */
    asObject(ret)   = (TObject*)shp;
    asTag(ret)		= TYSHORTVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;

    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)shp;
        asTag(&ivTval) = shp->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            
            asInt(ndx) = vectorIndex++;
        
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP,  ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)shp, argv[cdrIndex]);
            }
        }
    }
else
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"long") == 0) || (strcmp(SymbolArray(argv[0]),"Long") == 0)))
    {    /*  This is a request to construct a long vector. */
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    lhp = TLongVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)lhp, size);

    /*  Initialize the Long Vector object */
    asObject(ret)   = (TObject*)lhp;
    asTag(ret)		= TYLONGVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;

    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)lhp;
        asTag(&ivTval) = lhp->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            
            asInt(ndx) = vectorIndex++;
        
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP,  ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)lhp, argv[cdrIndex]);
            }
        }
    }
else
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"pcode") == 0) || (strcmp(SymbolArray(argv[0]),"Pcode") == 0)))
    {  /*  This is a request to construct a Pcode vector. */
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    pp = TPcodeVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)pp, size);

    /*  Initialize the Pcode Vector object */
    asObject(ret)   = (TObject*)pp;
    asTag(ret)		= TYPCODEVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;
    
    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)pp;
        asTag(&ivTval) = pp->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            
            asInt(ndx) = vectorIndex++;
        
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP, ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)pp, argv[cdrIndex]);
            }
        }
    }
else if (argv[0].Tag == TYSYMBOL &&
			(argv[0].u.Symbol == TSymbol_MakeUnique(gCP,gTP,"Complex") ||
			 argv[0].u.Symbol == TSymbol_MakeUnique(gCP,gTP,"complex") ||
			 argv[0].u.Symbol == TSymbol_MakeUnique(gCP,gTP,"cpx")))
	{	// Strip off the symbol before calling TCpxVector_MakeNew
		*ret = TCpxVector_MakeNew(gCP, gTP, argc - 1, &argv[1]);
	}
else
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"bit") == 0) || (strcmp(SymbolArray(argv[0]),"Bit") == 0)))
    {
    /*  This is a request to construct a bit vector. */
	if (argc == 1)
		{
		size = 0;
		}
	else
	if ((argc >= 2) && 
		(argv[1].Tag == TYTEXT) && 
		(argv[1].u.Text[0] == '~') &&
		(argv[1].u.Text[1] == 0))
		{
		size = argc - 2;
		}
	else
	if ((argc >= 2) && (isNumIndex(&argv[1])))
		{
		size = asNumIndex(&argv[1]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    bp = TBitVector_New(gCP,gTP);
    asObject(&ivTval) = (TObject*)bp;
    asTag(&ivTval) = bp->itsObjectType;
    TBitVector_SetMaxIndex(gCP,gTP,ivTval, size);

    /*  Initialize the Bit Vector object */
    asObject(ret)   = (TObject*)bp;
    asTag(ret)		= TYBITVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    vectorIndex             = 0;
    argIndex                = 2;
    cdrIndex                = argc;

    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > 2)
        {
        asObject(&ivTval) = (TObject*)bp;
        asTag(&ivTval) = bp->itsObjectType;
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = 2;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = 2;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            
            asInt(ndx) = vectorIndex++;
        
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP,  ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)bp, argv[cdrIndex]);
            }
        }
    }
else        /*  Construct a normal Vector object */
    {
	if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"normal") == 0) || (strcmp(SymbolArray(argv[0]),"Word") == 0)))
        startIndex = 1;
    else
        startIndex = 0;
	if (argc == startIndex)
		{
		size = 0;
		}
	else
	if ((argc >= (startIndex + 1)) && 
		(argv[startIndex].Tag == TYTEXT) && 
		(argv[startIndex].u.Text[0] == '~') &&
		(argv[startIndex].u.Text[1] == 0))
		{
		size = argc - (startIndex + 1);
		}
	else
	if ((argc >= (startIndex + 1)) && (isNumIndex(&argv[startIndex])))
		{
		size = asNumIndex(&argv[startIndex]);
		}
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        
	Normal:
    sp = TVector_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)sp, size);
    
    /*  Initialize the Vector object */
    asObject(ret)   = (TObject*)sp;
    asTag(ret)		= TYVECTOR;
    asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    argIndex                = ++startIndex;
    vectorIndex             = 0;
    cdrIndex                = argc;
    
    /*  Initialize the vector only if necessary (important time saving). */
    if (argc > startIndex)
        {
        asObject(&ivTval) = (TObject*)sp;
        asTag(&ivTval) = sp->itsObjectType;
        
        while (vectorIndex < size)
            {
            if (argIndex >= argc) argIndex = startIndex;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex + 1;
                argIndex = startIndex;
                }
            if (argIndex < argc) *fill = argv[argIndex++];
            
            asInt(ndx) = vectorIndex++;
            (*_TObject_TypeSetIV1(asTag(&ivTval)))(gCP, gTP,  ivTval, *ndx, *fill);
            }
        /*  Save the optional cdr argument */
        if ((argIndex < argc) && 
            (asTag(&argv[argIndex]) == TYPCODE) && 
            (asShort(&argv[argIndex]) == PERIODTOK))
            {
            cdrIndex = argIndex + 1;
            }
        if (cdrIndex < argc)
            {
            FObject_SetCdr(gCP,gTP,(TObject*)sp, argv[cdrIndex]);
            }
        }
    }
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FMake_Matrix

Return a Matrix object of the specified rank and dimensions (argv[0...]) with the specified
initial values and optional cdr value.  

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, fill the Matrix with zeros. 
        If too few arguments are specified, fill the Matrix with repeating
            patterns of the specified initializers.
        If the special character '.' is encountered, assign the cdr value. 
            
        (makeMatrix "~" 1 2 3)     =>      #(mat| 1 2 3) 
        (makeMatrix 5)             =>      #(mat| 0 0 0 0 0) 
        (makeMatrix 5 1)           =>      #(mat| 1 1 1 1 1) 
        (makeMatrix 5 1 2 3)       =>      #(mat| 1 2 3 1 2) 
        (makeMatrix 5 1 2 3 . 6)   =>      #(mat| 1 2 3 1 2 . 6) 
        
        Also handle special vector requests such as:
  
        (makeMatrix number: 3 2 3 4)

#endif

TVAL FMake_Matrix(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                     rank = 1;
NUM                     size = 0;
NUM                     dimensions[_MAXDIMENSIONS];
NUM                     dn = 0;
NUM                     matrixIndex = 0;
NUM                     startIndex = 0;
NUM                     argIndex = 0;
NUM                     cdrIndex = argc;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ndx);
DeclareTVAL(fill);
DeclareTVAL(ivTval);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
EndFrame
 
/*  The first argument should be the requested rank. */
/*  Note: If there is no rank argument, then the matrix defaults to a rank of one. */

asTag(ndx) = TYNUM;
for (dn = 0; dn < _MAXDIMENSIONS; ++dn) dimensions[dn] = 1;
if (argc == 0)
    {
    size = 1;
    rank = 1;
	startIndex = 0;	
	goto Normal;
    }

/* Is this a request to construct a number matrix? */
if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"number") == 0) || (strcmp(SymbolArray(argv[0]),"Number") == 0)))
    {
	/* Get the rank and size of the new NumMatrix. */
	/* Note: This may be either size alone or rank dim1 ... dim3. */

    startIndex = 1;
  	if ((argc - startIndex) == 0)
		{
		/* No NumMatrix size was specified. Default to size zero, rank one. */
		size = 0;
		rank = 1;
		}
	else
	if ((argc - startIndex) == 1)
		{
		/* Only a Matrix size was specified. Default to rank of one. */
		if ((argv[startIndex].Tag == TYTEXT) && 
			(argv[startIndex].u.Text[0] == '~') &&
			(argv[startIndex].u.Text[1] == 0))
			{
			size = argc - startIndex - 1;
			}
		else
		if (isNumIndex(&argv[startIndex])) 
			{
			size = asNumIndex(&argv[startIndex]);
			}
		else
			{
			FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
			}		
		++startIndex;
		if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		rank = 1;
		}
	else
	if ((argv[startIndex].Tag == TYTEXT) && 
		(argv[startIndex].u.Text[0] == '~') &&
		(argv[startIndex].u.Text[1] == 0))
		{
		size = argc - startIndex - 1;
		++startIndex;
		if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		rank = 1;
		}
	else
		{
		/* A NumMatrix rank and a dimension list have been specified. */
		if (!isNumIndex(&argv[startIndex])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		rank = asNumIndex(&argv[startIndex]);
		++startIndex;
		if ((rank < 1) || (rank > _MAXDIMENSIONS))  FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		if ((argc - startIndex) < rank)  FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		size = 1;
		for (dn = 0; dn < rank; ++dn) 
			{
			if (!isNumIndex(&argv[startIndex])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
			dimensions[dn] = asNumIndex(&argv[startIndex]);
			++startIndex;
			if (dimensions[dn] <= 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
			size *= dimensions[dn];
			}
		if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
    
    /*  Create and initialize the NumMatrix object */
    
    np = TNumMatrix_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)np, size);    
    asObject(ret)   = (TObject*)np;
    asTag(ret)		= TYNUMMATRIX;

    /*  Set the NumMatrix rank and dimension list values. */

	asObject(ivTval) = (TObject*)np;
    asTag(ivTval) = np->itsObjectType;
	np->itsRank = rank;
	for (dn = 0; dn < _MAXDIMENSIONS; ++dn) np->itsDimensions[dn] = dimensions[dn];

    /*  Initialize the NumMatrix values only if necessary (important time saving). */
	asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    argIndex                = startIndex;
    matrixIndex             = 0;
    cdrIndex                = argc;
    
    if (argc > startIndex)
        {
        asObject(ivTval) = (TObject*)np;
        asTag(ivTval) = np->itsObjectType;
        
        while (matrixIndex < size)
            {
            if (argIndex >= argc) argIndex = startIndex;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex;
                argIndex = startIndex;
                }
            if (argIndex < argc) *fill = argv[argIndex++];            
            asInt(ndx) = matrixIndex++;        
            (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP, gTP,  *ivTval, *ndx, *fill);
            }         
        
        /*  Save the optional cdr argument */
        if ((cdrIndex < argc) && 
            (asTag(&argv[cdrIndex]) == TYPCODE) && 
            (asShort(&argv[cdrIndex]) == PERIODTOK))
            {
            FObject_SetCdr(gCP,gTP,(TObject*)np, argv[cdrIndex]);
            }
        }
    }
else        /*  Construct a normal Matrix object */
    {
	if  ((argv[0].Tag == TYSYMBOL) && ((strcmp(SymbolArray(argv[0]),"normal") == 0) || (strcmp(SymbolArray(argv[0]),"Word") == 0)))
        startIndex = 1;
    else
        startIndex = 0;
        
	/* Get the rank and size of the new matrix. */
	/* Note: This may be either size alone or rank dim1 ... dim3. */

	if ((argc - startIndex) == 0)
		{
		/* No Matrix size was specified. Default to size zero, rank one. */
		size = 0;
		rank = 1;
		}
	else
	if ((argc - startIndex) == 1)
		{
		/* Only a Matrix size was specified. Default to rank of one. */
		if ((argv[startIndex].Tag == TYTEXT) && 
			(argv[startIndex].u.Text[0] == '~') &&
			(argv[startIndex].u.Text[1] == 0))
			{
			size = argc - startIndex - 1;
			}
		else
		if (isNumIndex(&argv[startIndex])) 
			{
			size = asNumIndex(&argv[startIndex]);
			}
		else
			{
			FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
			}		
		++startIndex;
		if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		rank = 1;
		}
	else
	if ((argv[startIndex].Tag == TYTEXT) && 
		(argv[startIndex].u.Text[0] == '~') &&
		(argv[startIndex].u.Text[1] == 0))
		{
		size = argc - startIndex - 1;
		++startIndex;
		if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		rank = 1;
		}
	else
		{
		/* A Matrix rank and a dimension list have been specified. */
		if (!isNumIndex(&argv[startIndex])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		rank = asNumIndex(&argv[startIndex]);
		++startIndex;
		if ((rank < 1) || (rank > _MAXDIMENSIONS))  FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		if ((argc - startIndex) < rank)  FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		size = 1;
		for (dn = 0; dn < rank; ++dn) 
			{
			if (!isNumIndex(&argv[startIndex])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
			dimensions[dn] = asNumIndex(&argv[startIndex]);
			++startIndex;
			if (dimensions[dn] <= 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
			size *= dimensions[dn];
			}
		if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
    
    /*  Create and initialize the Matrix object */
    
	Normal:
    mp = TMatrix_New(gCP,gTP);
    FObject_SetMaxIndex(gCP,gTP,(TObject*)mp, size);    
    asObject(ret)   = (TObject*)mp;
    asTag(ret)		= TYMATRIX;

    /*  Set the matrix rank and dimension list values. */

	asObject(ivTval) = (TObject*)mp;
    asTag(ivTval) = mp->itsObjectType;
	mp->itsRank = rank;
	for (dn = 0; dn < _MAXDIMENSIONS; ++dn) mp->itsDimensions[dn] = dimensions[dn];

    /*  Initialize the matrix values only if necessary (important time saving). */
	asTag(fill)     = TYNUM;
    asInt(fill)     = 0;
    argIndex                = startIndex;
    matrixIndex             = 0;
    cdrIndex                = argc;
    
    if (argc > startIndex)
        {
        asObject(ivTval) = (TObject*)mp;
        asTag(ivTval) = mp->itsObjectType;
        
        while (matrixIndex < size)
            {
            if (argIndex >= argc) argIndex = startIndex;
            if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
                {
                cdrIndex = argIndex;
                argIndex = startIndex;
                }
            if (argIndex < argc) *fill = argv[argIndex++];            
            asInt(ndx) = matrixIndex++;        
            (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP, gTP,  *ivTval, *ndx, *fill);
            }         
        
        /*  Save the optional cdr argument */
        if ((cdrIndex < argc) && 
            (asTag(&argv[cdrIndex]) == TYPCODE) && 
            (asShort(&argv[cdrIndex]) == PERIODTOK))
            {
            FObject_SetCdr(gCP,gTP,(TObject*)mp, argv[cdrIndex]);
            }
        }
    }

FrameExit(*ret);
}
