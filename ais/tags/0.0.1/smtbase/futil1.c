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

#define _C_FUTIL1
#define _SMARTBASE

#if 0
FUtil1.c

Implementation of a wide range of utility and support functions which are not associated 
with any specific class of objects but are used to support the SmartLisp environment.

PARENT:             None. 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "futil1.h"
#include    "fmake.h"
#include    "fpred2.h"
#include    "fcompile.h"
#include    "tdiction.h"
#include    "tdirect.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tfltvec.h"
#include    "tintvec.h"
#include    "tobjvec.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "fpred.h"
#include    "fconvert.h"
#include    "fconio.h"
#include    "flisp.h"
#include    "fvmscpt.h"
#include    "futil2.h"
#include    "futil3.h"
#include    "tdatabas.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include	"tcpx.h"
#include	"tcpxvec.h"
#include	"tshortvec.h"
#include	"tlongvec.h"
#include	"fvmscpt.h"

#define _APPLYPARMSMAX  1000

LpTVAL      FUtil1_pParser;    

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_Init

Initialize a part of the Utility portion of the SmartLisp function library.  

#endif

TVAL FUtil1_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FUtil1_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FUtil1_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"apply",(LpFUNC)&FUtil1_Apply);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"balance",(LpFUNC)&FUtil1_Balance);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cons",(LpFUNC)&FUtil1_Cons);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"remove",(LpFUNC)&FUtil1_Delete);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"delq",(LpFUNC)&FUtil1_Delq);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"eval",(LpFUNC)&FUtil1_Eval);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"errorTrap",(LpFUNC)&FUtil1_ErrorTrap);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"gc",(LpFUNC)&FUtil1_Gc);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"procedureEnvironment",(LpFUNC)&FUtil1_ProcedureEnv);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"theStructure",(LpFUNC)&FUtil1_TheEnv);
ExitOnError(*ec);
 
FUtil1_pParser = FSmartbase_GetSymbolValuePtr(gCP,gTP,"_parser",TRUE);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_Balance

The balance Procedure locates the substring (if any) of the source, starting at the 
specified displacement, that is enclosed by parentheses. If the specified source start 
location is not enclosed by parens, the balance procedure returns false. Since the 
SmartLisp language is heavily (like all Lisp dialects) involved with matching open 
and close parens, this procedure can be used often during automated Script editing.

Examples:

    (balance  "(lambda (n) (+ n 10))"  9)   =>  "(n)"
    (balance  "(lambda (n) (+ n 10))"  0)   =>  false

#endif

TVAL FUtil1_Balance(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM                 lSelStart;
NUM                 lSelEnd;
NUM                 lTextEnd;
NUM                 lTmp;
LpCHAR              textPtr;
CHAR                chLast;
CHAR                chFirst;
CHAR                chTmp;
NUM                 bracketCount;
NUM                 braceCount;
NUM                 parenCount;
StartFrame
DeclareTVAL(ret);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Obtain the string pointer and length. */
if (argv[0].Tag == TYTEXT)
    {
    textPtr = &argv[0].u.Text[0];
    lTextEnd = strlen(textPtr);
    }
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    textPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (textPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    lTextEnd = SubLen(argv[0]);
    }
else
if (argv[0].Tag == TYSTRING)
    {
    textPtr = (char*)*asString(&argv[0])->itsCString;
    lTextEnd = strlen(textPtr);
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }   

/*  Obtain the start index. */
if (isNumIndex(&argv[1]))
    {
    lSelStart = asNumIndex(&argv[1]);
    if ((lSelStart < 0) || (lSelStart > lTextEnd))
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    lSelEnd = lSelStart;
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }   

/*  Search forward and backward in the buffer to find the nearest balanceable character */

chFirst = 0;
parenCount = braceCount = bracketCount = 0;
lSelStart = lSelStart ? lSelStart - 1 : lSelStart;
for(lTmp = lSelStart; lTmp >= 0 && chFirst == 0; lTmp--)
    {
    chTmp = *(textPtr + lTmp);
    switch(chTmp)
        {
        case '[':
            if(braceCount == 0)
                {
                chFirst = chTmp;
                lSelStart = lTmp;
                }
            else
                braceCount++;
        break;
        
        case ']':
            braceCount--;
        break;
        
        case '(':
            if(parenCount == 0)
                {
                chFirst = chTmp;
                lSelStart = lTmp;
                }
            else
                parenCount++;
        break;
        
        case ')':
            parenCount--;
        break;
        
        case '{':
            if(bracketCount == 0)
                {
                chFirst = chTmp;
                lSelStart = lTmp;
                }
            else
                bracketCount++;
        break;

        case '}':
            bracketCount--;
        break;
        
        }
    }
    
chLast = 0;
parenCount = braceCount = bracketCount = 0;
for(lTmp = lSelEnd; lTmp < lTextEnd && chLast == 0; lTmp++)
    {
    chTmp = *(textPtr + lTmp);
    switch(chTmp)
        {
        case ']':
            if(braceCount == 0)
                {
                chLast = chTmp;
                lSelEnd = lTmp;
                }
            else
                braceCount++;
        break;
        
        case '[':
            braceCount--;
        break;
        
        case ')':
            if(parenCount == 0)
                {
                chLast = chTmp;
                lSelEnd = lTmp;
                }
            else
                parenCount++;
        break;
        
        case '(':
            parenCount--;
        break;
        
        case '}':
            if(bracketCount == 0)
                {
                chLast = chTmp;
                lSelEnd = lTmp;
                }
            else
                bracketCount++;
        break;

        case '{':
            bracketCount--;
        break;
        
        }
    }

    
if ((chFirst != 0) && (chLast != 0))
    {
    if ((chFirst == '(' && chLast != ')' ) ||
        (chFirst == '[' && chLast != ']' ) ||
        (chFirst == '{' && chLast != '}' ))
        {
        *ret = gCP->TObject_FALSE;
        }
    else
        {
        lSelEnd   = lSelEnd >= lTextEnd ? lTextEnd -1 : lSelEnd + 1;
        *ret = FSmartbase_CnvFromSubstring(gCP,gTP,textPtr,lSelStart,lSelEnd-lSelStart);
        }
    }
else
    {
    *ret = gCP->TObject_FALSE;
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_Cons

The FUtil1_Cons function returns a Pair object whose Car is the first argument
and whose Cdr is the second argument.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FUtil1_Cons(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,pp);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Create a new Pair object.  */

pp = TPair_New(gCP,gTP);
asObject(ret) = (TObject*)pp;
asTag(ret) = TYPAIR;
pp->itsCar = argv[0];
if(asTag(&argv[1]) != TYSYMBOL || asSymbol(&argv[1]) != gCP->TLambda_nil)
    pp->itsCdr = argv[1];

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_Delete

The FUtil1_Delete function returns a list {argv[1]} with all occurrences of 
the specified object {argv[0]} removed. The FUtil1_Delete function uses isEqual 
to compare {argv[0]} with each element in {argv[1]}. The {argv[1]} may be a 
proper list, a vector, or a Structure. 
Several examples follow.

    (define  A  `(A  B  C))         =>  (A  B  C)
    (define  B  `(A  B  C))         =>  (A  B  C)
    (define  C  (list  A  `C  B))   =>  ((A  B  C)  C  (A  B  C))
    (remove   A  C )                =>  (C )
    (setq  C  (list  A  `C  B))     =>  ((A  B  C)  C  (A  B  C))
    (define  A  #(1  2  3))         =>  #(1  2  3)
    (remove  1  A )                 =>  #(2  3)
    (setq  C  #{`A 3  `B 4  `C  5}) =>  #(`A 3  `B 4  `C 5)
    (remove  `B  C)                 =>  #(`A 3  `C 5)

#endif

TVAL FUtil1_Delete(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
NUM                     size;
NUM                     argIndex;
NUM                     indexOf;
TVAL                    parmv[2];
LpTVAL                  target;
LpTVAL                  source;
NUM						cn;
StartFrame
DeclareOBJ(TPair,pp);
DeclareOBJ(TPair,pp2);
DeclareOBJ(TVector,vp);
DeclareOBJ(TVector,vp2);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TMatrix,mp2);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TNumMatrix,np2);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TBitVector,vBitt);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TByteVector,vBytet);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TNumVector,vNumt);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TFltVector,vFltt);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TIntVector,vIntt);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TShtVector,vShtt);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TLongVector,vLngt);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TObjVector,vObjt);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TStructure,ep2);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDictionary,dp2);
DeclareOBJ(TDirectory,xp);
DeclareOBJ(TDirectory,xp2);
DeclareOBJ(TCpxVector,vCpxp);
DeclareOBJ(TCpxVector,vCpxt);
DeclareTVAL(ret);
DeclareTVAL(result);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVAL(ivTval);
EndFrame


/*  Make sure there are two arguments and that the second is not null! */

*ret = gCP->Tval_VOID;
if ((argc != 2) || (isNullTval(&argv[1]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
parmv[0] = argv[0];
target = ret;
source = &argv[1];

switch(asTag(source))
    {
    case TYPAIR:
        while ((!isNullTval(source)) && (asTag(source) == TYPAIR))
            {
            pp = (TPair*)asObject(source);
            parmv[1] = pp->itsCar;
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                pp2 = TPair_New(gCP,gTP);
                asObject(target) = (TObject*)pp2;
                asTag(target) = TYPAIR;
                pp2->itsCar = pp->itsCar;
                target = &pp2->itsCdr;
                *target = gCP->Tval_VOID;
                }
            source = &pp->itsCdr;
            }
        FrameExit(*ret);
    break;
    
    case TYVECTOR:
        vp = (TVector*)asObject(source);
        size = vp->itsMaxItemIndex;
        vp2 = TVector_New(gCP,gTP);
        vp2->itsCdr = vp->itsCdr;
        asTag(target) = TYVECTOR;
        asObject(target) = (TObject*)vp2;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            parmv[1] = atHMTval(vp->itsTvalArray,indexOf);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vp2,argIndex+1);
                atHMTval(vp2->itsTvalArray,argIndex++) = atHMTval(vp->itsTvalArray,indexOf);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYMATRIX:
        mp = (TMatrix*)asObject(source);
		if (mp->itsRank != 1)
			{
			DeleteBad:
			*ec = TERROR("delete: Matrix must be of rank one!");
			FrameExit(*ec);
			}
        size = mp->itsMaxItemIndex;
        mp2 = TMatrix_New(gCP,gTP);
        mp2->itsCdr = mp->itsCdr;
		mp2->itsRank = 1;
		for (cn=0;cn<_MAXDIMENSIONS;++cn) mp2->itsDimensions[cn] = 1;
		mp2->itsDimensions[0] = 0;
        asTag(target) = TYMATRIX;
        asObject(target) = (TObject*)mp2;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            parmv[1] = atHMTval(mp->itsTvalMatrix,indexOf);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)mp2,argIndex+1);
                atHMTval(mp2->itsTvalMatrix,argIndex++) = atHMTval(mp->itsTvalMatrix,indexOf);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYNUMMATRIX:
        np = (TNumMatrix*)asObject(source);
		if (np->itsRank != 1) goto DeleteBad;
        size = np->itsMaxItemIndex;
        np2 = TNumMatrix_New(gCP,gTP);
        np2->itsCdr = np->itsCdr;
		np2->itsRank = 1;
		for (cn=0;cn<_MAXDIMENSIONS;++cn) np2->itsDimensions[cn] = 1;
		np2->itsDimensions[0] = 0;
        asTag(target) = TYNUMMATRIX;
        asObject(target) = (TObject*)np2;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            parmv[1] = TREAL(atHMReal(np->itsRealMatrix,indexOf));
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)np2,argIndex+1);
                atHMReal(np2->itsRealMatrix,argIndex++) = atHMReal(np->itsRealMatrix,indexOf);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYBITVECTOR:
        vBitp = (TBitVector*)asObject(source);
        size = vBitp->itsMaxItemIndex;
        vBitt = TBitVector_New(gCP,gTP);
        vBitt->itsCdr = vBitp->itsCdr;
        asObject(target) = (TObject*)vBitt;
        asTag(target) = TYBITVECTOR;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for ( asObject(ivTval) = (TObject*)vBitt,
        asTag(ivTval) = vBitt->itsObjectType; 
        indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vBitt,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP, *ivTval,*ndx2 , parmv[1]);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYBYTEVECTOR:
        vBytep = (TByteVector*)asObject(source);
        size = vBytep->itsMaxItemIndex;
        vBytet = TByteVector_New(gCP,gTP);
        vBytet->itsCdr = vBytep->itsCdr;
        asObject(target) = (TObject*)vBytet;
        asTag(target) = TYBYTEVECTOR;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for (asObject(ivTval) = (TObject*)vBytet,
        asTag(ivTval) = vBytet->itsObjectType; 
        indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vBytet,argIndex+1);

                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))( gCP,gTP,*ivTval,*ndx2,parmv[1]);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYINTVECTOR:
        vIntp = (TIntVector*)asObject(source);
        size = vIntp->itsMaxItemIndex;
        vIntt = TIntVector_New(gCP,gTP);
        vIntt->itsCdr = vIntp->itsCdr;
        asObject(target) = (TObject*)vIntt;
        asTag(target) = TYINTVECTOR;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for( asObject(ivTval) = (TObject*)vIntt,
        asTag(ivTval) = vIntt->itsObjectType; indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vIntt,argIndex+1);

                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,parmv[1]);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYSHORTVECTOR:
        vShtp = (TShtVector*)asObject(source);
        size = vShtp->itsMaxItemIndex;
        vShtt = TShtVector_New(gCP,gTP);
        vShtt->itsCdr = vShtp->itsCdr;
        asObject(target) = (TObject*)vShtt;
        asTag(target) = TYSHORTVECTOR;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for( asObject(ivTval) = (TObject*)vShtt,
        asTag(ivTval) = vShtt->itsObjectType; indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vShtt,argIndex+1);

                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,parmv[1]);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYLONGVECTOR:
        vLngp = (TLongVector*)asObject(source);
        size = vLngp->itsMaxItemIndex;
        vLngt = TLongVector_New(gCP,gTP);
        vLngt->itsCdr = vLngp->itsCdr;
        asObject(target) = (TObject*)vLngt;
        asTag(target) = TYLONGVECTOR;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for( asObject(ivTval) = (TObject*)vLngt,
        asTag(ivTval) = vLngt->itsObjectType; indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vLngt,argIndex+1);

                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,parmv[1]);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYNUMVECTOR:
        vNump = (TNumVector*)asObject(source);
        size = vNump->itsMaxItemIndex;
        vNumt = TNumVector_New(gCP, gTP);
        vNumt->itsCdr = vNump->itsCdr;
        asObject(target) = (TObject*)vNumt;
        asTag(target) = TYNUMVECTOR;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vNumt,
        asTag(ivTval) = vNumt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vNumt,argIndex+1);
                
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,  *ivTval,*ndx2 , parmv[1]);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYFLTVECTOR:
        vFltp = (TFltVector*)asObject(source);
        size = vFltp->itsMaxItemIndex;
        vFltt = TFltVector_New(gCP,gTP);
        vFltt->itsCdr = vFltp->itsCdr;
        asObject(target) = (TObject*)vFltt;
        asTag(target) = TYFLTVECTOR;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vFltt,
        asTag(ivTval) = vFltt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vFltt,argIndex+1);
                
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,  *ivTval,*ndx2 , parmv[1]);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYOBJVECTOR:
        vObjp = (TObjVector*)asObject(source);
        size = vObjp->itsMaxItemIndex;
        vObjt = TObjVector_New(gCP,gTP);
        vObjt->itsCdr = vObjp->itsCdr;
        asObject(target) = (TObject*)vObjt;
        asTag(target) = TYOBJVECTOR;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vObjt,
        asTag(ivTval) = vObjt->itsObjectType; indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vObjt,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,  *ivTval,*ndx2 , parmv[1]);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;

    case TYSTRUCTURE:
        ep = (TStructure*)asObject(source);
        size = ep->itsMaxItemIndex;
        ep2 = TStructure_New(gCP,gTP);
        asObject(target) = (TObject*)ep2;
        asTag(target) = TYSTRUCTURE;
        ep2->itsCdr = ep->itsCdr;
        ep2->itsMethods = ep->itsMethods;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            asObject(&parmv[1]) = atHMBind(ep->itsDictionaryArray,indexOf).Key;
            asTag(&parmv[1]) = asObject(&parmv[1])->itsObjectType;
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
				/* if the key does not match, move the binding to the result structure */
                FObject_SetMaxIndex(gCP,gTP,(TObject*)ep2,argIndex+1);
                atHMBind(ep2->itsDictionaryArray,argIndex++) = atHMBind(ep->itsDictionaryArray,indexOf);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;
    
    case TYDICTIONARY:
        dp = (TDictionary*)asObject(source);
        size = dp->itsMaxItemIndex;
        dp2 = TDictionary_New(gCP,gTP);
        asObject(target) = (TObject*)dp2;
        asTag(target) = TYDICTIONARY;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            asObject(&parmv[1]) = atHMBind(dp->itsDictionaryArray,indexOf).Key;
            asTag(&parmv[1]) = asObject(&parmv[1])->itsObjectType;
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                TDictionary_SetMaxIndex(gCP, gTP, *target, argIndex+1);
                atHMBind(dp2->itsDictionaryArray,argIndex++) = atHMBind(dp->itsDictionaryArray,indexOf);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;
    
    case TYDIRECTORY:
        xp = (TDirectory*)asObject(source);
        size = xp->itsMaxItemIndex;
        xp2 = TDirectory_New(gCP,gTP);
        asObject(target) = (TObject*)xp2;
        asTag(target) = TYDIRECTORY;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            parmv[1] = atHMPBind(xp->itsDirectoryArray,indexOf).Key;
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                TDirectory_SetMaxIndex(gCP,gTP,*target, argIndex+1);
                atHMPBind(xp2->itsDirectoryArray,argIndex++) = atHMPBind(xp->itsDirectoryArray,indexOf);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;
	case TYCPXVECTOR:
		vCpxp = (TCpxVector*)asObject(source);
		size = vCpxp->itsMaxItemIndex;
		vCpxt = TCpxVector_New(gCP, gTP);
		vCpxt->itsCdr = vCpxp->itsCdr;
		asObject(target) = (TObject*)vCpxt;
		asTag(target) = TYCPXVECTOR;
		indexOf = 0;
		argIndex = 0;
		asTag(ndx1) = TYNUM;
		asTag(ndx2) = TYNUM;
		for(asObject(ivTval) = (TObject*)vCpxt,
		asTag(ivTval) = vCpxt->itsObjectType;indexOf < size;)
		{	asInt(ndx1) = indexOf;
			parmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
			*result = FPredicate2_Equal(gCP,gTP,(NUM)2,&parmv[0]);
			ExitOnError(*result);
			if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
			{	FObject_SetMaxIndex(gCP,gTP,(TObject*)vCpxt,argIndex+1);
				asInt(ndx2) = argIndex++;
				(*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,  *ivTval,*ndx2 , parmv[1]);
			}
			++indexOf;
		}
		if (argIndex <= 0)
			*target = gCP->Tval_VOID;
		FrameExit(*ret);
	break;
    default:
    break;
    }

*ret = gCP->Tval_VOID;
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_Delq

The FUtil1_Delq function returns a list {argv[1]} with all occurrences of 
the specified object {argv[0]} removed. The FUtil1_Delq function uses isEq 
to compare {argv[0]} with each element in {argv[1]}. The {argv[1]} may be a 
proper list, a vector, or a Structure. 
Several examples follow.

    (define  A  `(A  B  C))         =>  (A  B  C)
    (define  B  `(A  B  C))         =>  (A  B  C)
    (define  C  (list  A  `C  B))   =>  ((A  B  C)  C  (A  B  C))
    (delq   A  C )                  =>  (C )
    (setq  C  (list  A  `C  B))     =>  ((A  B  C)  C  (A  B  C))
    (define  A  #(1  2  3))         =>  #(1  2  3)
    (delq  1  A )                   =>  #(2  3)
    (setq  C  #{`A 3  `B 4  `C  5}) =>  #(`A 3  `B 4  `C 5)
    (delq  `B  C)                   =>  #(`A 3  `C 5)

#endif

TVAL FUtil1_Delq(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
NUM                 size;
NUM                 argIndex;
NUM                 indexOf;
LpTVAL              target;
LpTVAL              source;
NUM					cn;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,pp);
DeclareOBJ(TPair,pp2);
DeclareOBJ(TVector,vp);
DeclareOBJ(TVector,vp2);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TMatrix,mp2);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TNumMatrix,np2);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TBitVector,vBitt);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TByteVector,vBytet);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TNumVector,vNumt);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TFltVector,vFltt);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TIntVector,vIntt);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TShtVector,vShtt);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TLongVector,vLngt);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TObjVector,vObjt);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TStructure,ep2);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDictionary,dp2);
DeclareOBJ(TDirectory,xp);
DeclareOBJ(TDirectory,xp2);
DeclareOBJ(TCpxVector,vCpxp);
DeclareOBJ(TCpxVector,vCpxt);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVAL(result);
DeclareTVAL(ivTval);
DeclareTVALArray(prmv,2);

EndFrame
 
/*  Make sure there are two arguments and that the second is not null! */

*ret = gCP->Tval_VOID;
if ((argc != 2) || (isNullTval(&argv[1]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
prmv[0] = argv[0];
target = ret;
source = &argv[1];

switch(asTag(source))
    {
    case TYPAIR:
        while ((!isNullTval(source)) && (asTag(source) == TYPAIR))
            {
            pp = (TPair*)asObject(source);
            prmv[1] = pp->itsCar;
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0] );
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                pp2 = TPair_New(gCP,gTP);
                asObject(target) = (TObject*)pp2;
                asTag(target) = TYPAIR;
                pp2->itsCar = pp->itsCar;
                target = &pp2->itsCdr;
                *target = gCP->Tval_VOID;
                }
            source = &pp->itsCdr;
            }
        FrameExit(*ret);
    break;
    
    case TYVECTOR:
        vp = (TVector*)asObject(source);
        size = vp->itsMaxItemIndex;
        vp2 = TVector_New(gCP,gTP);
        asObject(target) = (TObject*)vp2;
        asTag(target) = TYVECTOR;
        vp2->itsCdr = vp->itsCdr;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            prmv[1] = atHMTval(vp->itsTvalArray,indexOf);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vp2,argIndex+1);
                atHMTval(vp2->itsTvalArray,argIndex++) = atHMTval(vp->itsTvalArray,indexOf);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYMATRIX:
        mp = (TMatrix*)asObject(source);
        size = mp->itsMaxItemIndex;
        mp2 = TMatrix_New(gCP,gTP);
        asObject(target) = (TObject*)mp2;
        asTag(target) = TYMATRIX;
        mp2->itsCdr = mp->itsCdr;
		mp2->itsRank = 1;
		for (cn=0;cn<_MAXDIMENSIONS;++cn) mp2->itsDimensions[cn] = 1;
		mp2->itsDimensions[0] = 0;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            prmv[1] = atHMTval(mp->itsTvalMatrix,indexOf);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)mp2,argIndex+1);
                atHMTval(mp2->itsTvalMatrix,argIndex++) = atHMTval(mp->itsTvalMatrix,indexOf);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYNUMMATRIX:
        np = (TNumMatrix*)asObject(source);
        size = np->itsMaxItemIndex;
        np2 = TNumMatrix_New(gCP,gTP);
        asObject(target) = (TObject*)np2;
        asTag(target) = TYNUMMATRIX;
        np2->itsCdr = np->itsCdr;
		np2->itsRank = 1;
		for (cn=0;cn<_MAXDIMENSIONS;++cn) np2->itsDimensions[cn] = 1;
		np2->itsDimensions[0] = 0;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            prmv[1] = TREAL(atHMReal(np->itsRealMatrix,indexOf));
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)np2,argIndex+1);
                atHMReal(np2->itsRealMatrix,argIndex++) = atHMReal(np->itsRealMatrix,indexOf);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYBITVECTOR:
        vBitp = (TBitVector*)asObject(source);
        size = vBitp->itsMaxItemIndex;
        vBitt = TBitVector_New(gCP,gTP);
        asObject(target) = (TObject*)vBitt;
        asTag(target) = TYBITVECTOR;
        vBitt->itsCdr = vBitp->itsCdr;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vBitt,
        asTag(ivTval) = vBitt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vBitt,argIndex+1);

                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))( gCP,gTP, *ivTval,*ndx2,  prmv[1]);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYBYTEVECTOR:
        vBytep = (TByteVector*)asObject(source);
        size = vBytep->itsMaxItemIndex;
        vBytet = TByteVector_New(gCP,gTP);
        asObject(target) = (TObject*)vBytet;
        asTag(target) = TYBYTEVECTOR;
        vBytet->itsCdr = vBytep->itsCdr;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vBytet,
        asTag(ivTval) = vBytet->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vBytet,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,prmv[1]);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYINTVECTOR:
        vIntp = (TIntVector*)asObject(source);
        size = vIntp->itsMaxItemIndex;
        vIntt = TIntVector_New(gCP,gTP);
        asObject(target) = (TObject*)vIntt;
        asTag(target) = TYINTVECTOR;
        vIntt->itsCdr = vIntp->itsCdr;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vIntt,
        asTag(ivTval) = vIntt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vIntt,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,prmv[1]);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYSHORTVECTOR:
        vShtp = (TShtVector*)asObject(source);
        size = vShtp->itsMaxItemIndex;
        vShtt = TShtVector_New(gCP,gTP);
        asObject(target) = (TObject*)vShtt;
        asTag(target) = TYSHORTVECTOR;
        vShtt->itsCdr = vShtp->itsCdr;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vShtt,
        asTag(ivTval) = vShtt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vShtt,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,prmv[1]);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    
    case TYLONGVECTOR:
        vLngp = (TLongVector*)asObject(source);
        size = vLngp->itsMaxItemIndex;
        vLngt = TLongVector_New(gCP,gTP);
        asObject(target) = (TObject*)vLngt;
        asTag(target) = TYLONGVECTOR;
        vLngt->itsCdr = vLngp->itsCdr;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vLngt,
        asTag(ivTval) = vLngt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vLngt,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,prmv[1]);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYNUMVECTOR:
        vNump = (TNumVector*)asObject(source);
        size = vNump->itsMaxItemIndex;
        vNumt = TNumVector_New(gCP, gTP);
        asObject(target) = (TObject*)vNumt;
        asTag(target) = TYNUMVECTOR;
        vNumt->itsCdr = vNump->itsCdr;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        
        for(asObject(ivTval) = (TObject*)vNumt,
        asTag(ivTval) = vNumt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vNumt,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,prmv[1]);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYFLTVECTOR:
        vFltp = (TFltVector*)asObject(source);
        size = vFltp->itsMaxItemIndex;
        vFltt = TFltVector_New(gCP,gTP);
        asObject(target) = (TObject*)vFltt;
        asTag(target) = TYFLTVECTOR;
        vFltt->itsCdr = vFltp->itsCdr;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        
        for(asObject(ivTval) = (TObject*)vFltt,
        asTag(ivTval) = vFltt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vFltt,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,prmv[1]);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYOBJVECTOR:
        vObjp = (TObjVector*)asObject(source);
        size = vObjp->itsMaxItemIndex;
        vObjt = TObjVector_New(gCP,gTP);
        asObject(target) = (TObject*)vObjt;
        asTag(target) = TYOBJVECTOR;
        vObjt->itsCdr = vObjp->itsCdr;
        indexOf = 0;
        argIndex = 0;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        for(asObject(ivTval) = (TObject*)vObjt,
        asTag(ivTval) = vObjt->itsObjectType;indexOf < size;)
            {
            asInt(ndx1) = indexOf;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)vObjt,argIndex+1);
                asInt(ndx2) = argIndex++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,prmv[1]);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    case TYSTRUCTURE:
        ep = (TStructure*)asObject(source);
        size = ep->itsMaxItemIndex;
        ep2 = TStructure_New(gCP,gTP);
        asObject(target) = (TObject*)ep2;
        asTag(target) = TYSTRUCTURE;
        ep2->itsCdr = ep->itsCdr;
        ep2->itsMethods = ep->itsMethods;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            asObject(&prmv[1]) = atHMBind(ep->itsDictionaryArray,indexOf).Key;
            asTag(&prmv[1]) = asObject(&prmv[1])->itsObjectType;
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                FObject_SetMaxIndex(gCP,gTP,(TObject*)ep2,argIndex+1);
                atHMBind(ep2->itsDictionaryArray,argIndex++) = atHMBind(ep->itsDictionaryArray,indexOf);
                }
            ++indexOf;
            }
        FrameExit(*ret);
    break;
    
    
    case TYDICTIONARY:
        dp = (TDictionary*)asObject(source);
        size = dp->itsMaxItemIndex;
        dp2 = TDictionary_New(gCP,gTP);
        asObject(target) = (TObject*)dp2;
        asTag(target) = TYDICTIONARY;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            asObject(&prmv[1]) = atHMBind(dp->itsDictionaryArray,indexOf).Key;
            asTag(&prmv[1]) = asObject(&prmv[1])->itsObjectType;
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                TDictionary_SetMaxIndex(gCP, gTP, *target, argIndex+1);
                atHMBind(dp2->itsDictionaryArray,argIndex++) = atHMBind(dp->itsDictionaryArray,indexOf);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;
    
    case TYDIRECTORY:
        xp = (TDirectory*)asObject(source);
        size = xp->itsMaxItemIndex;
        xp2 = TDirectory_New(gCP,gTP);
        asObject(target) = (TObject*)xp2;
        asTag(target) = TYDIRECTORY;
        indexOf = 0;
        argIndex = 0;
        while (indexOf < size)
            {
            prmv[1] = atHMPBind(xp->itsDirectoryArray,indexOf).Key;
            *result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
                {
                TDirectory_SetMaxIndex(gCP, gTP, *target, argIndex+1);
                atHMPBind(xp2->itsDirectoryArray,argIndex++) = atHMPBind(xp->itsDirectoryArray,indexOf);
                }
            ++indexOf;
            }
        if (argIndex <= 0)
            {
            *target = gCP->Tval_VOID;
            }
        FrameExit(*ret);
    break;
	case TYCPXVECTOR:
		vCpxp = (TCpxVector*)asObject(source);
		size = vCpxp->itsMaxItemIndex;
		vCpxt = TCpxVector_New(gCP, gTP);
		asObject(target) = (TObject*)vCpxt;
		asTag(target) = TYCPXVECTOR;
		vCpxt->itsCdr = vCpxp->itsCdr;
		indexOf = 0;
		argIndex = 0;
		asTag(ndx1) = TYNUM;
		asTag(ndx2) = TYNUM;
		for(asObject(ivTval) = (TObject*)vCpxt,
		asTag(ivTval) = vCpxt->itsObjectType;indexOf < size;)
		{	asInt(ndx1) = indexOf;
			prmv[1] = (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP, *source, *ndx1);
			*result = FPredicate_EQ(gCP,gTP,(NUM)2,&prmv[0]);
			ExitOnError(*result);
			if ((asTag(result) == TYBOLE) && (asBool(result) == FALSE))
			{	FObject_SetMaxIndex(gCP,gTP,(TObject*)vCpxt,argIndex+1);
				asInt(ndx2) = argIndex++;
				(*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval,*ndx2,prmv[1]);
			}
			++indexOf;
		}
		FrameExit(*ret);
	break;
    default:
    break;
    }

*ret = gCP->Tval_VOID;
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_ErrorTrap

The FUtil1_ErrorTrap function evaluates the first argument {argv[0]} and 
if the return result is an error, the function evaluates the second argument {argv[1]}.

#endif

TVAL FUtil1_ErrorTrap(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
/*  Make sure there are exactly two arguments. */

if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Eval the first argument and then the second (if necessary). */

*ret = FUtil1_Eval(gCP,gTP,1,&argv[0]);
if (ret->Tag == TYERROR)
    {
    *ret = FUtil1_Eval(gCP,gTP,1,&argv[1]);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_Gc

The gc cProcedure initiates a garbage collection sequence (if there are no arguments).
If there is a single boolean argument, the system garbage collection switch is set
accordingly.

Args:

	(none)				Perform a garbage collection 
	true				Turn on garbage collection (make garbage collection possible)
	false				Turn off garbage collection (make garbage collection impossible)
	compact				Perform garbage collection (with compaction)

#endif

TVAL FUtil1_Gc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
SHORT   oldInhibit = gCP->TObject_GarbageInhibit;
NUM				n;
NUM				N;
NUM				indexOf;
TLambda*			Lambda;
TYPE			type;
TObject*		object;
CHAR			flag;
LpCHAR			destCp;
LpCHAR			fromCp;
POINTER*		handleSlot;
LpMHANDLEINFO	oldHandle;
LpMHANDLEINFO	newHandle;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  There must be zero arguments. */


*ret = gCP->Tval_VOID;
if (argc == 0)
    {
    /* Perform a garbage collection */
    gCP->TObject_GarbageInhibit = 0;
    TObject_MarkAndSweep(gCP,gTP);
    goto Last;
    }
else
if ((argc == 1) && (argv[0].Tag == TYBOLE))
    {
    /* Turn garbage collection on/off (make garbage collection possible/impossible) */
    gCP->TObject_GarbageON = asBool(&argv[0]);
    goto Last;
    }
else
if ((argc == 1) && (argv[0].Tag == TYTEXT) && (strcmp(&argv[0].u.Text[0],"compact") == 0))
    {
    goto GcCompact;
    }
else
if ((argc == 1) && (argv[0].Tag == TYSTRING) && (strcmp(CharArray(argv[0]),"compact") == 0))
    {
    goto GcCompact;
    }
else
if ((argc == 1) && (argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"compact") == 0))
    {
    goto GcCompact;
    }
else
if ((argc == 1) && (argv[0].Tag == TYQUOTEDSYMBOL) && (strcmp(SymbolArray(argv[0]),"compact") == 0))
    {
    goto GcCompact;
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
    
    
/* ****************************************************************************/
/*  (gc compact:)                                                             */
/*                                                                            */
/*  Compact all data block referred to by all live objects, except those      */
/*  owned by active Lambdas.                                                    */
/* ****************************************************************************/
GcCompact:

/*  Drop all of the native code vectors for all inactive Lambda objects. */
/*  Note: This frees up a great deal of data block space.               */
gCP->TObject_GarbageInhibit = 0;
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    flag = _TObject_ObjectFlag(indexOf);
    object = _TObject_ObjectByIndex(indexOf);
    if ((flag != _TObject_OfVOID) && (object->itsObjectType == TYLAMBDA))
        {
        Lambda = (TLambda*)object;
        if (Lambda->InUse == 0) 
			{
			Lambda->NativeCodeVector = NIL;
			}
        }
    }

/* Perform a garbage collection to free all unnecessary data blocks. */
TObject_MarkAndSweep(gCP,gTP);

/*  Unmark all objects. */
/*  Note:   The nil object is never garbage collected. */
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    flag = _TObject_ObjectFlag(indexOf);
    object = _TObject_ObjectByIndex(indexOf);
    if (flag != _TObject_OfVOID)
        {
        _TObject_ObjectFlag(indexOf) |= _TObject_OfMARK;
        _TObject_ObjectFlag(indexOf) ^= _TObject_OfMARK;
        }
    }
    
/* Mark all native code vectors owned by active Lambda objects. */
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    flag = _TObject_ObjectFlag(indexOf);
    object = _TObject_ObjectByIndex(indexOf);
    if ((flag != _TObject_OfVOID) && (object->itsObjectType == TYLAMBDA))
        {
        Lambda = (TLambda*)object;
        if (Lambda->InUse != 0) 
			{
			/* TObject_MarkObj(gCP,gTP,_TObject_ObjectByIndex(indexOf)); */
			TObject_MarkObj(gCP,gTP,(TObject*)Lambda->NativeCodeVector);
			TObject_MarkObj(gCP,gTP,(TObject*)Lambda->PcodeVector);
			}
        }
    }
    
/* Perform a memory compaction of all data blocks except those OWNED by MARKED objects. */
/* Note: We unmarked every object, remaining after garbage collection, and then we      */
/*       re-marked only the code vectors of currently active Lambdas. Now any MARKED     */
/*       object is either an active code vector or is owned by an active code vector.   */
/*       We do NOT want to move any object which is MARKED. All other objects can be    */
/*       moved if it reduces memory fragmentation to do so.                             */ 
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    flag = _TObject_ObjectFlag(indexOf);
    object = (TObject*)&((TLambda*)*gCP->TObject_MainObjectHeaderArray)[indexOf];
	/* ==============>We do NOT want to move any object which is MARKED<==============  */
    if ((flag != _TObject_OfVOID) && ((flag & _TObject_OfMARK) == 0))
        {
		type = object->itsObjectType;
        switch (type)
			{
			case TYBYTEVECTOR:
				handleSlot = (POINTER*)&((TByteVector*)object)->itsByteArray;
				if (((TByteVector*)object)->itsImmediatePtr == NULL)
					{
					CompactMe:
					/* Compact data block if possible. */
					oldHandle = (LpMHANDLEINFO)*handleSlot;
					if (oldHandle == NULL) goto DontCompactMe;
					newHandle = (LpMHANDLEINFO)gCP->FMemory_FrameNextMHandle[oldHandle->Frame];
					if ((newHandle == NULL) || (newHandle >= oldHandle)) goto DontCompactMe;
					newHandle = (LpMHANDLEINFO)FMemory_New(gCP,gTP,oldHandle->userSize,TRUE);
					N = oldHandle->userSize;
					fromCp = (LpCHAR)&oldHandle->data[0];
					destCp = (LpCHAR)&newHandle->data[0];
					for (n = 0; n < N; ++n)
						{
						destCp[n] = fromCp[n];
						}
					newHandle->userSize = oldHandle->userSize;
					FMemory_Free(gCP,gTP,(HMemory)oldHandle);
					*handleSlot = (POINTER)newHandle;
					}
				DontCompactMe:
			    break;
			
			case TYBITVECTOR:
				if (((TBitVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TBitVector*)object)->itsBitArray;
					goto CompactMe;
					}
			    break;
			
			case TYCPXVECTOR:
				if (((TCpxVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TCpxVector*)object)->itsCpxArray;
					goto CompactMe;
					}
			    break;
			
			case TYDIRECTORY:
				if (((TDirectory*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TDirectory*)object)->itsDirectoryArray;
					goto CompactMe;
					}
			    break;
			
			case TYDICTIONARY:
				if (((TDictionary*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TDictionary*)object)->itsDictionaryArray;
					goto CompactMe;
					}
			    break;
			
			case TYERROR:
				if (((TError*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TError*)object)->itsCString;
					goto CompactMe;
					}
			    break;
			
			case TYFLTVECTOR:
				if (((TFltVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TFltVector*)object)->itsFloatArray;
					goto CompactMe;
					}
			    break;
			
			case TYINTVECTOR:
				if (((TIntVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TIntVector*)object)->itsIntArray;
					goto CompactMe;
					}
			    break;
			
			case TYMATRIX:
				if (((TMatrix*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TMatrix*)object)->itsTvalMatrix;
					goto CompactMe;
					}
			    break;
			
			case TYNUMMATRIX:
				if (((TNumMatrix*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TNumMatrix*)object)->itsRealMatrix;
					goto CompactMe;
					}
			    break;
			
			case TYNUMVECTOR:
				if (((TNumVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TNumVector*)object)->itsRealArray;
					goto CompactMe;
					}
			    break;
			
			case TYOBJVECTOR:
				if (((TObjVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TObjVector*)object)->itsObjectArray;
					goto CompactMe;
					}
                break;
                
			case TYPCODEVECTOR:
				if (((TPcodeVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TPcodeVector*)object)->itsInstructionArray;
					goto CompactMe;
					}
			    break;
			
			case TYBRICK:
				if (((TBrick*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TBrick*)object)->itsFieldArray;
					goto CompactMe;
					}
			    break;
			
			case TYSHORTVECTOR:
				if (((TShtVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TShtVector*)object)->itsShortArray;
					goto CompactMe;
					}
			    break;
			
			case TYLONGVECTOR:
				if (((TLongVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TLongVector*)object)->itsLongArray;
					goto CompactMe;
					}
			    break;
			
			case TYSYMBOL:
				if (((TSymbol*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TSymbol*)object)->itsCString;
					goto CompactMe;
					}
			    break;
			
			case TYSTRING:
				if (((TString*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TString*)object)->itsCString;
					goto CompactMe;
					}
			    break;
			
			case TYSTRUCTURE:
				if (((TStructure*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TStructure*)object)->itsDictionaryArray;
					goto CompactMe;
					}
			    break;
			
			case TYVECTOR:
				if (((TVector*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TVector*)object)->itsTvalArray;
					goto CompactMe;
					}
			    break;
			
			case TYWORKSPACE:
				if (((TWorkspace*)object)->itsImmediatePtr == NULL)
					{
					handleSlot = (POINTER*)&((TWorkspace*)object)->itsObjectArray;
					goto CompactMe;
					}
			    break;
			
			default:
				break;
			}
        }
    }

Last:
gCP->TObject_GarbageInhibit = oldInhibit;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_ProcedureEnv

The FUtil1_ProcedureEnv function returns the permanent variable Structure for the
argument. There can be only one argument, and it must be a Procedure.

#endif

TVAL FUtil1_ProcedureEnv(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareOBJ(TLambda,mp);
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if ((argc != 1) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  If the argument is a Procedure, return its enclosing Structure. */

if (asTag(&argv[0]) == TYLAMBDA)
    {
    mp = (TLambda*)asObject(&argv[0]);
    *ret = TOBJ(mp->PersistantVariables);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_TheEnv

The FUtil1_TheEnv function returns the current persistent variable Structure. 

Note:   Exactly zero arguments are expected, anything else is an error.

#endif

TVAL FUtil1_TheEnv(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
argv = argv; // NOOP to hide unused parameter warning message
*ret = gCP->Tval_VOID;
if (argc != 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

if (gTP->TLambda_CurrentProcedure == NULL)
    {
    asTag(ret) = TYGLOBALS;
    }
else
    {
    *ret = TOBJ(gTP->TLambda_CurrentProcedure->PersistantVariables);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FUtil1_CmpConstants

This procedure is intended to determine if two SmartLisp constants are identical.

Compare a left value with a right value and return the following codes:

    HIGH                        1       (left > right)
    EQUAL                       0       (left = right)
    LOW                         -1      (left < right)

#endif

TVAL FUtil1_CmpConstants(LpXCONTEXT gCP,LpTHREAD gTP, TVAL left, TVAL right)
{
LpF2TVALS   aFunction;
REAL        rtemp;
StartFrame
DeclareTVAL(result);
DeclareOBJ(TCpx, lp);
DeclareOBJ(TCpx, rp);
EndFrame

result->Tag = TYCOMPARE;

/*  First compare each constant's tags. */

if (left.Tag < right.Tag)
    result->u.Compare = LOW;
else
if (left.Tag > right.Tag)
    result->u.Compare = HIGH;
else
switch (left.Tag)
    {
    case TYVOID:
        result->u.Compare = EQUAL;
        break;
    
    case TYTEXT:
        rtemp = strcmp((char*)&asText(&left)[0], (char*)&asText(&right)[0]);
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
        break;
        
    case TYSTRING:
        rtemp = strcmp((char *)&atHMChar(((TString*)asObject(&left))->itsCString,0),
                       (char *)&atHMChar(((TString*)asObject(&right))->itsCString,0));
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
        break;
        
    case TYSYMBOL:
        rtemp = left.u.Obj - right.u.Obj;
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
        break;
        
    case TYQUOTEDSYMBOL:
        rtemp = left.u.Obj - right.u.Obj;
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
		if (result->u.Compare == EQUAL) 
			{
			rtemp = left.QuoteCnt - right.QuoteCnt;
			result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
			}
        break;
        
    case TYNUM:
        rtemp = left.u.Int - right.u.Int;
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
        break;
        
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        rtemp = left.u.Real - right.u.Real;
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
        break;
        
    case TYCHAR:
        rtemp = left.u.Char - right.u.Char;
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
        break;
    case TYCPX:
		lp = left.u.Complex;
		rp = right.u.Complex;
        rtemp = (lp->itsReal == rp->itsReal) ? lp->itsImag - rp->itsImag :
			lp->itsReal - rp->itsReal;
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
        break;
    case TYBOLE:
        rtemp = left.u.Bool - right.u.Bool;
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
        break;
        
    case TYQUOTEDPAIR:
        rtemp = left.u.Obj - right.u.Obj;
        result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
		if (result->u.Compare == EQUAL) 
			{
			rtemp = left.QuoteCnt - right.QuoteCnt;
			result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
			}
        break;
        
    default:
        if (_TObject_TypeFlag(left.Tag) == _TObject_TfTOBJECT )
            {
            rtemp = left.u.Obj - right.u.Obj;
            result->u.Compare = (short)(rtemp ? (rtemp > 0 ? HIGH : LOW) : EQUAL);
            }
        else
            {
            aFunction = (LpF2TVALS)_TObject_TypeCompare(asTag(&left));
            *result = ((*aFunction)(gCP,gTP,left, right));
            }
        break;
    }

FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_Eval

The FUtil1_Eval function evaluates the argument {argv[0]} and returns the result. 
The current dynamic environment is used during evaluation. The argument {argv[0]} 
may be a String, a cProcedure, a Procedure, or a Spreadsheet Cell Array. 

Several examples follow.
    
    (define  X  99)                         =>  99
    (define  Y  100)                        =>  100
    (eval  "(+  X  Y)")                     =>  199
    (define  A  (compile  "(+  X  Y)"))     =>  #<Procedure 294>
    (eval  A)                               =>  199

#endif

TVAL FUtil1_Eval(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
LpCHAR              theSource;
CHAR                temp;

StartFrame
DeclareOBJ(TLambda,mp);
DeclareOBJ(TLambda,proc);
DeclareOBJ(TString,aTString);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVAL(argTval);
EndFrame

/*  Make sure there is only one argument. */
*ret = gCP->Tval_VOID;
if ((argc != 1) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (gTP->TLambda_CurrentProcedure != NIL)
    mp = gTP->TLambda_CurrentProcedure;
else
    mp = gTP->TLambda_TheProc;


/*  If the argument is a Symbol, return its global value. */

if (argv[0].Tag == TYSYMBOL)
    {
    *ec = Symbol(argv[0])->itsGlobalValue;
    FrameExit(*ec);
    }

/*  If the argument is a Procedure or a cProcedure, evaluate it. */


if (asTag(&argv[0]) == TYLAMBDA || asTag(&argv[0]) == TYCPROCEDURE)
    {
    FrameExit(FSmartbase_Evalv(gCP,gTP,argv[0],(NUM)argc-1,&argv[1]));
    }


/*  If the argument is a TEXT, evaluate it. */

if (asTag(&argv[0]) == TYTEXT || asTag(&argv[0]) == TYSTRING || asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    proc = TLambda_New(gCP,gTP);
    if (asTag(&argv[0]) == TYTEXT)
        {
        theSource = &asText(&argv[0])[0];
        
        asObject(argTval) = (TObject*)proc;
        asTag(argTval) = proc->itsObjectType;
        *ret = FLisp_CompileMe(gCP, gTP, *argTval, theSource);
		ExitOnError(*ret);
        }
    else if (asTag(&argv[0]) == TYSTRING)
        {
        aTString = (TString*)asObject(&argv[0]);
        asObject(argTval) = (TObject*)proc;
        asTag(argTval) = proc->itsObjectType;
        *ret = FLisp_HCompileMe(gCP, gTP, *argTval, (HMemory)aTString->itsCString);
		ExitOnError(*ret);
        }
    else
        {
        theSource = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (theSource == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        temp = theSource[SubLen(argv[0])];
        theSource[SubLen(argv[0])] = 0;

        asObject(argTval) = (TObject*)proc;
        asTag(argTval) = proc->itsObjectType;
        
        *ret = FLisp_CompileMe(gCP, gTP, *argTval, theSource);
        theSource[SubLen(argv[0])] = temp;
        ExitOnError(*ret);
        }
        
	if (ret->Tag == TYLAMBDA) 
		*ret = _VmEvaluate(asProcedure(ret),(NUM)0,&argv[1]);
    FrameExit(*ret);
    }
    
if (asTag(&argv[0]) == TYPAIR)
    {
    *ret = FMake_List(gCP,gTP,argc,argv);
    ExitOnError(*ret);
    *ret = FCompile_Compile(gCP,gTP,1,ret);
    ExitOnError(*ret);
	if (ret->Tag == TYLAMBDA) 
		*ret = _VmEvaluate(asProcedure(ret),(NUM)0,&argv[1]);
    FrameExit(*ret);
    }
    
if (asTag(&argv[0]) == TYQUOTEDPAIR)
    {
    *ret = argv[0];
    --asQuoteCnt(ret);
	if (asQuoteCnt(ret) <= 0)
		ret->Tag = TYPAIR;
    FrameExit(*ret);
    }
    
*ret = gCP->Tval_VOID;
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil1_Apply

Invokes the Procedure or cProcedure using the specified list as the arguments.

Note:   Exactly two arguments are expected, anything else is an error.
        A maximum of _APPLYPARMSMAX (500) parameters are allowed.

#endif

TVAL FUtil1_Apply(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
LpTVAL              rp;
LpTVAL              arglist = NULL;
NUM                 size;
NUM                 indexOf = 0;
LpREAL				xp;				// -> next pair of numbers in array
StartFrame
DeclareOBJ(TPair,pp);
DeclareOBJ(TVector,vp);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TCpxVector, vCpxp);
DeclareOBJ(TCpx, cp);
DeclareTVAL(ret);
DeclareTVAL(theValue);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVALArray(prmv,_APPLYPARMSMAX);

EndFrame
 
/*  Initialization */
*ret = gCP->Tval_VOID;
if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if ((asTag(&argv[0]) != TYLAMBDA) && (asTag(&argv[0]) != TYCPROCEDURE) && (asTag(&argv[0]) != TYCFUNCTION))
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[1]))
    {
    case TYPAIR:
    /*  Extract the arguments from a proper List. */
    
    if (asObj(&argv[1]) != NIL)
        {
        indexOf = 0;
        rp = &argv[1];
        while ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
            {
            if(indexOf + 1 >= _APPLYPARMSMAX )
                {
				ApplyArgumentsTooLarge:
				*ec = TERROR("!apply: too many apply arguments or macro too large!");
                FrameExit(*ec);
                }
            pp = (TPair*)asObject(rp);
            prmv[indexOf++] = pp->itsCar;
            rp = &pp->itsCdr;
            }
        arglist = &prmv[0];
        }   
    break;
    
    case TYSTRUCTURE:
        if (asObj(&argv[1]) != NIL)
            {
            indexOf = 0;
            ep = (TStructure*)asObject(&argv[1]);
            size = ep->itsMaxItemIndex;
            
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
        
            while (indexOf < size)
                {
                prmv[indexOf] = atHMBind(ep->itsDictionaryArray,indexOf).Value;
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vp = (TVector*)asObject(&argv[1]);
            size = vp->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            while (indexOf < size)
                {
                prmv[indexOf] = atHMTval(vp->itsTvalArray,indexOf);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYMATRIX:
        if (asObj(&argv[1]) != NIL)
            {
            mp = (TMatrix*)asObject(&argv[1]);
            size = mp->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            while (indexOf < size)
                {
                prmv[indexOf] = atHMTval(mp->itsTvalMatrix,indexOf);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYNUMMATRIX:
        if (asObj(&argv[1]) != NIL)
            {
            np = (TNumMatrix*)asObject(&argv[1]);
            size = np->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            while (indexOf < size)
                {
                prmv[indexOf] = TREAL(atHMReal(np->itsRealMatrix,indexOf));
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYBITVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vBitp = (TBitVector*)asObject(&argv[1]);
            size = vBitp->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < size)
                {
                asInt(ndx1) = indexOf;
                prmv[indexOf] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1],*ndx1);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYBYTEVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vBytep = (TByteVector*)asObject(&argv[1]);
            size = vBytep->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < size)
                {
                asInt(ndx1) = indexOf;
                prmv[indexOf] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1],*ndx1);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYINTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vIntp = (TIntVector*)asObject(&argv[1]);
            size = vIntp->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < size)
                {
                asInt(ndx1) = indexOf;
                prmv[indexOf] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1],*ndx1);
                ++indexOf;
                }
            arglist = &prmv[0];
            }

    case TYSHORTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vShtp = (TShtVector*)asObject(&argv[1]);
            size = vShtp->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < size)
                {
                asInt(ndx1) = indexOf;
                prmv[indexOf] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1],*ndx1);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYLONGVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vLngp = (TLongVector*)asObject(&argv[1]);
            size = vLngp->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < size)
                {
                asInt(ndx1) = indexOf;
                prmv[indexOf] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1],*ndx1);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYNUMVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vNump = (TNumVector*)asObject(&argv[1]);
            size = vNump->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < size)
                {
                asInt(ndx1) = indexOf;
                prmv[indexOf] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYFLTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vFltp = (TFltVector*)asObject(&argv[1]);
            size = vFltp->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < size)
                {
                asInt(ndx1) = indexOf;
                prmv[indexOf] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP, gTP, argv[1], *ndx1);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
    
    case TYOBJVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            vObjp = (TObjVector*)asObject(&argv[1]);
            size = vObjp->itsMaxItemIndex;
            if(size >= _APPLYPARMSMAX )
                {
                goto ApplyArgumentsTooLarge;
                }       
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < size)
                {
                asInt(ndx1) = indexOf;
                prmv[indexOf] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP, argv[1], *ndx1);
                ++indexOf;
                }
            arglist = &prmv[0];
            }
    break;
	case TYCPXVECTOR:
		if (argv[1].u.CpxVector != NULL)
		{	vCpxp = argv[1].u.CpxVector;
			size = vCpxp->itsMaxItemIndex;
			if(size >= _APPLYPARMSMAX )
				goto ApplyArgumentsTooLarge;

			// Create a complex number for each pair in the complex vector
			theValue->Tag = TYCPX;
			xp = (LpREAL)*vCpxp->itsCpxArray;
			for (indexOf = 0; indexOf < size; ++indexOf)
			{	theValue->u.Complex = cp = TCpx_New(gCP, gTP);
				cp->itsReal = *xp++;
				cp->itsImag = *xp++;
				prmv[indexOf] = *theValue;	
			}
			arglist = &prmv[0];
		}
    break;

	case TYSTRING:
    case TYVOID:
	case TYNUM:
	case TYLONG:
	case TYREAL:
	case TYTEXT:
    FrameExit(FSmartbase_Evalv(gCP,gTP,argv[0],(NUM)argc-1,&argv[1]));
    break;

    default:
        FrameExit(TERROR("!apply: type of argument is not recognized!"));
    break;
    }

/*  Invoke a Procedure or cProcedure object against the arguments. */

*ret = FSmartbase_Evalv(gCP,gTP,argv[0],indexOf,arglist);
FrameExit(*ret);
}
