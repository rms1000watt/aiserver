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

#define _C_FUTIL2
#define _SMARTBASE

#if 0
FUtil2.c

Implementation of a wide range of utility and support functions which are not associated 
with any specific class of objects but are used to support the SmartLisp environment.

PARENT:             None. 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "futil2.h"
#include    "futil3.h"
#include    "tdiction.h"
#include    "tdirect.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "tlongvec.h"
#include    "tnumvec.h"
#include    "tfltvec.h"
#include    "tintvec.h"
#include    "tobjvec.h"
#include    "tlambda.h"
#include    "twkspace.h"
#include    "fproc.h"
#include    "fpred.h"
#include    "fconvert.h"
#include    "fconio.h"
#include    "flisp.h"
#include    "tpcodvec.h"
#include    "fvmscpt.h"
#include    "futil1.h"
#include    "tdirect.h"
#include    "tdatabas.h"
#include    "terror.h"
#include    "fmemory.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include    "tobjvec.h"
#include	"tcpx.h"
#include	"tcpxvec.h"
#include	"tshortvec.h"
#include	"tbrick.h"


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Init

Initialize a part of the Utility portion of the SmartLisp function library.  

#endif

TVAL FUtil2_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FUtil2_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FUtil2_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"reverse",(LpFUNC)&FUtil2_Reverse);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorFill",(LpFUNC)&FUtil2_Vectorfill);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"invoke",(LpFUNC)&FUtil2_Invoke);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"offset",(LpFUNC)&FUtil2_Offset);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"pointer",(LpFUNC)&FUtil2_Pointer);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"length",(LpFUNC)&FUtil2_Length);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"dimension",(LpFUNC)&FUtil2_Dimension);
ExitOnError(*ec);

TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringLength"),  aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"copy",(LpFUNC)&FUtil2_Copy);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"append",(LpFUNC)&FUtil2_Append);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringAppend"),  aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"call_cc",(LpFUNC)&FUtil2_Callcc);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"call_with_current_continuation"),  aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"reset",(LpFUNC)&FUtil2_Reset);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"quit",(LpFUNC)&FUtil2_Quit);
ExitOnError(*ec);

FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ref",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"listRef"),    aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringRef"),  aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"vectorRef"),  aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"nth"),        aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol, (LpCHAR)"isBound",(LpFUNC)&FUtil2_Boundp);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol, (LpCHAR)"inspect",(LpFUNC)&FUtil2_Inspect);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol, (LpCHAR)"lock",(LpFUNC)&FUtil3_Lock);
ExitOnError(*ec);

FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refString",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refBitVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refText",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refSymbol",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refBytVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refPcdVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refObjVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refShtVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refIntVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refNumVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refFltVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refMatrix",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refNumMatrix",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refCpxVector",(LpFUNC)&FUtil2_Ref);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refStrValue",(LpFUNC)&FUtil2_RefValue);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refDicValue",(LpFUNC)&FUtil2_RefValue);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refDirValue",(LpFUNC)&FUtil2_RefValue);
ExitOnError(*ec);

FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refStrKey",(LpFUNC)&FUtil2_RefKey);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refDicKey",(LpFUNC)&FUtil2_RefKey);
ExitOnError(*ec);
FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refDirKey",(LpFUNC)&FUtil2_RefKey);
ExitOnError(*ec);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FUtil2_AddVariable

Add a symbol to a SmartLisp Structure chain. The specified symbol and
value are added to the end of the current bindings. The current Structure
is resized to accomodate the new data.  If the FUtil2_AddVariable function receives 
the _globals object, a new Structure will be created and chained before the
_globals.

Note:   The FUtil2_AddVariable function operates correctly if self is nil. In the nil
        case, a new Structure object is created.

#endif

TVAL FUtil2_AddVariable(LpXCONTEXT gCP,LpTHREAD gTP, TVAL envTval, TSymbol *symName, TVAL value)
{
StartFrame
DeclareOBJ(TStructure,aStructure);
DeclareTVAL(sym);
DeclareTVAL(global);
EndFrame
 
/*  If envTval is nil, create a new Structure object, and add the new binding to it. */
if (isNullTval(&envTval))
    {
    aStructure = TStructure_New(gCP,gTP);
    asTag(&envTval) =  TYSTRUCTURE;
    asObject(&envTval) = (TObject *)aStructure;
    asTag(sym) = TYSYMBOL;
    asObject(sym) = (TObject*)symName;
    
    TStructure_AddNewValue(gCP,gTP,envTval, *sym, value);
    
    FrameExit( envTval);
    }
    
/*  If envTval is already a Structure, add the new (symbol,value) binding */
if (asTag(&envTval) == TYSTRUCTURE)
    {
    aStructure = (TStructure*)asObject(&envTval);
    asTag(sym) = TYSYMBOL;
    asObject(sym) = (TObject*)symName;
    TStructure_AddNewValue(gCP,gTP,envTval, *sym, value);
    FrameExit( envTval);
    }

/* If envTval is the _globals, create a new Structure and chain it before the _globals. */
if (asTag(&envTval) == TYGLOBALS)
    {
    *global = envTval;
    aStructure = TStructure_New(gCP,gTP);
    asTag(&envTval) =  TYSTRUCTURE;
    asObject(&envTval) = (TObject *)aStructure;
    asTag(sym) = TYSYMBOL;
    asObject(sym) = (TObject*)symName;
    TStructure_AddNewValue(gCP,gTP,envTval, *sym, value);
    FObject_SetCdr(gCP,gTP,(TObject*)aStructure, *global);
    FrameExit( envTval);
    }
FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_DefVariable

Add a symbol to a SmartLisp Structure chain. The specified symbol and
value are added to the end of the current bindings. The current Structure
is resized to accomodate the new data.  If the FUtil2_DefVariable function receives 
the _globals object, it assigns the value only.

Note:   The FUtil2_DefVariable function operates correctly if self is nil. In the nil
        case, a new Structure object is created.
        
        If the variable name already exists, in the Structure, the value is 
        assigned only.

#endif

TVAL FUtil2_DefVariable(LpXCONTEXT gCP,LpTHREAD gTP, TVAL envTval, TSymbol* symbol, TVAL value)
{
StartFrame
DeclareOBJ(TStructure,sp);
DeclareTVAL(ec);
DeclareTVAL(sym);
EndFrame
 
/*  If envTval is nil, create a new Structure object, and add the */
/*  new binding to it. */

asTag(sym) = TYSYMBOL;
asObject(sym) = (TObject*)symbol;

if (isNullTval(&envTval))
    {
    sp = TStructure_New(gCP,gTP);
    asObject(&envTval) = (TObject*)sp;
    asTag(&envTval) = TYSTRUCTURE;
    TStructure_AddNewValue(gCP,gTP,envTval, *sym, value);
    FrameExit(envTval);
    }
    
/*  If &envTval is already a Structure, grow it by one position if needed */

if (asTag(&envTval) == TYSTRUCTURE)
    {
    sp = (TStructure *)asObject(&envTval);

    *ec = TStructure_AddNewValue(gCP,gTP,envTval,*sym,value);

    if(isERROR(ec))
        {
        FrameExit(*ec);
        }
    else
        {
        FrameExit(envTval);
        }
    }

/*  If &envTval is the _globals, assign the new value to its symbol. */

if (asTag(&envTval) == TYGLOBALS)
    {
    TSymbol_SetGlobalValue(gCP,gTP,asSymbol(sym), value);
    FrameExit(envTval);
    }

FrameExit(gCP->TObject_ERROR_BADSYMBOL);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Dimension

Return the dimension of the specified object.

Type            Dimension
Struture        1
Vector          1
XxxVector       1
Brick          1 or 2
Matrix          1, 2, or 3
MatrixRow       1 or 2
NumMatrix       1, 2, or 3
NumMatrixRow    1 or 2
Scalars         0

#endif

TVAL FUtil2_Dimension(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
NUM                 lengthOf;
StartFrame
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareTVAL(ret);
EndFrame
 
/*  Initialization */

asTag(ret)  = TYNUM;
asInt(ret) = 0;
if (argc != 1) goto BadArgument;
lengthOf = 0;

/*  Is the target object the empty List? */

if (isNullTval(&argv[0]))
    {
    FrameExit(*ret); 
    }
else
    {
    switch (argv[0].Tag)
        {
        case TYDICTIONARY:
        case TYDIRECTORY:
        case TYOBJREPOSITORY:
        case TYSTRUCTURE:
            asInt(ret) = 1;
            break;
            
        case TYBRICK:
            asInt(ret) = (argv[0].u.Brick->itsRowCount > 1) ? 2 : 1;
            break;
        
        case TYMATRIX:
            asInt(ret) = argv[0].u.Matrix->itsRank;
            break;
            
        case TYMATRIXROW:
            if (FldIdx(argv[0]) >= 0)
                asInt(ret) = 1;
            else
            if (RowIdx(argv[0]) >= 0)
                {
                if (MatrixFromRow(argv[0])->itsRank == 2)
                    asInt(ret) = 1;
                else
                if (MatrixFromRow(argv[0])->itsRank == 3)
                    asInt(ret) = 2;
                }
            break;

        case TYNUMMATRIX:
            asInt(ret) = argv[0].u.NumMatrix->itsRank;
            break;
            
        case TYNUMMATRIXROW:
            if (FldIdx(argv[0]) >= 0)
                asInt(ret) = 1;
            else
            if (RowIdx(argv[0]) >= 0)
                {
                if (NumMatrixFromRow(argv[0])->itsRank == 2)
                    asInt(ret) = 1;
                else
                if (NumMatrixFromRow(argv[0])->itsRank == 3)
                    asInt(ret) = 2;
                }
            break;

        case TYBITVECTOR:
        case TYBYTEVECTOR:
        case TYINTVECTOR:
        case TYPCODEVECTOR:
        case TYNUMVECTOR:
        case TYFLTVECTOR:
        case TYSHORTVECTOR:
        case TYLONGVECTOR:
        case TYOBJVECTOR:
        case TYWORKSPACE:
        case TYCPXVECTOR:
        case TYVECTOR:
            asInt(ret) = 1;
            break;

        case TYTEXT:
        case TYSTRING:
        case TYSYMBOL:
        case TYQUOTEDSYMBOL:
        case TYSTRINGSUBSTR:
            asInt(ret) = 1;
            break;

        default:
            asInt(ret) = 0;
            break;
        }
    }

FrameExit(*ret); 

BadArgument:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_SymbolToNativeType

Return the native type for the specified symbol. If no native type can be found
with the specified symbol, return FALSE.

Note:   This only works for native type symbols. It does not find user defined
        types.

#endif

TVAL FUtil2_SymbolToNativeType(LpXCONTEXT gCP,LpTHREAD gTP, LpTYPE type, TSymbol *name)
{
StartFrame
EndFrame

*type = 0;
while (*type < gCP->TObject_MaxTypes)
    {
    if (!strcmp((char*)_TObject_TypeName(*type), (char*)&atHMChar(name->itsCString,0)))
        FrameExit(gCP->TObject_TRUE);
    ++(*type);
    }

/*  We only get here if no match was found. */

*type = 0;
FrameExit(gCP->TObject_FALSE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_CongruentProcs

Check two SmartLisp Procedure objects to see if they are the same. The 
advancedMethods switch is TRUE if we wish to allow constant folding, 
expression reduction, etc.; otherwise, only isomorphic Procedure objects
will be considered congruent. The FUtil2_CongruentProcs function returns TObject_TRUE
if the two Procedure objects are the same, and TObject_FALSE otherwise.

Note:   No longer used!

#endif

TVAL FUtil2_CongruentProcs(LpXCONTEXT gCP,LpTHREAD gTP, OBJ target,OBJ source,BOLE advancedMethods)
{
gTP = gTP; // NOOP to hide unused parameter warning message
target = target; // NOOP to hide unused parameter warning message
source = source; // NOOP to hide unused parameter warning message
advancedMethods = advancedMethods; // NOOP to hide unused parameter warning message
return(gCP->TObject_FALSE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Reverse

The reverse cProcedure creates and returns a new list which contains 
the elements of the argument {obj} in reverse order. The argument {obj} 
may be a proper list, a range, or a vector. 

Several examples follow.

    (reverse  `(1  2  3  4))            =>  (4  3  2  1)
    (reverse  #(1  2  3  4))            =>  #(4  3  2  1)
    (reverse  `((1  2)  3  4))          =>  (4  3  (1  2))
    (reverse  `(1))                     =>  (1)
    (reverse  `())                      =>  ()

#endif

TVAL FUtil2_Reverse(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM                     size;
NUM                     argIndex;
NUM                     indexOf;
LpTVAL                  target;
LpTVAL                  source;
NUM						cn;
REAL				*sp, *dp;		// Source, destination pointers
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
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TObjVector,vObjt);
DeclareOBJ(TCpxVector, vCpxp);
DeclareOBJ(TCpxVector, vCpxt);
DeclareTVAL(ivTval);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVAL(ret);

EndFrame
 
/*  Make sure there is one argument and that it is not null! */

*ret = gCP->Tval_VOID;
if ((argc != 1) || (isNullTval(&argv[0]))) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
target = ret;
source = &argv[0];

/*  Is the source {argv[0]} a Pair ? */

switch(asTag(source))
    {
    case TYPAIR:
        while ((!isNullTval(source)) && (asTag(source) == TYPAIR))
            {
            pp = (TPair*)asObject(source);
            pp2 = TPair_New(gCP,gTP);
            pp2->itsCdr = *ret;
            pp2->itsCar = pp->itsCar;
            asObject(ret) = (TObject*)pp2;
            asTag(ret) = TYPAIR;
            source = &pp->itsCdr;
            }
        FrameExit(*ret);
    break;
 
    case TYVECTOR:
        vp = (TVector*)asObject(source);
        size = vp->itsMaxItemIndex;
        vp2 = TVector_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)vp2,size);
        vp2->itsCdr = vp->itsCdr;
        indexOf = 0;
        argIndex = size;
        while (indexOf < size)
            {
            atHMTval(vp2->itsTvalArray,--argIndex) = atHMTval(vp->itsTvalArray,indexOf++);
            }
        asObject(ret) = (TObject*)vp2;
        asTag(ret) = TYVECTOR;
        FrameExit(*ret);
    break;

    case TYMATRIX:
        mp = (TMatrix*)asObject(source);
        size = mp->itsMaxItemIndex;
        mp2 = TMatrix_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)mp2,size);
        mp2->itsCdr = mp->itsCdr;
        mp2->itsRank = mp->itsRank;
        for (cn=0;cn<_MAXDIMENSIONS;++cn) mp2->itsDimensions[cn] = np->itsDimensions[cn];
        indexOf = 0;
        argIndex = size;
        while (indexOf < size)
            {
            atHMTval(mp2->itsTvalMatrix,--argIndex) = atHMTval(mp->itsTvalMatrix,indexOf++);
            }
        asObject(ret) = (TObject*)mp2;
        asTag(ret) = TYMATRIX;
        FrameExit(*ret);
    break;

    case TYNUMMATRIX:
        np = (TNumMatrix*)asObject(source);
        size = np->itsMaxItemIndex;
        np2 = TNumMatrix_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)np2,size);
        np2->itsRank = np->itsRank;
        for (cn=0;cn<_MAXDIMENSIONS;++cn) np2->itsDimensions[cn] = np->itsDimensions[cn];
        indexOf = 0;
        argIndex = size;
        while (indexOf < size)
            {
            atHMReal(np2->itsRealMatrix,--argIndex) = atHMReal(np->itsRealMatrix,indexOf++);
            }
        asObject(ret) = (TObject*)np2;
        asTag(ret) = TYNUMMATRIX;
        FrameExit(*ret);
    break;

    case TYBITVECTOR:       
        vBitp = (TBitVector*)asObject(source);
        size = vBitp->itsMaxItemIndex;
        vBitt = TBitVector_New(gCP,gTP);

        FObject_SetMaxIndex(gCP,gTP,(TObject*)vBitt,size);

        vBitt->itsCdr = vBitp->itsCdr;
        indexOf = 0;
        argIndex = size;
        
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;

        asObject(ivTval) = (TObject*)vBitt;
        asTag(ivTval) = vBitt->itsObjectType;
        
        while (indexOf < size)
            {
            asInt(ndx1) = --argIndex;
            asInt(ndx2) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP,*ivTval, *ndx1, 
                (*_TObject_TypeGetIV1(asTag(source)))(gCP,gTP,*source, *ndx2) );
            }
        
        asObject(ret) = (TObject*)vBitt;
        asTag(ret) = TYBITVECTOR;
        FrameExit(*ret);
    break;
        
    case TYBYTEVECTOR:
        vBytep = (TByteVector*)asObject(source);
        size = vBytep->itsMaxItemIndex;
        vBytet = TByteVector_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)vBytet,size);

        vBytet->itsCdr = vBytep->itsCdr;
        indexOf = 0;
        argIndex = size;
        while (indexOf < size)
            {
            atHMChar(vBytet->itsByteArray,--argIndex) = atHMChar(vBytep->itsByteArray,indexOf++);
            }
        asObject(ret) = (TObject*)vBytet;
        asTag(ret) = TYBYTEVECTOR;
        FrameExit(*ret);
    break;
    
    case TYNUMVECTOR:
        vNump = (TNumVector*)asObject(source);
        size = vNump->itsMaxItemIndex;
        vNumt = TNumVector_New(gCP, gTP);
        
        FObject_SetMaxIndex(gCP,gTP,(TObject*)vNumt,size);
        vNumt->itsCdr = vNump->itsCdr;
        indexOf = 0;
        argIndex = size;
        while (indexOf < size)
            {
            atHMReal(vNumt->itsRealArray,--argIndex) = atHMReal(vNump->itsRealArray,indexOf++);
            }
        asObject(ret) = (TObject*)vNumt;
        asTag(ret) = TYNUMVECTOR;
        FrameExit(*ret);
    break;
    
    case TYFLTVECTOR:
        vFltp = (TFltVector*)asObject(source);
        size = vFltp->itsMaxItemIndex;
        vFltt = TFltVector_New(gCP,gTP);
        
        FObject_SetMaxIndex(gCP,gTP,(TObject*)vFltt,size);
        vFltt->itsCdr = vFltp->itsCdr;
        indexOf = 0;
        argIndex = size;
        while (indexOf < size)
            {
            atHMFloat(vFltt->itsFloatArray,--argIndex) = atHMFloat(vFltp->itsFloatArray,indexOf++);
            }
        asObject(ret) = (TObject*)vFltt;
        asTag(ret) = TYFLTVECTOR;
        FrameExit(*ret);
    break;
    
    case TYINTVECTOR:
        vIntp = (TIntVector*)asObject(source);
        size = vIntp->itsMaxItemIndex;
        vIntt = TIntVector_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)vIntt,size);

        vIntt->itsCdr = vIntp->itsCdr;
        indexOf = 0;
        argIndex = size;
        while (indexOf < size)
            {
            atHMInt(vIntt->itsIntArray,--argIndex) = atHMInt(vIntp->itsIntArray,indexOf++);
            }
        asObject(ret) = (TObject*)vIntt;
        asTag(ret) = TYINTVECTOR;
        FrameExit(*ret);
    break;
    
    case TYOBJVECTOR:
        vObjp = (TObjVector*)asObject(source);
        size = vObjp->itsMaxItemIndex;
        vObjt = TObjVector_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)vObjt,size);
        vObjt->itsCdr = vObjp->itsCdr;
        indexOf = 0;
        argIndex = size;
        while (indexOf < size)
            {
            atHMObject(vObjt->itsObjectArray,--argIndex) = atHMObject(vObjp->itsObjectArray,indexOf++);
            }
        asObject(ret) = (TObject*)vObjt;
        asTag(ret) = TYOBJVECTOR;
        FrameExit(*ret);
    break;

	case TYCPXVECTOR:
		vCpxp = (TCpxVector*)asObject(source);
		size = vCpxp->itsMaxItemIndex;
		vCpxt = TCpxVector_New(gCP, gTP);

		vCpxt->itsCdr = vCpxp->itsCdr;
		if (size > 0)
		{	FObject_SetMaxIndex(gCP, gTP, (TObject*)vCpxt, size);
			sp = (LpREAL)(*vCpxp->itsCpxArray) + 2 * size;
			dp = (LpREAL)(*vCpxt->itsCpxArray);
			// Copy each pair from back of source to front of destination
			for (indexOf = 0; indexOf < size; ++indexOf)
			{	*dp++ = *(--sp - 1);
				*dp++ = *sp--;
			}
		}
		ret->u.CpxVector = vCpxt;
		ret->Tag = TYCPXVECTOR;
		FrameExit(*ret);
    break;

    default:
            goto Invalid;
    break;
    }
 
Invalid:
*ret = gCP->Tval_VOID;
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Vectorfill

The FUtil2_Vectorfill function fills a Vector object with the specified
value. 

	(vectorFill vector value [incFunction])

Note:   At least two arguments are expected, anything else is an error.

#endif

TVAL FUtil2_Vectorfill(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
NUM                 indexOf;
NUM                 size;
StartFrame
DeclareOBJ(TVector,vp);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TCpxVector,vCpxp);
DeclareTVAL(ret);
DeclareTVAL(incFunction);
DeclareTVAL(fillValue);
DeclareTVAL(ndx1);
EndFrame
 
/*  There must be at least two arguments, and the first must be a vector type. */

*ret = gCP->Tval_VOID;
*incFunction = gCP->Tval_VOID;
if ((argc > 3) || (argc < 2) || isNullTval(&argv[0])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (argc == 3) *incFunction = argv[2];
*fillValue = argv[1];

switch(asTag(&argv[0]))
    {
    case TYVECTOR:
        vp = (TVector*)asObject(&argv[0]);
        size = vp->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
			}
    break;

    case TYMATRIX:
        mp = (TMatrix*)asObject(&argv[0]);
        size = mp->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
            }
    break;

    case TYNUMMATRIX:
        np = (TNumMatrix*)asObject(&argv[0]);
        size = np->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
            }
    break;

    case TYBITVECTOR:
        vBitp = (TBitVector*)asObject(&argv[0]);
        size = vBitp->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
            }
    break;
        
    case TYBYTEVECTOR:
        vBytep = (TByteVector*)asObject(&argv[0]);
        size = vBytep->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
            }
    break;
    
    case TYNUMVECTOR:
        vNump = (TNumVector*)asObject(&argv[0]);
        size = vNump->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
            }
    break;
    
    case TYFLTVECTOR:
        vFltp = (TFltVector*)asObject(&argv[0]);
        size = vFltp->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
            }
    break;
    
    case TYINTVECTOR:
        vIntp = (TIntVector*)asObject(&argv[0]);
        size = vIntp->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
            }
    break;
    
    
    case TYOBJVECTOR:
        vObjp = (TObjVector*)asObject(&argv[0]);
        size = vObjp->itsMaxItemIndex;
        indexOf = 0;
        asTag(ndx1) = TYNUM;
        while (indexOf < size)
            {
            asInt(ndx1) = indexOf++;
            (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
				{
				*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
				}
            }
    break;
    case TYCPXVECTOR:
		vCpxp = argv[0].u.CpxVector;
		size = vCpxp->itsMaxItemIndex;
		ndx1->Tag = TYNUM;
		for (indexOf = 0; indexOf < size; ++indexOf)
		{	ndx1->u.Int = indexOf;
			TCpxVector_SetIV1(gCP,gTP, argv[0], *ndx1,*fillValue);
			if (incFunction->Tag != TYVOID)
			{	*fillValue = FSmartbase_Eval(gCP,gTP,*incFunction,1,*fillValue);
				ExitOnError(*fillValue);
			}
		}
	break;
    default:
        goto Invalid;
    break;
    }
    
*ret = argv[0];
FrameExit(*ret);

Invalid:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Invoke

The FUtil2_Invoke function invokes the first argument using the remaining
arguments as parameters. 

#endif

TVAL FUtil2_Invoke(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc < 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if ((asTag(&argv[0]) != TYLAMBDA) && (asTag(&argv[0]) != TYCPROCEDURE))
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Invoke a Procedure or cProcedure object against the arguments. */

if (asTag(&argv[0]) == TYLAMBDA || asTag(&argv[0]) == TYCPROCEDURE)
    {
	*ret = FSmartbase_Evalv(gCP,gTP,argv[0],argc-1,&argv[1]);
    FrameExit(*ret);
    }
    
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Offset

Return the offset of the specified indices from the specified object. The offset Procedure can operate on 
the following types: a Structure, a List, a String, a Symbol,  a Buffer, or a Vector.

#endif

TVAL FUtil2_Offset(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
NUM					n;
SHORT               successSW;
StartFrame
DeclareOBJ(TObjVector,attv);
DeclareTVAL(offset);
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);
EndFrame
 
/*  Initialization */

ret->Tag  = TYNUM;
ret->u.Int = 0;
if (argc <= 0) goto BadArgument;

/*  Is the target object the empty List? */

switch (argv[0].Tag)
    {
    case TYSTRUCTURE:
		switch(argc)
			{
			case 2:
				if ((isNumIndex(&argv[1])) && (asNumIndex(&argv[1]) >= 0) && (asNumIndex(&argv[1]) < argv[0].u.Structure->itsMaxItemIndex)) 
					{
					ret->u.Int = asNumIndex(&argv[1]) * BINDARRAYITEMSIZE;
					ret->Tag = TYNUM;
					}
				else	
				if ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL))
					{
					*ret = TStructure_SearchKey(gCP,gTP,argv[0].u.Structure,argv[1].u.Object,(short*)&successSW);
					if (successSW != 0) goto BadArgument;
					ret->u.Int *= BINDARRAYITEMSIZE;
					}
				else
					goto BadArgument;
				break;

			case 3:
				if (!(isNumIndex(&argv[1])) || (asNumIndex(&argv[1]) < 0) || (asNumIndex(&argv[1]) >= argv[0].u.Structure->itsMaxItemIndex)) goto BadArgument;
				if (!(isNumIndex(&argv[2])) || (asNumIndex(&argv[1]) < 0) || (asNumIndex(&argv[1]) > 1)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * BINDARRAYITEMSIZE) + (asNumIndex(&argv[2]) * sizeof(TVAL));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYDICTIONARY:
		switch(argc)
			{
			case 2:
				if ((isNumIndex(&argv[1])) && (asNumIndex(&argv[1]) >= 0) && (asNumIndex(&argv[1]) < argv[0].u.Dictionary->itsMaxItemIndex)) 
					{
					ret->u.Int = asNumIndex(&argv[1]) * BINDARRAYITEMSIZE;
					ret->Tag = TYNUM;
					}
				else
				if (_TObject_TypeFlag(argv[1].Tag) == _TObject_TfTOBJECT)
					{
					*ret = TDictionary_BSearchKey(gCP,gTP,argv[0].u.Dictionary,argv[1].u.Object,(short*)&successSW);
					if (successSW != 0) goto BadArgument;
					ret->u.Int *= BINDARRAYITEMSIZE;
					}
				else
					goto BadArgument;
				break;

			case 3:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (asNumIndex(&argv[1]) >= argv[0].u.Dictionary->itsMaxItemIndex)) goto BadArgument;
				if (!(isNumIndex(&argv[2])) || (asNumIndex(&argv[1]) < 0) || (asNumIndex(&argv[1]) > 1)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * BINDARRAYITEMSIZE) + (asNumIndex(&argv[2]) * sizeof(TVAL));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYDIRECTORY:
		switch(argc)
			{
			case 2:
				*ret = TDirectory_BSearchKey(gCP,gTP,argv[0].u.Directory,argv[1],(short*)&successSW);
				if (successSW != 0) goto BadArgument;
				ret->u.Int *= (2 * TVALARRAYITEMSIZE);
				break;

			case 3:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.Directory->itsMaxItemIndex)) goto BadArgument;
				if (!(isNumIndex(&argv[2])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) > 1)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * 2 * TVALARRAYITEMSIZE) + (asNumIndex(&argv[2]) * sizeof(TVAL));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYBYTEVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.ByteVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = asNumIndex(&argv[1]);
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYFLTVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.FltVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(FLOAT));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYINTVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.IntVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(NUM));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYNUMVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.NumVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(REAL));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYOBJVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.ObjVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(OBJ));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYPCODEVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.PcodeVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(NUM));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYSHORTVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.ShortVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(SHORT));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYLONGVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.LongVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(NUM32));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYSYMBOL:
    case TYQUOTEDSYMBOL:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.Symbol->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(CHAR));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYSTRING:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.String->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(CHAR));
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYCPXVECTOR:
		switch(argc)
			{
			case 2:
				if (!(isNumIndex(&argv[1])) || (isNumIndex(&argv[1]) < 0) || (isNumIndex(&argv[1]) >= argv[0].u.CpxVector->itsMaxItemIndex)) goto BadArgument;
				ret->u.Int = (asNumIndex(&argv[1]) * sizeof(REAL) * 2);
				ret->Tag = TYNUM;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYVECTOR:
		switch(argc)
			{
			case 2:
				if ((isNumIndex(&argv[1])) && (isNumIndex(&argv[1]) >= 0) && (isNumIndex(&argv[1]) < argv[0].u.Vector->itsMaxItemIndex))
					{
					ret->u.Int = (asNumIndex(&argv[1]) * sizeof(REAL) * 2);
					ret->Tag = TYNUM;
					}
				else
				if ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL))
					{
					attv = argv[0].u.Vector->itsAttributes;
					for (n = 0; (attv != NIL) && (n < attv->itsMaxItemIndex); ++n)
						{
						/* Check if the index matches a symbol in the attributes vector */
						if (atHMObject(attv->itsObjectArray,n) == argv[1].u.Object)
							{
							/*  Make sure array index is in range. */
							if (n >= argv[0].u.Vector->itsMaxItemIndex)
								{
								ret->u.Int = (n * sizeof(TVAL));
								ret->Tag = TYNUM;
								}
							else
								goto BadArgument;
							}
						}
					if ((attv == NULL) || (n >= attv->itsMaxItemIndex)) goto BadArgument;
					}
				else
					goto BadArgument;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYBRICK:
		switch(argc)
			{
			case 2:
				/* Retrieve the offset of the record field name or field index */
				*offset = TSYMBOL("offset");
				*ret = TBrick_GetIV2(gCP,gTP,argv[0],argv[1],*offset);
				if (ret->Tag != TYNUM) goto BadArgument;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYBRICKROW:
		switch(argc)
			{
			case 1:
				/* Retrieve the offset of the record field name or field index */
				*offset = TSYMBOL("offset");
				prmv[0].Tag = TYBRICK;
				prmv[0].u.Object = _TObject_ObjectByIndex(argv[0].u.Index[0]);
				prmv[1].Tag = TYNUM;
				prmv[1].u.Int = argv[0].u.Index[1];
				*ret = TBrick_GetIV2(gCP,gTP,prmv[0],prmv[1],*offset);
				if (ret->Tag != TYNUM) goto BadArgument;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYMATRIX:
		switch(argc)
			{
			case 2:
				if ((isNumIndex(&argv[1])) && (isNumIndex(&argv[1]) >= 0) && (isNumIndex(&argv[1]) < argv[0].u.Matrix->itsMaxItemIndex))
					{
					ret->u.Int = (asNumIndex(&argv[1]) * sizeof(TVAL));
					ret->Tag = TYNUM;
					}
				else
					goto BadArgument;
				break;

			case 3:
				if ((argv[0].u.Matrix->itsRank == 2) && 
					(isNumIndex(&argv[1])) && 
					(isNumIndex(&argv[2])) &&
					(asNumIndex(&argv[1]) >= 0) && 
					(asNumIndex(&argv[1]) < argv[0].u.Matrix->itsDimensions[0]) && 
					(asNumIndex(&argv[2]) >= 0) && 
					(asNumIndex(&argv[2]) < argv[0].u.Matrix->itsDimensions[1]))
					{
					ret->u.Int = (asNumIndex(&argv[1]) * argv[0].u.Matrix->itsDimensions[1]) + asNumIndex(&argv[2]);
					ret->u.Int *= sizeof(TVAL);
					ret->Tag = TYNUM;
					}
				else
					goto BadArgument;
				break;

			case 4:
				if ((argv[0].u.Matrix->itsRank == 3) && 
					(isNumIndex(&argv[1])) && 
					(isNumIndex(&argv[2])) &&
					(asNumIndex(&argv[1]) >= 0) && 
					(asNumIndex(&argv[1]) < argv[0].u.Matrix->itsDimensions[0]) && 
					(asNumIndex(&argv[2]) >= 0) && 
					(asNumIndex(&argv[2]) < argv[0].u.Matrix->itsDimensions[1]) &&
					(asNumIndex(&argv[3]) >= 0) && 
					(asNumIndex(&argv[3]) < argv[0].u.Matrix->itsDimensions[2]))
					{
					ret->u.Int = (asNumIndex(&argv[1]) * argv[0].u.Matrix->itsDimensions[1] * argv[0].u.Matrix->itsDimensions[2]) + (asNumIndex(&argv[2]) * argv[0].u.Matrix->itsDimensions[2]) + asNumIndex(&argv[3]);
					ret->u.Int *= sizeof(TVAL);
					ret->Tag = TYNUM;
					}
				else
					goto BadArgument;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    case TYNUMMATRIX:
		switch(argc)
			{
			case 2:
				if ((isNumIndex(&argv[1])) && (isNumIndex(&argv[1]) >= 0) && (isNumIndex(&argv[1]) < argv[0].u.NumMatrix->itsMaxItemIndex))
					{
					ret->u.Int = (asNumIndex(&argv[1]) * sizeof(REAL));
					ret->Tag = TYNUM;
					}
				else
					goto BadArgument;
				break;

			case 3:
				if ((argv[0].u.NumMatrix->itsRank == 2) && 
					(isNumIndex(&argv[1])) && 
					(isNumIndex(&argv[2])) &&
					(asNumIndex(&argv[1]) >= 0) && 
					(asNumIndex(&argv[1]) < argv[0].u.NumMatrix->itsDimensions[0]) && 
					(asNumIndex(&argv[2]) >= 0) && 
					(asNumIndex(&argv[2]) < argv[0].u.NumMatrix->itsDimensions[1]))
					{
					ret->u.Int = (asNumIndex(&argv[1]) * argv[0].u.NumMatrix->itsDimensions[1]) + asNumIndex(&argv[2]);
					ret->u.Int *= sizeof(REAL);
					ret->Tag = TYNUM;
					}
				else
					goto BadArgument;
				break;

			case 4:
				if ((argv[0].u.NumMatrix->itsRank == 3) && 
					(isNumIndex(&argv[1])) && 
					(isNumIndex(&argv[2])) &&
					(asNumIndex(&argv[1]) >= 0) && 
					(asNumIndex(&argv[1]) < argv[0].u.NumMatrix->itsDimensions[0]) && 
					(asNumIndex(&argv[2]) >= 0) && 
					(asNumIndex(&argv[2]) < argv[0].u.NumMatrix->itsDimensions[1]) &&
					(asNumIndex(&argv[3]) >= 0) && 
					(asNumIndex(&argv[3]) < argv[0].u.NumMatrix->itsDimensions[2]))
					{
					ret->u.Int = (asNumIndex(&argv[1]) * argv[0].u.NumMatrix->itsDimensions[1] * argv[0].u.NumMatrix->itsDimensions[2]) + (asNumIndex(&argv[2]) * argv[0].u.NumMatrix->itsDimensions[2]) + asNumIndex(&argv[3]);
					ret->u.Int *= sizeof(REAL);
					ret->Tag = TYNUM;
					}
				else
					goto BadArgument;
				break;

			default:
				goto BadArgument;
				break;
			}
        break;
        
    default:
        goto BadArgument;
        break;
    }

FrameExit(*ret); 

BadArgument:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Pointer

Return a pointer (memory address) to the specified indices into the specified object. 
The pointer Procedure can operate on all types.

#endif

TVAL FUtil2_Pointer(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
LpCHAR				fieldArrayPtr;
LpBIND				bindPtr;
NUM                 index1;
NUM                 index2;
StartFrame
DeclareOBJ(TString,sp);
DeclareOBJ(TStructure,ep);
DeclareOBJ(TVector,vp);
DeclareOBJ(TPair,pp);
DeclareOBJ(TSymbol,symbolOf);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TBrick,rp);
DeclareTVAL(ret);
EndFrame
 
/*  Initialization */

ret->Tag = TYNUM;
ret->u.Int = 0;
if (argc != 1) goto BadArgument;

/*  Is the target object the a zero length value? */

switch (argv[0].Tag)
    {
    case TYLAMBDA:
    case TYMACRO:
    case TYBITVECTOR:
    case TYBYTEVECTOR:
	case TYCPXVECTOR:
    case TYDICTIONARY:
    case TYDIRECTORY:
    case TYFLTVECTOR:
    case TYINTVECTOR:
    case TYLONGVECTOR:
    case TYMATRIX:
    case TYNUMMATRIX:
    case TYNUMVECTOR:
    case TYOBJVECTOR:
	case TYPAIR:
    case TYPCODEVECTOR:
	case TYQUOTEDPAIR:
    case TYQUOTEDSYMBOL:
    case TYBRICK:
    case TYSHORTVECTOR:
    case TYSTRING:
    case TYSTRUCTURE:
    case TYSYMBOL:
    case TYVECTOR:
    case TYWORKSPACE:
        ret->u.Pointer = (char*)*argv[0].u.Lambda->itsNilArray;
        break;
        
    case TYMATRIXROW:
        mp = (TMatrix*)_TObject_MainObjectList(ObjIdx(argv[0]));
		fieldArrayPtr = (POINTER)*mp->itsTvalMatrix;
		index1 = RowIdx(argv[0]);
		index2 = FldIdx(argv[0]);
		if(index2 >= 0)
		    {
		    ret->u.Pointer = (POINTER)fieldArrayPtr+(sizeof(TVAL)*((index1*mp->itsDimensions[1]*mp->itsDimensions[2]) + (index2*mp->itsDimensions[2])));
		    }
		else
		if (mp->itsRank == 3)
		    {
		    ret->u.Pointer = (POINTER)fieldArrayPtr+(sizeof(TVAL)*(index1*mp->itsDimensions[1]*mp->itsDimensions[2]));
		    }
	    else
	        {
	        ret->u.Pointer = (POINTER)fieldArrayPtr+(sizeof(TVAL)*(index1*mp->itsDimensions[1]));
	        }
        break;

    case TYNUMMATRIXROW:
        np = (TNumMatrix*)_TObject_MainObjectList(ObjIdx(argv[0]));
		fieldArrayPtr = (POINTER)*np->itsRealMatrix;
		index1 = RowIdx(argv[0]);
		index2 = FldIdx(argv[0]);
		if(index2 >= 0)
		    {
		    ret->u.Pointer = (POINTER)fieldArrayPtr+(sizeof(REAL)*((index1*np->itsDimensions[1]*np->itsDimensions[2]) + (index2*np->itsDimensions[2])));
		    }
		else
		if (np->itsRank == 3)
		    {
		    ret->u.Pointer = (POINTER)fieldArrayPtr+(sizeof(REAL)*(index1*np->itsDimensions[1]*np->itsDimensions[2]));
		    }
	    else
	        {
	        ret->u.Pointer = (POINTER)fieldArrayPtr+(sizeof(REAL)*(index1*np->itsDimensions[1]));
	        }
        break;

   case TYBRICKFIELD:
        rp = (TBrick*)_TObject_MainObjectList(ObjIdx(argv[0]));
		bindPtr = BindArray(rp->itsFieldList);
		index1 = RowIdx(argv[0]);
		index2 = FldIdx(argv[0]);
		fieldArrayPtr = (char*)*rp->itsFieldArray;
		fieldArrayPtr += (index1*rp->itsRowByteCount);
		ret->u.Pointer = (POINTER)(fieldArrayPtr+bindPtr[index2].Value.Offset);		    
		break;

   case TYBRICKROW:
        rp = (TBrick*)_TObject_MainObjectList(ObjIdx(argv[0]));
		fieldArrayPtr = (char*)*rp->itsFieldArray;
		index1 = RowIdx(argv[0]);
		ret->u.Pointer = (POINTER)fieldArrayPtr+(index1*rp->itsRowByteCount);		    
		break; 

   case TYSTRINGSUBSTR:
        sp = (TString*)_TObject_MainObjectList(ObjIdx(argv[0]));
        fieldArrayPtr = (POINTER)*sp->itsCString;
		index1 = SubOff(argv[0]);
        ret->u.Pointer = (POINTER)(fieldArrayPtr+index1);
        break;

    case TYTEXT:
        ret->u.Pointer = (char *)&argv[0].u.Text[0];
        break;
        
    default:
        ret->u.Pointer = NULL;
        break;
    }

FrameExit(*ret); 

BadArgument:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Length

Return the length of the specified object. The length Procedure can operate on 
the following types: a Structure, a List, a String, a Symbol, 
a Buffer, or a Vector.

#endif

TVAL FUtil2_Length(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
LpBIND				bindPtr;
NUM                 lengthOf;
StartFrame
DeclareOBJ(TStructure,ep);
DeclareOBJ(TVector,vp);
DeclareOBJ(TPair,pp);
DeclareOBJ(TSymbol,symbolOf);
DeclareOBJ(TMatrix,mp);
DeclareOBJ(TNumMatrix,np);
DeclareOBJ(TBrick,rp);
DeclareTVAL(ret);
EndFrame
 
/*  Initialization */

ret->Tag  = TYNUM;
ret->u.Int = 0;
if (argc != 1) goto BadArgument;
lengthOf = 0;

/*  Is the target object the a zero length value? */

if (argv[0].Tag <= TYWORDPOINTER)
    {
    FrameExit(*ret); 
    }
else
/*  Is the target object a List? */
if ((argv[0].Tag == TYPAIR) || (argv[0].Tag == TYQUOTEDPAIR))
    {
    ++lengthOf;
    pp = (TPair*)asObject(&argv[0]);
    while (isLexMorphPair(&pp->itsCdr) && (asObj(&pp->itsCdr) != NIL))
        {
        ++lengthOf;
        pp = (TPair*)asObject(&pp->itsCdr);
        }
    asInt(ret) = lengthOf;
    }
else
    {
    switch (argv[0].Tag)
        {
        case TYLAMBDA:
        case TYMACRO:
            *ret = TLambda_Length(gCP,gTP,argv[0]);
            break;
            
        case TYBITVECTOR:
            asInt(ret) = ((TBitVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYBYTEVECTOR:
            asInt(ret) = ((TByteVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYCPXVECTOR:
            asInt(ret) = ((TCpxVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYDICTIONARY:
            asInt(ret) = Dictionary(argv[0])->itsMaxItemIndex;
            break;
            
        case TYDIRECTORY:
            asInt(ret) = Directory(argv[0])->itsMaxItemIndex;
            break;
            
        case TYFLTVECTOR:
            asInt(ret) = ((TFltVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYINTVECTOR:
            asInt(ret) = ((TIntVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYLONGVECTOR:
            asInt(ret) = ((TLongVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYMATRIX:
            asInt(ret) = ((TMatrix*)(asObject(&argv[0])))->itsMaxItemIndex;
            break;

        case TYMATRIXROW:
            mp = (TMatrix*)_TObject_MainObjectList(ObjIdx(argv[0]));
            /* MatrixRow with 2 indexes on a Rank 3 Matrix */
            if (FldIdx(argv[0]) >= 0)
                ret->u.Int = mp->itsDimensions[2];
            else
            /* MatrixRow with 1 index on a Rank 3 Matrix */
            if (mp->itsRank == 3)
                ret->u.Int = mp->itsDimensions[1] * mp->itsDimensions[2];
            else
            /* MatrixRow with 1 index on a Rank 2 Matrix */
                ret->u.Int = mp->itsDimensions[1];
            break;

        case TYNUMMATRIX:
            asInt(ret) = ((TNumMatrix*)(asObject(&argv[0])))->itsMaxItemIndex;
            break;

        case TYNUMMATRIXROW:
            np = (TNumMatrix*)_TObject_MainObjectList(ObjIdx(argv[0]));
            /* NumMatrixRow with 2 indexes on a Rank 3 NumMatrix */
            if (FldIdx(argv[0]) >= 0)
                ret->u.Int = np->itsDimensions[2];
            else
            /* NumMatrixRow with 1 index on a Rank 3 NumMatrix */
            if (np->itsRank == 3)
                ret->u.Int = np->itsDimensions[1] * np->itsDimensions[2];
            else
            /* NumMatrixRow with 1 index on a Rank 2 NumMatrix */
                ret->u.Int = np->itsDimensions[1];
            break;

        case TYNUMVECTOR:
            asInt(ret) = ((TNumVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYOBJREPOSITORY:
            *ret = TDatabase_Length(gCP,gTP,argv[0]);
            break;
            
        case TYPCODEVECTOR:
            asInt(ret) = ((TPcodeVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYBRICK:
            asInt(ret) = argv[0].u.Brick->itsRowCount;
            break;
            
        case TYBRICKFIELD:
            rp = (TBrick*)_TObject_MainObjectList(ObjIdx(argv[0]));
	        bindPtr = &BindArray(rp->itsFieldList)[FldIdx(argv[0])];
	        ret->u.Int = (NUM)bindPtr->Value.Modifier;		    
            break;

        case TYBRICKROW:
            rp = (TBrick*)_TObject_MainObjectList(ObjIdx(argv[0]));
	        ret->u.Int = (NUM)rp->itsFieldCount;		    
	        break; 

        case TYSHORTVECTOR:
            asInt(ret) = ((TShtVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        case TYSTRING:
            asInt(ret) = strlen((char *)*((TString*)(argv[0].u.Object))->itsCString);
            break; 

        case TYSTRUCTURE:
            asInt(ret) = ((TStructure*)(asObject(&argv[0])))->itsMaxItemIndex;
            break;
            
        case TYTEXT:
            asInt(ret) = strlen((char *)&asData(&argv[0]));
            break;
            
        case TYSTRINGSUBSTR:
            asInt(ret) = SubLen(argv[0]);
            break;

        case TYSYMBOL:
            asInt(ret) = strlen((char *)*((TSymbol*)(argv[0].u.Object))->itsCString);
            break; 
            
        case TYVECTOR:
            asInt(ret) = ((TVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break;

        case TYWORKSPACE:
        case TYOBJVECTOR:
            asInt(ret) = ((TObjVector*)(asObject(&argv[0])))->itsMaxItemIndex;
            break; 

        default:
            ret->u.Int = 0;
            break;
        }
    }

FrameExit(*ret); 

BadArgument:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FUtil2_Reset

The reset  cProcedure returns the SmartLisp system to the top level. 
A value of true is returned. The reset procedure is often used to 
terminate from a nested procedure. 

An example follows.

    (define  (quit?)
        (if  (are-we-done?)  (reset)))  =>  #<Procedure 453>

#endif

TVAL FUtil2_Reset(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
argc = argc; // NOOP to hide unused parameter warning message
argv = argv; // NOOP to hide unused parameter warning message
FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
return(gCP->Tval_VOID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FUtil2_Quit

The quit cProcedure shuts the SmartBase engine down and returns control
to the host operating system. 

An example follows.

    (quit)  =>  ...shuts the SmartBase engine down...

#endif

TVAL FUtil2_Quit(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
argc = argc; // NOOP to hide unused parameter warning message
argv = argv; // NOOP to hide unused parameter warning message
FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_QUIT);
return(gCP->Tval_VOID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FUtil2_Callcc

The FUtil2_Callcc function creates a Continuation object and passes it to the
specified Procedure object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FUtil2_Callcc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
StartFrame
DeclareOBJ(TContinuation,cp);
DeclareTVAL(ret);
DeclareTVAL(parm);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 1 || (asTag(&argv[0]) != TYLAMBDA) )
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Create a new Continuation object.  */

gTP->TLambda_TheContinuation = NULL;
asObject(parm) = (TObject*)TContinuation_New(gCP, gTP);
asTag(parm) = TYCONTINUATION;
cp = asContinuation(parm);

/*  Pass the new Continuation object to the specified Procedure and */
/*  reset the scope of self.  */

*ret = _VmEvaluate(asProcedure(&argv[0]),(NUM)1,parm);
/**ec = lspCall(asObj(&argv[0]),(NUM)1,parm,*ret); */
cp->inScope = FALSE;

/*  Has the new Continuation been evaluated by the Procedure?  */

if (isERROR(ret) && !strcmp((char*)asText(ret), (char*)asText(&gCP->TObject_ERROR_CONTINUATION)))
    {
    if (cp == gTP->TLambda_TheContinuation)
        {
        *ret = cp->itsResult;
        gTP->TLambda_TheContinuation = NULL;
        }
    FrameExit(*ret);
    }
else
    gTP->TLambda_TheContinuation = NULL;
    
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Ref

Return the indexed value of a aything. An invalid index 
returns the empty list (). All integer indices (Strings, Symbols, Lists, 
Buffers, Spreadsheets, and Vectors) start at 0.

We support objects which will take 1,2 or 3 indices.

In addition, we support nonindexed referencing of arguments as in: (ref arg).

Note:   (ref A1) will return the contents of cell A1.
        (ref A1:B6) will return the absolute range $A$1:$B$6.
        (ref all-else) will return the argument without alteration.

Note:   We must support multiple indices, in which case, the left hand indices
        operating on the object must produce an object which can be indexed by
        the right hand indices.

#endif

TVAL FUtil2_Ref(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = gCP->Tval_VOID;

/* If the first argument is #void, then return #void, */
/* but do not generate an error message. */

if ((argc > 0) && (argv[0].Tag == TYVOID))
	{
	FrameExit(gCP->Tval_VOID);
	}

switch(argc)
    {
    case 1:
        /*  Manage nonindexed object references */
        
        /* Neither a range nor a cell, return the thing itself. */
            
        *ret= argv[0];
        break;
    
    case 2:
		if (argv[0].Tag == TYNUM)
			{
			/*  Load the value from the source register pointer, indexed by the integer index. */
			/*  Note: The source DeclaredType field tells the type of register pointer. */
			if (argv[1].Tag == TYNUM)
				{
				switch(argv[0].DeclaredType)
					{
					case TYCHARPOINTER:
						ret->u.Int = (NUM)((LpCHAR)argv[0].u.Int)[argv[1].u.Int];
						ret->Tag = TYCHAR;
						break;

					case TYFLOATPOINTER:
						ret->u.Real = (REAL)((LpFLOAT)argv[0].u.Int)[argv[1].u.Int];
						ret->Tag = TYREAL;
						break;

					case TYINTPOINTER:
					case TYJUMPPOINTER:
						ret->u.Int = (NUM)((LpNUM)argv[0].u.Int)[argv[1].u.Int];
						ret->Tag = TYNUM;
						break;

					case TYREALPOINTER:
						ret->u.Real = (REAL)((LpREAL)argv[0].u.Int)[argv[1].u.Int];
						ret->Tag = TYREAL;
						break;

					case TYSHORTPOINTER:
						ret->u.Int = (NUM)((LpSHORT)argv[0].u.Int)[argv[1].u.Int];
						ret->Tag = TYNUM;
						break;

					case TYLONGPOINTER:
						ret->u.Int = (NUM)((LpNUM32)argv[0].u.Int)[argv[1].u.Int];
						ret->Tag = TYNUM;
						break;

					case TYWORDPOINTER:
						*ret = ((TVAL*)argv[0].u.Int)[argv[1].u.Int];
						break;

					default:
						*ret = gCP->TObject_ERROR_INVALID_ARGLIST;
						break;
					}
				}
			}
		else
        if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
            *ret = (*_TObject_TypeGetIV1(argv[0].Tag))(gCP,gTP,argv[0],argv[1]);
        else
            *ret = TObject_GetIV1(gCP,gTP,argv[0],argv[1]);
    break;
    
    case 3:
        if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
            *ret = (*_TObject_TypeGetIV2(argv[0].Tag))(gCP,gTP, argv[0],argv[1], argv[2]);
        else
            *ret = TObject_GetIV2(gCP,gTP,argv[0],argv[1],argv[2]);
    break;
    
    case 4:
        if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
            *ret = (*_TObject_TypeGetIV3(argv[0].Tag))(gCP,gTP,argv[0],argv[1], argv[2], argv[3]);
        else
            *ret = TObject_GetIV3(gCP,gTP,argv[0],argv[1],argv[2],argv[3]);
    break;
    
    default:
        *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    break;
    }

FrameExit(*ret);

}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_RefKey

Return the indexed key of a aything.

#endif

TVAL FUtil2_RefKey(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = gCP->Tval_VOID;

/* Make sure only the key is returned. */

switch(argc)
    {
    case 2:
        if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
            *ret = (*_TObject_TypeGetIV2(argv[0].Tag))(gCP,gTP, argv[0],TINT(0), argv[2]);
        else
            *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    break;
    
    default:
        *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    break;
    }

FrameExit(*ret);

}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_RefValue

Return the indexed value of a aything.

#endif

TVAL FUtil2_RefValue(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = gCP->Tval_VOID;

/* Make sure only the value is returned. */

switch(argc)
    {
    case 2:
        if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
            *ret = (*_TObject_TypeGetIV2(argv[0].Tag))(gCP,gTP, argv[0],TINT(1), argv[2]);
        else
            *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    break;
    
    default:
        *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    break;
    }

FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FUtil2_SetVariable

Set a value in a SmartLisp Structure chain. The value is set if the 
specified symbol is found in the current bindings, or any of the parent 
Structures.

Note:   The FUtil2_SetVariable function operates correctly if envTval is _globals.
        In this case, the value is set in the global symbol table.

#endif

TVAL FUtil2_SetVariable(LpXCONTEXT gCP,LpTHREAD gTP,TVAL envTval,TVAL sym, TVAL newTval)
{
StartFrame
DeclareOBJ(TStructure,sp);
DeclareTVAL(ret);
EndFrame

/*  If envTval is nil, return an error condition. */

if (isNullTval(&envTval)) FrameExit(gCP->TObject_ERROR_BADSYMBOL);

/*  If envTval is _globals, set the global symbol table value. */

if (asTag(&envTval) == TYGLOBALS) 
    {
	*ret = TSymbol_SetIVGlobal(gCP,gTP,sym,newTval);
    FrameExit(*ret);
    }

if (asTag(&envTval) == TYSTRUCTURE)
    {
    
    /*  Check the current bindings for a matching symbol */
    
    sp = (TStructure*)asObject(&envTval);
    *ret = (*_TObject_TypeSetIV1(asTag(&envTval)))(gCP,gTP, envTval, sym, newTval);
    if(!isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        {
        
        /*  Check the parent Structure for a matching symbol */
        *ret = FUtil2_SetVariable(gCP, gTP,sp->itsCdr,sym, newTval);
        FrameExit(*ret);
        }
    }
FrameExit(gCP->TObject_ERROR_BADSYMBOL);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FUtil2_BoundVariable

Find a symbol in a SmartLisp Structure chain. True is returned if the
specified symbol is found in the current bindings, or any of the parent 
Structures. Otherwise, a false is returned.

Note:   The FUtil2_BoundVariable function operates correctly if anEnvTval is TYGLOBALS. 
        In this case, FUtil2_BoundVariable always returns true.

#endif

TVAL FUtil2_BoundVariable(LpXCONTEXT gCP,LpTHREAD gTP, TVAL aTval, TSymbol *symName)
{
StartFrame
DeclareOBJ(TStructure,aStructure);
DeclareTVAL(ret);
DeclareTVAL(index);
EndFrame

/*  Return if it is this the nil Structure */
if (isNullTval(&aTval)) FrameExit(gCP->TObject_ERROR_INVALID);

/*  Return TObject_OKif it is this the global Structure */
if (asTag(&aTval) == TYGLOBALS) FrameExit(gCP->TObject_OK);

/*  Check the current Structure for a matching symbol.  */
if (asTag(&aTval) == TYSTRUCTURE)
    {
    aStructure = (TStructure*)asObject(&aTval);
    asTag(index) = TYSYMBOL;
    asObject(index) = (TObject*)symName;
    
    /*  Check the current Structure bindings for a matching symbol */
    *ret = (*_TObject_TypeGetIV1(asTag(&aTval)))(gCP,gTP, aTval, *index);
    if(!isERROR(ret))
        {
        FrameExit(gCP->TObject_OK);
        }
    else
        {
        /* Check its cdr for a matching symbol. */
		*ret = FUtil2_BoundVariable(gCP,gTP,aStructure->itsCdr,symName);
        FrameExit(*ret);
        }
    }
FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FUtil2_GetVariable

Get a value from a SmartLisp Structure chain. The value is returned 
if the specified symbol is found in the current bindings, any of the parent 
Structures, otherwise it is returned from the global value for the symbol.

Note:   The FUtil2_GetVariable function operates correctly if envTval is _globals.
        In this case, the value is sought in the global symbol table.

#endif

TVAL FUtil2_GetVariable(LpXCONTEXT gCP,LpTHREAD gTP, TVAL envTval, TSymbol* symbol )
{
StartFrame
DeclareOBJ(TStructure,sp);
DeclareTVAL(ret);
DeclareTVAL(sym);
EndFrame

/*  If envTval is nil, return a no match condition. */

*ret = gCP->Tval_VOID;
if (isNullTval(&envTval)) FrameExit(gCP->TObject_ERROR_BADSYMBOL);

asTag(sym) = TYSYMBOL;
asObject(sym) = (TObject*)symbol;

/*  If envTval is _globals, return the global symbol table value. */

if (asTag(&envTval) == TYGLOBALS) 
    {
	*ret = TSymbol_GetIVGlobal(gCP,gTP,*sym);
    FrameExit(*ret);
    }

/*  If envTval is a Structure, search it and its parent. */

if (asTag(&envTval) == TYSTRUCTURE)
    {
    
    /*  Check the current bindings for a matching symbol */
    
    sp = (TStructure*)asObject(&envTval);
    *ret = (*_TObject_TypeGetIV1(asTag(&envTval)))(gCP,gTP, envTval, *sym);
    if(!isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        {
        
        /*  Check the parent Structure for a matching symbol */
        *ret =  FUtil2_GetVariable(gCP,gTP,sp->itsCdr,symbol);
        FrameExit(*ret);
        }
    }
FrameExit(gCP->TObject_ERROR_BADSYMBOL);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Copy

Return an exact copy of the source type.

#endif

TVAL FUtil2_Copy(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
if (isNullTval(&argv[0])) FrameExit(argv[0]);

*ret = FObject_Copy(gCP,gTP,argv[0]);
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_QuickAppend

Return an exact copy of the source type.

#endif

TVAL FUtil2_QuickAppend(LpXCONTEXT gCP,LpTHREAD gTP,LpTVAL first, LpTVAL second)     
{
TVAL	argv[2];
 
argv[0] = *first;
argv[1] = *second;
return(FUtil2_Append(gCP,gTP,2,argv));
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Append

Concatenate multiple objects together and return the concatenated result. The
first argument determines the type of the result. The append Procedure can
operate on the following types: a Structure, a List, a String, 
a Buffer, or a Vector.

#endif

TVAL FUtil2_Append(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
HMChar                  theData = NULL;
LpCHAR                  sourcePtr;
LpCHAR                  resultPtr;
NUM                     sourceLength = 0;
NUM                     resultLength;
NUM                     totalLength;
NUM                     lengthOf;
NUM                     indexOf;
NUM                     indexSource;
NUM                     indexResult;
LpCHAR					strbuf;
CHAR					tmpbuf[256];
HMChar                  sourceHandle;
REAL                    *srcp, *dstp;
TVAL                    ndx1;
TVAL                    ndx2;
NUM						cn;
TYPE					tag;
StartFrame
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
DeclareOBJ(TPair,pp);
DeclareOBJ(TString,sp);
DeclareOBJ(TString,sourceString);
DeclareOBJ(TVector,vt);
DeclareOBJ(TMatrix,mt);
DeclareOBJ(TNumMatrix,nt);
DeclareOBJ(TBitVector,vBitt);
DeclareOBJ(TByteVector,vBytet);
DeclareOBJ(TNumVector,vNumt);
DeclareOBJ(TFltVector,vFltt);
DeclareOBJ(TIntVector,vIntt);
DeclareOBJ(TShtVector,vShtt);
DeclareOBJ(TLongVector,vLngt);
DeclareOBJ(TObjVector,vObjt);
DeclareOBJ(TStructure,et);
DeclareOBJ(OBJ,copyPtr);
DeclareOBJ(TString,resultString);
DeclareOBJ(TCpxVector,vCpxp);
DeclareOBJ(TCpxVector,vCpxt);
DeclareOBJ(TCpx, cp);
DeclareTVAL(ivTval);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVAL(rp);
EndFrame

/* We ignore void arguments (if they occur at the beginning) */
 
Validation:
if (argc < 1)
	{
	*ret = TERROR("!append: missing or void arguments!");
	FrameExit(*ret);
	}

if (isNullTval(&argv[0]))
	{
	argv = &argv[1];
	--argc;
	goto Validation; 
	}

/*  Initialization */

*ret = gCP->Tval_VOID;
totalLength = 0;
indexOf = 0;

switch(asTag(&argv[0]))
    {
    case TYPAIR:
        /*  Make a copy of each Pair in each list and append it to the result. */
        /*  Note:   All of the source arguments must also be Lists. */
        
        *ret = FObject_Copy(gCP,gTP,argv[0]);
        pp = (TPair*)asObject(ret);
        while (++indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYPAIR) || (asObj(&argv[indexOf]) == NIL))
				{
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
				}

            *rp = FObject_Copy(gCP,gTP,argv[indexOf]);

            while ((asTag(&pp->itsCdr) == TYPAIR) && (asObj(&pp->itsCdr) != NIL))
                {
                pp = (TPair*)asObject(&pp->itsCdr);
                }
            pp->itsCdr = *rp;
            }
        FrameExit(*ret); 
    break;
    
    case TYSTRUCTURE:
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be Structures. */
        
        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYSTRUCTURE) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TStructure*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Structure object large enough to hold the result. */
        
        et = TStructure_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)et,totalLength);
        asObject(ret) = (TObject*)et;
        asTag(ret) = TYSTRUCTURE;
        ep = (TStructure*)asObject(&argv[0]);
        et->itsCdr = ep->itsCdr;
        
        
        /*  Make a copy of each binding in each Structure and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            ep = (TStructure*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = ep->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMBind(et->itsDictionaryArray,indexResult++) = atHMBind(ep->itsDictionaryArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYSTRUCTURE;
        FrameExit(*ret); 
    break;
        
    case TYCHAR:
    case TYBOLE:
    case TYNUM:
    case TYSHORT:
    case TYREAL:
    case TYMONEY:
    case TYDATE:
    case TYSYMBOL:
    case TYSTRING:
    case TYERROR:
    case TYSTRINGSUBSTR:
    case TYTEXT:
        /*  Convert each argument to a string and append them. */
        /*  First try to fit the result into a tval (as text). */ 
        asTag(ret) = TYTEXT;
        asText(ret)[0] = 0;
        resultPtr = &ret->u.Text[0];
        resultString = NULL;

        indexResult = 0;
        indexOf = 0;
        totalLength = 0;
        resultLength = 0;
        while (indexOf < argc)
            {	/*  Convert each argument to text and load its text pointer. */
            sourceHandle = NIL;

            if ((argv[indexOf].Tag == TYSTRING) && (argv[indexOf].u.Obj != NIL))
                {
                sourceString = ((TString*)(asObject(&argv[indexOf])));
                sourceLength = sourceString->itsMaxItemIndex - (sourceString->itsMaxItemIndex > 0);
                sourceHandle = sourceString->itsCString;
                if (sourceHandle == NULL)
                    {
					NilStringError:
					if (theData != NULL)
						{
						FMemory_Free(gCP,gTP,(HMemory)theData);
						theData = NULL;
						}
                    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
                    }
                sourcePtr = &atHMChar(sourceHandle,0);
                if (sourcePtr == NULL) goto NilStringError;
                }
            else
             if ((argv[indexOf].Tag == TYERROR) && (argv[indexOf].u.Obj != NIL))
                {
                sourceString = ((TString*)(asObject(&argv[indexOf])));
                sourceLength = sourceString->itsMaxItemIndex - (sourceString->itsMaxItemIndex > 0);
                sourceHandle = sourceString->itsCString;
                if (sourceHandle == NULL) goto NilStringError;
                sourcePtr = &atHMChar(sourceHandle,0);
                if (sourcePtr == NULL) goto NilStringError;
                }
            else           
			if ((argv[indexOf].Tag == TYSYMBOL) && (argv[indexOf].u.Obj != NIL))
                {
                sourceString = ((TString*)(asObject(&argv[indexOf])));
                sourceHandle = sourceString->itsCString;
                if (sourceHandle == NULL) goto NilStringError;
                sourcePtr = &atHMChar(sourceHandle,0);
                if (sourcePtr == NULL) goto NilStringError;
                sourceLength = strlen(sourcePtr);
                }
            else
            if ((asTag(&argv[indexOf]) == TYTEXT))
                {
                sourceLength = strlen((char*)&asText(&argv[indexOf])[0]);
                sourcePtr = &asText(&argv[indexOf])[0];
                }
            else
            if ((asTag(&argv[indexOf]) == TYSTRINGSUBSTR))
                {
                sourcePtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[indexOf]);
                if (sourcePtr == NULL) goto NilStringError;
                sourceLength = SubLen(argv[indexOf]);
                }
            else
            if ((asTag(&argv[indexOf]) == TYCHAR))
                {
                sourceLength = 1;
                argv[indexOf].u.Text[1] = 0;
                sourcePtr = &asText(&argv[indexOf])[0];
                }
            else
            if ((argv[indexOf].Tag == TYNUM)	||
			    (argv[indexOf].Tag == TYSHORT)	||
			    (argv[indexOf].Tag == TYREAL)	||
			    (argv[indexOf].Tag == TYMONEY)	||
			    (argv[indexOf].Tag == TYDATE)	||
			    (argv[indexOf].Tag == TYCPX)	||
			    (argv[indexOf].Tag == TYBOLE))
                {
				tmpbuf[0] = 0;
                *ec = TObject_CnvToText(gCP, gTP, tmpbuf, 255, argv[indexOf]);
                sourceLength = strlen((char*)tmpbuf);
                sourcePtr = tmpbuf;
                }
            else
                {
				theData = (HMChar)FMemory_New(gCP,gTP,(NUM)_FSmartbase_MAXBUFFERLEN+2,TRUE);
				strbuf = &atHMChar(theData,0);
                *ec = TObject_CnvToText(gCP, gTP, strbuf, _FSmartbase_MAXBUFFERLEN, argv[indexOf]);
                sourceLength = strlen((char*)strbuf);
                sourcePtr = strbuf;
                }
            
            /*  Compute the total result length. Make sure the result is */
            /*  large enough to hold all the appended characters. Append */
            /*  the source characters to the result. */

            totalLength = resultLength + sourceLength;
            if (totalLength < (MAXTVALTEXTLEN-1))
                {
                resultPtr = &asText(ret)[resultLength];
                strncpy((char *)resultPtr, (char *)sourcePtr, sourceLength);
                resultPtr[sourceLength] = 0;
                }
            else
                {
                /*  Do we need to create a new result string ? */

                if (resultString == NULL)
                    {
                    /*  Make a new String object to hold the result. */
                    
                    resultString = TString_New(gCP,gTP);
                    if (resultString == NULL)
                        {
						if (theData != NULL)
							{
							FMemory_Free(gCP,gTP,(HMemory)theData);
							theData = NULL;
							}
						FrameExit(gCP->TObject_ERROR_OUT_OF_MEMORY); 
                        }
                    FObject_SetMaxIndex(gCP,gTP,(TObject*)resultString,resultLength + 1);
                    strcpy((char *)&atHMChar(resultString->itsCString,0), (char *)&asText(ret)[0]);
                    asObject(ret) = (TObject*)resultString;
                    asTag(ret) = TYSTRING;
                    }

                /*  Resize the result String object to accept the new characters. */
                
                FObject_SetMaxIndex(gCP,gTP,(TObject*)resultString,totalLength + 1);
                strncpy((char *)&atHMChar(resultString->itsCString,resultLength),(char *)sourcePtr, sourceLength);
                atHMChar(resultString->itsCString,totalLength) = 0;
                }
            

            /*  Unlock the source handle (if necessary). */
            
			if (theData != NULL)
				{
				FMemory_Free(gCP,gTP,(HMemory)theData);
				theData = NULL;
				}
            resultLength += sourceLength;
            ++indexOf;
            }
            
		if (theData != NULL)
			{
			FMemory_Free(gCP,gTP,(HMemory)theData);
			theData = NULL;
			}
        FrameExit(*ret); 
    break;
    
    case TYVECTOR:
        {
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYVECTOR) || (asObj(&argv[indexOf]) == NIL))
                {
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
                }
            totalLength += ((TVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        
        vt = TVector_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)vt,totalLength);
        asObject(ret) = (TObject*)vt;
        vp = (TVector*) asObject(&argv[0]);
        vt->itsCdr = vp->itsCdr;
        
        
        /*  Make a copy of each tval in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            vp = (TVector*) asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vp->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMTval(vt->itsTvalArray,indexResult++) = atHMTval(vp->itsTvalArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYVECTOR;
        FrameExit(*ret); 
        }
    break;
        
    case TYMATRIX:
        {
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be matrices. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYMATRIX) || (asObj(&argv[indexOf]) == NIL))
                {
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
                }
            totalLength += ((TMatrix*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        
        mt = TMatrix_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)mt,totalLength);
        ret->u.Matrix = mt;
        ret->Tag = TYMATRIX;
        mp = (TMatrix*) asObject(&argv[0]);
        mt->itsCdr = mp->itsCdr;
        mt->itsRank = 1;
		for (cn=0;cn<_MAXDIMENSIONS;++cn) mt->itsDimensions[cn] = 1;
		mt->itsDimensions[0] = totalLength;
        
        
        /*  Make a copy of each tval in each matrix and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            mp = (TMatrix*) asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = mp->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMTval(mt->itsTvalMatrix,indexResult++) = atHMTval(mp->itsTvalMatrix,indexSource++);
                }
            ++indexOf;  
            }
        FrameExit(*ret); 
        }
    break;
        
    case TYNUMMATRIX:
        {
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be matrices. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYNUMMATRIX) || (asObj(&argv[indexOf]) == NIL))
                {
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
                }
            totalLength += ((TNumMatrix*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new NumMatrix object large enough to hold the result. */
        
        
        nt = TNumMatrix_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)nt,totalLength);
        ret->u.NumMatrix = nt;
        ret->Tag = TYNUMMATRIX;
        np = (TNumMatrix*) asObject(&argv[0]);
        nt->itsCdr = np->itsCdr;
        nt->itsRank = 1;
		for (cn=0;cn<_MAXDIMENSIONS;++cn) nt->itsDimensions[cn] = 1;
		nt->itsDimensions[0] = totalLength;
        
        
        /*  Make a copy of each tval in each matrix and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            np = (TNumMatrix*) asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = np->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMReal(nt->itsRealMatrix,indexResult++) = atHMReal(np->itsRealMatrix,indexSource++);
                }
            ++indexOf;  
            }
        FrameExit(*ret); 
        }
    break;
        
    case TYBITVECTOR:
        {
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYBITVECTOR) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TBitVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        vBitt = TBitVector_New(gCP,gTP);

        asObject(ivTval) = (TObject*)vBitt;
        asTag(ivTval) = vBitt->itsObjectType;
        TBitVector_SetMaxIndex(gCP,gTP,*ivTval, totalLength);

        asObject(ret) = (TObject*)vBitt;
        vBitp = (TBitVector*) asObject(&argv[0]);
        vBitt->itsCdr = vBitp->itsCdr;
        
        /*  Make a copy of each bit in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        asTag(&ndx1) = TYNUM;
        asTag(&ndx2) = TYNUM;
        
        asObject(ivTval) = (TObject*)vBitt;
        asTag(ivTval) = vBitt->itsObjectType;
        
        while (indexOf < argc)
            {
            vBitp = (TBitVector*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vBitp->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                asInt(&ndx1) = indexResult++;
                asInt(&ndx2) = indexSource++;
                (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP, *ivTval, ndx1, 
                    (*_TObject_TypeGetIV1(asTag(&argv[indexOf])))(gCP,gTP,argv[indexOf], ndx2) );
                }
            ++indexOf;  
            }
        asTag(ret) = TYBITVECTOR;
        FrameExit(*ret); 
        }
    break;
        
    case TYBYTEVECTOR:
        {
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYBYTEVECTOR) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TByteVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        vBytet = TByteVector_New(gCP,gTP);
        
        asObject(ivTval) = (TObject*)vBytet;
        asTag(ivTval) = vBytet->itsObjectType;
        TByteVector_SetMaxIndex(gCP,gTP,*ivTval,totalLength);

        asObject(ret) = (TObject*)vBytet;
        vBytep = (TByteVector*)asObject(&argv[0]);
        vBytet->itsCdr = vBytep->itsCdr;
        
        
        /*  Make a copy of each byte in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            vBytep = (TByteVector*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vBytep->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMChar(vBytet->itsByteArray,indexResult++) = atHMChar(vBytep->itsByteArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYBYTEVECTOR;
        FrameExit(*ret); 
        }
    break;
    
    case TYNUMVECTOR:
        {
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYNUMVECTOR) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TNumVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        vNumt = TNumVector_New(gCP, gTP);
        
        asObject(ivTval) = (TObject*)vNumt;
        asTag(ivTval) = vNumt->itsObjectType;
        TNumVector_SetMaxIndex(gCP,gTP,*ivTval,totalLength);
        
        asObject(ret) = (TObject*)vNumt;
        vNump = (TNumVector*)asObject(&argv[0]);
        vNumt->itsCdr = vNump->itsCdr;
        
        /*  Make a copy of each num in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            vNump = (TNumVector*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vNump->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMReal(vNumt->itsRealArray,indexResult++) = atHMReal(vNump->itsRealArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYNUMVECTOR;
        FrameExit(*ret); 
        }
    break;
    
    case TYFLTVECTOR:
        {
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYFLTVECTOR) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TFltVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        vFltt = TFltVector_New(gCP,gTP);
        
        asObject(ivTval) = (TObject*)vFltt;
        asTag(ivTval) = vFltt->itsObjectType;
        TFltVector_SetMaxIndex(gCP,gTP,*ivTval,totalLength);
        
        asObject(ret) = (TObject*)vNumt;
        vFltp = (TFltVector*)asObject(&argv[0]);
        vFltt->itsCdr = vFltp->itsCdr;
        
        /*  Make a copy of each num in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            vFltp = (TFltVector*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vFltp->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMFloat(vFltt->itsFloatArray,indexResult++) = atHMFloat(vFltp->itsFloatArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYFLTVECTOR;
        FrameExit(*ret); 
        }
    break;
    
    case TYINTVECTOR:
        {       
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYINTVECTOR) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TIntVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        vIntt = TIntVector_New(gCP,gTP);

        asObject(ivTval) = (TObject*)vIntt;
        asTag(ivTval) = vIntt->itsObjectType;
        TIntVector_SetMaxIndex(gCP,gTP,*ivTval,totalLength);

        asObject(ret) = (TObject*)vIntt;
        vIntp = (TIntVector*)asObject(&argv[0]);
        vIntt->itsCdr = vIntp->itsCdr;
        
        
        /*  Make a copy of each int in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            vIntp = (TIntVector*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vIntp->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMInt(vIntt->itsIntArray,indexResult++) = atHMInt(vIntp->itsIntArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYINTVECTOR;
        FrameExit(*ret); 
        }
    break;
    
    case TYSHORTVECTOR:
        {       
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYSHORTVECTOR) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TShtVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        vShtt = TShtVector_New(gCP,gTP);

        asObject(ivTval) = (TObject*)vShtt;
        asTag(ivTval) = vShtt->itsObjectType;
        TShtVector_SetMaxIndex(gCP,gTP,*ivTval,totalLength);

        asObject(ret) = (TObject*)vShtt;
        vShtp = (TShtVector*)asObject(&argv[0]);
        vShtt->itsCdr = vShtp->itsCdr;
        
        
        /*  Make a copy of each int in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            vShtp = (TShtVector*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vShtp->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMShort(vShtt->itsShortArray,indexResult++) = atHMShort(vShtp->itsShortArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYSHORTVECTOR;
        FrameExit(*ret); 
        }
    break;
    
    case TYLONGVECTOR:
        {       
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYLONGVECTOR) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TLongVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        vLngt = TLongVector_New(gCP,gTP);

        asObject(ivTval) = (TObject*)vLngt;
        asTag(ivTval) = vLngt->itsObjectType;
        TLongVector_SetMaxIndex(gCP,gTP,*ivTval,totalLength);

        asObject(ret) = (TObject*)vLngt;
        vLngp = (TLongVector*)asObject(&argv[0]);
        vLngt->itsCdr = vLngp->itsCdr;
        
        
        /*  Make a copy of each int in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            vLngp = (TLongVector*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vLngp->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMLong(vLngt->itsLongArray,indexResult++) = atHMLong(vLngp->itsLongArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYLONGVECTOR;
        FrameExit(*ret); 
        }
    break;
    
    case TYOBJVECTOR:
        {
        /*  Compute the total length of all the arguments. */
        /*  Note:   All of the arguments must be object vectors. */

        while (indexOf < argc)
            {
            if ((asTag(&argv[indexOf]) != TYOBJVECTOR) || (asObj(&argv[indexOf]) == NIL))
                FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            totalLength += ((TObjVector*)asObject(&argv[indexOf++]))->itsMaxItemIndex;
            }
            
        
        /*  Make a new Vector object large enough to hold the result. */
        
        vObjt = TObjVector_New(gCP,gTP);
        FObject_SetMaxIndex(gCP,gTP,(TObject*)vObjt,totalLength);
        asObject(ret) = (TObject*)vObjt;
        vObjp = (TObjVector*)asObject(&argv[0]);
        vObjt->itsCdr = vObjp->itsCdr;
        
        
        /*  Make a copy of each int in each vector and append it to the result. */
        
        indexResult = 0;
        indexOf = 0;
        while (indexOf < argc)
            {
            vObjp = (TObjVector*)asObject(&argv[indexOf]);
            indexSource = 0;
            lengthOf = vObjp->itsMaxItemIndex;
            while (indexSource < lengthOf)
                {
                atHMObject(vObjt->itsObjectArray,indexResult++) = atHMObject(vObjp->itsObjectArray,indexSource++);
                }
            ++indexOf;  
            }
        asTag(ret) = TYOBJVECTOR;
        FrameExit(*ret); 
        }
    break;

	case TYCPXVECTOR:
	case TYCPX:
		//  Compute the total length of all the arguments which must be complex vectors
		for (indexOf = 0; indexOf < argc; ++indexOf)
		{	tag = argv[indexOf].Tag;
			vCpxp = argv[indexOf].u.CpxVector;
			if ((tag != TYCPXVECTOR && tag != TYCPX) || vCpxp == NULL)
				FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
			totalLength += (tag == TYCPX) ? 1 : vCpxp->itsMaxItemIndex;
		}
		//  Make a new Vector object large enough to hold the result.
		ret->u.CpxVector = vCpxt = TCpxVector_New(gCP, gTP);
		ret->Tag = TYCPXVECTOR;
		TCpxVector_SetMaxIndex(gCP, gTP, *ret, totalLength);
		// Add first cdr, if any.
		if (argv[0].Tag == TYCPXVECTOR)
		{	vCpxp = argv[0].u.CpxVector;
			vCpxt->itsCdr = vCpxp->itsCdr;
		}
		//  Append elements in each vector to the result.
		dstp = (LpREAL)*vCpxt->itsCpxArray;		
		for (indexOf = 0; indexOf < argc; ++indexOf)
		{	if (argv[indexOf].Tag == TYCPX)
			{	cp = argv[indexOf].u.Complex;
				*dstp++ = cp->itsReal;
				*dstp++ = cp->itsImag;
			}
			else
			{	vCpxp = argv[indexOf].u.CpxVector;
				srcp = (LpREAL)*vCpxp->itsCpxArray;
				lengthOf = 2 * vCpxp->itsMaxItemIndex;
				for (indexSource = 0; indexSource < lengthOf; ++indexSource)
					*dstp++ = *srcp++;
			}
		}
		FrameExit(*ret); 
    break;
    default:
    break;
    }
    
	*ret = TERROR("!append: Unable to recognize argument!");
	FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil2_Boundp

Return the indexed value of a bound object in the specified Structure. An unbound object 
returns false. A bound object returns the index of the binding pair within the specified
Structure. All integer indices start at 0.

#endif

TVAL FUtil2_Boundp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 indexOf;
COMPARE             compareCode;

StartFrame
DeclareTVAL(ret);
EndFrame

/*  There must be only two arguments, a target and an index. */

if (argc != 2)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Search the Structure for the object. */

if (argv[0].Tag == TYSTRUCTURE)
    {
/*  There must be only two arguments, a Structure and an Object. */

if (_TObject_TypeFlag(asTag(&argv[1])) != _TObject_TfTOBJECT)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

    ret->Tag = TYNUM;
    for (indexOf = 0; indexOf < asStructure(&argv[0])->itsMaxItemIndex; ++indexOf)
        {
        if (atHMBind(asStructure(&argv[0])->itsDictionaryArray,indexOf).Key == asObject(&argv[1]))
            {
            ret->u.Int = indexOf;
            FrameExit(*ret);
            }
        }

    FrameExit(gCP->TObject_FALSE);
    }
else        
/*  Search the Dictionary for the object. */

if (argv[0].Tag == TYDICTIONARY)
    {
/*  There must be only two arguments, a Structure and an Object. */

if (_TObject_TypeFlag(asTag(&argv[1])) != _TObject_TfTOBJECT)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

    *ret = TDictionary_BSearchKey(gCP,gTP,Dictionary(argv[0]), asObject(&argv[1]), &compareCode);
    if (compareCode == 0)
        FrameExit(*ret);

    FrameExit(gCP->TObject_FALSE);
    }
else        
/*  Search the Directory for the object. */

if (argv[0].Tag == TYDIRECTORY)
    {
    *ret = TDirectory_BSearchKey(gCP,gTP,Directory(argv[0]),argv[1],&compareCode);
    if (compareCode == 0)
        FrameExit(*ret);

    FrameExit(gCP->TObject_FALSE);
    }
        
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FUtil2_Inspect

The inspect procedure displays the current workspace object statistics
on the Console.

Note:   If no argument is passed, the amount of free memory will be returned.
        If stack: is passed, the TopOfStack Index will be returned.
        If stack: n are passed, the *n value will be returned.
        If Name: n are passed, the object with the specified index will be returned.
        If an object is passed, the Index for the specified object will be returned.
        If an integer is passed, the object with the specified index will be returned.
		If globals: is passed, all current global symbols will be returned in a vector of strings.
        If |objectList|: is passed, a vector of all active objects will be returned in an object vector.
        If true is passed, basic workspace statistics will be returned in a vector of strings.
		If UsedBlockCount: is passed then gCP->FMemory_UsedBlockCount will be returned
		If UsedMemoryBytes: is passed then gCP->FMemory_UsedMemoryBytes will be returned
		If BlockedBlockCount: is passed then gCP->FMemory_BlockedBlockCount will be returned
		If BlockedBlockBytes: is passed then gCP->FMemory_BlockedBlockBytes will be returned
		If FreeBlockCount: is passed then gCP->FMemory_FreeBlockCount will be returned
		If FreeBlockBytes: is passed then gCP->FMemory_FreeBlockBytes will be returned
		If SystemCheckCount: is passed then gCP->FMemory_SystemCheckCount will be returned
		If JoinCount: is passed then gCP->FMemory_JoinCount will be returned
		If SplitCount: is passed then gCP->FMemory_SplitCount will be returned
		If CopyCount: is passed then gCP->FMemory_CopyCount will be returned
        
The inspect Procedure shows the current workspace object statistics on the Console.  
If an argument of true is passed, full statistics for each open object are displayed.  
If an argument of _globals is passed, the global variable bindings are displayed.  
If an integer argument is passed, the specified oid is displayed and returned as a 
tagged value.  
*/
#endif

TVAL FUtil2_Inspect(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
OBJ                     anOID;
NUM                     oid;
NUM                     tmpInt;
char                    buf[500];
StartFrame
DeclareOBJ(TObject,object);
DeclareOBJ(TSymbol,symbol);
DeclareTVAL(ret);
DeclareTVAL(err);
DeclareTVAL(tmp);
DeclareTVAL(objList);
EndFrame

/*  Reset all inspect options. */

*ret = gCP->Tval_VOID;

/*  There must be no more than one argument, and it determines */
/*  the inspect option settings. */

if (argc == 0)
    {
    asTag(ret) = TYNUM;
    asInt(ret) = FMemory_FreeSpace(gCP, gTP);
    FrameExit(*ret);
    }
else
/*  Are we inspecting an object repository. */  

if ((argc >= 1) && (asTag(&argv[0]) == TYOBJREPOSITORY))
    {
    *ret = TDatabase_Inspect(gCP,gTP,argc,argv);
    FrameExit(*ret);
    }	
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"UsedBlockCount")))
	{ return(TINT(gCP->FMemory_UsedBlockCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"UsedMemoryBytes")))
	{ return(TINT(gCP->FMemory_UsedMemoryBytes));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"FreeBlockCount")))
	{ return(TINT(gCP->FMemory_FreeBlockCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"FreeMemoryBytes")))
	{ return(TINT(gCP->FMemory_FreeMemoryBytes));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"SystemCheckCount")))
	{ return(TINT(gCP->FMemory_SystemCheckCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"JoinCount")))
	{ return(TINT(gCP->FMemory_JoinCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"SplitCount")))
	{ return(TINT(gCP->FMemory_SplitCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"CopyCount")))
	{ return(TINT(gCP->FMemory_CopyCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"ReuseCount")))
	{ return(TINT(gCP->FMemory_ReuseCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"FreeListHitCount")))
	{ return(TINT(gCP->FMemory_FreeListHitCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"LargerFrameHitCount")))
	{ return(TINT(gCP->FMemory_LargerFrameHitCount));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"NewTime")))
	{ return(TINT(gCP->FMemory_NewTime));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"FreeTime")))
	{ return(TINT(gCP->FMemory_FreeTime));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"ResizeTime")))
	{ return(TINT(gCP->FMemory_ResizeTime));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"TimerFrequency")))
	{ return(TINT(gCP->FMemory_TimerFrequency));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"Custom01")))
	{ return(TINT(gCP->Custom01));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"Custom02")))
	{ return(TINT(gCP->Custom02));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"Custom03")))
	{ return(TINT(gCP->Custom03));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"Custom04")))
	{ return(TINT(gCP->Custom04));
	}
else
if ((argc == 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"globals")))
    {
	*objList = TObjVector_MakeNew(gCP,gTP,0,argv);
    anOID = asNumIndex(&argv[1]);
    oid = anOID;
	tmpInt = 0;
    for (oid = 1; oid < gCP->TObject_MaxObjectCount; ++oid)
		{
		if (_TObject_ObjectFlag(oid) != _TObject_OfVOID)
			{
			object = _TObject_ObjectByIndex(oid);
			if ((object->itsObjectIndex == oid) && (object->itsObjectType != TYERROR))
				{
				ret->Tag = object->itsObjectType;
				ret->u.Object = object;
				*err = FSmartbase_Set(gCP,gTP,3,*objList,TINT(tmpInt),*ret);
				ExitOnError(*err);
				++tmpInt;
				}
			}
		}

	FrameExit(*objList);
    }
else
if ((argc >= 1) && (asTag(&argv[0]) == TYSYMBOL) && (asSymbol(&argv[0]) == TSymbol_MakeUnique(gCP,gTP,"stack")))
    {
    if (argc == 1)
        {
        FrameExit(TINT(TopOfStack));
        }
    else
    if ((argc == 2) && isNumIndex(&argv[1]))
        {
        tmpInt = asNumIndex(&argv[1]);
        if ((tmpInt >= 0) && (tmpInt < TopOfStack))
            {
            FrameExit(gTP->TvalStack[tmpInt]);
            }
        FrameExit(gCP->TObject_ERROR_BADINDEX);
        }
    else
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    }
else
if ((argc == 2) && (asTag(&argv[0]) == TYSYMBOL) && isNumIndex(&argv[1]))
    {
    anOID = asNumIndex(&argv[1]);
    oid = anOID;
    if ((oid > NIL) && 
        (oid < gCP->TObject_MaxObjectCount) && 
        (_TObject_ObjectFlag(oid) != _TObject_OfVOID))
        {
        object = _TObject_ObjectByIndex(oid);
		if (object->itsObjectIndex == oid)
			{
			asTag(ret) = object->itsObjectType;
			asObj(ret) = (OBJ)object;
			FrameExit(*ret);
			}
		else
			{
			FrameExit(gCP->Tval_VOID);
			}
        }
    else
        {
        FrameExit(gCP->Tval_VOID);
        }
    }
else
if (argc == 1)
    {
    if ((asTag(&argv[0]) == TYBOLE) && (asBool(&argv[0]) == TRUE))
        {
        *ret = FSmartbase_Eval(gCP,gTP,TGVALUE("makeVector"),1,TINT(20));
        ExitOnError(*ret);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Memory Block Count = %i",(LpCHAR)&gCP->FMemory_UsedBlockCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(0),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Used Memory Bytes  = %i",(LpCHAR)&gCP->FMemory_UsedMemoryBytes);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(1),*tmp);
        ExitOnError(*err);


        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Blocked Block Count = %i",(LpCHAR)&gCP->FMemory_BlockedBlockCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(2),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Blocked Memory Bytes  = %i",(LpCHAR)&gCP->FMemory_BlockedMemoryBytes);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(3),*tmp);
        ExitOnError(*err);


        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Free Block Count = %i",(LpCHAR)&gCP->FMemory_FreeBlockCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(4),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Free Block Bytes  = %i",(LpCHAR)&gCP->FMemory_FreeMemoryBytes);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(5),*tmp);
        ExitOnError(*err);

        tmpInt = FMemory_FreeSpace(gCP, gTP);
        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Free User Bytes  = %i",(LpCHAR)&tmpInt);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(6),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"System Check Count = %i",(LpCHAR)&gCP->FMemory_SystemCheckCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(7),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Block Join Count = %i",(LpCHAR)&gCP->FMemory_JoinCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(8),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Block Split Count = %i",(LpCHAR)&gCP->FMemory_SplitCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(9),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Memory Copy Count = %i",(LpCHAR)&gCP->FMemory_CopyCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(10),*tmp);
        ExitOnError(*err);

		FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Block Count = %i",(LpCHAR)&gCP->FMemory_ReuseCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(11),*tmp);
        ExitOnError(*err);

		FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Free List Hit Count = %i",(LpCHAR)&gCP->FMemory_FreeListHitCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(12),*tmp);
        ExitOnError(*err);

		FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Larger Frame Free List Hit Count = %i",(LpCHAR)&gCP->FMemory_LargerFrameHitCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(13),*tmp);
        ExitOnError(*err);

		tmpInt = (gCP->TObject_initMaxObjects*_FSmartbase_ObjectHeaderMaxSize)/1048576;
        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Total Object header memory in MB  = %i",(LpCHAR)&tmpInt);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(14),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Total Object Count  = %i",(LpCHAR)&gCP->TObject_initMaxObjects);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(15),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Used Object Count  = %i",(LpCHAR)&gCP->TObject_UsedObjectCount);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(16),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Free Object Index  = %i",(LpCHAR)&gCP->TObject_FreeObjectIndex);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(17),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"New Block Function Time  = %i",(LpCHAR)&gCP->FMemory_NewTime);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(18),*tmp);
        ExitOnError(*err);

		FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Free Block Function Time = %i",(LpCHAR)&gCP->FMemory_FreeTime);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(19),*tmp);
        ExitOnError(*err);

		FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Resize Block Function Time = %i",(LpCHAR)&gCP->FMemory_ResizeTime);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(20),*tmp);
        ExitOnError(*err);

        FConio_sprintf(gCP,gTP,buf,(LpCHAR)"Garbage Switch  = %b",(LpCHAR)&gCP->TObject_GarbageON);
        *tmp = FSmartbase_CnvFromText(gCP,gTP,buf);
        *err = FSmartbase_Set(gCP,gTP,3,*ret,TINT(21),*tmp);
        ExitOnError(*err);
        FrameExit(*ret);
        }
    else
    if (asTag(&argv[0]) == TYWORKSPACE)
        {
        *ret = FSmartbase_Evals(gCP,gTP,"(map (lambda(x) (ref x name:)) _currentViews)",FALSE);
        FrameExit(*ret);
        }
    else
    if (isNumIndex(&argv[0]))
        {
		anOID = asNumIndex(&argv[0]);
		oid = anOID;
		if ((oid > NIL) && 
			(oid < gCP->TObject_MaxObjectCount) && 
			(_TObject_ObjectFlag(oid) != _TObject_OfVOID))
			{
			object = _TObject_ObjectByIndex(oid);
			if (object->itsObjectIndex == oid)
				{
				asTag(ret) = object->itsObjectType;
				asObj(ret) = (OBJ)object;
				FrameExit(*ret);
				}
			else
				{
				FrameExit(gCP->Tval_VOID);
				}
			}
		else
			{
			FrameExit(gCP->Tval_VOID);
			}
        }
    else
    if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT )
        {
        asTag(ret) = TYNUM;
        asInt(ret) = asObject(&argv[0])->itsObjectIndex;
        FrameExit(*ret);
        }
    else
    if (argv[0].Tag == TYVOID)
        {
        asTag(ret) = TYNUM;
        asInt(ret) = 0;
        FrameExit(*ret);
        }
    }
    
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
