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

#define _C_FUTIL3
#define _SMARTBASE

#if 0
FUtil3.c

Implementation of a wide range of utility and support functions which are not associated 
with any specific class of objects but are used to support the SmartLisp environment.

PARENT:             None.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "futil3.h"
#include    "tdatabas.h"
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
#include    "futil1.h"
#include    "futil2.h"
#include    "fpred2.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include	"tcpxvec.h"
#include	"tshortvec.h"
#include	"tlongvec.h"

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_Init

Initialize a part of the Utility portion of the SmartLisp function library.  

#endif

TVAL FUtil3_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FUtil3_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FUtil3_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"associate",(LpFUNC)&FUtil3_Assoc);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"assoc"),  aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"member",(LpFUNC)&FUtil3_Member);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"inside",(LpFUNC)&FUtil3_Inside);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"insert",(LpFUNC)&FUtil3_VectorInsert);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"delete",(LpFUNC)&FUtil3_VectorDelete);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refValues",(LpFUNC)&FUtil3_RefValues);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"refAttributes",(LpFUNC)&FUtil3_RefAttributes);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"setAttributes",(LpFUNC)&FUtil3_SetAttributes);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"version",(LpFUNC)&FUtil3_Version);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isVersion",(LpFUNC)&FUtil3_Version);
ExitOnError(*ec);

FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_Assoc

The FUtil3_Assoc function returns the first pair in a list of Pairs {argv[1]} 
whose car component matches the key argument key {argv[0]}. If no Pair in the 
target has a car component matching the key, then false is returned. 

The target must be a proper list each of whose elements are Pairs. 
To match the key to the car component of each pair in the target, 
FUtil3_Assoc uses FPredicate2_Equal.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FUtil3_Assoc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
LpTVAL                  rp;
StartFrame
DeclareOBJ(TPair,pp);
DeclareOBJ(TPair,kp);
DeclareTVAL(result);
DeclareTVAL(argr);
DeclareTVALArray(prmv,2);

EndFrame
 
/*  Initialization */

asTag(argr) = TYBOLE;
asBool(argr) = FALSE;
if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Check the current List of Pairs for a matching key.  */
/*  If not, return false. */

if ((asTag(&argv[1]) == TYPAIR) && (asObj(&argv[1]) != NIL))
    {
    prmv[1] = argv[0];
    rp = &argv[1];
    while ((asTag(rp) == TYPAIR) && (asObj(rp) != NIL))
        {
        pp = (TPair*)asObject(rp);
        if ((asTag(&pp->itsCar) == TYPAIR) && (asObj(&pp->itsCar) != NIL))
            {
            kp = (TPair*)asObject(&pp->itsCar);
            prmv[0] = kp->itsCar;
            *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
            ExitOnError(*result);
            if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                {
                FrameExit(pp->itsCar);
                }
            }
        rp = &pp->itsCdr;
        }
    
    FrameExit(*argr);
    }

FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_Member

The FUtil3_Member function determines whether or not the specified object {key} 
is a member of the {target}. The target may be a list, a Structure, 
or a Vector.

If the target is a proper list, FUtil3_Member returns the sublist beginning with 
the occurrence of a specific object {key} in the target list {target}.

If the target is a Vector, FUtil3_Member returns the index of the occurrence of 
a specific object {key} in the target vector {target}.

If the target is a Structure, FUtil3_Member returns the index of the occurrence 
of a specific object{key} in the target Structure {target}.

In comparing the {key} with the elements of {target}, the FUtil3_Member function 
uses isEqual.

If {key} is not found, false is returned.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FUtil3_Member(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
LpTVAL                  rp;
NUM                     indexOf;
BOLE                    searchByValue = FALSE;
StartFrame
DeclareOBJ(TStructure,ep);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDirectory,xp);
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
DeclareOBJ(TPair,pp);
DeclareOBJ(TPair,kp);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TObject,anObject);
DeclareOBJ(TCpxVector, vCpxp);
DeclareTVAL(result);
DeclareTVAL(ret);
DeclareTVAL(ndx1);
DeclareTVALArray(prmv,2);

EndFrame
 
/*  Initialization */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

/* Structures, Dictionaries, and Directories have an optional 3rd argument */
if (argc == 3)
	{
	if (argv[1].Tag == TYSTRUCTURE  ||
		argv[1].Tag == TYDICTIONARY   ||
		argv[1].Tag == TYDIRECTORY )
		{
		if (argv[2].Tag == TYNUM)
			if (asInt(&argv[2]) == 0)
				searchByValue = FALSE;
			else
				searchByValue = TRUE;
		else
			FrameExit(*ret);
		}
	else
		FrameExit(*ret);
	}
else

if (argc != 2) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[1]))
    {
    case TYVOID:
		FrameExit(*ret);
    break;

    case TYSTRUCTURE:
        if(asObj(&argv[1]) != NIL)
            {
            ep = (TStructure*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < ep->itsMaxItemIndex;++indexOf)
                {
				/* Do we want to search for the {key} by value ? */
				if (searchByValue == TRUE) 
					{
					/* Search for the {key} by value */
					prmv[0] = atHMBind(ep->itsDictionaryArray,indexOf).Value ;
					prmv[1] = argv[0];
					*result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
					ExitOnError(*result);
					if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
						{
						ret->Tag = TYNUM;
						ret->u.Int = indexOf;
						FrameExit(*ret);
						}
					}
				else
					{
					/* Search for the {key} by key */
					prmv[0].Tag = atHMBind(ep->itsDictionaryArray,indexOf).Key->itsObjectType ;
					prmv[0].u.Object = atHMBind(ep->itsDictionaryArray,indexOf).Key ;
					prmv[1] = argv[0];
					*result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
					ExitOnError(*result);
					if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
						{
						ret->Tag = TYNUM;
						ret->u.Int = indexOf;
						FrameExit(*ret);
						}
					}
                }
            
            FrameExit(*ret);
            }
    break;

    case TYDICTIONARY:
        if(asObj(&argv[1]) != NIL)
            {
            dp = (TDictionary*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < dp->itsMaxItemIndex;++indexOf)
                {
				/* Do we want to search for the {key} by value ? */
				if (searchByValue == TRUE) 
					{
					/* Search for the {key} by value */
					prmv[0] = atHMBind(dp->itsDictionaryArray,indexOf).Value;
					prmv[1] = argv[0];
					*result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
					ExitOnError(*result);
					if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
						{
						ret->Tag = TYNUM;
						ret->u.Int = indexOf;
						FrameExit(*ret);
						}
					}
				else
					{
					/* Search for the {key} by key */
					prmv[0].Tag = atHMBind(dp->itsDictionaryArray,indexOf).Key->itsObjectType ;
					prmv[0].u.Object = atHMBind(dp->itsDictionaryArray,indexOf).Key ;
					prmv[1] = argv[0];
					*result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
					ExitOnError(*result);
					if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
						{
						ret->Tag = TYNUM;
						ret->u.Int = indexOf;
						FrameExit(*ret);
						}
					}
                }
            
            FrameExit(*ret);
            }
    break;

    case TYOBJREPOSITORY:
        if(asObj(&argv[1]) != NIL)
            {
            prmv[0] = TSYMBOL("position");
			*ret = TDatabase_GetIV2(gCP,gTP,argv[1],prmv[0],argv[0]);
			FrameExit(*ret);
            }
	break;

    case TYDIRECTORY:
        if(asObj(&argv[1]) != NIL)
            {
            xp = (TDirectory*)asObject(&argv[1]);
            prmv[1] = argv[0];
            for (indexOf = 0;indexOf < xp->itsMaxItemIndex;++indexOf)
                {
				if (searchByValue == TRUE)
					prmv[0] = atHMPBind(xp->itsDirectoryArray,indexOf).Value;
				else
					prmv[0] = atHMPBind(xp->itsDirectoryArray,indexOf).Key;
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
               }
            
            FrameExit(*ret);
            }
    break;

    case TYVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vp = (TVector*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < vp->itsMaxItemIndex;++indexOf)
                {
                prmv[0] = atHMTval(vp->itsTvalArray,indexOf);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            
            FrameExit(*ret);
            }
    break;
    
    case TYMATRIX:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            mp = (TMatrix*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < mp->itsMaxItemIndex;++indexOf)
                {
                prmv[0] = atHMTval(mp->itsTvalMatrix,indexOf);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            
            FrameExit(*ret);
            }
    break;
    
    case TYNUMMATRIX:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            np = (TNumMatrix*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < np->itsMaxItemIndex;++indexOf)
                {
                prmv[0] = TREAL(atHMReal(np->itsRealMatrix,indexOf));
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            
            FrameExit(*ret);
            }
    break;
    
    case TYBITVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vBitp = (TBitVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vBitp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYBYTEVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vBytep = (TByteVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vBytep->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYINTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vIntp = (TIntVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vIntp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP, gTP, argv[1], *ndx1);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYSHORTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vShtp = (TShtVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vShtp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP, gTP, argv[1], *ndx1);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYLONGVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vLngp = (TLongVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vLngp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP, gTP, argv[1], *ndx1);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYNUMVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vNump = (TNumVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vNump->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP, argv[1], *ndx1);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYFLTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vFltp = (TFltVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vFltp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYOBJVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vObjp = (TObjVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vObjp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;
	case TYCPXVECTOR:
		if (asObj(&argv[1]) != NIL)
		{	prmv[1] = argv[0];
			vCpxp = (TCpxVector*)asObject(&argv[1]);
			asTag(ndx1) = TYNUM;
			for (indexOf = 0;indexOf < vCpxp->itsMaxItemIndex;++indexOf)
			{	asInt(ndx1) = indexOf;
				prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP, argv[1], *ndx1);
				*result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
				ExitOnError(*result);
				if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
				{	ret->Tag = TYNUM;
					ret->u.Int = indexOf;
					FrameExit(*ret);
				}
			}
			FrameExit(*ret);
		}
	break;
    default:
    break;
    
    case TYPAIR:
    case TYQUOTEDPAIR:
        if ((asTag(&argv[1]) == TYPAIR || asTag(&argv[1]) == TYQUOTEDPAIR) && (asObj(&argv[1]) != NIL))
            {
            prmv[1] = argv[0];
            rp = &argv[1];
			indexOf = 0;
            while ((asTag(rp) == TYPAIR || asTag(rp) == TYQUOTEDPAIR) && (asObj(rp) != NIL))
                {
                pp = (TPair*)asObject(rp);
                prmv[0] = pp->itsCar;
                *result = FPredicate2_Equal(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
					//*ret = *rp;
                    //FrameExit(*ret);
                    }
                rp = &pp->itsCdr;
				++indexOf;

                }
            
            FrameExit(*ret);
            }
        break;
    }

FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_Inside

The FUtil3_Inside function determines whether or not the specified object {key} 
is a member of the {target}. The target may be a list, a Structure, 
or a Vector.

If the target is a proper list, FUtil3_Inside returns the sublist beginning with 
the occurrence of a specific object {key} in the target list {target}.

If the target is a Vector, FUtil3_Inside returns the index of the occurrence of 
a specific object {key} in the target vector {target}.

If the target is a Structure, FUtil3_Inside returns the index of the occurrence 
of a specific object{key} in the target Structure {target}.

In comparing the {key} with the elements of {target}, the FUtil3_Inside function 
uses isEqual.

If {key} is not found, false is returned.

Note:   Exactly two arguments are expected, anything else is an error.
		The inside function uses the isIdentical predicate.

#endif

TVAL FUtil3_Inside(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
LpTVAL                  rp;
NUM                     indexOf;
BOLE                    searchByValue = FALSE;
StartFrame
DeclareOBJ(TStructure,ep);
DeclareOBJ(TDictionary,dp);
DeclareOBJ(TDirectory,xp);
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
DeclareOBJ(TPair,pp);
DeclareOBJ(TPair,kp);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TObject,anObject);
DeclareOBJ(TCpxVector, vCpxp);
DeclareTVAL(result);
DeclareTVAL(ret);
DeclareTVAL(ndx1);
DeclareTVALArray(prmv,2);

EndFrame
 
/*  Initialization */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

/* Structures, Dictionaries, and Directories have an optional 3rd argument */
if (argc == 3)
	{
	if (argv[1].Tag == TYSTRUCTURE  ||
		argv[1].Tag == TYDICTIONARY   ||
		argv[1].Tag == TYDIRECTORY )
		{
		if (argv[2].Tag == TYNUM)
			if (asInt(&argv[2]) == 0)
				searchByValue = FALSE;
			else
				searchByValue = TRUE;
		else
			FrameExit(*ret);
		}
	else
		FrameExit(*ret);
	}
else

if (argc != 2) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[1]))
    {
    case TYVOID:
		FrameExit(*ret);
    break;

    case TYSTRUCTURE:
        if(asObj(&argv[1]) != NIL)
            {
            ep = (TStructure*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < ep->itsMaxItemIndex;++indexOf)
                {
				/* Do we want to search for the {key} by value ? */
				if (searchByValue == TRUE) 
					{
					/* Search for the {key} by value */
					prmv[0] = atHMBind(ep->itsDictionaryArray,indexOf).Value ;
					prmv[1] = argv[0];
					*result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
					ExitOnError(*result);
					if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
						{
						ret->Tag = TYNUM;
						ret->u.Int = indexOf;
						FrameExit(*ret);
						}
					}
				else
					{
					/* Search for the {key} by key */
					prmv[0].Tag = atHMBind(ep->itsDictionaryArray,indexOf).Key->itsObjectType ;
					prmv[0].u.Object = atHMBind(ep->itsDictionaryArray,indexOf).Key ;
					prmv[1] = argv[0];
					*result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
					ExitOnError(*result);
					if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
						{
						ret->Tag = TYNUM;
						ret->u.Int = indexOf;
						FrameExit(*ret);
						}
					}
                }
            
            FrameExit(*ret);
            }
    break;

    case TYDICTIONARY:
        if(asObj(&argv[1]) != NIL)
            {
            dp = (TDictionary*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < dp->itsMaxItemIndex;++indexOf)
                {
				/* Do we want to search for the {key} by value ? */
				if (searchByValue == TRUE) 
					{
					/* Search for the {key} by value */
					prmv[0] = atHMBind(dp->itsDictionaryArray,indexOf).Value;
					prmv[1] = argv[0];
					*result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
					ExitOnError(*result);
					if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
						{
						ret->Tag = TYNUM;
						ret->u.Int = indexOf;
						FrameExit(*ret);
						}
					}
				else
					{
					/* Search for the {key} by key */
					prmv[0].Tag = atHMBind(dp->itsDictionaryArray,indexOf).Key->itsObjectType ;
					prmv[0].u.Object = atHMBind(dp->itsDictionaryArray,indexOf).Key ;
					prmv[1] = argv[0];
					*result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
					ExitOnError(*result);
					if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
						{
						ret->Tag = TYNUM;
						ret->u.Int = indexOf;
						FrameExit(*ret);
						}
					}
                }
            
            FrameExit(*ret);
            }
    break;

    case TYOBJREPOSITORY:
        if(asObj(&argv[1]) != NIL)
            {
            prmv[0] = TSYMBOL("position");
			*ret = TDatabase_GetIV2(gCP,gTP,argv[1],prmv[0],argv[0]);
			FrameExit(*ret);
            }
	break;

    case TYDIRECTORY:
        if(asObj(&argv[1]) != NIL)
            {
            xp = (TDirectory*)asObject(&argv[1]);
            prmv[1] = argv[0];
            for (indexOf = 0;indexOf < xp->itsMaxItemIndex;++indexOf)
                {
				if (searchByValue == TRUE)
					prmv[0] = atHMPBind(xp->itsDirectoryArray,indexOf).Value;
				else
					prmv[0] = atHMPBind(xp->itsDirectoryArray,indexOf).Key;
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
               }
            
            FrameExit(*ret);
            }
    break;

    case TYVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vp = (TVector*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < vp->itsMaxItemIndex;++indexOf)
                {
                prmv[0] = atHMTval(vp->itsTvalArray,indexOf);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            
            FrameExit(*ret);
            }
    break;
    
    case TYMATRIX:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            mp = (TMatrix*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < mp->itsMaxItemIndex;++indexOf)
                {
                prmv[0] = atHMTval(mp->itsTvalMatrix,indexOf);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            
            FrameExit(*ret);
            }
    break;
    
    case TYNUMMATRIX:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            np = (TNumMatrix*)asObject(&argv[1]);
            for (indexOf = 0;indexOf < np->itsMaxItemIndex;++indexOf)
                {
                prmv[0] = TREAL(atHMReal(np->itsRealMatrix,indexOf));
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            
            FrameExit(*ret);
            }
    break;
    
    case TYBITVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vBitp = (TBitVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vBitp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYBYTEVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vBytep = (TByteVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vBytep->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYINTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vIntp = (TIntVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vIntp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP, gTP, argv[1], *ndx1);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYSHORTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vShtp = (TShtVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vShtp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP, gTP, argv[1], *ndx1);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYLONGVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vLngp = (TLongVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vLngp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP, gTP, argv[1], *ndx1);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYNUMVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vNump = (TNumVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vNump->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP, argv[1], *ndx1);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYFLTVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vFltp = (TFltVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vFltp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;

    case TYOBJVECTOR:
        if (asObj(&argv[1]) != NIL)
            {
            prmv[1] = argv[0];
            vObjp = (TObjVector*)asObject(&argv[1]);
            asTag(ndx1) = TYNUM;
            for (indexOf = 0;indexOf < vObjp->itsMaxItemIndex;++indexOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP,argv[1], *ndx1);
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
                    }
                }
            FrameExit(*ret);
            }
    break;
	case TYCPXVECTOR:
		if (asObj(&argv[1]) != NIL)
		{	prmv[1] = argv[0];
			vCpxp = (TCpxVector*)asObject(&argv[1]);
			asTag(ndx1) = TYNUM;
			for (indexOf = 0;indexOf < vCpxp->itsMaxItemIndex;++indexOf)
			{	asInt(ndx1) = indexOf;
				prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP, argv[1], *ndx1);
				*result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
				ExitOnError(*result);
				if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
				{	ret->Tag = TYNUM;
					ret->u.Int = indexOf;
					FrameExit(*ret);
				}
			}
			FrameExit(*ret);
		}
	break;
    default:
    break;
    
    case TYPAIR:
    case TYQUOTEDPAIR:
        if ((asTag(&argv[1]) == TYPAIR || asTag(&argv[1]) == TYQUOTEDPAIR) && (asObj(&argv[1]) != NIL))
            {
            prmv[1] = argv[0];
            rp = &argv[1];
			indexOf = 0;
            while ((asTag(rp) == TYPAIR || asTag(rp) == TYQUOTEDPAIR) && (asObj(rp) != NIL))
                {
                pp = (TPair*)asObject(rp);
                prmv[0] = pp->itsCar;
                *result = FPredicate2_Identicalp(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*result);
                if ((asTag(result) == TYBOLE) && (asBool(result) == TRUE))
                    {
                    ret->Tag = TYNUM;
                    ret->u.Int = indexOf;
                    FrameExit(*ret);
					//*ret = *rp;
                    //FrameExit(*ret);
                    }
                rp = &pp->itsCdr;
				++indexOf;

                }
            
            FrameExit(*ret);
            }
        break;
    }

FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_VectorInsert

The vectorInsert Procedure destructively inserts the specified new value into the 
vector at the specified index location.  The vector is increased in size to accomodate 
the new value. 

For example

    (vectorInsert  #(4.2  6  5.1)  1  3)  =>  #(4.2  3  6  5.1)

Note:   The vectorInsert procedure operates on all Vector types.

#endif

TVAL FUtil3_VectorInsert(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
 StartFrame
 DeclareTVAL(err);
 EndFrame

/*  Handle each different type of vector. */

switch (asTag(&argv[0]))
    {
    case TYVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TVector_Insert(gCP, gTP, argv[0], argv[1], argv[2]);
    break;
        
    case TYMATRIX:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TMatrix_Insert(gCP, gTP, argv[0], argv[1], argv[2]);
    break;
        
    case TYNUMMATRIX:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TNumMatrix_Insert(gCP, gTP, argv[0], argv[1], argv[2]);
    break;
        
    case TYBITVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TBitVector_Insert(gCP, gTP, argv[0], argv[1], argv[2]);
    break;
    
    case TYBYTEVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TByteVector_Insert(gCP, gTP, argv[0], argv[1], argv[2]);
    break;
    
    case TYINTVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TIntVector_Insert(gCP,gTP,argv[0], argv[1], argv[2]);
    break;
    
    case TYSHORTVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TShtVector_Insert(gCP,gTP,argv[0], argv[1], argv[2]);
    break;
    
    case TYLONGVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TLongVector_Insert(gCP,gTP,argv[0], argv[1], argv[2]);
    break;
    
    case TYNUMVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TNumVector_Insert(gCP,gTP,argv[0],argv[1],argv[2]);
    break;
    
    case TYFLTVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TFltVector_Insert(gCP,gTP,argv[0],argv[1],argv[2]);
    break;
    
    case TYOBJVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TObjVector_Insert(gCP, gTP, argv[0], argv[1], argv[2]);
    break;
    
    case TYSTRUCTURE:
        if (argc != 4) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TStructure_Insert(gCP,gTP,argv[0],argv[1], argv[2], argv[3]);
    break;
    
    case TYDICTIONARY:
        if (argc != 4) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TDictionary_Insert(gCP,gTP, argv[0],argv[1], argv[2], argv[3]);
    break;
    
    case TYDIRECTORY:
        if (argc != 4) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TDirectory_Insert(gCP, gTP, argv[0],argv[1], argv[2], argv[3]);
    break;
    case TYCPXVECTOR:
        if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *err = TCpxVector_Insert(gCP,gTP,argv[0],argv[1],argv[2]);
    break;
    default:
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    break;
    }
    
if (err->Tag == TYERROR)
	{
	FrameExit(*err);
	}
else
	{
	FrameExit(argv[0]);
	}
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_VectorDelete

The  vectorDelete  Procedure destructively deletes the specified index element 
from the vector.  The vector is decreased in size to accomodate the lost value. 

For example

    (vectorDelete  #(4.2  3  6  5.1)  1)  =>  #(4.2  6  5.1)

Note:   The vectorDelete procedure operates on all Vector types.

#endif

TVAL FUtil3_VectorDelete(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
 TVAL  ret;

/*  Make sure we have the proper number of arguments. */

if (argc != 2) 
    return(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Handle each different type of vector. */

switch (asTag(&argv[0]))
    {
    case TYVECTOR:
        ret = TVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
        
    case TYMATRIX:
        ret = TMatrix_Delete(gCP,gTP,argv[0],argv[1]);
    break;
        
    case TYNUMMATRIX:
        ret = TNumMatrix_Delete(gCP,gTP,argv[0],argv[1]);
    break;
        
    case TYBITVECTOR:
        ret = TBitVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
    
    case TYBYTEVECTOR:
        ret = TByteVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
    
    case TYINTVECTOR:
        ret = TIntVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
    
    case TYSHORTVECTOR:
        ret = TShtVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
    
    case TYLONGVECTOR:
        ret = TLongVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
    
    case TYNUMVECTOR:
        ret = TNumVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
    
    case TYFLTVECTOR:
        ret = TFltVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
    
    case TYOBJVECTOR:
        ret = TObjVector_Delete(gCP,gTP,argv[0], argv[1]);
    break;

    case TYDICTIONARY:
        ret = TDictionary_Delete(gCP,gTP,argv[0],argv[1]);
    break;

    case TYDIRECTORY:
        ret = TDirectory_Delete(gCP, gTP, argv[0], argv[1]);
    break;

    case TYSTRUCTURE:
        ret = TStructure_Delete(gCP, gTP, argv[0], argv[1]);
    break;
    case TYOBJREPOSITORY:
        ret = TDatabase_Delete(gCP, gTP, argv[0], argv[1]);
    break;
    case TYCPXVECTOR:
        ret = TCpxVector_Delete(gCP,gTP,argv[0],argv[1]);
    break;
    default:
        return(gCP->TObject_ERROR_INVALID_ARGLIST);
    break;
    }
    
/* Return the original argument (if no error). */

if (ret.Tag == TYERROR) 
	return(ret);
else
	return(argv[0]);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_Version

The  isVersion  Procedure returns the current SmartBaset version id. 

For example

    (isVersion)  =>  1.6

#endif

TVAL FUtil3_Version(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
EndFrame

argv = argv; // NOOP to hide unused parameter warning message
/*  Make sure we have the proper number of arguments. */

if (argc != 0) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Return the current version id */

*ret = TSTRING(CURRENT_VERSION);

FrameExit(*ret);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FUtil3_Lock

Inclusively locks all the nonempty symbols in the global symbol table.

(lock  _globals)

#endif

TVAL FUtil3_Lock(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 oid;

StartFrame
DeclareOBJ(TSymbol,sym);
DeclareOBJ(TObjVector,vp);
EndFrame

if (argc != 1 || asTag(&argv[0]) != TYGLOBALS) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

for (oid = 0;oid < gCP->TObject_MaxObjectCount; ++oid)
    {
    if ((_TObject_ObjectFlag(oid) != _TObject_OfVOID) &&
        ((_TObject_ObjectByIndex(oid))->itsObjectType == TYSYMBOL))
        {
        sym = (TSymbol*)_TObject_ObjectByIndex(oid);
        if (sym->itsGlobalValue.Tag != TYVOID || 
            (_TObject_ObjectFlag(sym->itsObjectIndex) & _TObject_OfPERM)
            /*|| sym->itsCProcedure != NULL*/)
            {
            sym->itsGlobalLock = TRUE;
            }
        }
    }

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_RefValues

The  refValues  function returns the values vector assigned to this target.

#endif

TVAL FUtil3_RefValues(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM					i;
StartFrame
DeclareTVAL(ret);
EndFrame;

/*  Make sure we have the proper number of arguments. */

if (argc != 1)
	{
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

/*  Return the values Vector assigned to this target. */
/*  Note: This will depend upon the type of the target object. */

switch (argv[0].Tag)
	{
	case TYVECTOR:
		/*  A Vector is its own values vector. */

		*ret = argv[0];
	break;

	case TYSTRUCTURE:
		/*  Create a Vector containing the Values assigned to this target. */

		ret->u.Vector = TVector_New(gCP,gTP);
		ret->Tag = TYVECTOR;
		TVector_SetMaxIndex(gCP,gTP,*ret,Structure(argv[0])->itsMaxItemIndex);

		for (i = 0; i < Structure(argv[0])->itsMaxItemIndex; ++i)
			{
			TvalArray(*ret)[i] = BindArray(argv[0])[i].Value;
			}
	break;

	case TYDICTIONARY:
		/*  Create a Vector containing the Values assigned to this target. */

		ret->u.Vector = TVector_New(gCP,gTP);
		ret->Tag = TYVECTOR;
		TVector_SetMaxIndex(gCP,gTP,*ret,Structure(argv[0])->itsMaxItemIndex);

		for (i = 0; i < Dictionary(argv[0])->itsMaxItemIndex; ++i)
			{
			TvalArray(*ret)[i] = BondArray(argv[0])[i].Value;
			}
	break;

	case TYDIRECTORY:
		/*  Create a Vector containing the Values assigned to this target. */

		ret->u.Vector = TVector_New(gCP,gTP);
		ret->Tag = TYVECTOR;
		TVector_SetMaxIndex(gCP,gTP,*ret,Structure(argv[0])->itsMaxItemIndex);

		for (i = 0; i < Directory(argv[0])->itsMaxItemIndex; ++i)
			{
			TvalArray(*ret)[i] = PBindArray(argv[0])[i].Value;
			}
	break;

	default:
		return(gCP->TObject_ERROR_INVALID_ARGLIST);
	break;
	}

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FUtil3_RefAttributes

The  refAttributes  function returns the attributes object vector assigned to this vector
(if any). 

#endif

TVAL FUtil3_RefAttributes(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM					i;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TVector,self);
DeclareOBJ(TObjVector,attv);
EndFrame

/*  Make sure we have the proper number of arguments. */

if (argc != 1)
	{
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

/*  Set the attributes of a single Vector or of a block of Vectors. */
/*  Note: This will depend upon the type of the target object. */

switch (argv[0].Tag)
	{
	case TYVOID:
		/*  Return the current object vector of attributes assigned to this Vector. */

		*ret = gCP->Tval_VOID;
	break;

	case TYVECTOR:
		/*  Return the current object vector of attributes assigned to this Vector. */

		self = argv[0].u.Vector;
		ret->u.ObjVector = self->itsAttributes;
		if (self->itsAttributes == NULL) 
			*ret = gCP->Tval_VOID;
		else
			ret->Tag =self->itsAttributes->itsObjectType;
	break;

	case TYOBJVECTOR:
		/*  Return an object vector of the attributes in this object. */

		*ret = argv[0];
	break;

	case TYSTRUCTURE:
		/*  Return an object vector of the attributes in this object. */

		attv = TObjVector_New(gCP,gTP);
		*ret = TOBJ(attv);
		TObjVector_SetMaxIndex(gCP,gTP,*ret,Structure(argv[0])->itsMaxItemIndex);
		for (i = 0; i < Structure(argv[0])->itsMaxItemIndex; ++i)
			{
			ObjArray(*ret)[i] = BindArray(argv[0])[i].Key;
			}
	break;

	case TYDICTIONARY:
		/*  Return an object vector of the attributes in this object. */

		attv = TObjVector_New(gCP,gTP);
		*ret = TOBJ(attv);
		TObjVector_SetMaxIndex(gCP,gTP,*ret,Dictionary(argv[0])->itsMaxItemIndex);
		for (i = 0; i < Dictionary(argv[0])->itsMaxItemIndex; ++i)
			{
			ObjArray(*ret)[i] = BondArray(argv[0])[i].Key;
			}
	break;

	case TYDIRECTORY:
		/*  Return an object vector of the attributes in this object. */

		attv = TObjVector_New(gCP,gTP);
		*ret = TOBJ(attv);
		TObjVector_SetMaxIndex(gCP,gTP,*ret,Dictionary(argv[0])->itsMaxItemIndex);
		for (i = 0; i < Directory(argv[0])->itsMaxItemIndex; ++i)
			{
			TObjVector_SetIV1(gCP,gTP,*ret,TINT(i),PBindArray(argv[0])[i].Key);
			}
	break;

	default:
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	break;
	}

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
/*

FUtil3_SetAttributes

The  setAttributes  function sets the attributes object vector assigned to this vector
(if any).

Note:	The setAttributes function can also set the attributes of a block of Vectors. The
		block's spine may be a Dictionary, Directory, Structure, or ObjVector.

*/

TVAL FUtil3_SetAttributes(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
TObjVector*		theAttributes;
NUM				i;

gTP = gTP; // NOOP to hide unused parameter warning message
/*  Make sure we have the proper number of arguments. */

if (argc != 2)
	{
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

/*  Make sure we only assign object vectors or nil as attributes. */

if (argv[1].Tag == TYOBJVECTOR)
	theAttributes = argv[1].u.ObjVector;
else
	theAttributes = NULL;


/*  Set the attributes of a single Vector or of a block of Vectors. */
/*  Note: This will depend upon the type of the target object. */

switch (argv[0].Tag)
	{
	case TYVOID:
		/*  Assign the specified attributes to this Void is a noop, not an error. */

		argv[0] = gCP->Tval_VOID;
	break;

	case TYVECTOR:
		/*  Assign the specified attributes to this Vector. */

		argv[0].u.Vector->itsAttributes = theAttributes;
	break;

	case TYOBJVECTOR:
		/*  Assign the specified attributes to each Vector in this block. */

		for (i = 0; i < ObjVector(argv[0])->itsMaxItemIndex; ++i)
			{
			if ((ObjArray(argv[0])[i] != NULL) && (ObjArray(argv[0])[i]->itsObjectType == TYVECTOR))
				{
				((TVector*)ObjArray(argv[0])[i])->itsAttributes = theAttributes;
				}
			}
	break;

	case TYSTRUCTURE:
		/*  Assign the specified attributes to each Vector in this block. */

		for (i = 0; i < Structure(argv[0])->itsMaxItemIndex; ++i)
			{
			if (BindArray(argv[0])[i].Value.Tag == TYVECTOR)
				{
				BindArray(argv[0])[i].Value.u.Vector->itsAttributes = theAttributes;
				}
			}
	break;

	case TYDICTIONARY:
		/*  Assign the specified attributes to each Vector in this block. */

		for (i = 0; i < Dictionary(argv[0])->itsMaxItemIndex; ++i)
			{
			if (BondArray(argv[0])[i].Value.Tag == TYVECTOR)
				{
				BondArray(argv[0])[i].Value.u.Vector->itsAttributes = theAttributes;
				}
			}
	break;

	case TYDIRECTORY:
		/*  Assign the specified attributes to each Vector in this block. */

		for (i = 0; i < Directory(argv[0])->itsMaxItemIndex; ++i)
			{
			if (PBindArray(argv[0])[i].Value.Tag == TYVECTOR)
				{
				PBindArray(argv[0])[i].Value.u.Vector->itsAttributes = theAttributes;
				}
			}
	break;

	default:
		return(gCP->TObject_ERROR_INVALID_ARGLIST);
	break;
	}

return(argv[0]);
}
