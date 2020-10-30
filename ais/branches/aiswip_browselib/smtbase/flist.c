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

#define _C_FLIST
#define _SMARTBASE

#if 0
FList.c

Implementation of utility functions which are used to support pair and list manipulation.

PARENT:             None. 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  
    
#endif

#include "flist.h"
TVAL FList_Setlastcdr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL FObject_SetCdr(LpXCONTEXT gCP, LpTHREAD gTP, TObject* anObject, TVAL newCdr);

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Init

Initialize the list portion of the SmartLisp function library.  

#endif

TVAL FList_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FList_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FList_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"car",(LpFUNC)&FList_Car);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdr",(LpFUNC)&FList_Cdr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caar",(LpFUNC)&FList_Caar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cadr",(LpFUNC)&FList_Cadr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdar",(LpFUNC)&FList_Cdar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cddr",(LpFUNC)&FList_Cddr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caaar",(LpFUNC)&FList_Caaar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caadr",(LpFUNC)&FList_Caadr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cadar",(LpFUNC)&FList_Cadar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caddr",(LpFUNC)&FList_Caddr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdaar",(LpFUNC)&FList_Cdaar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdadr",(LpFUNC)&FList_Cdadr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cddar",(LpFUNC)&FList_Cddar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdddr",(LpFUNC)&FList_Cdddr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caaaar",(LpFUNC)&FList_Caaaar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caaadr",(LpFUNC)&FList_Caaadr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caadar",(LpFUNC)&FList_Caadar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caaddr",(LpFUNC)&FList_Caaddr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cadaar",(LpFUNC)&FList_Cadaar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cadadr",(LpFUNC)&FList_Cadadr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"caddar",(LpFUNC)&FList_Caddar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cadddr",(LpFUNC)&FList_Cadddr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdaaar",(LpFUNC)&FList_Cdaaar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdaadr",(LpFUNC)&FList_Cdaadr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdadar",(LpFUNC)&FList_Cdadar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdaddr",(LpFUNC)&FList_Cdaddr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cddaar",(LpFUNC)&FList_Cddaar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cddadr",(LpFUNC)&FList_Cddadr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cdddar",(LpFUNC)&FList_Cdddar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cddddr",(LpFUNC)&FList_Cddddr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"setCar",(LpFUNC)&FList_Setcar);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"setCdr",(LpFUNC)&FList_Setcdr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"setLastCdr",(LpFUNC)&FList_Setlastcdr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"last",(LpFUNC)&FList_Last);
ExitOnError(*ec);
alsoSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"last-pair");
TSymbol_SetGlobalValue(gCP,gTP,alsoSymbol, aSymbol->itsGlobalValue);

FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FList_Car

Return the car value of a Structure, a Pair, or a Vector.

Note:   The car of a Structure is the Structure itself. 
        The car of a Vector is the vector itself. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FList_Car(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])   
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,sp);
EndFrame

*ret = gCP->Tval_VOID;

if (argc < 1)
	{
	*ret = TERROR("!car: Missing argument!");
	FrameExit(*ret);
	}

if (argc > 1)
	{
	*ret = TERROR("!car: Too many arguments!");
	FrameExit(*ret);
	}

switch(asTag(&argv[0]))
    {
    case TYPAIR:
    case TYQUOTEDPAIR:
        sp = asPair(&argv[0]);
        *ret = sp->itsCar;
        FrameExit( *ret); 
    break;
    
    case TYSTRUCTURE:
    case TYVECTOR:
    case TYMATRIX:
    case TYNUMMATRIX:
    case TYBITVECTOR:
    case TYBYTEVECTOR:
    case TYINTVECTOR:
    case TYSHORTVECTOR:
    case TYLONGVECTOR:
    case TYNUMVECTOR:
    case TYFLTVECTOR:
    case TYOBJVECTOR:
	case TYCPXVECTOR:
        *ret = argv[0];
        FrameExit( *ret); 
    break;

    default:
        *ret = TERROR("!car: Expecting a Structure, a Pair, or a Vector!");
		FrameExit(*ret);
    break;
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdr

Return the cdr value of any C++ Object.

Note:   An optional integer index may be present. If present, this
		optional integer index will perform the cdr operation repeatedly.
  
#endif

TVAL FList_Cdr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])   
{
NUM			cdrIndex = 0;
NUM			cdrMax = 0;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TObject,op);
EndFrame

if (argc < 1)
	{
	*ret = TERROR("!cdr: Missing argument!");
	FrameExit(*ret);
	}

if (argc > 2)
	{
	*ret = TERROR("!cdr: Too many arguments!");
	FrameExit(*ret);
	}


/*	An optional repeating index may be present. */
if (argc == 2) 
	{
	if (isNumIndex(&argv[1]))
		{
		cdrMax = asNumIndex(&argv[1]);
		goto Good;
		}
	else 
		{
		*ret = TERROR("!cdr: Expecting argument 2 to be a number!");
		FrameExit(*ret);
		}
	}
else
/*	If no optional repeating index is present, set */
/*	the repeat factor to perform the operation once. */
if (argc == 1)
	{
	cdrMax = 1;
	}
else
	{
	*ret = TERROR("!cdr: Too many arguments!");
	FrameExit(*ret);
	}

Good:
*ret = argv[0];

for (cdrIndex = 0; cdrIndex < cdrMax; cdrIndex++)
	{
	if (_TObject_TypeFlag(asTag(ret)) == _TObject_TfTOBJECT)
		{
		op = asObject(ret);
		*ret = FObject_GetCdr(gCP,gTP,asObject(ret));
		ExitOnError(*ret);
		}
	else
		{
		*ret = TERROR("!cdr: Expecting a Structure, a Pair, or a Vector!");
		FrameExit(*ret);
		}
	}

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caar

The FList_Caar function returns the caar of the specified list. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*ret = FList_Car(gCP,gTP,(NUM)1,result1);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cadr

The FList_Cadr function returns the cadr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cadr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*ret = FList_Car(gCP,gTP,(NUM)1,result1);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdar

The FList_Cdar function returns the cdar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result1);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cddr

The FList_Cddr function returns the cddr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cddr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result1);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caaar

The FList_Caaar function returns the caaar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caaar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*ret = FList_Car(gCP,gTP,(NUM)1,result2);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caadr

The FList_Caadr function returns the caadr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caadr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*ret = FList_Car(gCP,gTP,(NUM)1,result2);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cadar

The FList_Cadar function returns the cadar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cadar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*ret = FList_Car(gCP,gTP,(NUM)1,result2);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caddr

The FList_Caddr function returns the caddr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caddr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*ret = FList_Car(gCP,gTP,(NUM)1,result2);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdaar

The FList_Cdaar function returns the cdaar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdaar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result2);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdadr

The FList_Cdadr function returns the cdadr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdadr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result2);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cddar

The FList_Cddar function returns the cddar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cddar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result2);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdddr

The FList_Cdddr function returns the cdddr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdddr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result2);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caaaar

The FList_Caaaar function returns the caaaar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caaaar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*result3 = FList_Car(gCP,gTP,(NUM)1,result2);
*ret = FList_Car(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caaadr

The FList_Caaadr function returns the caaadr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caaadr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*result3 = FList_Car(gCP,gTP,(NUM)1,result2);
*ret = FList_Car(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caadar

The FList_Caadar function returns the caadar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caadar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*result3 = FList_Car(gCP,gTP,(NUM)1,result2);
*ret = FList_Car(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caaddr

The FList_Caaddr function returns the caaddr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caaddr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*result3 = FList_Car(gCP,gTP,(NUM)1,result2);
*ret = FList_Car(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cadaar

The FList_Cadaar function returns the cadaar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cadaar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*result3 = FList_Cdr(gCP,gTP,(NUM)1,result2);
*ret = FList_Car(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cadadr

The FList_Cadadr function returns the cadadr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cadadr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*result3 = FList_Cdr(gCP,gTP,(NUM)1,result2);
*ret = FList_Car(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Caddar

The FList_Caddar function returns the caddar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Caddar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*result3 = FList_Cdr(gCP,gTP,(NUM)1,result2);
*ret = FList_Car(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cadddr
The FList_Cadddr function returns the cadddr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.
#endif

TVAL FList_Cadddr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*result3 = FList_Cdr(gCP,gTP,(NUM)1,result2);
*ret = FList_Car(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdaaar

The FList_Cdaaar function returns the cdaaar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdaaar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*result3 = FList_Car(gCP,gTP,(NUM)1,result2);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdaadr

The FList_Cdaadr function returns the cdaadr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdaadr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*result3 = FList_Car(gCP,gTP,(NUM)1,result2);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdadar

The FList_Cdadar function returns the cdadar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdadar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*result3 = FList_Car(gCP,gTP,(NUM)1,result2);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdaddr

The FList_Cdaddr function returns the cdaddr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdaddr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*result3 = FList_Car(gCP,gTP,(NUM)1,result2);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cddaar

The FList_Cddaar function returns the cddaar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cddaar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*result3 = FList_Cdr(gCP,gTP,(NUM)1,result2);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cddadr

The FList_Cddadr function returns the cddadr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cddadr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Car(gCP,gTP,(NUM)1,result1);
*result3 = FList_Cdr(gCP,gTP,(NUM)1,result2);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cdddar

The FList_Cdddar function returns the cdddar of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cdddar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Car(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*result3 = FList_Cdr(gCP,gTP,(NUM)1,result2);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Cddddr

The FList_Cddddr function returns the cddddr of the specified list. 
Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FList_Cddddr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(result1);
DeclareTVAL(result2);
DeclareTVAL(result3);
EndFrame

*result1 = FList_Cdr(gCP,gTP,argc,argv);
*result2 = FList_Cdr(gCP,gTP,(NUM)1,result1);
*result3 = FList_Cdr(gCP,gTP,(NUM)1,result2);
*ret = FList_Cdr(gCP,gTP,(NUM)1,result3);
FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Setcar

The  setCar  cProcedure sets the car component of the argument {obj} 
to the value of the expression {exp}, and returns the value. The 
argument {obj} must be a Pair. 

Several examples follow.

    (define  Y  `(3  4))                    =>  (3  4)
    Y                                       =>  (3  4)
    (setCar  Y  (*  3  4))					=>  (12  4)
    Y                                       =>  (12  4)

#endif

TVAL FList_Setcar(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
gTP = gTP; // NOOP to hide unused parameter warning message
/*  There must be exactly two arguments,  */
/*  and the first must be a Pair. */

if ((argc != 2) || (isNullTval(&argv[0])) || ((asTag(&argv[0]) != TYPAIR) && (asTag(&argv[0]) != TYQUOTEDPAIR)))
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
 
/*  Set the car of the Pair. */

((TPair*)asObject(&argv[0]))->itsCar = argv[1];
return(argv[0]); 
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Setcdr

The  setCdr  cProcedure sets the cdr component of the argument {obj} 
to the value of the expression {exp}, and returns the object. The 
argument {obj} may be a Pair, a Vector, a Structure. 

Several examples follow.

    (define  Y  `(3  4))                    =>  (3  4)
    Y                                       =>  (3  4)
    (setCdr  Y  (*  3  4))                =>  (3 . 12)
    Y                                       =>  (3 . 12)
    (setCdr  #(1  2  .  3)  22)           =>  #(1  2  .  22)
    (setCdr  #{`A  2}  (*  3  4))         =>  #{`A 2  . 12}
    (setCdr  #[A  2]  `(4))               =>  #[A 2 . (4)]
    (setCdr  Y  `(8))                     =>  (3  8)
    (setCdr  (cdr  Y)  `(8))              =>  (3  4  8)

#endif

TVAL FList_Setcdr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

/*  There must be exactly two arguments.  */

if ((argc != 2) || (isNullTval(&argv[0]))) 
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
    { 
    *ret = FObject_SetCdr(gCP,gTP,asObject(&argv[0]),argv[1]);
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID);
    }

if(isERROR(ret))
    {
    FrameExit(*ret);
    }
else
    {
    FrameExit(argv[0]);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Setlastcdr

The  setLastCdr  cProcedure sets the cdr component of the argument {obj} 
to the value of the expression {exp}, and returns the object. The 
argument {obj} may be a Pair, a Vector, a Structure. 

Several examples follow.

    (define  Y  `(3  4))                    =>  (3  4)
    Y                                       =>  (3  4)
    (setLastCdr  Y  (*  3  4))            =>  (3 4 . 12)
    Y                                       =>  (3 . 12)
    (setLastCdr  #(1  2  .  3)  22)       =>  #(1  2  .  22)
    (setLastCdr  #{`A  2}  (*  3  4))     =>  #{`A 2  . 12}
    (setLastCdr  #[A  2]  `(4))           =>  #[A 2 . (4)]

#endif

TVAL FList_Setlastcdr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(set);
EndFrame

/*  There must be exactly two arguments.  */

if ((argc != 2) || (isNullTval(&argv[0]))) 
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    
if ((asTag(&argv[0]) == TYPAIR) || (asTag(&argv[0]) == TYQUOTEDPAIR))
    {
    *set = FList_Last(gCP,gTP,1, &argv[0]);
    if(isERROR(set))
        {
        FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    }
else
    *set = argv[0];
    
*ret = FObject_SetCdr(gCP,gTP,asObject(set), argv[1]);

if(isERROR(ret))
    {
    FrameExit( *ret);
    }
else
    {
    FrameExit( argv[0]);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
FList_Last

The last  cProcedure returns the last pair in a list {list}. The last pair 
is defined to be the first element of the list whose cdr component is not 
a pair. 

Several examples follow.

    (last  `(1  2  3  4))           =>  (4)
    (last  `(1  2  .  4))           =>  (2  .  4)

#endif

TVAL FList_Last(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
LpTVAL                  rp;
StartFrame
DeclareOBJ(TPair,pp);
DeclareTVAL(ret);
EndFrame


/*  Initialization */

*ret = gCP->Tval_VOID;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Return the last Pair of a proper List. */

if (((asTag(&argv[0]) == TYPAIR) || (asTag(&argv[0]) == TYQUOTEDPAIR)) && (asObj(&argv[0]) != NIL))
    {
    rp = &argv[0];
    while (((asTag(rp) == TYPAIR) || (asTag(rp) == TYQUOTEDPAIR)) && (asObj(rp) != NIL))
        {
        pp = (TPair*)asObject(rp);
        *ret = *rp;
        rp = &pp->itsCdr;
        }
    FrameExit(*ret);
    }
    
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
