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

#define _C_FPROPERTY
#define _SMARTBASE
#if 0
FProperty.c

Implementation of functions which are used to manage properties and structures.

PARENT:             None. 

AUTHORS:            Michael F. Korns

#endif

#include "fpropty.h"
#include "tstruct.h"
#include "tdiction.h"
#include "tdirect.h"
#include "tlambda.h"
#include "fmake.h"
#include "terror.h"
#include "futil2.h"


/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_Init

Initialize the Property portion of the SmartLisp function library.  

#endif

TVAL FProperty_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FProperty_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FProperty_Initialized = 1;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"proprecord",(LpFUNC)&FProperty_Proprecord);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"proplist",(LpFUNC)&FProperty_Proplist);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"putprop",(LpFUNC)&FProperty_Putprop);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"getProp",(LpFUNC)&FProperty_Getprop);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"remProp",(LpFUNC)&FProperty_Remprop);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"addMethod",(LpFUNC)&FProperty_AddMethod);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"methodsOf",(LpFUNC)&FProperty_Methodsof);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"fieldsOf",(LpFUNC)&FProperty_Fieldsof);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"new",(LpFUNC)&FProperty_Factory);
ExitOnError(*ec);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_Proprecord

The proprecord cProcedure returns the property list associated with the 
symbol {name}; however, the property list is returned in the form of 
a Structure. If the argument is not associated with a property list, 
`() is returned. 

Several examples follow.

    (proprecord `BLUE-WHALE)                    =>  ()
    (putprop    `BLUE-WHALE  `OCEAN  `MAMMAL)   =>  OCEAN
    (proprecord `BLUE-WHALE)                    =>  #{`MAMMAL  OCEAN}
    (putprop    `BLUE-WHALE  `YES  `BIG)            =>  YES
    (proprecord `BLUE-WHALE)                    =>  #{`MAMMAL  OCEAN  `BIG  YES}

#endif

TVAL FProperty_Proprecord(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
/*  There must be only one argument, and it must be a symbol! */

*ret = gCP->Tval_VOID;
if ((argc != 1) || (asTag(&argv[0]) != TYSYMBOL)) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Get the property list value associated with the name */

*ret = FUtil2_GetVariable(gCP,gTP,gTP->TLambda_ThePropList,asSymbol(&argv[0]));
if (isERROR(ret))
    *ret = gCP->Tval_VOID;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_Proplist

The proplist  cProcedure returns the property list associated with {name}. 
If the argument is not associated with a property list, `() is returned. 

Several examples follow.

    (proplist  `BLUE-WHALE)                     =>  ()
    (putprop   `BLUE-WHALE  `OCEAN  `MAMMAL)    =>  OCEAN
    (proplist  `BLUE-WHALE)                     =>  (MAMMAL  OCEAN)

#endif

TVAL FProperty_Proplist(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(record);
EndFrame
 
*record = FProperty_Proprecord(gCP,gTP,argc,argv);
ExitOnError(*record);
if (record->Tag == TYVOID)
	{
	FrameExit(*ret);
	}
	
*ret = FConvert_ObjToList(gCP,gTP,argc,record);
ExitOnError(*ret);

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_Putprop

The putprop cProcedure adds the value {argv[1]} the the named property list {argv[0]} 
under the property {argv[2]}. If the {property} already exists, the new value {argv[1]} 
replaces the old value. A value of {argv[1]} is always returned. 

Several examples follow.

    (proplist  `BLUE-WHALE)                     =>  ()
    (putprop   `BLUE-WHALE  `OCEAN  `MAMMAL)    =>  OCEAN
    (proplist  `BLUE-WHALE)                     =>  (OCEAN  MAMMAL)

#endif

TVAL FProperty_Putprop(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(proplist);
DeclareTVAL(name);
DeclareTVAL(property);
DeclareTVAL(value);
DeclareTVAL(ec);
EndFrame
 
/*  There must be exactly three arguments. */

*ret = gCP->Tval_VOID;
if ((argc != 3) || 
    (asTag(&argv[0]) != TYSYMBOL) ||
    (asTag(&argv[2]) != TYSYMBOL)) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
*name = argv[0];
*value        = argv[1];
*property     = argv[2];

/*  Get the property list value associated with the name */

*proplist = FUtil2_GetVariable(gCP,gTP,gTP->TLambda_ThePropList,asSymbol(name));
/**ec = lspGetVariable(gTP->TLambda_ThePropList,asSymbol(name),proplist); */
if (isERROR(proplist))
    {
    *proplist = gCP->Tval_VOID;
    }
*ec = FUtil2_DefVariable(gCP,gTP,*proplist,asSymbol(property),*value);
if(!isERROR(ec))
    {
    *proplist = *ec;
    }
ExitOnError(*ec);
*ec = FUtil2_DefVariable(gCP,gTP,gTP->TLambda_ThePropList,asSymbol(name),*proplist);
if(!isERROR(ec))
    {
    gTP->TLambda_ThePropList = *ec;
    }
ExitOnError(*ec);

*ret = *value;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_Getprop

The getProp cProcedure returns the value  for the specified property {prop}
from the property list for the specified symbol {name}. 

Several examples follow.

    (putprop  `BLUE-WHALE  `OCEAN  `MAMMAL)     =>  OCEAN
    (getProp  `BLUE-WHALE  `MAMMAL)             =>  OCEAN
    (*proplist  `BLUE-WHALE)                      =>  (OCEAN  MAMMAL)

#endif

TVAL FProperty_Getprop(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(proplist);
DeclareTVAL(name);
DeclareTVAL(property);
EndFrame
 
/*  There must be exactly two arguments. */

*ret = gCP->Tval_VOID;
if ((argc != 2) || 
    (asTag(&argv[0]) != TYSYMBOL) ||
    (asTag(&argv[1]) != TYSYMBOL)) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
*name         = argv[0];
*property     = argv[1];

/*  Get the property list associated with the name */

*proplist = FUtil2_GetVariable(gCP,gTP,gTP->TLambda_ThePropList,asSymbol(name));
if (isERROR(proplist))
    {
    *proplist = gCP->Tval_VOID;
    }
*ret = FUtil2_GetVariable(gCP,gTP,*proplist,asSymbol(property));
if (isERROR(ret))
    {
    *ret = gCP->Tval_VOID;
    }

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_Remprop

The remProp cProcedure deletes the property {prop} from the property 
list associated with the symbol {name}. If the {prop} does not exist, 
a value of false is returned; otherwise a value of true is returned. 

Several examples follow.

    (putprop  `BLUE-WHALE  `OCEAN  `MAMMAL)     =>  OCEAN
    (*proplist `BLUE-WHALE)                       =>  (OCEAN  MAMMAL)
    (remProp  `BLUE-WHALE  `MAMMAL)             =>  true
    (*proplist `BLUE-WHALE)                       =>  ()

#endif

TVAL FProperty_Remprop(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
NUM			oldsize;
NUM			newsize;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(proplist);
DeclareTVAL(name);
DeclareTVAL(property);
DeclareTVAL(ec);
DeclareOBJ(TStructure,sp);
DeclareTVALArray(prmv,2);

EndFrame
 
/*  There must be exactly two arguments. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if ((argc != 2) || 
    (asTag(&argv[0]) != TYSYMBOL) ||
    (asTag(&argv[1]) != TYSYMBOL)) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

*name         = argv[0];
*property     = argv[1];

/*  Get the property list associated with the *name */

*proplist = FUtil2_GetVariable(gCP,gTP,gTP->TLambda_ThePropList,asSymbol(name));
if (isERROR(proplist)) FrameExit(*ret);

sp = proplist->u.Structure;
oldsize = sp->itsMaxItemIndex;

/*  Delete the property from the property list. */

prmv[0] = *property;
prmv[1] = *proplist;


*proplist = FUtil1_Delete(gCP,gTP,(NUM)2,&prmv[0]);

/* Check for error or if property not found condition */
if (isERROR(proplist))
	{
	asTag(ret) = TYBOLE;
	asBool(ret) = FALSE;
	FrameExit(*ret);
	}

sp = proplist->u.Structure;
newsize = sp->itsMaxItemIndex;

if (newsize == oldsize)
	{
	asTag(ret) = TYBOLE;
	asBool(ret) = FALSE;
	FrameExit(*ret);
	}

*ec = FUtil2_DefVariable(gCP,gTP,gTP->TLambda_ThePropList,asSymbol(name),*proplist);
if(!isERROR(ec))
    {
    gTP->TLambda_ThePropList = *ec;
    }
ExitOnError(*ec);


/*  Always return a value of true. */

asTag(ret) = TYBOLE;
asBool(ret) = TRUE;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_AddMethod

Add the specified Procedure or cProcedure object to the methods list of the
specified type.

    (addMethod `TypeName `Message Procedure)

Note:   Exactly three arguments are expected, anything else is an error. 
  
#endif

TVAL FProperty_AddMethod(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
TYPE            type;
StartFrame
DeclareOBJ(TSymbol,typSymbol);
DeclareOBJ(TSymbol,msgSymbol);
DeclareTVAL(ret);
DeclareTVAL(native);
EndFrame
 
/*  Initialization and argument validation. */

*ret = gCP->Tval_VOID;
if (argc != 3) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

if ((asTag(&argv[0]) != TYSYMBOL) ||
    ((asTag(&argv[1]) != TYSYMBOL) && (asTag(&argv[1]) != TYQUOTEDSYMBOL)) ||
    ((asTag(&argv[2]) != TYCPROCEDURE) && (asTag(&argv[2]) != TYCFUNCTION) && (asTag(&argv[2]) != TYLAMBDA)))
    { 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

typSymbol = asSymbol(&argv[0]);
msgSymbol = asSymbol(&argv[1]);

*native = FUtil2_SymbolToNativeType(gCP,gTP,&type,typSymbol);

/*  Locate the methods table for native types. */

if ((native->Tag == TYBOLE) && (native->u.Bool == TRUE))
    {
    *ret = _TObject_TypeMethods(type);
    }
else
/*  Locate the methods table for user defined types (see defstruct). */

    {
    *ret = TOBJ(typSymbol->itsUserTypeMethods);
    }
    
/*  If the method binding already exists, then reset it. Otherwise, make a */
/*  new method binding for the specified type. */

if (ret->Tag == TYVOID)
    {
    *ret = FMake_Structure(gCP,gTP,0,argv);
    }
*ret = TStructure_SetIV1(gCP,gTP,*ret,argv[1],argv[2]);

/*  Set the methods table for native types. */

if ((native->Tag == TYBOLE) && (native->u.Bool == TRUE))
    {
    _TObject_TypeMethods(type) = *ret;
    }
else
/*  Set the methods table for user defined types (see defstruct). */

    {
    typSymbol->itsUserTypeMethods = (TDictionary*)ret->u.Object;
    }
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_Methodsof

Return the methods list of the specified type. The methods list is either the
empty list (), or a Structure object.

    (methodsOf `TypeName)

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FProperty_Methodsof(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
TYPE                    type;
StartFrame
DeclareOBJ(TSymbol,typSymbol);
DeclareTVAL(faces);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareTVAL(ec);
EndFrame
 
/* Initialization and argument validation. */

*ret = gCP->Tval_VOID;
if (argc != 1) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* ----------------------------------------- */
/* Handle case where argument is a Structure */
/* ----------------------------------------- */

if (argv[0].Tag == TYSTRUCTURE)
	{
	if (argv[0].u.Structure->itsMethods != NIL)
		{
		ret->u.Structure = (TStructure*)argv[0].u.Structure->itsMethods;
		ret->Tag = TYSTRUCTURE;
		}
    
	FrameExit(*ret);
	}


/* -------------------------------------- */
/* Handle case where argument is an Lambda */
/* -------------------------------------- */

if (argv[0].Tag == TYLAMBDA)
	{
	if (argv[0].u.Lambda->Interfaces == NIL) 
		{
		*faces = TOBJ(argv[0].u.Lambda->Interfaces);
		*tmp = TSYMBOL("methods");
		*ret = TStructure_GetIV1(gCP,gTP,*faces,*tmp);
		}

	FrameExit(*ret);
	}


/* -------------------------------------- */
/* Handle case where argument is a Symbol */
/* -------------------------------------- */

if ((argv[0].Tag != TYSYMBOL) && (argv[0].Tag != TYQUOTEDSYMBOL))
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

typSymbol = argv[0].u.Symbol;
*ec = FUtil2_SymbolToNativeType(gCP,gTP,&type,typSymbol);

/*  Return the type's methods list for native types. */

if ((ec->Tag == TYBOLE) && (ec->u.Bool == TRUE))
    {
    *ret = _TObject_TypeMethods(type);
    }
else
/*  Return the type's methods list for user defined types (see defstruct). */
    {
    *ret = TOBJ(typSymbol->itsUserTypeMethods);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProperty_Fieldsof

Return the fields list of the specified structure type. The fields list 
is either the empty list (), or a Structure object.

    (fieldsOf `TypeName)

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FProperty_Fieldsof(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
TYPE                    type;
StartFrame
DeclareOBJ(TSymbol,typSymbol);
DeclareTVAL(ret);
DeclareTVAL(ec);
EndFrame
 
/* Initialization and argument validation. */

*ret = gCP->Tval_VOID;
if (argc != 1) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* ----------------------------------------- */
/* Handle case where argument is a Structure */
/* ----------------------------------------- */

if (argv[0].Tag == TYSTRUCTURE)
	{
	FrameExit(argv[0]);
	}


/* -------------------------------------- */
/* Handle case where argument is an Lambda */
/* -------------------------------------- */

if (argv[0].Tag == TYLAMBDA)
	{
	FrameExit(TOBJ(argv[0].u.Lambda->ClassVariables));
	}

/* -------------------------------------- */
/* Handle case where argument is a Symbol */
/* -------------------------------------- */

if ((argv[0].Tag != TYSYMBOL) && (argv[0].Tag != TYQUOTEDSYMBOL))
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

typSymbol = asSymbol(&argv[0]);
*ec = FUtil2_SymbolToNativeType(gCP,gTP,&type,typSymbol);

/*  Return the fields list for user defined types (see defstruct). */

if ((ec->Tag == TYBOLE) && (ec->u.Bool == FALSE))
    *ret = TOBJ(typSymbol->itsUserTypeFields);
else
/*  Return the fields list for a native type. */

    *ret = _TObject_TypeFields(type);

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FProperty_Factory

The FProperty_Factory cProcedure is a structure (see defstruct) constructor 
which creates a structure of the type {*name} with specified field names set 
to the specified values. Each time the new  cProcedure is invoked, a new 
structure of type {*name} is created.

Note:   In SmartLisp FProperty_Factory is bound to the *name: new.
        If no arguments are specified, return (the empty Structure).
        If the special character `.` is encountered, assign the cdr value. 
            
        (defstruct  employee  *name  job  salary) =>  employee
        (new  `employee  `salary  2201.34)          =>  #{`name  ()  `job  ()  `salary  2201.34}
        (new  `employee  `salary  2201.34 . 6)      =>  #{`name  ()  `job  ()  `salary  2201.34 . 6}
        (new  `employee )                           =>  #{`name  ()  `job  ()  `salary  ()}
  
Note:   Also new can be used to create new Lambda objects, using the
        specified Lambda object as a clone pattern.

#endif

TVAL FProperty_Factory(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
NUM                     argIndex;
TYPE                    type;
StartFrame
DeclareTVAL(fnew);
DeclareOBJ(TStructure,sp);
DeclareOBJ(TSymbol,name);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVAL(eol); 
EndFrame
 
*eol       = TGVALUE("_eol");

/*  The new function can be used to clone new Lambdas. */

if (argv[0].Tag == TYLAMBDA) 
    {
	/* WARNING: This may return an Error object instead of an Lambda object! */
    *ret = TLambda_NewCopy(gCP,gTP,argv[0],argc-1,&argv[1]);
    /* Return the newly copied Lambda or Error object. */
    FrameExit(*ret);
    }

/*  The new function can be used to copy non-factory arguments. */

if ((argc == 1) && (argv[0].Tag != TYSYMBOL) && (argv[0].Tag != TYQUOTEDSYMBOL)) 
    {
	/* WARNING: This may return an Error object instead of a copied object! */
    *ret = FUtil2_Copy(gCP,gTP,argc,argv);
    /* Return the newly copied object. */
    FrameExit(*ret);
    }

/*  Make sure the first argument is a valid structure type symbol! */

*ret = gCP->Tval_VOID;
if ((argc < 1) || ((argv[0].Tag != TYSYMBOL) && (argv[0].Tag != TYQUOTEDSYMBOL))) 
    {
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,
							"!new expects a symbol or Lambda argument for argument 1, received = %a!", 
							&argv[0]);
	FrameExit(*ret);
    }
name = asSymbol(&argv[0]);

/*  Check the symbol against all known user defined types. */

if (name->itsUserTypeFields != NULL)
    { 
    /*  Create a copy of the pattern Structure stored in the Fields vector */
    /*  for the structure type. */

    *tmp = TOBJ(name->itsUserTypeFields);
    sp = (TStructure*)TStructure_Copy(gCP,gTP,*tmp);

    sp->itsMethods = (TSymbol*)name->itsUserTypeMethods;
	if (sp->itsMethods == NIL) sp->itsMethods = (TSymbol*)TStructure_New(gCP,gTP);
	sp->itsCdr = argv[0];
    ret->u.Object = (TObject*)sp;
    ret->Tag = TYSTRUCTURE;

    /*  Initialize each argument binding to the new Structure. */

    argIndex = 1;
    while ((argIndex + 1) < argc)
        {
    
        /*  If we find the special character '.', set the Structure's cdr. */
    
        if ((asTag(&argv[argIndex]) == TYPCODE) && (asShort(&argv[argIndex]) == PERIODTOK))
            {
            sp->itsCdr = argv[argIndex+1];
            FrameExit(*ret);
            }
    
        /*  Add the binding to the Structure, and move to the next binding pair. */
        /*  Note:   Each binding must always begin with a symbol. */
    
        if (asTag(&argv[argIndex]) != TYSYMBOL)
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        *ec = FUtil2_DefVariable(gCP,gTP,*ret,asSymbol(&argv[argIndex]),argv[argIndex+1]);
        if(!isERROR(ec))
            {
            *ret = *ec;
            }
        ExitOnError(*ec);
        argIndex += 2;
        }

    /*  Bindings must always come in pairs. */

    if (argIndex < argc) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
else
/*  Check the symbol against all known native types. */
    {
    *ec = FUtil2_SymbolToNativeType(gCP,gTP,&type,name);
    if (isFALSE(ec))
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }

    *fnew = TFUNCTION(_TObject_TypeNew(type));
    *ret = FSmartbase_Evalv(gCP,gTP,*fnew,argc-1,&argv[1]);
    }

FrameExit(*ret);
}
