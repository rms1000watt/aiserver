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

#define _C_FDEFINE
#define _SMARTBASE

#if 0
FDefine.c
    
This source file contains some of the cMacros supported by the 
SmartLisp interpreter. In particular the implementation of cMacros used to
define new objects and expressions are handled here.


AUTHORS:            Michael F. Korns

MODIFICATIONS:  
    
#endif

#include "fdefine.h"
#include "tstruct.h"
#include "tdiction.h"
#include "tdirect.h"
#include "futil3.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FDefine_Init

Initialize the control portion of the SmartLisp function library.  

#endif

TVAL FDefine_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,savSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FDefine_Initialized) 
    {
    FrameExit(gCP->TObject_OK);
    }
gCP->FDefine_Initialized = TRUE;
    
/* Register the SmartLisp cMacros contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"defineStructure",(LpFUNC)&FDefine_Defstruct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"set",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setCellArray");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setNth");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"vectorSet");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringSet");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, savSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setString",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setBitVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setBytVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setPcdVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setObjVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setShtVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setIntVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setNumVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setFltVector",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setMatrix",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setNumMatrix",(LpFUNC)&FDefine_Set);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setStrValue",(LpFUNC)&FDefine_SetValue);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setStrKey",(LpFUNC)&FDefine_SetKey);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setDicValue",(LpFUNC)&FDefine_SetValue);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setDicKey",(LpFUNC)&FDefine_SetKey);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setDirValue",(LpFUNC)&FDefine_SetValue);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"setDirKey",(LpFUNC)&FDefine_SetKey);
ExitOnError(*ec);


FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDefine_Defstruct

The FDefine_Defstruct cProcedure creates and initializes a new SmartLisp Structure 
object with specified field names.The cProcedure macro always returns the newly 
defined type symbol {name}. The optional keyword include: causes the named 
fields in the specified parent object structure to be included in this new 
structure. There may be one or more field names:  fieldname.

The FDefine_Defstruct cProcedure creates a new Structure object which has the
specified field names and generates the following.

    A new constructor procedure, make_name, which creates a structure of 
    the type name. Each time the make_name  Procedure is invoked, a new 
    structure of type name is created.
    
    A predicate  procedure, name?, which takes one argument and returns 
    true if the argument is a structure of type name; otherwise, the 
    predicate returns false.
    
    A new type symbol, name, which can be assigned methods and which will 
    respond to messages.

    (defineStructure employee: name: job: salary:)
                        =>  employee
    (define X (make_employee  salary: 2201.34))
                        =>  #{name: ()  job:  ()  salary:  2201.34}
                        
#endif

TVAL FDefine_Defstruct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])   
{
NUM                 argNdx;
TYPE                type;
StartFrame
DeclareOBJ(TSymbol,name);
DeclareOBJ(TSymbol,parent);
DeclareOBJ(TSymbol,field);
DeclareOBJ(TSymbol,factory);
DeclareOBJ(TSymbol,predicate);
DeclareTVAL(ec);
DeclareTVAL(value);
DeclareTVAL(fields);
DeclareTVAL(ret);
EndFrame
 
/* Initialization */

if ((asTag(&argv[0]) != TYSYMBOL))
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*ret = gCP->Tval_VOID;
*value = gCP->Tval_VOID;
argNdx = 0;

/* Evaluate the new structure type name symbol */

name = asSymbol(&argv[argNdx++]);

/*  The new structure name must be either not be an existing type, */
/*  or it must be a structure type (which can be overwritten).  */

*ec = FUtil2_SymbolToNativeType(gCP,gTP,&type,name);
if ((ec->Tag == TYBOLE) && (ec->u.Bool == FALSE))
    {
    name->itsUserTypeFields = NULL;
    name->itsUserTypeMethods = NULL;
    name->itsUserTypeParent = NULL;
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Is there an include keyword ? If so, we must set the parent  */
/*  information for the new structure type.  */

if ((argc > argNdx) && 
    (asTag(&argv[argNdx]) == TYSYMBOL) &&
    (Symbol(argv[argNdx]) == gCP->TLambda_include))
    {
    if (argv[argNdx+1].Tag != TYSYMBOL)
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

    parent = Symbol(argv[++argNdx]);
    if (parent == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        
    name->itsUserTypeParent = parent;
    if (parent->itsUserTypeFields != NULL)
        name->itsUserTypeFields = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(parent->itsUserTypeFields));
    else
        name->itsUserTypeFields = NULL;
    if (parent->itsUserTypeMethods != NULL)
        name->itsUserTypeMethods = (TDictionary*)TStructure_Copy(gCP,gTP,TOBJ(parent->itsUserTypeMethods));
    else
        name->itsUserTypeMethods = NULL;
    *fields = TOBJ(name->itsUserTypeFields);

    argNdx++;
    }
else
    {
    name->itsUserTypeFields = TStructure_New(gCP,gTP);
    *fields = TOBJ(name->itsUserTypeFields);
    }

/*  For each field name, add a binding to the new structure type's */
/*  field list. When we see a right paren, we are done. */

while (argNdx < argc )
    {
    if (argv[argNdx].Tag == TYSYMBOL)
        field = Symbol(argv[argNdx]);
    else
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    *ec = FUtil2_DefVariable(gCP,gTP,*fields,field,*value);
    ExitOnError(*ec);
    argNdx++;
    }

/*  We skip over the final right paren, and we will return */
/*  the new structure type name. */

asTag(ret)    = TYSYMBOL;
asObject(ret) = (TObject*)name;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDefine_Set

Sets the specified symbol variable binding, in the current Structure, to the given value.

#endif

TVAL FDefine_Set(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(name);
DeclareTVAL(ret);
DeclareTVAL(ec);
DeclareTVAL(target);
EndFrame

/* Initialization */
*name = argv[0];

if (name->Tag == TYSYMBOL)
    {
    /*  Is this assignment to a variable symbol ? */
    switch(argc)
        {
        case 2:
			/*  Set the value using the global Structure.  */
            *ret = argv[1];
            *ec = TSymbol_SetGlobalValue(gCP,gTP,asSymbol(name), argv[1]);
            ExitOnError(*ec);
        break;
        
        default:
            /*  Handle case of implicit object ref... */
            *target = TSymbol_GetGlobalValue(gCP,gTP,*name);
            *ret = *target;
            goto Assignment;
        break;
        }
    }
else    
    {
    /*  This is an assignment into an indexed object. */
    Assignment:

    /*  Use the number of arguments to determine the number of indices.  */
    switch(argc)
        {
        case 3:
            /*  Handle case of a target, one index, and a new value. */
			if (argv[0].Tag == TYNUM)
				{
				if (argv[1].Tag == TYNUM)
					{
					switch(argv[0].DeclaredType)
						{
						case TYCHARPOINTER:
							((LpCHAR)argv[0].u.Int)[argv[1].u.Int] = (CHAR)argv[2].u.Int;
							break;

						case TYFLOATPOINTER:
							((LpFLOAT)argv[0].u.Int)[argv[1].u.Int] = (FLOAT)argv[2].u.Real;
							break;

						case TYINTPOINTER:
						case TYJUMPPOINTER:
							((LpNUM)argv[0].u.Int)[argv[1].u.Int] = (NUM)argv[2].u.Int;
							break;

						case TYREALPOINTER:
							((LpREAL)argv[0].u.Int)[argv[1].u.Int] = (REAL)argv[2].u.Real;
							break;

						case TYSHORTPOINTER:
							((LpSHORT)argv[0].u.Int)[argv[1].u.Int] = (SHORT)argv[2].u.Int;
							break;

						case TYLONGPOINTER:
							((LpNUM32)argv[0].u.Int)[argv[1].u.Int] = (NUM32)argv[2].u.Int;
							break;

						case TYWORDPOINTER:
							((LpTVAL)argv[0].u.Int)[argv[1].u.Int] = argv[2];
							break;

						default:
							FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
							break;
						}

					*ret = argv[0];
					}
				else
					{
					FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
					}
				}
            else
				{
				*ec = TObject_SetIV1(gCP,gTP,argv[0], argv[1], argv[2]);
				ExitOnError(*ec);
				*ret = *ec;
				}
        break;
        
        case 4:
            /*  Handle case of a target, two indices, and a new value. */
            
            *ec = TObject_SetIV2(gCP,gTP,argv[0], argv[1], argv[2], argv[3]);
            ExitOnError(*ec);
            *ret = *ec;
        break;
        
        case 5:
            /*  Handle case of a target, three indices, and a new value. */
            
            *ec = TObject_SetIV3(gCP,gTP,argv[0], argv[1], argv[2], argv[3], argv[4]);
            ExitOnError(*ec);
            *ret = *ec;
        break;
        
        default:
            FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
        break;
        
        }
    }

/*  We return the value of the variable after it was set. */
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDefine_SetKey

Sets the specified specified Key, in the current object, to the specified value.

#endif

TVAL FDefine_SetKey(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ec);
EndFrame

/*  Use the number of arguments to determine the number of indices.  */
switch(argc)
    {
    case 3:
        /*  Handle case of a target, one index, and a new value. */
        
        *ec = TObject_SetIV2(gCP,gTP,argv[0], argv[1], TINT(0), argv[2]);
        ExitOnError(*ec);
        *ret = *ec;
    break;
    
    default:
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    break;
    
    }

/*  We return the value of the variable after it was set. */
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDefine_SetValue

Sets the specified specified Value, in the current object, to the specified value.

#endif

TVAL FDefine_SetValue(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(ec);
EndFrame

/*  Use the number of arguments to determine the number of indices.  */
switch(argc)
    {
    case 3:
        /*  Handle case of a target, one index, and a new value. */
        
        *ec = TObject_SetIV2(gCP,gTP,argv[0], argv[1], TINT(1), argv[2]);
        ExitOnError(*ec);
        *ret = *ec;
    break;
    
    default:
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    break;
    
    }

/*  We return the value of the variable after it was set. */
FrameExit(*ret);
}
