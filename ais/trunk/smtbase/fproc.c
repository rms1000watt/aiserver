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

#define _C_FPROCEDURE
#define _SMARTBASE
#if 0
FProcedure.c

Support for the Procedure object.

This source file contains the support functions for the Lisp 
Procedure Object. A Procedure object contains a compiled Lisp formula. 
Adopted from Scheme and other dialects of Lisp, it is one of the main 
mechanisms for storing Lisp functions or scripts in Lisp 


PARENT:              

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fproc.h"
#include "fobject.h" // MFK
#include "fmake.h"
#include "futil3.h"
#include "fpred2.h"

/*  Define all global variables for this Class. */


/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_Send

The  send  Procedure supports symbolic messaging with any number of arguments. The message 
argument must be an element of type Symbol, and there must be at least one argument. 
The action taken by the symbolic message is dependent upon the type of the first argument.  
The message symbol is looked up in the Methods Structure attached to the type 
(see the defmethod Macro).  If the message symbol is found, the associated Procedure is 
invoked against the whole argument list.  If the message symbol is not found, an error 
condition results.  

For example
    (defmethod  Number:  square(n)  (*  n  n))  =>  Number-square
    (send  square:  0)                          =>  0
    (send  square:  4)                          =>  16
    (send  square:  #\w)                        =>  !value!

#endif

TVAL FProcedure_Send(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TYPE                type;
StartFrame
DeclareOBJ(TSymbol,userType);
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVAL(proc);
EndFrame

if ((argc < 2) || (argv[0].Tag != TYSYMBOL)) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
type = argv[1].Tag;

/*  Send message to a user defined type (see defstruct). */

if ((type == TYSTRUCTURE) && (Structure(argv[1])->itsMethods != NULL))
    {
    userType = Structure(argv[1])->itsMethods;
    if (userType != NULL)
        {
        *tmp = TOBJ(userType->itsUserTypeMethods);
        *proc = FUtil2_GetVariable(gCP,gTP,*tmp,Symbol(argv[0]));
        }
    else
        {
        *proc = gCP->Tval_VOID;
        }
    }
else
/*  Send message to a native type. */

    {
    *proc = FUtil2_GetVariable(gCP,gTP,_TObject_TypeMethods(type),argv[0].u.Symbol);
    }

*ret = FSmartbase_Evalv(gCP,gTP,*proc,argc-1,&argv[1]);
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_Make

The  makeLambda  Procedure creates a new Procedure object  

For example
    
    (makeLambda  )  =>  #<Lambda 5723>
    
Note:   an optional string or text argument will create a new Procedure
        by lexing, morphing, and compiling the text. 

#endif

TVAL FProcedure_Make(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 indexOf = -1;
StartFrame
DeclareTVAL(proc);
EndFrame

/*  Create the virgin Lambda object. */
proc->u.Lambda = TLambda_New(gCP,gTP);
proc->Tag = TYLAMBDA;

/*  Initialize the fields of the new Procedure object. */

while (++indexOf < argc)
    {
    /*  Each field name must be followed by an initialization value. */
    if ((indexOf + 1) >= argc)
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

    if (argv[indexOf].Tag == TYSYMBOL)
        {
        /*  Are we initializating the Self variables: Sv? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Sv)
            {
			++indexOf;
			proc->u.Lambda->ClassVariables = (argv[indexOf].Tag != TYSTRUCTURE) ? NIL : argv[indexOf].u.Structure; 
            }
        else
        /*  Are we initializating the Argument variables: Av? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Av)
            {
			++indexOf;
			proc->u.Lambda->ArgumentVariables = (argv[indexOf].Tag != TYSTRUCTURE) ? NIL : argv[indexOf].u.Structure; 
            }
        else
        /*  Are we initializating the Temporary variables: Tv? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Tv)
            {
			++indexOf;
			proc->u.Lambda->TemporaryVariables = (argv[indexOf].Tag != TYSTRUCTURE) ? NIL : argv[indexOf].u.Structure; 
            }
        else
        /*  Are we initializating the Persistent variables: Pv? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Pv)
            {
			++indexOf;
			proc->u.Lambda->PersistantVariables = (argv[indexOf].Tag != TYSTRUCTURE) ? NIL : argv[indexOf].u.Structure; 
            }
        else
        /*  Are we initializating the Persistent  constant variables: Cv? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Cv)
            {
			++indexOf;
			proc->u.Lambda->ConstantVariables = (argv[indexOf].Tag != TYSTRUCTURE) ? NIL : argv[indexOf].u.Structure; 
            }
        else
        /*  Are we initializating the Register  constant variables: Rv? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Rv)
            {
			++indexOf;
			proc->u.Lambda->RegisterVariables = (argv[indexOf].Tag != TYSTRUCTURE) ? NIL : argv[indexOf].u.Structure; 
            }
        else
        /*  Are we initializating the Pcode vector: Rv? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Pc)
            {
			++indexOf;
			proc->u.Lambda->PcodeVector = (argv[indexOf].Tag != TYPCODEVECTOR) ? NIL : argv[indexOf].u.PcodeVector; 
            }
        else
        /*  Are we initializating the lexical token vector: Sc? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Sc)
            {
            ++indexOf; 
            proc->u.Lambda->DebuggerSource = (_TObject_TypeFlag(argv[indexOf].Tag) == _TObject_TfTOBJECT) ? argv[indexOf].u.Object : NIL; 
            }
        else
        /*  Are we initializating the native code vector: Nc? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Nc)
            {
			++indexOf;
			proc->u.Lambda->NativeCodeVector = (argv[indexOf].Tag != TYBYTEVECTOR) ? NIL : argv[indexOf].u.ByteVector; 
            }
        else
        /*  Are we initializating the interfaces structure: In? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_In)
            {
			++indexOf;
			proc->u.Lambda->Interfaces = (argv[indexOf].Tag != TYSTRUCTURE) ? NIL : argv[indexOf].u.Structure; 
            }
        else
        /*  Are we initializating the virtual machine evaluator: Vm? */
        if (argv[indexOf].u.Object == (TObject*)gCP->TLambda_Vm)
            {
 			if (argv[++indexOf].Tag == TYVMEVALUATOR)
				{
				proc->u.Lambda->VirtualMachine = argv[indexOf].u.Symbol; 
				}
			else
				{
				FrameExit(TERROR("!Tried to set an Lambda's Vm property with a non VM Evaluator function!"));
				}
           }
        else
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    else
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

FCompile_SetupRegs(gCP,gTP,proc->u.Lambda);

/*  Return the new Procedure object. */
FrameExit(*proc);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_MakeMacro

The  makeMacro  Procedure creates a new Macro Lambda object  

For example
    
    (makeMacro  )  =>  #<Macro 5723>
    
Note:   an optional string or text argument will create a new Macro Lambda
        by lexing, morphing, and compiling the text. 

#endif

TVAL FProcedure_MakeMacro(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(proc);
EndFrame

/*  Create the virgin Lambda object. */
*proc = FProcedure_Make(gCP, gTP, argc, argv);
ExitOnError(*proc);
proc->Tag = TYMACRO;

/*  Return the new Macro Lambda. */
FrameExit(*proc);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_Procedurep

The isProcedure  Procedure returns true if the argument is a Procedure object  

For example
    (isProcedure foo)    =>  true

#endif

TVAL FProcedure_Procedurep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
gTP = gTP; // NOOP to hide unused parameter warning message
/*  We can only have one argument */
if (argc != 1) return(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Return true if the argument is a Procedure */
if (argv[0].Tag == TYLAMBDA)
    return(gCP->TObject_TRUE);

return(gCP->TObject_FALSE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_FailureReset

Reset framing and any other systems which may be affected when we do a jump into the
TCL exception handler, i.e. file not found, out of mem, etc...

#endif

void  FProcedure_FailureReset(LpXCONTEXT gCP,LpTHREAD gTP)
{
for (gTP->TvalStackIdx = gTP->MaxTvalStackSize; gTP->TvalStackIdx > 0; --gTP->TvalStackIdx)
	gTP->TvalStack[gTP->TvalStackIdx] = gCP->Tval_VOID;

gTP->TvalStackIdx = 0;
gTP->ObjStackIdx = 0;
gTP->RecursionCount = 0;

gTP->FCompile_LambdaNest = 0;
gTP->FCompile_lambdaSwitch = 0;
gTP->TLambda_TheProc  = gTP->TLambda_saveProc;

}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_recNumber

Recognize a numeric constant in a Lisp source string.

Note:   If the number is terminated with a % sign, treat the number
        as a percentage. 

#endif

TVAL  FProcedure_recNumber(LpXCONTEXT gCP,LpTHREAD gTP,HMChar hSrc,LpNUM iChar,LpREAL aNumber,LpTVAL result)
{
LpREAL              rs;
LpCHAR              sp;
NUM                 count;
NUM                 m;
NUM                 n;
NUM                 sn;
NUM                 nx;
REAL                x;
REAL                y;
REAL                e;
REAL                f;
REAL                sign;
NUM                 nsign;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(isInt);
DeclareTVAL(isReal);
EndFrame

/* Initialize.  */

sp = &atHMChar(hSrc,*iChar);
rs = aNumber;
*isInt = gCP->TObject_FALSE;
*isReal = gCP->TObject_FALSE;

/* Test for signed quantity.  */

if (*sp == '-')
    {
    sign = -1.0;
    nsign = -1;
    m = 1;
    ++sp;
    }
else
if (*sp == '+')
    {
    sign = 1.0;
    nsign = 1;
    m = 1;
    ++sp;
    }
 else
    {
    m = 0;
    sign = 1.0;
    nsign = 1;
    }

/* A real numeric constant may begin with an integer.  */

*isInt = FProcedure_recInteger(gCP,gTP,&nx,&x,&count,sp);
if (isInt->u.Bool == TRUE)
    sp += count;
else
    x = 0.0;

/* Some real constants are sandwiched with a '.'.  */

if (*sp == '.')
    {
    ++count;
    *isReal = FProcedure_recFrac(gCP,gTP,&y,&n,++sp);
    if (isReal->u.Bool == TRUE)
        {
        sp += n;
        count += n;
        x += y;
        if ((*sp == 'e') || (*sp == 'E'))
            {
            ++count;
            sn = 1.0;
            if (*(++sp) == '-')
                {
                ++count;
                sn = -1.0;
                ++sp;
                }
            else
            if (*sp == '+')
                {
                ++count;
                sn = 1.0;
                ++sp;
                }
            *ret = FProcedure_recInt(gCP,gTP,&y,&n,sp);
            if (ret->u.Bool == TRUE)
                {
                sp += n;
                count += n;
                if (y != 0.0)
                    {
                    e = 10.0;
                    f = (REAL)sn;
                    y *= f;
                    f = log(e);
                    e = f * y;
                    f = exp(e);
                    x *= f;
                    }
                }
            }
        }
    else
        y = 0.0;
    }
else
/* Some real constants are sandwiched with an 'E' or 'e'.  */

if ((count > 0) && ((*sp == 'E') || (*sp == 'e')))
    {
	*isReal = gCP->TObject_TRUE;
    ++count;
    sn = 1.0;
    if (*(++sp) == '-')
        {
        ++count;
        sn = -1.0;
        ++sp;
        }
    else
    if (*sp == '+')
        {
        ++count;
        sn = 1.0;
        ++sp;
        }
    *ret = FProcedure_recInt(gCP,gTP,&y,&n,sp);
    if (ret->u.Bool == TRUE)
        {
        sp += n;
        count += n;
        if (y != 0.0)
            {
            e = 10.0;
            f = (REAL)sn;
            y *= f;
            f = log(e);
            e = f * y;
            f = exp(e);
            x *= f;
            }
        }
	}

/* All percent constants are terminated with a '%'.  */

if ((*sp == '%') && (count > 0))
    {
    *iChar += (m + ++count);
    *rs = (x * sign) / 100; 
    *ret = gCP->TObject_TRUE;
	result->Tag = TYREAL;
	result->u.Real = *rs;
    }
else
if (count > 0)
    {
    n = *rs = x * sign;   
    n = nx * nsign;

	if (isReal->u.Bool == TRUE)
        {
        result->Tag = TYREAL;
        result->u.Real = *rs;
        }
    else
        {
        result->Tag = TYNUM;
        result->u.Int = n;
        }

	*iChar += (m + count);
    *ret = gCP->TObject_TRUE;
    }
else
    {
    *rs = 0.0;  
    *ret = gCP->TObject_FALSE;
    }
    
FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_recString

Recognize a string constant in a Lisp source string.  

#endif

TVAL FProcedure_recString(LpXCONTEXT gCP,LpTHREAD gTP,HMChar hSrc,LpNUM iChar,LpTVAL lpTval)
{
CHAR                stringBuf[1024];
NUM                 index;
 
/* Initialize.  */

for(index = 0;  ((&atHMChar(hSrc,0))[*iChar] != 0); )
    {
    if(((&atHMChar(hSrc,0))[*iChar] == '\\' ) && ((&atHMChar(hSrc,0))[*iChar+1] != 0))
        {
        /*  Process "escaped" characters like embedded double quotes (")  */
        
        (*iChar)++;
        stringBuf[index++] = (&atHMChar(hSrc,0))[(*iChar)++];       
        }
    else    
    if((&atHMChar(hSrc,0))[*iChar] == '"' )
        break;
    else    
        {
        stringBuf[index++] = (&atHMChar(hSrc,0))[(*iChar)++];
        }
    }
 
stringBuf[index]   = 0;
if ((&atHMChar(hSrc,0))[*iChar] == '"')
    ++(*iChar);
    
/*  Cleanup */

/*return FObject_TvalAnyCnv(TyTVAL, lpTval, TYTEXT,stringBuf); */

*lpTval = TObject_CnvFromText(gCP, gTP, stringBuf);
return *lpTval;
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_recDate

Recognize a date string constant in a Lisp source string.  

#endif

TVAL FProcedure_recDate(LpXCONTEXT gCP,LpTHREAD gTP,HMChar hSrc,LpNUM iChar,LpTVAL lpTval)
{
*lpTval = FProcedure_cnvDate(gCP, gTP, &atHMChar(hSrc,0),iChar);
return(*lpTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_recName

Recognize a name or symbol in a Lisp source string.

Note1:   Lisp also supports long symbols with imbedded blanks if they are
         enclosed in vertical bars, for example:  |This is a long name|
        
Note2:   Lisp also supports register offset addressing and automatic conversion
		 into compound register offset address variable names, as follows:
           register[:integer:]						==>	|regoffset:register[:integer:]|					
           register[basis(name)]					==>	|regoffset:register[basis(name)]|					
           register[basis(index1)]					==>	|regoffset:register[basis(index1)]|
           register[basis(index1,index2)]			==>	|regoffset:register[basis(index1,index2)]|
           register[basis(index1,index2,index3)]	==>	|regoffset:register[basis(index1,index2,index3)]|
		 
		 Fact: These 
        
Note3:   We have also included recognition here for symbols enclosed in single quotes
         as in exceltalk. It is critical that any other lex routines trap leading single
         quotes if they are to have a different synactical meaning, i.e. the lisp lexer
         must do this.

Note4:   We have also included recognition here for constrained global variable symbols.
         By convention, Lisp constrained variables begin with the % symbol. Any
         use of a constrained variable serves to declare it.

Note5:   We have also included recognition here for global state variable symbols.
         By convention, Lisp global state variables begin with the & symbol. Any
         use of a global state variable serves to declare it.

#endif

TVAL FProcedure_recName(LpXCONTEXT gCP,LpTHREAD gTP,HMChar hSrc,LpNUM iChar,TSymbol **symName,BOLE arithSW)
{
LpCHAR              start;
LpCHAR              end;
LpCHAR              offend;
NUM                 len;
CHAR                stringBuf[1024];
StartFrame
DeclareTVAL(tmp);
EndFrame
 
/*  Recognize long symbols with special characters and embedded blanks. */

start = &atHMChar(hSrc,*iChar);

switch(*start)
    {
    case '|':
        for (end = ++start;*(end) != '|' && (*end != 0); ++end) {}
        
        if((len = end - start) >= (NUM)sizeof(stringBuf))
            goto Bad;
        else
            {
            _FMemory_memcpy( stringBuf, start,  len);
            }
        
        *(iChar) += len + 2; /* Drop the inclosing bars. */
        stringBuf[len]   = 0;
        *symName = TSymbol_MakeUnique(gCP,gTP,stringBuf);

        /*  Do we have a global state variable declaration ? */
        /*  Global state variables are symbols which begin with */
        /*  the & symbol. Any use of a global state variable */
        /*  serves to declare it. */
        
        if ((stringBuf[0] == '&') && (stringBuf[1] > ' '))
            {
            tmp->Tag = TYSYMBOL;
            tmp->u.Object = (TObject*)*symName;
            }

        FrameExit(gCP->TObject_TRUE);
    break;
    
    case '\'':
        for (end = ++start;*(end) != '\'' && (*end != 0); ++end) {}
        
        if((len = end - start) >= (NUM)sizeof(stringBuf))
            goto Bad;
        else
            {
            _FMemory_memcpy( stringBuf, start,  len);
            }
        
        *(iChar) += len + 2; /* Drop the inclosing quotes. */
        stringBuf[len]   = 0;
        *symName = TSymbol_MakeUnique(gCP,gTP,stringBuf);

        /*  Do we have a constrained variable declaration ? */
        /*  Constrained variables are symbols which begin with */
        /*  the % symbol. Any use of a constrained variable */
        /*  serves to declare it. */

        /*  Do we have a global state variable declaration ? */
        /*  Global state variables are symbols which begin with */
        /*  the & symbol. Any use of a global state variable */
        /*  serves to declare it. */
        
        FrameExit(gCP->TObject_TRUE);
    break;
    
    default:
        if (ISSYMBOL((NUM)*start))
            {
			/* Are arithmetic operator symbols + - / * < > = to be separated? */
			if (arithSW)
				{
				if (ISOPER((NUM)*start))
					{
					for (end = start; ISOPER((NUM)*end); ++end) {}
					}
				else
					{
					for (end = start; (ISSYMBOL((NUM)*end) && !ISOPER((NUM)*end)); ++end) {}
					}
				}
			else
			/* Arithmetic operator symbols are NOT to be separated. */
				{
				for (end = start; ISSYMBOL((NUM)*end); ++end) {}
				}

			/* **********************************************************************************************  */
			/* Start looking for a possible register offset address variable name, as follows:                */
            /*   register[:integer:]					==>	|regoffset:register[:integer:)]|				  */					
            /*   register[basis(name)]					==>	|regoffset:register[basis(name)]|				  */					
            /*   register[basis(index1)]				==>	|regoffset:register[basis(index1)]|			      */
            /*   register[basis(index1 index2)]			==>	|regoffset:register[basis(index1,index2)]|		  */
            /*   register[basis(index1 index2 index3)]	==>	|regoffset:register[basis(index1,index2,index3)]| */
			/* ********************************************************************************************** */            

			if (*end == '[')
				{
				offend = end;

				/* Attempt to recognize a constant integer offset */
				if (*(offend+1) == ':')
					{
					for (offend+=2; ISDIGIT((NUM)*offend); ++offend) {}
					if ((*(offend++) != ':') || (*(offend++) != ']')) goto Bad; 
					end = offend;
					if((len = end - start) >= (NUM)(sizeof(stringBuf) - 10)) goto Bad;

					strcpy(stringBuf,"regoffset:");
					_FMemory_memcpy( &stringBuf[10], start, len);
					stringBuf[len+10] = 0;
					*(iChar) += len;
					*symName = TSymbol_MakeUnique(gCP,gTP,stringBuf);
            
					FrameExit(gCP->TObject_TRUE);
					}

				/* Attempt to recognize the basis name. */
				for (offend++; ISSYMBOL((NUM)*offend); ++offend) {}
				if (((NUM)offend - (NUM)end) == 1) goto FoundAName;
				if (*offend++ != '(') goto FoundAName;

				/* Attempt to recognize the field name. */
				if ISSYMBOL((NUM)*offend)
					{
					for (offend++; ISSYMBOL((NUM)*offend); ++offend) {}
					goto RecognizeTrailingParen;
					}

				/* Attempt to recognize index1. */
				if (ISDIGIT((NUM)*offend++) == 0) goto FoundAName;
				for (offend++; ISDIGIT((NUM)*offend); ++offend) {}

				/* Attempt to recognize index2. */
				if (*offend == ',')
					{
					if (ISDIGIT((NUM)*offend++) == 0) goto FoundAName; 
					for (offend++; ISDIGIT((NUM)*offend); ++offend) {}

					/* Attempt to recognize index3. */
					if (*offend == ',')
						{
						if (ISDIGIT((NUM)*offend++) == 0) goto FoundAName; 
						for (offend++; ISDIGIT((NUM)*offend); ++offend) {}
						}
					}

				RecognizeTrailingParen:
				if ((*offend++ != ')') || (*offend++ != ']')) goto FoundAName;
				end = offend;
				if((len = end - start) >= (NUM)(sizeof(stringBuf) - 10)) goto Bad;

				strcpy(stringBuf,"regoffset:");
				_FMemory_memcpy( &stringBuf[10], start, len);
				stringBuf[len+10] = 0;
				*(iChar) += len;
				*symName = TSymbol_MakeUnique(gCP,gTP,stringBuf);
            
				FrameExit(gCP->TObject_TRUE);
				}

			/* ********************************************************************************************** */
			/* End looking for a possible register offset address variable name.                              */
			/* ********************************************************************************************** */            

			FoundAName:
            if((len = end - start) >= (NUM)sizeof(stringBuf))
                goto Bad;
            else
                {
                _FMemory_memcpy( stringBuf, start,  len);
                }
            
            *(iChar) += len;
            stringBuf[len]   = 0;
            *symName = TSymbol_MakeUnique(gCP,gTP,stringBuf);
            
            FrameExit(gCP->TObject_TRUE);
            }
    break;
    }

/*  Can't recognize this as a symbol. */

Bad:

FrameExit(gCP->TObject_FALSE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_NewCProcedure

The FProcedure_NewCProcedure function converts the specified text into a symbol. 

Note:   TYCPROCEDURE functions must be of type LpFUNC, and must have the following
        standard argument list:
        
            LpFUNC(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif

TVAL FProcedure_NewCProcedure(LpXCONTEXT gCP,LpTHREAD gTP,TSymbol **symName,LpCHAR funcName,LpFUNC lpFunc)
{
StartFrame
DeclareTVAL(tmpTval);
EndFrame

*symName = TSymbol_MakeUnique(gCP,gTP,funcName);
(*symName)->itsCProcedure =  lpFunc;

tmpTval->Tag = TYCPROCEDURE;
tmpTval->u.Object = (TObject*)*symName;

/*  Calling SetGlobalValue with a non-void value will automatically PERM the symbol */

TSymbol_SetGlobalValue(gCP,gTP,*symName, *tmpTval);

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_LoadCProcedure


#endif

TVAL FProcedure_LoadCProcedure(LpXCONTEXT gCP,LpTHREAD gTP,HMChar aHandle, NUM theFileID, NUM bResolve)
{
StartFrame
DeclareTVAL(tmpTval);
DeclareOBJ(TSymbol,theSymbol);
EndFrame

*tmpTval = TSymbol_Load(gCP,gTP,(HMemory)aHandle, theFileID, bResolve);
if((tmpTval->Tag != TYERROR) && (bResolve != 0))
    {
    theSymbol = tmpTval->u.Symbol;
    tmpTval->Tag = TYCPROCEDURE;
    tmpTval->u.Symbol = theSymbol;
    
    /*  Calling SetGlobalValue with a non-void value will automatically PERM the symbol */
    
    TSymbol_SetGlobalValue(gCP,gTP,theSymbol,*tmpTval);
    FrameExit(*tmpTval);
    }

FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_LoadCMacro


#endif

TVAL FProcedure_LoadCMacro(LpXCONTEXT gCP,LpTHREAD gTP,HMChar aHandle, NUM theFileID, NUM bResolve)
{
StartFrame
DeclareTVAL(tmpTval);
DeclareOBJ(TSymbol,theSymbol);
EndFrame

*tmpTval = TSymbol_Load(gCP,gTP,(HMemory)aHandle, theFileID, bResolve);
if((tmpTval->Tag != TYERROR) && (bResolve != 0))
    {
    theSymbol = tmpTval->u.Symbol;
    tmpTval->Tag = TYCMACRO;
    tmpTval->u.Symbol = theSymbol;
    
    /*  Calling SetGlobalValue with a non-void value will automatically PERM the symbol */
    
    TSymbol_SetGlobalValue(gCP,gTP,theSymbol,*tmpTval);
    FrameExit(*tmpTval);
    }

FrameExit(gCP->TObject_ERROR_INVALID);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_LoadMacro

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    FProcedure_LoadMacro(LpXCONTEXT gCP,LpTHREAD gTP,HMChar aHandle, NUM theFileID, NUM bResolve)
{
return(TLambda_Load(gCP,gTP,(HMemory)aHandle,theFileID,bResolve));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_recInt

The FProcedure_recInt function recognizes integers. If an integer is recognized, it 
is returned in the specified argument as a REAL (this allows very large
integers). The FProcedure_recInt function returns the length of the integer recognized, 
or false if no integer is recognized.

#endif

TVAL FProcedure_recInt(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL realResult, LpNUM count, LpCHAR stringInput)
{
REAL        sign;
NUM         sinc;

gTP = gTP; // NOOP to hide unused parameter warning message
/*Initialize */
*count = 0;
*realResult = 0.0;
sign = 1.0;
sinc = 0;

/* Is there a negative sign? */
if (*stringInput == '-')
    {
    ++(*count);
    ++sinc;
    ++stringInput;
    sign = -1.0;
    }

/* Recognize each integer digit */
while (ISDIGIT((NUM)*stringInput))
    {
    /* shift the previous result by a factor of ten */
    *realResult *= 10.0;
    /* add the next digit */
    *realResult += (REAL)(*(stringInput++)-'0');
    *count += (sinc + 1);           
    sinc = 0;           
    }

/* Set the sign of the final result value    */
*realResult *= sign;

/* Return TRUE if any characters were recognized */
if (*count > 0)
    return(gCP->TObject_TRUE);
else
    return(gCP->TObject_FALSE);
}                                                                           

#if 0
FProcedure_recInteger

The FProcedure_recInteger function recognizes a signed contiguous series of ascii digits as Integers. 
If an integer is recognized, it is returned as both a REAL (32bit this allows very large
integers) and as a NUM (64bit). 

The FProcedure_recInteger function returns TRUE if the integer recognized or FALSE otherwise.

#endif

TVAL FProcedure_recInteger(LpXCONTEXT gCP,LpTHREAD gTP,LpNUM intResult, LpREAL realResult, LpNUM count, LpCHAR stringInput)
{
REAL        sign;
NUM         sinc;

gTP = gTP; // NOOP to hide unused parameter warning message
/*Initialize */
*count = 0;
*realResult = 0.0;
*intResult = 0;
sign = 1.0;
sinc = 0;

/* Is there a negative sign? */
if (*stringInput == '-')
    {
    ++(*count);
    ++sinc;
    ++stringInput;
    sign = -1.0;
    }

/* Recognize each integer digit */
while (ISDIGIT((NUM)*stringInput))
    {
    /* shift the previous result by a factor of ten */
    *realResult *= 10.0;
    *intResult *= 10;
    /* add the next digit */
    *realResult += (REAL)(*(stringInput)-'0');
    *intResult += (NUM)(*(stringInput++)-'0');
    *count += (sinc + 1);           
    sinc = 0;           
    }

/* Set the sign of the final result value    */
*realResult *= sign;
*intResult *= (NUM)sign;

/* Return TRUE if any characters were recognized */
if (*count > 0)
    return(gCP->TObject_TRUE);
else
    return(gCP->TObject_FALSE);
}                                                                           

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_recFrac

The FProcedure_recFrac function recognizes fractions. If a fraction is recognized, it 
is returned in the specified argument as a REAL. The FProcedure_recFrac function 
returns the length of the real string recognized, or zero if no real is 
recognized.

#endif

TVAL FProcedure_recFrac(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL realResult, LpNUM count, LpCHAR stringInput)
{
REAL    realPower;
REAL    sign;
NUM sinc;

gTP = gTP; // NOOP to hide unused parameter warning message
/*Initialize */
*count = 0;
*realResult = 0.0;
realPower = 1.0;
sign = 1.0;
sinc = 0;

/* Is there a negative sign? */
if (*stringInput == '-')
    {
     ++sinc;
     ++stringInput;
     sign = -1.0;
    }

/* Recognize each integer digit */
while (ISDIGIT((NUM)*stringInput))
    {
    /* Adjust the fraction power */
    realPower /= 10.0;
    /* Add the recognized digit */
    *realResult += (realPower * (REAL)(*(stringInput++)-'0'));
    ++(*count);         
    *count += sinc;             
    sinc = 0;           
    }
    
/* Set the sign of the final result value    */
*realResult *= sign;

/* Return TRUE if any characters were recognized */
if (*count > 0)
    return(gCP->TObject_TRUE);
else
    return(gCP->TObject_FALSE);
}                                                                           

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_NewSpecialForm

The FProcedure_NewSpecialForm function converts the specified text into a symbol. 
The symbol`s value in the symbol global value member is given the value of a TYSPECIALFORM. 
The function pointer value is saved in the symbol`s global value member.

Note:   TYSPECIALFORM functions must be of type LpFUNC, and must have the following
        standard argument list:
        
            LpFUNC(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif

TVAL FProcedure_NewSpecialForm(LpXCONTEXT gCP,LpTHREAD gTP,TSymbol **symName, LpCHAR funcName, LpFUNC lpFunc)
{
StartFrame
DeclareTVAL(tmpTval);
EndFrame

(*symName) = TSymbol_MakeUnique(gCP,gTP,funcName);
(*symName)->itsCProcedure =  lpFunc;

tmpTval->Tag = TYSPECIALFORM;
tmpTval->u.Symbol = *symName;

/*  Calling SetGlobalValue with a non-void value will automatically PERM the symbol */

(*symName)->itsGlobalValue = *tmpTval;

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_PrintcProcedure


#endif

TVAL FProcedure_PrintcProcedure(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
NUM                     tmpLen;
 
gTP = gTP; // NOOP to hide unused parameter warning message
/*  Quit if the output string is already too long */
tmpLen = strlen((char*)&atHMChar(asSymbol(&self)->itsCString,0));
if ((*size + tmpLen + 20) > gCP->TObject_MaxOutputLen) 
    {
    return(gCP->TObject_FALSE);
    }

/*  Format output in temporary buffer */
if (self.Tag == TYVMEVALUATOR)
	sprintf((char*)&buf[*size],"#<VmEvaluator %s>",&atHMChar(asSymbol(&self)->itsCString,0));
else
	sprintf((char*)&buf[*size],"#<Function %s>",&atHMChar(asSymbol(&self)->itsCString,0));
*size += (tmpLen + 14);

return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_PrintSpecialForm


#endif

TVAL FProcedure_PrintSpecialForm(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
NUM                     tmpLen;
 
gTP = gTP; // NOOP to hide unused parameter warning message
/*  Quit if the output string is already too long */
tmpLen = strlen((char*)&atHMChar(asSymbol(&self)->itsCString,0));
if ((*size + tmpLen + 20) > gCP->TObject_MaxOutputLen) 
    {
    return(gCP->TObject_FALSE);
    }

/*  Format output in temporary buffer */
sprintf((char*)&buf[*size],"#<SpecialForm %s>",&atHMChar(asSymbol(&self)->itsCString,0));
*size += (tmpLen + 15);

return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_PrintcMacro


#endif

TVAL FProcedure_PrintcMacro(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
NUM                     tmpLen;
 
gTP = gTP; // NOOP to hide unused parameter warning message
/*  Quit if the output string is already too long */
tmpLen = strlen((char*)&atHMChar(asSymbol(&self)->itsCString,0));
if ((*size + tmpLen + 20) > gCP->TObject_MaxOutputLen) 
    {
    return(gCP->TObject_FALSE);
    }

/*  Format output in temporary buffer */
sprintf((char*)&buf[*size],"#<cMacro %s>",&atHMChar(asSymbol(&self)->itsCString,0));
*size += (tmpLen + 10);

return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_cnvDate

Recognize a date string constant in a Lisp source string.  

#endif

TVAL FProcedure_cnvDate(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pSource,LpNUM iChar)
{
NUM                 index;
BOLE                bc;
NUM                 leapyear;
REAL                leapyearcycles;
REAL                truncatedleapyearcycles;
REAL                year;
NUM                 month;
NUM                 day;
REAL                julian;
NUM                 dayofmonth;
REAL                leapyeardays;
NUM					year400th;
NUM					year100th;
NUM					year4th;
StartFrame
DeclareTVAL(retTval);
EndFrame
 
/*  The source string must begin with a # symbol. */

if (pSource[*iChar+0] != '#') FrameExit(gCP->TObject_ERROR_INVALID);

/*  The source string must proceed with a month abbreviation. */

switch (pSource[*iChar+1])
    {
    case    'A':
        if ((pSource[*iChar+2] == 'p') && (pSource[*iChar+3] == 'r'))
            {month = 3; day = 30; julian = 90;}
        else
        if ((pSource[*iChar+2] == 'u') && (pSource[*iChar+3] == 'g'))
            {month = 7; day = 31; julian = 212;}
        else
            goto Bad;
        break;
        
    case    'D':
        if ((pSource[*iChar+2] == 'e') && (pSource[*iChar+3] == 'c'))
            {month = 11; day = 31; julian = 334;}
        else
            goto Bad;
        break;
        
    case    'F':
        if ((pSource[*iChar+2] == 'e') && (pSource[*iChar+3] == 'b'))
            {month = 1; day = 29; julian = 31;}
        else
            goto Bad;
        break;
        
    case    'J':
        if ((pSource[*iChar+2] == 'a') && (pSource[*iChar+3] == 'n'))
            {month = 0; day = 31; julian = 0;}
        else
        if ((pSource[*iChar+2] == 'u') && (pSource[*iChar+3] == 'n'))
            {month = 5; day = 30; julian = 151;}
        else
        if ((pSource[*iChar+2] == 'u') && (pSource[*iChar+3] == 'l'))
            {month = 6; day = 31; julian = 181;}
        else
            goto Bad;
        break;
        
    case    'M':
        if ((pSource[*iChar+2] == 'a') && (pSource[*iChar+3] == 'r'))
            {month = 2; day = 31; julian = 59;}
        else
        if ((pSource[*iChar+2] == 'a') && (pSource[*iChar+3] == 'y'))
            {month = 4; day = 31; julian = 120;}
        else
            goto Bad;
        break;
        
    case    'N':
        if ((pSource[*iChar+2] == 'o') && (pSource[*iChar+3] == 'v'))
            {month = 10; day = 30; julian = 304;}
        else
            goto Bad;
        break;
        
    case    'O':
        if ((pSource[*iChar+2] == 'c') && (pSource[*iChar+3] == 't'))
            {month = 9; day = 31; julian = 273;}
        else
            goto Bad;
        break;
        
    case    'S':
        if ((pSource[*iChar+2] == 'e') && (pSource[*iChar+3] == 'p'))
            {month = 8; day = 30; julian = 243;}
        else
            goto Bad;
        break;
        
    default:
        Bad:
        FrameExit(gCP->TObject_ERROR_INVALID);
        break;
    }
if ((pSource[*iChar+4] != ',') && (pSource[*iChar+4] != '.'))  goto Bad;
index = 5;
    
/*  Recognized the day of the month, and add it to the cumulative julian day. */

if (!ISDIGIT((NUM)pSource[*iChar+index])) goto Bad;
dayofmonth = pSource[*iChar+index] - '0';
if (ISDIGIT((NUM)pSource[*iChar+(++index)]))
    {
    dayofmonth *= 10;
    dayofmonth += pSource[*iChar+(index++)] - '0';
    }
if ((pSource[*iChar+(index)] != ',') && (pSource[*iChar+(index)] != '.')) goto Bad;
++index;
if ((dayofmonth < 1) || (dayofmonth > day)) goto Bad;

julian += dayofmonth -1;

/*  Recognized the year. */

if (!ISDIGIT((NUM)pSource[*iChar+index])) goto Bad;
year = 0;
while (ISDIGIT((NUM)pSource[*iChar+index]))
    {
    year *= 10;
    year += pSource[*iChar+(index++)] - '0';
    }

/*  Recognized the AD or BC suffixes. */

if ((pSource[*iChar+index] == 'B') && (pSource[*iChar+index+1] == 'C'))
    {
    bc = TRUE;
    index += 2;
    }
else
if ((pSource[*iChar+index] == 'A') && (pSource[*iChar+index+1] == 'D'))
    {
    bc = FALSE;
    index += 2;
    }
else
    bc = FALSE;

/*  Compute the number of leap years (every fourth year is a leap year). */

if (!bc)
    {
	AnnoDomini:
	year400th = 0;
	year100th = 0;
	year4th = 0;
	leapyeardays = 0;
	if (year != 0)
		{
		julian += 365;
		--year;

		truncatedleapyearcycles = floor(year / 400);
		leapyearcycles = truncatedleapyearcycles;
		leapyeardays = leapyearcycles * 146097;
		year -= leapyearcycles * 400;
		if (year == 399) year400th = 1;

		truncatedleapyearcycles = floor(year / 100);
		leapyearcycles = truncatedleapyearcycles;
		leapyeardays += leapyearcycles * 36524;
		year -= leapyearcycles * 100;
		if ((year == 99) && (year400th == 0)) year100th = 1;

		truncatedleapyearcycles = floor(year / 4);
		leapyearcycles = truncatedleapyearcycles;
		leapyeardays += leapyearcycles * 1461;
		year -= leapyearcycles * 4;
		if ((year == 3) && (year100th == 0) && (year400th == 0)) year4th = 1;
		}

	leapyear = 0;
	if ((year400th == 1) || (year4th == 1)) leapyear = 1;

    if (year == 1)
        julian += 365;
    else
    if (year == 2)
        julian += 730;
    else
    if (year == 3)
		{
        julian += 1095;
		if (month >= 2)
			julian += leapyear;
		}

    julian += leapyeardays;
    }
else
    {
    if (year < 1) goto AnnoDomini;

	--year;

	year400th = 0;
	year100th = 0;
	year4th = 0;
	truncatedleapyearcycles = floor(year / 400);
	leapyearcycles = truncatedleapyearcycles;
	leapyeardays = leapyearcycles * 146097;
	year -= leapyearcycles * 400;
	if (year == 399) year400th = 1;

	truncatedleapyearcycles = floor(year / 100);
	leapyearcycles = truncatedleapyearcycles;
	leapyeardays += leapyearcycles * 36524;
	year -= leapyearcycles * 100;
	if ((year == 99) && (year400th == 0)) year100th = 1;

	truncatedleapyearcycles = floor(year / 4);
	leapyearcycles = truncatedleapyearcycles;
	leapyeardays += leapyearcycles * 1461;
	year -= leapyearcycles * 4;
	if ((year == 3) && (year100th == 0) && (year400th == 0)) year4th = 1;

	leapyear = 0;
	if ((year400th == 1) || (year4th == 1)) leapyear = 1;

    if (year == 0)
		{
        julian = 365 - julian;
		}
    else
    if (year == 1)
		{
        julian = 365 - julian;
		leapyeardays += 365;
		}
    else
    if (year == 2)
		{
        julian = 365 - julian;
		leapyeardays += 730;
		}
    else
    if (year == 3)
		{
		if (month > 1) julian += leapyear;
        julian = (365 + leapyear) - julian;
		leapyeardays += 1095;
		}

    julian += leapyeardays;
    julian = 0 - julian;
    }

/*  Support for recognizing time. */

if(pSource[*iChar+index] == ':')
    {
    NUM     tmpNdx;
    
    tmpNdx = *iChar+index;
    *retTval = FProcedure_cnvTime(gCP, gTP, pSource, &tmpNdx);
    ExitOnError(*retTval);
    
    index = tmpNdx - *iChar;
    julian += retTval->u.Real;
    }

/*  return the final recognized date value, and set the index. */

*iChar += index;
retTval->Tag = TYDATE;
retTval->u.Real = julian;
FrameExit(*retTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FProcedure_cnvTime

Recognize a time string constant in a Lisp source string. 
We handle :hh:mm:ss or hh:mm:ss in 24 or 12 (AM/PM) formats.

Note: We may wish to add range checking and AMPM recognition.

#endif

TVAL FProcedure_cnvTime(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pSource,LpNUM iChar)
{
REAL    tmpReal;
NUM     count;
NUM     tmpNdx;
NUM     hours;
NUM     mins;
NUM     secs;
StartFrame
DeclareTVAL(ret);
EndFrame

hours = mins = secs = 0;
tmpNdx = *iChar;

/*  Skip over initial ':' if any. */

if (pSource[tmpNdx] == ':')
    ++tmpNdx;
    
*ret = FProcedure_recInt(gCP, gTP, &tmpReal, &count, pSource + tmpNdx);
ExitOnError(*ret);

if((ret->Tag == TYBOLE) && (ret->u.Bool == TRUE))
    {
    hours = tmpReal;
    tmpNdx += count;
    
    if(pSource[tmpNdx] == ':')
        {
        ++tmpNdx;
        *ret = FProcedure_recInt(gCP, gTP, &tmpReal, &count, pSource + tmpNdx);
        ExitOnError(*ret);
        
        if ((ret->Tag == TYBOLE) && (ret->u.Bool == TRUE))
            {
            mins = tmpReal;
            tmpNdx += count;
            
            if(pSource[tmpNdx] == ':')
                {
                ++tmpNdx;
                *ret = FProcedure_recInt(gCP, gTP, &tmpReal, &count, pSource + tmpNdx);
                ExitOnError(*ret);
                
                if((ret->Tag == TYBOLE) && (ret->u.Bool == TRUE))
                    {
                    secs = tmpReal;
                    tmpNdx += count;
                    }
                else
                    goto BadCleanUp;
                }
            }
        else
            goto BadCleanUp;
        }
    }
else
    goto BadCleanUp;
    
/* Check for AM or PM suffix after removing spaces. */

while (pSource[tmpNdx] == ' ') {++tmpNdx;}
if (((pSource[tmpNdx] == 'p') || (pSource[tmpNdx] == 'P')) &&
    ((pSource[tmpNdx+1] == 'm') || (pSource[tmpNdx+1] == 'M')))
    {
    hours += 12;
    }
    
tmpReal = (hours*3600.0 + mins*60.0 + secs)/86400.0;

*iChar = tmpNdx;

ret->Tag = TYREAL;
ret->u.Real = tmpReal;

FrameExit(*ret);

BadCleanUp:

FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
