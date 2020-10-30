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

#if 0
FCompile.h

This include file contains external variable declarations, macro definitions and function prototypes
for the FCompile.c source file.

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FCompile
#define _H_FCompile

#include    "foptimize.h"
#include    "tvector.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tintvec.h"
#include    "tlambda.h"
#include    "tpcodvec.h"

/*  SWITCHES */

/*  The compilation of nested lambda objects is a two phase process, we use FCompile_lambdaSwitch */
/*  to keep track of which phase is occuring for the lambda object currently under compilation. */
/*  For a description of lambda objects see the SmartLisp Ref. Guide under lambda and define. */
 
/*  (in gTP) NUM  FCompile_lambdaSwitch; */

/*  FCompile_LambdaNest is a counter which is incremented everytime we start the compilation */
/*  of a nested lambda object and decremented every time we complete that process. */

/*  (in gTP) NUM  FCompile_LambdaNest; */

/*  MACRO DEFINITIONS */

/*  These are the MACROS when framing is ENABLED */
/*  We override the basic framing macros to manage the SFtemp variable store as well. */

#define _FCompile_StartFrame    StartFrame SHORT __Base__ = _TempN(compilerState);
#define _FCompile_EndFrame      EndFrame

#define _FCompile_ResetBase     _TempN(compilerState) = __Base__

#define _FCompile_ResetFrame    FrameReset; _FCompile_ResetBase

#define _FCompile_FrameExit(sts)        \
    {_FCompile_ResetFrame; return(sts);}

#define _FCompile_FrameChk(sts)         \
if (asTag(&sts) == TYERROR)             \
    _FCompile_FrameExit(sts)

/*  We define a set of constants to be used for indices for a case statement to control exeuction */
/*  in the compiler. In order to speed up performance in the compiler we create a Structure */
/*  which contains the symbols for the global values of special forms as keys and these indices as */
/*  values. With this mechanism we can do a binary search on the Structure to get to the index */
/*  case for the function to call, and thus avoid doing a series of if-then-else compares to try */
/*  to match the global-value for a given special form. */

#define _FCompile_goto      0
#define _FCompile_define    1
#define _FCompile_lambda    2
#define _FCompile_if        3
#define _FCompile_while     4
#define _FCompile_loop      5
#define _FCompile_begin     6
#define _FCompile_and       7
#define _FCompile_or        8
#define _FCompile_send      9
#define _FCompile_argCount  10
#define _FCompile_argfetch  11
#define _FCompile_let       12
#define _FCompile_case      13
#define _FCompile_cond      14
#define _FCompile_setq      15

/*  We define the dimension for the hashtable used by the compiler to maintain scoping  */
/*  information for variable (symbol) information. In order for the hashing algorithm to function  */
/*  properly, the hashtable must have a prime dimension. For more information see the  */
/*  FCompile_LookUp(...) function header. */

#define _FCompile_TableSize     17

/*  We utilize the vars (Tv) temporary Structure for allocation and management of temporary */
/*  variables used by the compiler for function calls and special forms. The first 3 bindings */
/*  (0-2) in the temporary Structure are reserved for use during the process of pushing function */
/*  call arguments, the fourth binding is always reserved as the return location to be used */
/*  by special forms, this is indicated in the definition below. */

#if 0

We utilize the access macros shown here to provide a mechanism to gain access to the
different TVALS in the compiler state vector.

The data objects used to manage the compiler state are stored in a TVector an accessed via
Macros  as indicated in FCompile_SetupProc in FCompile.c and as shown here:

_Flags(compilerState)

    A TVAL containing three integer indices utilized to track temporary variable usage, 
    see below for descriptions of how each is utilized:
    
    _TempN(compilerState)

        An short integer index used to track which temporary variable is 
        available for use in code generation for function calls. Three temporaries are 
        allocated for each lambda object before its vars are allocated.
    
    _TempBaseN(state)
        
        An short integer index used to store the base of temporary variable allocation in the Tv.
        This will occur after the allocation of any vars.
                
    _SFReturnN(compilerState)
    
        An short integer index used to track which temporary variable is available for 
        use in code generation for returns from special forms.
                
    _NestLvlN(compilerState)
    
        An short integer index used to track the parenthetical nesting level during compilation.
        If we know that we are at level 0 then we may generate VMRETURN instructions directly
        and avoid moving data into temporary result locations.
                
    _LastInstrN(compilerState)
    
        An short integer value used to remember the opcode for the last instruction appended to the
        pcode stream.
                
    _NotSwN(compilerState)
    
        An short integer value used to maintain the value of the not switch, which gets toggled each
        time we encoutner a (not special form.
                
_CurProc(compilerState)

    A TVAL containting  the current procedure object (the compilation target).
    which will contain among other things the following:
                
    _Av(compilerState)  :   A tval for the current Argument variables Structure.
    _Tv(compilerState)  :   A tval for the current Temporary variables Structure.
    _In(compilerState)  :   A tval for the current Lambda Interfaces Structure.
    _Pv(compilerState)  :   A tval for the current Permanent variables Structure.
    _Cv(compilerState)  :   A tval for the current Permanent  constant variables Structure.
    _Rv(compilerState)  :   A tval for the current register variables Structure.
    _Pc(compilerState)  :   A tval for the current PCode vector.
    _Sc(compilerState)  :   A tval for the current source code.
    _Db(compilerState)  :   A tval for the source code debugger vector.
    _Dm(compilerState)  :   The evaluate when doomed switch.

_Result(compilerState)

    A TVAL containing the symbol for the result of an expression. This is used internally
    by the various compiler procedures to keep track of the requested result of the compilation
    of an expression. In cases where side effects could occur by using the requested location, or
    if no location is specified, the compiler temporary variable allocation scheme would be 
    used to determine the result of an expression
    
    Consider the SmartLisp expression:
    
        (+ 1 1)
    
    which would compile to:
    
        0000: addi    aminteg aminteg amfboff 1                1                __T0            
        0004: return  amfboff amvoid  amvoid   __T0            

    In this example no target has been specified as the result of the expression and
    so the compiler uses the first available temporary to return the result.
    
    In contrast consider the SmartLisp expression:
    
    (setq foo 1)
    
    which would compile to:
    
         0000: move    aminteg amgvoff amvoid  1                foo             
         0003: return  amgvoff amvoid  amvoid   foo             
         
    Before the compiler called the setq special form it would set _Result(compilerState) to contain
    foo, then the setq code will examine the parse tree for the expression and determine whether
    it could place the result directly into foo or whether it should use the compilers allocation
    scheme for intermediate results before moving the final result into foo.

    In either case once code has been generated by a compiler procedure it must set 
    _LastResult(compilerState) (see below) to indicate the location for the result of the 
    instruction just compiled.
    
_LastResult(compilerState)

    A TVAL containing the symbol for the result of the last instruction for which code
    was generated.

_Labels(compilerState)

    A TVAL containing a Structure of label-pcode offset bindings. (See FCompile_ResolveGotos.)

_Gotos(compilerState)

    A TVAL containing a Structure of goto-pcode offset bindings.  (See FCompile_ResolveGotos.)

_Cells(compilerState)

    A TVAL containing a sorted TVector of all unique cells referenced in this lambda object.

_Ranges(compilerState)

    A TVAL containing a sorted TVector of all unique ranges referenced in this lambda object.

_Jumps(compilerState)

    A TVAL containing a sorted TIntVector of all jump instructions in this lambda object.
    
#endif

/*  The compiler state for the current lambda object is stored in a TVector state vector. */
/*  This vector has a dimension of 11 as indicated in this definition. */

#define _FCompile_SizeOfStateVector     12

#define _Temp(state)        atHMTval(((TVector*)state)->itsTvalArray,0)
#define _CurProc(state)     atHMTval(((TVector*)state)->itsTvalArray,1)
#define _Result(state)      atHMTval(((TVector*)state)->itsTvalArray,2)
#define _LastResult(state)  atHMTval(((TVector*)state)->itsTvalArray,3)
#define _Labels(state)      atHMTval(((TVector*)state)->itsTvalArray,4)
#define _Gotos(state)       atHMTval(((TVector*)state)->itsTvalArray,5)
#define _Jumps(state)       atHMTval(((TVector*)state)->itsTvalArray,8)
#define _Dbg(state)         atHMTval(((TVector*)state)->itsTvalArray,9)
#define _Flags1(state)      atHMTval(((TVector*)state)->itsTvalArray,10)
#define _Flags2(state)      atHMTval(((TVector*)state)->itsTvalArray,11)

/*  We utilize the access macros shown here to provide a mechanism to gain access to the */
/*  data contained in the TVALS in the compiler state vector. */

#define _DbgP(state)        ((TVector*)_Dbg(state).u.Object)
#define _JumpsP(state)      ((TIntVector*)_Jumps(state).u.Object)
#define _CurProcP(state)    ((TLambda*)_CurProc(state).u.Object)
#define _LabelsP(state)     ((TStructure*)_Labels(state).u.Object)
#define _GotosP(state)      ((TStructure*)_Gotos(state).u.Object)
#define _TempN(state)       _Flags1(state).u.Flag[0]
#define _TempBaseN(state)   _Flags1(state).u.Flag[1]
#define _SFReturnN(state)   _Flags1(state).u.Flag[2]
#define _NestLvlN(state)    _Flags1(state).u.Flag[3]
#define _LastInstrN(state)  _Flags2(state).u.Flag[0]
#define _NotSwN(state)      _Flags2(state).u.Flag[1]

/*  Macros are provided to access fields in the current procedure object. Please see */
/*  TProcedure.h for the definition of the procedure object. */

#define _Sv(state)          (_CurProcP(state)->ClassVariables)
#define _Av(state)          (_CurProcP(state)->ArgumentVariables)
#define _Tv(state)          (_CurProcP(state)->TemporaryVariables)
#define _In(state)          (_CurProcP(state)->Interfaces)
#define _Pv(state)          (_CurProcP(state)->PersistantVariables)
#define _Cv(state)          (_CurProcP(state)->ConstantVariables)
#define _Rv(state)          (_CurProcP(state)->RegisterVariables)
#define _Pc(state)          (_CurProcP(state)->PcodeVector)
#define _Sc(state)          (_CurProcP(state)->DebuggerSource)
#define _Dm(state)          (_CurProcP(state)->EvalWhenDoomed)
#define _PcP(state)         (_CurProcP(state)->PcodeVector)
#define _PvP(state)         (_CurProcP(state)->PersistantVariables)
#define _ScP(state)         _CurProcP(state)->DebuggerSource)

/*  We provide a set of macros to easily format the TVALS to be used in an argument list */
/*  to the FOptimize_OptimizePcode procedure.  */

#define _FCompile_MakeVOID(target) {(target).Tag = TYVOID; (target).u.Int = 0; (target).Modifier = AMVOID; (target).Offset = 0;}
#define _FCompile_MakeCON(target, tval) {(target) = (tval); (target).Offset = 0; (target).Modifier = AMWORD;}
#define _FCompile_MakeIMM(target, immediate) {(target).Tag = TYNUM; (target).u.Int = immediate; (target).Modifier = AMINTEGER; (target).Offset = 0;}
#define _FCompile_MakeINT(target, code) {(target).Tag = TYNUM; (target).u.Int = code;}
#define _FCompile_MakeTVL(target, tval) {target = tval;}
#define _FCompile_MakeOBJ(target, obj)  {(target).Tag = (obj)->itsObjectType; (target).u.Object = (TObject*)(obj);}

/*  Symbol references in virtual machine instructions are either stored in the form of modifier and */
/*  numeric index (offset), in the case of temporary, argument and permanent variables, or in the  */
/*  case of global variables as modifer and symbol index (offset). For this reason it  */
/*  is very convenient to provide a MACRO which will take a symbol TVAL which has been overloaded  */
/*  with modifier and offset information and format the "offset" of the TVAL with the appropriate  */
/*  value. */

#define _FCompile_MakeSYM(target, tval)\
if((tval).Modifier == AMGVOFFSET) {(target) = (tval);}\
else {(target) = (tval);(target).Tag = TYNUM;(target).u.Int = (tval).Offset;(target).Modifier = (tval).Modifier;(target).Offset = (tval).Offset;}

/*  We provide a macro to move to the next pair in a list or return nil when we reach */
/*  a pair whose cdr is not nil. */
    
#define _FCompile_CdrPair(p) (p != NULL && p->itsCdr.Tag == TYPAIR) ? p->itsCdr.u.Pair : NULL

/*  FUNCTION DECLARATIONS */

extern  TVAL    FCompile_Init				(LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL    FCompile_SetupRegs			(LpXCONTEXT gCP, LpTHREAD gTP,TLambda* Lambda);
extern  TVAL    FCompile_SetupVars			(LpXCONTEXT gCP, LpTHREAD gTP,TLambda* Lambda);
extern  TVAL    FCompile_Compile			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FCompile_CompilePrivate		(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FCompile_LookUp				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TSymbol* theSymbol);
extern  TVAL    FCompile_ResolveTemp		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TSymbol* theSymbol, NUM whichStructure, TVAL prefValue);
extern  TVAL    FCompile_Resolve			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TSymbol* theSymbol, NUM whichStructure);
extern  TVAL    FCompile_Bind				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair, TStructure* whichEnv );
extern  TVAL    FCompile_SetupProc			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state);
extern  TVAL    FCompile_Recognize			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_Call				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_Push				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair, LpNUM instruction);
extern  TVAL    FCompile_Singleton			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_svars				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_pvars				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_cvars				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_rvars				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_faces				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_vars				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_args				(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_ResolveGotos		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state);
extern  TVAL    FCompile_WritePcode			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TVAL wrdOpcode,TVAL opnd1,TVAL opnd2,TVAL opnd3);
extern  TVAL    FCompile_FixupJumps			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, NUM delta);
extern  TVAL    FCompile_RecognizeHead		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair);
extern  TVAL    FCompile_HashTval			(LpXCONTEXT gCP, LpTHREAD gTP, NUM tableSize, NUM start, NUM len, TVAL key);
extern  TVAL    FCompile_HashString			(LpXCONTEXT gCP, LpTHREAD gTP, NUM tableSize, LpCHAR key);
extern  TVAL    FCompile_SetupArg			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TVAL car, LpTVAL modTvalP, LpTVAL valTvalP );
extern  TVAL    FCompile_CreateConstant		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TVAL constant );
extern  TVAL    FCompile_RegisterToTemp		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TVAL original[], TVAL temp[]);
extern  TVAL    FCompile_TempToRegister		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TVAL original, TVAL temp);
extern  TVAL    FCompile_NewSpecialForm		(LpXCONTEXT gCP, LpTHREAD gTP, TSymbol **symName, LpCHAR funcName);
extern  TVAL	FCompile___Send				(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL	FCompile_BuildInitializer	(LpXCONTEXT gCP, LpTHREAD gTP, NUM modifier, TVAL type, TVAL name, TVAL constant);

#endif

