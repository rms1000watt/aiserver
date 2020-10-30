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
TLambda.h

This declaration contains the storage layout for the Smartbase Lambda
Object. An Lambda object is the fundamental executable data entity
stored in the Smartbase Lambda oriented database. 

PARENT:             TObject

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TLambda
#define _H_TLambda

#include "tsymbol.h"
#include "tcontin.h"


#ifdef _SMARTBASE


/*  The Lambda disk structure format */

typedef struct
    {
    TVAL            ClassVariables;
    TVAL            ArgumentVariables;
    TVAL            TemporaryVariables;
    TVAL            PersistantVariables;
    TVAL            ConstantVariables;
    TVAL            RegisterVariables;
	TVAL            PcodeVector;
    TVAL            DebuggerSource;
	TVAL			NativeCodeVector;
	TVAL			Interfaces;
    TVAL            VirtualMachine;
	NUM             EvalWhenDoomed;
    }TLambdaOnDisk;
#define TLambdaOnDiskPtr(h,n)    ((TLambdaOnDisk*)(_HMemoryPtr(h)+n))

/*  Declare the Smartbase Lambda object and framing macros. */
/*  Note:   The Lambda object is the primary executable object  */
/*          saved, loaded, and executed in the Smartbase database  */
/*          engine. It is the result of all lambda expressions */
/*          and is a central component of the Smartbase database.  */
/*  Lambda CLASS macro definitions */

#define VALUEOF(s)					 ((TSymbol*)(s))->itsGlobalValue
#define _VmEvaluate(Lambda,argc,argv) (*((LpVMEVALUATOR)(((Lambda)->VirtualMachine)->itsCProcedure)))(gCP, gTP, Lambda,argc,argv);


#if 0
LISP PCODE TOKEN

The SmartLisp_ interpreter supports a number of native SmartLisp_ operators
known as pcode tokens (pseudo codes). The declaration of the SmartLisp_ pcode
tokens is as follows. 

#endif

#define ARGSTOK                     0
#define ATTOK                       1
#define BACKSLASHTOK                2
#define BARTOK                      3
#define DOLLARTOK                   4
#define DQUOTETOK                   5
#define ENDTOK                      6
#define INFIXOFFTOK                 7
#define INFIXONTOK                  8
#define LBRACETOK                   9
#define LBRACKETTOK                 10
#define LDBRACKETTOK                11
#define LDICTTOK                    12
#define LENVIRONTOK                 13
#define LPARENTOK                   14
#define LPOUNDTOK                   15
#define LQUOTETOK                   16
#define NEWLINETOK                  17
#define PERCENTTOK                  18
#define PERIODTOK                   19
#define POUNDTOK                    20
#define PVARSTOK                    21
#define RBRACETOK                   22
#define RBRACKETTOK                 23
#define RDBRACKETTOK                24
#define RPARENTOK                   25
#define RQUOTETOK                   26
#define SCOLONTOK                   27
#define VARSTOK                     28
#define WHITESPACETOK               29
#define XERRTOK                     30
#define XENDTOK                     31
#define GOTOTOK                     32
#define LABELTOK                    33

/*  Function declarations */

extern  TVAL        TLambda_Clone		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL Pv,TVAL Cv);
extern  TVAL        TLambda_Decode		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpCHAR theSource,BOLE relativeCellRefs);
extern  TVAL	    TLambda_FriendCopy	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL parentClassVariables);
extern  TVAL	    TLambda_FriendNew	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL parentClassVariables, NUM argc, TVAL argv[]);
extern  TVAL        TLambda_GetCvMember	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpCHAR name);
extern  TVAL        TLambda_GetCvSymbol	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL name);
extern  TVAL        TLambda_GetPvMember	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpCHAR name);
extern  TVAL        TLambda_GetPvSymbol	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL name);
extern  TVAL        TLambda_SetPvMember	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpCHAR name,TVAL newValue);
extern  TVAL        TLambda_SetPvSymbol	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL name,TVAL newValue);
extern  TVAL        TLambda_SetPvCvSymbol(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL name,TVAL newValue);
extern  TVAL        TLambda_GetPvCvSymbol(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL name);
extern  void        TLambda_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TLambda_Length		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TLambda*		TLambda_New			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void        TLambda_Doomed		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);

/* Method to function conversions */

extern  void        TLambda_ComputeSize	(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, NUM* aSize);
extern  TObject*    TLambda_Copy			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern  TVAL	    TLambda_NewCopy		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, NUM argc, TVAL argv[]);
extern  TVAL        TLambda_GetIV1		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern  TVAL        TLambda_GetIV2		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2);
extern  TVAL        TLambda_GetIV3		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2, TVAL index3);
extern  TVAL        TLambda_GlobalMark	(LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL        TLambda_Load			(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  void        TLambda_Mark			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern  HMemory     TLambda_Save			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, HMemory anHMemory);
extern  TVAL        TLambda_SetIV1		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
extern  TVAL        TLambda_SetIV2		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2,TVAL newValue);
extern  TVAL        TLambda_SetIV3		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2, TVAL index3,TVAL newValue);
extern  TVAL		TLambda_NativeEval	(LpXCONTEXT gCP, LpTHREAD gTP,TLambda* proc, NUM argc, TVAL argv[]);   
extern  TVAL		TLambda_LambdaVMEval	(LpXCONTEXT gCP, LpTHREAD gTP,TLambda* proc, NUM argc, TVAL argv[]);   
extern  TVAL		TLambda_MakeVMLambda	(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);   


/*  Indices for various Lambda object Structures */

#define _TLambda_tmpEnv  0
#define _TLambda_prmEnv  1
#define _TLambda_curEnv  2


#ifdef  _C_TLambda
#include "futil2.h"
#include "fvmscpt.h"
#include "flisp.h"
#include "fcompile.h"
#include "fobject.h"
#include "fproc.h"
#include "fconio.h"
#endif
#endif
#endif
