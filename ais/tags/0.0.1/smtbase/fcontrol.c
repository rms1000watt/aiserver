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

#define _C_FCONTROL
#define _SMARTBASE 
#if 0
FControl.c
    
This source file contains some of the cMacros supported by the 
SmartLisp interpreter. In particular the implementation of control structures like
case statements and looping constructs are handled here.


AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fcontrol.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FControl_Init

Initialize the control portion of the SmartLisp function library.  

#endif

TVAL FControl_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FControl_Initialized == TRUE) /* Explicit checking in if-statement */
    {
    FrameExit(gCP->TObject_OK);
    }
gCP->FControl_Initialized = TRUE; /* Use TRUE instead of 1 */
    
/* Register the SmartLisp cMacros contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"mapc",(LpFUNC)&FControl_Mapc);
ExitOnError(*ec);
alsoSymbol  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"forEach");
TSymbol_SetGlobalValue(gCP,gTP,alsoSymbol, aSymbol->itsGlobalValue);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"map",(LpFUNC)&FControl_Map);
ExitOnError(*ec);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FControl_Mapc

The mapc cProcedure applies the specified procedure {argv[0]} to each argument 
in {argv[1]}. The argument {argv[1]} must be a proper list, a vector, or a structure.
The result of the last application is returned. The procedure 
{argv[0]} must be a procedure of one argument. 

Several examples follow.

    (mapc  isEven  ''(1  2  3  4))           =>  true
    (mapc  floor  ''(1.2  2.44))            =>  2
    (mapc  1+  #(1  2  3  4))               =>  5
    (mapc  1+  #{A: 1  B: 2  C: 3  D: 4})   =>  5

#endif

TVAL FControl_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;
/* Modified to give a more descriptive error message. */
if (argc != 2)
	{
	*ret = TERROR("!mapc: Expecting 2 arguments!" );
	FrameExit(*ret)
	}

if (asTag(&argv[1]) == TYVOID)
	{
    FrameExit(gCP->Tval_VOID);
	}
else
if ((asTag(&argv[0]) != TYLAMBDA) && (asTag(&argv[0]) != TYCPROCEDURE))
	{
	/* Modified to give a more descriptive error message. */
	*ret = TERROR("!mapc: Expecting argument 1 to be a function!");
	FrameExit(*ret);
	}

    /*  Apply to any vectored type. */

FrameExit(TObject_MapcObject(gCP, gTP, argv[1], argv[0]));

}

/*--------------------------------------------------------------------------------------- */
#if 0

FControl_Map

The map cProcedure applies the specified procedure {argv[0]} to each argument in 
{argv[1]}. The argument {argv[1]} must be a proper list, a vector, or a structure.
The map procedure returns a SmartLisp object (of the type of argv[1]) 
containing the result of the application. The procedure {argv[0]} must be 
a procedure of one argument. 

Several examples follow.

    (map  isEven  ''(1  2  3  4))            =>  (false  true  false  true)
    (map  floor  ''(1.2  2.44))             =>  (1  2)
    (map  1+  #(1  2  3  4))                =>  #(2  3  4  5)
    (map  1+  #{A: 1  B: 2  C: 3  D: 4})    =>  #{A: 2  B: 3  C: 4  D: 5}

#endif

TVAL FControl_Map(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareOBJ(TVector,vp2);
DeclareTVAL(ret);
EndFrame
 
/*  Initialization */

*ret = gCP->Tval_VOID;

if (argc != 2)
	{
	*ret = TERROR("!map: Expecting 2 arguments!" );
	FrameExit(*ret)
	}


if ((asTag(&argv[0]) != TYLAMBDA) && (asTag(&argv[0]) != TYCPROCEDURE))
	{
	*ret = TERROR("!map: Expecting argument 1 to be a function!");
	FrameExit(*ret);
	}


/*  Apply to any vectored type. */
  
*ret = TObject_MapObject(gCP,gTP,argv[1], argv[0]);
FrameExit(*ret);
    
}
