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
FConvert.h

This include file declares some of the cProcedures which implement the conversion 
functions supported by the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  
    
#endif
 
#ifndef _H_FConvert
#define _H_FConvert

#include    "tobject.h"

/*  Macro definitions */

TVAL FConvert_Init(LpXCONTEXT gCP, LpTHREAD gTP);

/*  Function declarations */
extern TVAL FConvert_ToInteger		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ToUInteger		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ToNumber		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ToDate			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ToMoney		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ToCharacter	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ToBoolean		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ToString		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_Downcase		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_Upcase			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ObjToList		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ObjToVector	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ObjToNumVector	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ObjToMatrix	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ObjToNumMatrix	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ObjToStructure	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);    
extern TVAL FConvert_ObjToDictionary(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]); 
extern TVAL FConvert_ObjToDirectory	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FConvert_ToComplex(LpXCONTEXT gCP,LpTHREAD gTP, TVAL arg); 
extern TVAL FConvert_ObjToComplex(LpXCONTEXT gCP,LpTHREAD gTP, LpNUM np, TVAL arg);
extern TVAL FConvert_ObjToCpxVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FConvert_ObjToDeclStructure(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);    

// extern TVAL FConvert_ObjToRange(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
// extern TVAL FConvert_CellToAbsolute(TVAL theValue);

#endif

#ifdef  _C_FCONVERT
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tfltvec.h"
#include    "tintvec.h"
#include    "tobjvec.h"
#endif

