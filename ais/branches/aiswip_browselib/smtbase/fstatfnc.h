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
FStatFnc.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FStatFnc
#define _H_FStatFnc

#include    "tobject.h"
#include    "tnumvec.h"

/*  Function declarations */

extern  TVAL FStatFnc_Init						(LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL FStatFnc_Counta					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Kurtosis					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_LoadRealNumbersCallback	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Median					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Range						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Skew						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Stdev						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Stdevp					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Sumsqr					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Var						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FStatFnc_Varp						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

#endif	// _H_FStatFnc

#ifdef _C_FSTATFNC
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "fproc.h"
#include    "ffloat.h"
#endif	// _C_FSTATFNC

