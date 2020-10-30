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
FFinance.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FFinance
#define _H_FFinance

#include    "tobject.h"

/*  Macro definitions */

#define _FFinance_IRR_CONVERG_FACTOR    (REAL)(1E-5)
#define  _FFinance_RATE_CONVERG_FACTOR  0.0000001


/*  Function declarations */

extern  TVAL FFinance_DB	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_DDB	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_FV	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_IPMT	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_IRR	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_MIRR	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_NPER	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_NPV	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_PMT	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_PPMT	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_PV	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_RATE	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_SLN	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_SYD	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FFinance_VDB	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

extern  TVAL FFinance_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  BOLE FFinance_GetArg(LpXCONTEXT gCP,LpTHREAD gTP, LpTVAL error, LpTVAL arg, LpREAL result);

#endif

#ifdef _C_FFINANCE
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "fproc.h"
#include    "fmath2.h"
#include    "fstatfnc.h"
#include    "ffloat.h"
#endif

