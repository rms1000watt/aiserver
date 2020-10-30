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
FDateFnc.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FDateFnc
#define _H_FDateFnc

#include    "tobject.h"

/*  Macro definitions */

#define _FDateFnc_StartingYear  1900

/*  Function declarations */

extern TVAL FDateFnc_IsBlank		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_date			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_dateconstant	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_datevalue		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_day			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_days360		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_month			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_year			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_now			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_today			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_hour			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_minute			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_second			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_time			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_timevalue		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FDateFnc_timeconstant	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

extern TVAL FDateFnc_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL FDateFnc_CnvYearToText	(LpXCONTEXT gCP,LpTHREAD gTP, NUM month, LpCHAR monthText);
extern TVAL FDateFnc_CnvMonthToText	(LpXCONTEXT gCP,LpTHREAD gTP, NUM month, LpCHAR monthText);
extern TVAL FDateFnc_recNumber		(LpXCONTEXT gCP,LpTHREAD gTP, LpCHAR  textP);
extern TVAL FDateFnc_TextToJulian	(LpXCONTEXT gCP,LpTHREAD gTP, LpCHAR textP, LpREAL julianP);
extern TVAL FDateFnc_monthNumber	(LpXCONTEXT gCP,LpTHREAD gTP, LpCHAR monthText, LpNUM intResultP);

extern TVAL FProcedure_cnvTime		(LpXCONTEXT gCP,LpTHREAD gTP, LpCHAR pSource,LpNUM iChar);

#endif

#ifdef _C_FDATEFNC
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tintvec.h"
#include    "tobjvec.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "fconvert.h"
#include    "fmath1.h"
#endif

