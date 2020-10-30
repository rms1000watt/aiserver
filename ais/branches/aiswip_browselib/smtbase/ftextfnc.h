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
FTextFnc.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FTextFnc
#define _H_FTextFnc

#include    "tobject.h"

/*  Macro definitions */

#define _FTextFnc_32000_TEXT_BUF    32000
#define _FTextFnc_256_TEXT_BUF      256

/*  Function declarations */

extern  TVAL FTextFnc_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL FTextFnc_char			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_code			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_clean			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_find			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_left			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_mid			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_right			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_fixed			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_dollar		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_text			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FTextFnc_replace		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_rept			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_substitute	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_trim			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_StringToVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_StringToBVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

extern  TVAL FTextFnc_StringCILT	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_StringCILE	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_StringCIEQ	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_StringCINE	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_StringCIGT	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FTextFnc_StringCIGE	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

extern  TVAL FTextFnc_MacroReplace	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
#endif


