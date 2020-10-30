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
FList.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FList
#define _H_FList

#include "tobject.h"
#ifdef _SMARTBASE
/*  Function declarations */
extern TVAL FList_Init		(LpXCONTEXT gCP,LpTHREAD gTP);

extern TVAL FList_Car		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdr		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caar		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cadr		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdar		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);      
extern TVAL FList_Caar		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cddr		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caaar		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caadr		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cadar		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caddr		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdaar		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdadr		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cddar		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdddr		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caaaar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caaadr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caadar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caaddr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cadaar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cadadr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Caddar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cadddr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdaaar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdaadr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdadar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdaddr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cddaar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cddadr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cdddar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Cddddr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FList_Setcar	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern TVAL FList_Setcdr	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern TVAL FList_Last		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);  
#endif
#endif

#ifdef _C_FLIST
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
#endif

