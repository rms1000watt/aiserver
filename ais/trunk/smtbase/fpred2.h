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
fpred2.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FPredicate2
#define _H_FPredicate2

#include    "tobject.h"

/*  Function declarations */
extern TVAL FPredicate2_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL FPredicate2_DeepCompare	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL left,TVAL right);
extern TVAL FPredicate2_FullCompare	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL left,TVAL right);
extern TVAL FPredicate2_FullIdentical(LpXCONTEXT gCP,LpTHREAD gTP,TVAL left,TVAL right);
extern NUM  FPredicate2_QuickCompare(LpXCONTEXT gCP,LpTHREAD gTP,LpTVAL left,LpTVAL right);


extern TVAL FPredicate2_Negativep	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Positivep	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Datep		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Moneyp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Numberp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Integerp	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Nullp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Pairp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Procedurep	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Macrop		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Compare		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Identicalp	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_CompareLT	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_CompareLE	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Equal		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_CompareNE	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_CompareGE	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_CompareGT	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Predicate	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Type		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Structure	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Dictionary	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Directory	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Congruentp	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Errorp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FPredicate2_Memeqv		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_IsInside	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Zerop		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FPredicate2_Complexp	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif

