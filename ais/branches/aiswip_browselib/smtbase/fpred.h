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
FPredicate.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FPredicate
#define _H_FPredicate

#include    "tobject.h"

/*  Function declarations */

extern  TVAL FPredicate_Init			(LpXCONTEXT gCP, LpTHREAD gTP);

extern  TVAL FPredicate_Atom			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_Boolean			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_Charp			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_CharAlphabeticp	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_CharLowerCasep	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_CharNumericp	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_CharWhitespacep	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_CharUpperCasep	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_CharAlphanumericp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_CharNamep		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_LT				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_LT				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_LT				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_LE				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_EQ				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_NE				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_GE				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_GT				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_GT				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_GT				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_Symbolp			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_Vectorp			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_Matrixp			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_BitVectorp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_ByteVectorp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_CpxVectorp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_IntVectorp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_NumVectorp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_FltVectorp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_Object			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_ObjVectorp		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_PcodeVectorp	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_Stringp			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FPredicate_Textp			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);


#endif

