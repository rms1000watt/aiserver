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
FFloat.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FFloat
#define _H_FFloat

#include    "tobject.h"

/*  Macro definitions */

#define _FFloat_CONSTANT_E  exp(1)

/*  ERROR CHECK MACROS USED ON ARGUMENTS THAT ARE TO BE EVAULATED from xl68881.h */

/*  NOTE:   errno is defined in math.c  ANSI STANDARD */



/*  Function declarations */

extern  BOLE FFloat_NANCHECK		(LpXCONTEXT gCP,LpTHREAD gTP,REAL x);
extern  BOLE FFloat_INFCHECK		(LpXCONTEXT gCP,LpTHREAD gTP,REAL x);
extern  BOLE FFloat_NEGCHECK		(LpXCONTEXT gCP,LpTHREAD gTP,REAL x);
extern  BOLE FFloat_ZEROCHECK		(LpXCONTEXT gCP,LpTHREAD gTP,REAL x);
extern  BOLE FFloat_NUMCHECK		(LpXCONTEXT gCP,LpTHREAD gTP,REAL x);
extern  BOLE FFloat_NUMERRCHECK		(LpXCONTEXT gCP,LpTHREAD gTP,REAL x);
extern  BOLE FFloat_POSNUMCHECK		(LpXCONTEXT gCP,LpTHREAD gTP,REAL x);
extern  BOLE FFloat_NORMALIZEZERO	(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL x);
extern  BOLE FFloat_REALTOBINARYVAL	(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL x);

#endif

