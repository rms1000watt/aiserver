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

#define _C_FFLOAT
#define _SMARTBASE

#if 0
FFloat.c

This source file contains some of the support routines which implement processor specific floating
point support for the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

PORTATION NOTE:     These math functions are dependent upon the host environment.
                    During portation, care must be taken to determine the proper
                    substitution code on the new host environment.
#endif

#include "ffloat.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_NANCHECK

Return TRUE if the argument IS an invalid REAL number.

#endif

BOLE FFloat_NANCHECK(LpXCONTEXT gCP,LpTHREAD gTP,REAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
number = x;
mantissa = frexp(number, &exponent);


return(!((exponent >= DBL_MIN_EXP) && (exponent <= DBL_MAX_EXP)));
}
    
/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_INFCHECK

Return TRUE if the argument IS an infinite REAL number.

#endif

BOLE FFloat_INFCHECK(LpXCONTEXT gCP,LpTHREAD gTP,REAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
number = x;
mantissa = frexp(number,&exponent);

return((exponent > DBL_MAX_EXP));
}
                            
/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_NEGCHECK

Return TRUE if the argument IS a negative REAL number.

#endif

BOLE FFloat_NEGCHECK(LpXCONTEXT gCP,LpTHREAD gTP,REAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
number = x;
mantissa = frexp(number,&exponent);

return((exponent >= DBL_MIN_EXP) && (exponent <= DBL_MAX_EXP) && (mantissa < 0));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_ZEROCHECK

Return TRUE if the argument IS a zero REAL number.

#endif

BOLE FFloat_ZEROCHECK(LpXCONTEXT gCP,LpTHREAD gTP,REAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
number = x;
mantissa = frexp(number,&exponent);

return((exponent == 0) && (mantissa == 0));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_NUMCHECK

Returns TRUE if the value IS a valid REAL number.

#endif

BOLE FFloat_NUMCHECK(LpXCONTEXT gCP,LpTHREAD gTP,REAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
number = x;
mantissa = frexp(number,&exponent);

return((exponent >= DBL_MIN_EXP) && (exponent <= DBL_MAX_EXP));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_NUMERRCHECK

Returns TRUE if the value is NOT a valid REAL number.

#endif

BOLE FFloat_NUMERRCHECK(LpXCONTEXT gCP,LpTHREAD gTP,REAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
number = x;
mantissa = frexp(number,&exponent);

return((exponent < DBL_MIN_EXP) || (exponent > DBL_MAX_EXP));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_POSNUMCHECK

Return TRUE if the argument IS a positive REAL number.

#endif

BOLE FFloat_POSNUMCHECK(LpXCONTEXT gCP,LpTHREAD gTP,REAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
number = x;
mantissa = frexp(number,&exponent);

return((exponent >= DBL_MIN_EXP) && (exponent <= DBL_MAX_EXP) && (mantissa > 0));
}
/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_NORMALIZEZERO

Return TRUE if the argument is NOT a valid REAL number. If the argument
is effectively zero, make sure that it is a normalized zero.

#endif

BOLE FFloat_NORMALIZEZERO(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
number = *x;
mantissa = frexp(number,&exponent);

if ((exponent == 0) && (mantissa < .5) && (mantissa > -0.5))
    *x = 0.0;
    
return(!((exponent >= DBL_MIN_EXP) && (exponent <= DBL_MAX_EXP)));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFloat_REALTOBINARYVAL

Return TRUE if the argument was sucessfully converted to a binary value
without generating any floating point system error conditions.

#endif

BOLE FFloat_REALTOBINARYVAL(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL x)
{
double      number;
double      mantissa;
int         exponent;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*x = ceil((double)*x);
    
number = *x;
mantissa = frexp(number,&exponent);

return((exponent >= DBL_MIN_EXP) && (exponent <= DBL_MAX_EXP));
}
