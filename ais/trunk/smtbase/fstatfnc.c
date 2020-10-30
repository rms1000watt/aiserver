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

#define _C_FSTATFNC
#define _SMARTBASE
#if 0
FStatFnc.c

This source file contains some of the cProcedures which implement the statistical 
functions supported by the SmartBase server.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fstatfnc.h"

static TVAL CalcSkewnessAndKurtosisCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
static TVAL CalcMomentsCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
static TVAL CalcSumSqrCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
static TVAL CalcRangeCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
static TVAL CalcNumTypesCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#define _FStatFnc_SQR(x) (x * x)

/* Static variables are used to store the result of mapc as a side effect. */

/*--------------------------------------------------------------------------------------- */
#if 0
FStatFnc_Init

Initialize the Statistics portion of the SmartLisp function library.  

#endif

TVAL FStatFnc_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TSymbol,aSymbol);
EndFrame
 
if(gCP->FStatfnc_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FStatfnc_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"skew",(LpFUNC)&FStatFnc_Skew);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"stdev",(LpFUNC)&FStatFnc_Stdev);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"stdevp",(LpFUNC)&FStatFnc_Stdevp);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"sumsqr",(LpFUNC)&FStatFnc_Sumsqr);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"range",(LpFUNC)&FStatFnc_Range);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"kurtosis",(LpFUNC)&FStatFnc_Kurtosis);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"median",(LpFUNC)&FStatFnc_Median);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"var",(LpFUNC)&FStatFnc_Var);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"varp",(LpFUNC)&FStatFnc_Varp);
ExitOnError(*ret);
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FStatFnc_Median

FStatFnc_Median returns the median of the given numbers.  The median is the
number in the middle of a set of numbers; i.e., half the numbers occur
above the meidan and half below.

#endif

TVAL FStatFnc_Median(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     low;
NUM     k;
NUM     cn;
REAL    xx;
LpREAL  x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
DeclareOBJ(TNumVector,tmpRealVector);
EndFrame

if (argc > 0)
    {
    /*  Initialization of results and call back function. */
    
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&FStatFnc_LoadRealNumbersCallback;
    
    gCP->FStatfnc_NumVector = tmpRealVector = TNumVector_New(gCP, gTP);
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = FStatFnc_LoadRealNumbersCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }
        
    /*  Sort the array */
    
    x = (LpREAL)(&atHMReal(tmpRealVector->itsRealArray,0));
    
    for (low = 0; low < gCP->FStatfnc_count; ++low)
    {
        for (k = low +1; k < gCP->FStatfnc_count; ++k)
        {
            if (x[low] > x[k])
            {
            xx = x[low];
            x[low] = x[k];
            x[k] = xx;
            }
        }
    }

    switch(gCP->FStatfnc_count)
        {
        case 0:
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        break;
        
        case 1:
            /*  If there is only one numeric argument, return it as the median. */
            
            asTag(argr) = TYREAL;
            asReal(argr) = atHMReal(tmpRealVector->itsRealArray,0);
        break;
        
        default:
            
            /*  If the number of arguments is odd, return the middle argument as the median. */

            if (gCP->FStatfnc_count & 1)
                {
                asTag(argr) = TYREAL;
                asReal(argr) = atHMReal(tmpRealVector->itsRealArray,gCP->FStatfnc_count/2);
                }
            else
                {
                /*  If the number of arguments is even, return the average of the middle two  */
                /*  arguments as the median. */
                
                asTag(argr) = TYREAL;
                asReal(argr) = (atHMReal(tmpRealVector->itsRealArray,gCP->FStatfnc_count/2) + 
                                atHMReal(tmpRealVector->itsRealArray,(gCP->FStatfnc_count/2)-1))/2;
                }
        break;
        
        }

    FrameExit(*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Skew

FStat_SKEW returns the value that characterizes the degree of symmetry of a
distribution around its mean.

#endif

TVAL FStatFnc_Skew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0) 
    {
    /*  Initialization of totals and call back function. */
    
    gCP->FStatfnc_xbar = 0.0;
    gCP->FStatfnc_stdev = 0.0;
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcMomentsCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcMomentsCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    if (gCP->FStatfnc_count  < 3)
        {
        FrameExit(gCP->TObject_ERROR_DIVIDE_BY_ZERO);
        }
        
    gCP->FStatfnc_stdev = (gCP->FStatfnc_stdev -  ((gCP->FStatfnc_xbar * gCP->FStatfnc_xbar) / gCP->FStatfnc_count )) / (gCP->FStatfnc_count - 1.0);
    
    gCP->FStatfnc_stdev = sqrt(gCP->FStatfnc_stdev);
    
    gCP->FStatfnc_xbar /= gCP->FStatfnc_count;
    
    gCP->FStatfnc_skew = 0.0;
    gCP->FStatfnc_kurtosis = 0.0;                 
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcSkewnessAndKurtosisCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcSkewnessAndKurtosisCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    gCP->FStatfnc_skew = (gCP->FStatfnc_skew) *  (gCP->FStatfnc_count / (( gCP->FStatfnc_count-1.0) * (gCP->FStatfnc_count - 2.0 )));
    
    if ( gCP->FStatfnc_count >= 4)
       gCP->FStatfnc_kurtosis =  (gCP->FStatfnc_kurtosis) *  (  (gCP->FStatfnc_count * (gCP->FStatfnc_count+1.0)) / (( gCP->FStatfnc_count-1.0)*(gCP->FStatfnc_count-2.0)*(gCP->FStatfnc_count-3.0)) ) 
             - ( (3.0 * (gCP->FStatfnc_count-1.0)*(gCP->FStatfnc_count-1.0)) / ((gCP->FStatfnc_count-2.0)*(gCP->FStatfnc_count-3.0)) );
    else
        gCP->FStatfnc_kurtosis = 0.0;
        
    asTag(argr) = TYREAL;
    asReal(argr) = gCP->FStatfnc_skew;
    FrameExit (*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Kurtosis

Returns the nondimensional quantity which measures the relative
peakness of flatness of a distribution.

#endif

TVAL FStatFnc_Kurtosis(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0) 
    {
    /*  Initialization of totals and call back function. */
    
    gCP->FStatfnc_xbar = 0.0;
    gCP->FStatfnc_stdev = 0.0;
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcMomentsCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcMomentsCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    if (gCP->FStatfnc_count  < 4 ) 
    {
        FrameExit(gCP->TObject_ERROR_DIVIDE_BY_ZERO);
    }
    gCP->FStatfnc_stdev = (gCP->FStatfnc_stdev -  ((gCP->FStatfnc_xbar * gCP->FStatfnc_xbar) / gCP->FStatfnc_count )) / (gCP->FStatfnc_count - 1.0);
    
    gCP->FStatfnc_stdev = sqrt(gCP->FStatfnc_stdev);
    
    gCP->FStatfnc_xbar /= gCP->FStatfnc_count;
    gCP->FStatfnc_skew = 0.0;
    gCP->FStatfnc_kurtosis = 0.0;                 
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcSkewnessAndKurtosisCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcSkewnessAndKurtosisCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    gCP->FStatfnc_skew = (gCP->FStatfnc_skew) *  (gCP->FStatfnc_count / (( gCP->FStatfnc_count-1.0) * (gCP->FStatfnc_count - 2.0 )));
    
    if ( gCP->FStatfnc_count >= 4)
        gCP->FStatfnc_kurtosis =  (gCP->FStatfnc_kurtosis) *  (  (gCP->FStatfnc_count * (gCP->FStatfnc_count+1.0)) / (( gCP->FStatfnc_count-1.0)*(gCP->FStatfnc_count-2.0)*(gCP->FStatfnc_count-3.0)) ) 
             - ( (3.0 * (gCP->FStatfnc_count-1.0)*(gCP->FStatfnc_count-1.0)) / ((gCP->FStatfnc_count-2.0)*(gCP->FStatfnc_count-3.0)) );
    else
        gCP->FStatfnc_kurtosis = 0.0;
        
    asTag(argr) = TYREAL;
    asReal(argr) = gCP->FStatfnc_kurtosis;
    FrameExit(*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Var

FStatFnc_Var returns the variance.

#endif

TVAL FStatFnc_Var(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0)
    {
    /*  Initialization of totals and call back function. */
    
    gCP->FStatfnc_xbar = 0.0;
    gCP->FStatfnc_stdev = 0.0;
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcMomentsCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcMomentsCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    if ((gCP->FStatfnc_count == 0) || (gCP->FStatfnc_count == 1)) 
    {
        FrameExit(gCP->TObject_ERROR_DIVIDE_BY_ZERO);
    }
    asTag(argr) = TYREAL;
    asReal(argr) = (gCP->FStatfnc_stdev - ((gCP->FStatfnc_xbar * gCP->FStatfnc_xbar) / gCP->FStatfnc_count)) / (gCP->FStatfnc_count - 1.0);

    FrameExit(*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Varp

FStatFnc_Var returns the variance of a sample population.

#endif

TVAL FStatFnc_Varp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0)
    {
    /*  Initialization of totals and call back function. */
    
    gCP->FStatfnc_xbar = 0.0;
    gCP->FStatfnc_stdev = 0.0;
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcMomentsCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcMomentsCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    if (gCP->FStatfnc_count == 0) 
        {
        FrameExit(gCP->TObject_ERROR_DIVIDE_BY_ZERO);
        }
        
    asTag(argr) = TYREAL;
    asReal(argr) = ((gCP->FStatfnc_stdev / gCP->FStatfnc_count) - ((gCP->FStatfnc_xbar * gCP->FStatfnc_xbar) / (gCP->FStatfnc_count * gCP->FStatfnc_count)));

    FrameExit(*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Stdev

FStatFnc_Stdev returns the standard deviation.

#endif

TVAL FStatFnc_Stdev(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0)
    {
    /*  Initialization of totals and call back function. */
    
    gCP->FStatfnc_xbar = 0.0;
    gCP->FStatfnc_stdev = 0.0;
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcMomentsCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcMomentsCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    if ((gCP->FStatfnc_count == 0) || (gCP->FStatfnc_count == 1)) 
        {
        FrameExit(gCP->TObject_ERROR_DIVIDE_BY_ZERO);
        }

    gCP->FStatfnc_stdev = (gCP->FStatfnc_stdev - ((gCP->FStatfnc_xbar * gCP->FStatfnc_xbar) / gCP->FStatfnc_count)) / (gCP->FStatfnc_count - 1.0);
    
    asTag(argr) = TYREAL;
    asReal(argr) = sqrt(gCP->FStatfnc_stdev);
    
    FrameExit(*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Stdevp

FStatFnc_Stdevp returns the standard deviation of a population.

#endif

TVAL FStatFnc_Stdevp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0)
    {
    /*  Initialization of totals and call back function. */
    
    gCP->FStatfnc_xbar = 0.0;
    gCP->FStatfnc_stdev = 0.0;
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcMomentsCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcMomentsCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    if (gCP->FStatfnc_count == 0) 
    {
        FrameExit(gCP->TObject_ERROR_DIVIDE_BY_ZERO);
    }
    
    gCP->FStatfnc_stdev = ((gCP->FStatfnc_stdev / gCP->FStatfnc_count) - ((gCP->FStatfnc_xbar * gCP->FStatfnc_xbar) / (gCP->FStatfnc_count * gCP->FStatfnc_count)));
    
    asTag(argr) = TYREAL;
    asReal(argr) = sqrt(gCP->FStatfnc_stdev);
    
    FrameExit(*argr);
    }
    
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Sumsqr

FStatFnc_Sumsqr returns the sum of the squares of its arguments.

#endif

TVAL FStatFnc_Sumsqr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0)
    {
    /*  Initialization of totals and call back function. */
    
    gCP->FStatfnc_xbar = 0.0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcSumSqrCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcSumSqrCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    /*  We simply place the result in gCP->FStatfnc_xbar. */
    
    asReal(argr) = gCP->FStatfnc_xbar;    
    asTag(argr) = TYREAL;

    FrameExit(*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Counta

This utility function returns the FStatFnc_count value of any  object that
evaluates to a numercal value thus contributing to the total FStatFnc_count.

#endif

TVAL FStatFnc_Counta(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0)
    {
    /*  Initialization of results and call back function. */
    
    gCP->FStatfnc_count = 0;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcNumTypesCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcNumTypesCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    /*  We simply place the result in FStatFnc_count. */
    
    asTag(argr) = TYNUM;
    asInt(argr) = gCP->FStatfnc_count;
    
    FrameExit(*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FStatFnc_Range

Given a minimum and maximum value in a list of argument, FStatFnc_Range returns the absolute 
distance between the two values by subtracting max - min.

#endif

TVAL FStatFnc_Range(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM     cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(argr);
EndFrame

if (argc > 0)
    {
    /*  Initialization of totals and call back function. */
    
    gCP->FStatfnc_minimum = _MAXREAL;
    gCP->FStatfnc_maximum = _MINREAL;
    asTag(&gCP->FStatfnc_CallBackTval) = TYCFUNCTION;
    asPointer(&gCP->FStatfnc_CallBackTval) = (POINTER)&CalcRangeCallback;
    
    for(cn = 0; cn < argc; cn++)
        {
        *ret = CalcRangeCallback(gCP,gTP,1,&argv[cn]);
        ExitOnError(*ret);
        }

    asTag(argr) = TYREAL;
    asReal(argr) = fabs(gCP->FStatfnc_maximum - gCP->FStatfnc_minimum);
    
    FrameExit(*argr);
    }
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
TVAL    FStatFnc_LoadRealNumbersCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(arg);
EndFrame

argc = argc; // NOOP to hide unused parameter warning message
tmp->Tag = TYREAL;

switch(asTag(&argv[0]))
    {
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        if(gCP->FStatfnc_NumVector != NULL)
            {
            asTag(arg) = gCP->FStatfnc_NumVector->itsObjectType;
            asObject(arg) = (TObject*)gCP->FStatfnc_NumVector;
            asReal(tmp) = asReal(argv );
            TNumVector_AddNewValue(gCP, gTP, *arg, *tmp);
            gCP->FStatfnc_count++;
            }
    break;
    
    case    TYNUM:
        if(gCP->FStatfnc_NumVector != NULL)
            {
            asTag(arg) = gCP->FStatfnc_NumVector->itsObjectType;
            asObject(arg) = (TObject*)gCP->FStatfnc_NumVector;
            asReal(tmp) = asInt(argv );
            TNumVector_AddNewValue(gCP,gTP,*arg,*tmp);
            gCP->FStatfnc_count++;
            }
    break;
    
    case TYVOID:
    break;
    
    case    TYCHAR:
        if(gCP->FStatfnc_NumVector != NULL)
            {
            asTag(arg) = gCP->FStatfnc_NumVector->itsObjectType;
            asObject(arg) = (TObject*)gCP->FStatfnc_NumVector;
            asReal(tmp) = asChar(argv );
            TNumVector_AddNewValue(gCP,gTP,*arg,*tmp);
            gCP->FStatfnc_count++;
            }
    break;
    
    default :
        FrameExit(TObject_Mapc(gCP, gTP, argv[0], gCP->FStatfnc_CallBackTval));
    break;
    
    }
FrameExit(argv[0]);
}

/*--------------------------------------------------------------------------------------- */
static TVAL CalcMomentsCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
argc = argc; // NOOP to hide unused parameter warning message
switch(asTag(&argv[0]))
    {
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        gCP->FStatfnc_xbar    += asReal(argv );
        gCP->FStatfnc_stdev   += _FStatFnc_SQR(asReal(argv ));
        gCP->FStatfnc_count++;
    break;
    
    case    TYNUM:
        gCP->FStatfnc_xbar    += asInt(argv );
        gCP->FStatfnc_stdev   += _FStatFnc_SQR(asInt(argv ));
        gCP->FStatfnc_count++;
    break;
    
    case TYVOID:
    break;
    
    case    TYCHAR:
        gCP->FStatfnc_xbar    += asChar(argv );
        gCP->FStatfnc_stdev   += _FStatFnc_SQR(asChar(argv ));
        gCP->FStatfnc_count++;
    break;
    
    default :
        return(TObject_Mapc(gCP, gTP, argv[0], gCP->FStatfnc_CallBackTval));
    break;
    
    }
return (argv[0]);
}

/*--------------------------------------------------------------------------------------- */
static TVAL CalcSumSqrCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
argc = argc; // NOOP to hide unused parameter warning message
switch(asTag(&argv[0]))
    {
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        gCP->FStatfnc_xbar    += _FStatFnc_SQR(asReal(argv));
    break;
    
    case    TYNUM:
        gCP->FStatfnc_xbar    += _FStatFnc_SQR(asInt(argv));
    break;
    
    case TYVOID:
    break;
    
    case    TYCHAR:
        gCP->FStatfnc_xbar    += _FStatFnc_SQR(asChar(argv));
    break;
    
    default :
        return(TObject_Mapc(gCP, gTP, argv[0], gCP->FStatfnc_CallBackTval));
    break;
    
    }
return (argv[0]);
}

/*--------------------------------------------------------------------------------------- */
static TVAL CalcNumTypesCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
argc = argc; // NOOP to hide unused parameter warning message
switch(asTag(&argv[0]))
    {
    case    TYDATE:
    case    TYMONEY:
    case    TYREAL:
    case    TYNUM:
    case    TYCHAR:
    case    TYTEXT:
    case    TYSTRINGSUBSTR:
    case    TYSTRING:
    case    TYBOLE:
        gCP->FStatfnc_count++;
    break;
    
    case TYVOID:
    break;
    
    default :
        return(TObject_Mapc(gCP, gTP, argv[0], gCP->FStatfnc_CallBackTval));
    break;
    
    }
return (argv[0]);
}

/*--------------------------------------------------------------------------------------- */
static TVAL CalcRangeCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
argc = argc; // NOOP to hide unused parameter warning message
switch(asTag(&argv[0]))
    {
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        if (asReal(argv ) < gCP->FStatfnc_minimum) 
            gCP->FStatfnc_minimum = asReal(argv );
        if (asReal(argv ) > gCP->FStatfnc_maximum) 
            gCP->FStatfnc_maximum = asReal(argv );
    break;
    
    case    TYNUM:
        if (asInt(argv ) < gCP->FStatfnc_minimum) 
            gCP->FStatfnc_minimum = asInt(argv );
        if (asInt(argv ) > gCP->FStatfnc_maximum) 
            gCP->FStatfnc_maximum = asInt(argv );
    break;
    
    case TYVOID:
        return(gCP->TObject_ERROR_BADCELL);
    break;
    
    case    TYCHAR:
        if (asChar(argv ) < gCP->FStatfnc_minimum) 
            gCP->FStatfnc_minimum = asChar(argv );
        if (asChar(argv ) > gCP->FStatfnc_maximum) 
            gCP->FStatfnc_maximum = asChar(argv );
    break;
    
    default :
        return(TObject_Mapc(gCP, gTP, argv[0], gCP->FStatfnc_CallBackTval));
    break;
    
    }
return (argv[0]);
}

/*--------------------------------------------------------------------------------------- */
static TVAL CalcSkewnessAndKurtosisCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL    x3,x4;
REAL    value;

argc = argc; // NOOP to hide unused parameter warning message
if (gCP->FStatfnc_stdev == 0.0) 
    return (gCP->TObject_ERROR_DIVIDE_BY_ZERO);  /*  no skewness/kurtosis when stdev = 0 */

value = 0.0;

switch( asTag(argv) )
    {
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        value = asReal(argv);
    break;
    
    case TYNUM:
        value = asInt(argv);
    break;
    
    case    TYCHAR:
        value = asChar(argv);
    break;
    
    case TYVOID:
    break;
    
    default :
        return(TObject_Mapc(gCP, gTP, argv[0], gCP->FStatfnc_CallBackTval));
    break;
    }
    
value =  ( value - gCP->FStatfnc_xbar ) / gCP->FStatfnc_stdev;
x3 = value * _FStatFnc_SQR(value);
x4 = x3 * value;
gCP->FStatfnc_skew  += x3;
gCP->FStatfnc_kurtosis  += x4;
gCP->FStatfnc_count++;

return(argv[0]);
}
