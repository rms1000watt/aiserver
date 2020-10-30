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

#define _C_FFINANCE
#define _SMARTBASE

#if 0
FFinance.c

This source file contains some of the cProcedures which implement the math functions
supported by the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "ffinance.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_Init

Initialize the Numeric portion of the SmartLisp function library.  

#endif

TVAL FFinance_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TSymbol,aSymbol);
EndFrame
 
if(gCP->FFinance_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FFinance_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"depreciation",(LpFUNC)&FFinance_DB);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"doubleDepreciation",(LpFUNC)&FFinance_DDB);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"futureValue",(LpFUNC)&FFinance_FV);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"interestPayment",(LpFUNC)&FFinance_IPMT);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"payment",(LpFUNC)&FFinance_PMT);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol, (LpCHAR)"principalPayment",(LpFUNC)&FFinance_PPMT);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"presentValue",(LpFUNC)&FFinance_PV);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"straightDepreciation",(LpFUNC)&FFinance_SLN);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sumDepreciation",(LpFUNC)&FFinance_SYD);
ExitOnError(*ret);
*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"variableDepreciation",(LpFUNC)&FFinance_VDB);
ExitOnError(*ret);


FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_VDB

The rate function returns the variable depreciation for an asset.

SYNTAX:     (variableDepreciation nper,pmt,pv,{fv},period, {type},{guess})

ARGUMENTS:  rate    :   is the interest rate per period.
            nper    :   is the total numbe rof payments period in an annuity
            pmt     :   the payment made each period. It cannot change over
                        the life of the annuity.
            pv      :   is the present value, or the lump-sum amount that
                        a series of future payments is worth right now.
                        if pv is ommited that default value is zero.
            type    :   is the number 0,1 and indicates when payments are
                        due.  If type is omitted, its default value is 0.
FORMULA:

EXAMPLE:
#endif

TVAL FFinance_VDB(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL        cost;
REAL        salvage;
REAL        life;
REAL        start_period;
REAL        end_period;
REAL        factor;
REAL        no_switch;

LONG        first_full_period;
LONG        last_full_period;

REAL        vdb;
REAL        ratio;
REAL        total;
REAL        straight;
REAL        total_allowed;
LONG        i;
BOLE        bReturnNegative;
StartFrame
DeclareTVAL(argr);
DeclareTVAL(ret);
EndFrame

/*  INITIALIZE THE VARIABLES */

*argr = gCP->Tval_VOID;
*ret = gCP->Tval_VOID;

factor = 2.0;
start_period = 0;
end_period = 0;
life = 0;
salvage = 0.0;
cost = 0.0;
no_switch = FALSE;

/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */

if (argc < 5)
	{
	*ret = TERROR("!variableDepreciation: Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 7)
	{
	*ret = TERROR("!variableDepreciation: Too many arguments!");
	FrameExit(*ret);
	}


if(FFinance_GetArg(gCP, gTP, ret, &argv[0], &cost))
	{
	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'cost' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, cost) || FFloat_NORMALIZEZERO(gCP, gTP, &cost))
	{
	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'cost' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[1], &salvage))
	{
	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'salvage' argument!");
	FrameExit(*ret);
	}

if( FFloat_NEGCHECK(gCP, gTP, salvage) || FFloat_NORMALIZEZERO(gCP, gTP, &salvage))
	{
	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'salvage' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[2], &life))
	{
	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'life' argument!");
	FrameExit(*ret);
	}

if( FFloat_NEGCHECK(gCP, gTP, life) || FFloat_ZEROCHECK(gCP, gTP, life))
	{
	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'life' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[3], &start_period))
	{
	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'start' argument!");
	FrameExit(*ret);
	}

if( FFloat_NEGCHECK(gCP, gTP, start_period))
	{
	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'start' argument!");
	FrameExit(*ret);
	}

if ((start_period > (life + 1))  || (start_period < 0)) 
    {
 	*ret = TERROR("!variableDepreciation: Expecting 'start' argument to be greater than 'life' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[4], &end_period))
    {
 	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'end' argument!");
	FrameExit(*ret);
	}

if( FFloat_NEGCHECK(gCP, gTP, end_period))
    {
 	*ret = TERROR("!variableDepreciation: Missing or invalid value for 'end' argument!");
	FrameExit(*ret);
	}

if (end_period > (life + 1)) 
    {
  	*ret = TERROR("!variableDepreciation: Expecting 'end' argument to be greater than 'life' argument!");
	FrameExit(*ret);
    }
    
if (argc > 5)
    {
    if(FFinance_GetArg(gCP, gTP, ret, &argv[5], &factor))
		{
  		*ret = TERROR("!variableDepreciation: Missing or invalid value for 'factor' argument!");
		FrameExit(*ret);
		}

    if( FFloat_NEGCHECK(gCP, gTP, factor) || FFloat_NORMALIZEZERO(gCP, gTP, &factor))
		{
  		*ret = TERROR("!variableDepreciation: Missing or invalid value for 'factor' argument!");
		FrameExit(*ret);
		}
    }

if (argc == 7)
    {
    if(FFinance_GetArg(gCP, gTP, ret, &argv[6], &no_switch))
		{
  		*ret = TERROR("!variableDepreciation: Missing or invalid value for 'switch' argument!");
		FrameExit(*ret);
		}
        
    FFloat_REALTOBINARYVAL(gCP, gTP, &no_switch);
    }

/*  SWITCH COST AND SALVAGE IF COST < SALVAGE */

bReturnNegative = FALSE;
if (cost < salvage) 
    {
    REAL u;
    
    u = cost;
    cost = salvage;
    salvage = u;
    bReturnNegative = TRUE;
    }

first_full_period = (LONG) floor( start_period);
last_full_period = (LONG) ceil( end_period);
ratio = factor / life;
straight = (cost - salvage) / life;
total_allowed = cost - salvage;

for (total = vdb = 0.00, i = 1; i <= last_full_period; i++)
{
    REAL z;
    
    z = (cost - total) * ratio; 
    if (!no_switch && (z < straight))
        z = straight;
    if (z > (total_allowed - total))
        z = total_allowed - total;
    total += z;

    if (i == first_full_period)
        vdb += (first_full_period - start_period) * z;
    else if (i == last_full_period)
        vdb += (1 - last_full_period + end_period) * z;
    else if (i > first_full_period && i < last_full_period)
        vdb += z;
    
}

asTag(argr) = TYREAL;
asReal(argr) = bReturnNegative ? -vdb : vdb;

FrameExit(*argr);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_SYD

Returns the sum-of-years` digits depreciation for an asset
for a specified period.

SYNTAX:
    (sumDepreciation cost,salvage,life,period)

ARGUMENTS:
    cost    :   initial cost of the asset
    salvage :   value at the end of the depreciation period
    life    :   number of periods over which the asset is depreciated
    per     :   the period; NOTE: assumes same units as life

ALGORITHM   :
    simply compute the following formula :  (x - y) / c

EXAMPLE:
#endif

TVAL FFinance_SYD(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL            cost;               /*  cost value of the asset         */
REAL            per;                /*  periof for which to calc. dp    */
REAL            life;               /*  true life of the asset          */
REAL            syd;                /*  sum-of-years depreciation       */
REAL            salvage;            /*  temporary storage for calc.     */
REAL            u;                  /*  temporary storage for calc.     */
REAL            v;                  /*  temporary storage for calc.     */
StartFrame
DeclareTVAL(argr);
DeclareTVAL(ret);
EndFrame

/*  INITIALIZE THE VARIABLES */

*argr = gCP->Tval_VOID;
*ret = gCP->Tval_VOID;

cost = 0.0;
salvage = 0.0;
life = 0.0;
per = 0.0;

/*  RETRIEVE AND VERIFY THE ARGUMENTS  */

if (argc < 3)
	{
	*ret = TERROR("!sumDepreciation: Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 4)
	{
	*ret = TERROR("!sumDepreciation: Too many arguments!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[0], &cost))
	{
	*ret = TERROR("!sumDepreciation: Missing or invalid 'cost' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, cost) || FFloat_NORMALIZEZERO(gCP, gTP, &cost))
	{
	*ret = TERROR("!sumDepreciation: Missing or invalid 'cost' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[1], &salvage))
	{
	*ret = TERROR("!sumDepreciation: Missing or invalid 'salvage' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, salvage) || FFloat_NORMALIZEZERO(gCP, gTP, &salvage))
	{
	*ret = TERROR("!sumDepreciation: Missing or invalid 'salvage' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[2], &life))
	{
	*ret = TERROR("!sumDepreciation: Missing or invalid 'life' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, life) || FFloat_ZEROCHECK(gCP, gTP, life))
	{
	*ret = TERROR("!sumDepreciation: Missing or invalid 'life' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[3], &per))
	{
	*ret = TERROR("!sumDepreciation: Missing or invalid 'period' argument!");
	FrameExit(*ret);
	}

if( FFloat_NEGCHECK(gCP, gTP, per) || FFloat_ZEROCHECK(gCP, gTP, per))
	{
	*ret = TERROR("!sumDepreciation: Missing or invalid 'period' argument!");
	FrameExit(*ret);
	}

/*  compute the sum-of-years digits depreciation for an asset for a specified period. */

if (per <= life) 
    {
    u = 2.0 * (cost - salvage) * (life - per + 1.0);
    v = life * (life + 1.0);
    
    syd = u / v;
    }
else 
    {
	*ret = TERROR("!sumDepreciation: Expecting 'period' value to be less than 'life' value!");
	FrameExit(*ret);
    }

asTag(argr) = TYREAL;
asReal(argr) = syd;
FrameExit(*argr);

    
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_SLN

Returns the straight-line depreciation for an asset for a single period.

SYNTAX:
    (straightDepreciation cost,salvage,life)

ARGUMENTS:
    cost    :   initial cost of the asset
    salvage :   value at the end of the depreciation period
    life    :   number of periods over which the asset is depreciated

ALGORITHM   :
    simply compute the following formula :  (x - y) / c

EXAMPLE:
    (straightDepreciation 30000,7500,10) ==> $2,250.00
#endif

TVAL FFinance_SLN(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL            cost;               /*  cost value of the asset         */
REAL            salvage;            /*  the declining balance           */
REAL            life;               /*  true life of the asset          */
REAL            sln;                /*  straight-line depreciation      */
StartFrame
DeclareTVAL(argr);
DeclareTVAL(ret);
EndFrame

/*  INITIALIZE THE VARIABLES  */

*argr = gCP->Tval_VOID;
*ret = gCP->Tval_VOID;

cost = 0.0;
salvage = 0.0;
life = 0.0;
sln = 0.0;

/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */

if (argc < 3)
	{
	*ret = TERROR("!straightDepreciation: Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 3)
	{
	*ret = TERROR("!straightDepreciation: Too many arguments!");
	FrameExit(*ret);
	}


if(FFinance_GetArg(gCP, gTP, ret, &argv[0], &cost))
	{
	*ret = TERROR("!straightDepreciation: Missing or invalid 'cost' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, cost) || FFloat_NORMALIZEZERO(gCP, gTP, &cost))
	{
	*ret = TERROR("!straightDepreciation: Missing or invalid 'cost' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[1], &salvage))
	{
	*ret = TERROR("!straightDepreciation: Missing or invalid 'salvage' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, salvage) || FFloat_NORMALIZEZERO(gCP, gTP, &salvage))
	{
	*ret = TERROR("!straightDepreciation: Missing or invalid 'salvage' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[2], &life))
	{
	*ret = TERROR("!straightDepreciation: Missing or invalid 'life' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, life) || FFloat_NORMALIZEZERO(gCP, gTP, &life))
	{
	*ret = TERROR("!straightDepreciation: Missing or invalid 'life' argument!");
	FrameExit(*ret);
	}

if(life == 0.0)
	{
	*ret = TERROR("!straightDepreciation: life' argument cannot be zero!");
	FrameExit(*ret);
	}


/*  COMPUTE THE STRAIGHT-LINE DEPRECIATION */

sln = (cost - salvage) / life;

asTag(argr) = TYREAL;
asReal(argr) = sln;

FrameExit(*argr);
    
}


/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_PV

The FFinance_PV returns the present value of an investment based on periodic,
constant payments and a constant interest rate.

SYNTAX:     (presentValue rate,nper,pmt,fv, type)

ARGUMENTS:  rate    :   is the interest rate per period.
            nper    :   is the total numbe rof payments period in an annuity
            pmt     :   the payment made each period. It cannot change over
                        the life of the annuity.
            fv      :   is the future value, or the lump-sum amount that
                        a series of future payments is worth right now.
                        if pv is ommited that default value is zero.
            type    :   is the number 0,1 and indicates when payments are
                        due.  If type is omitted, its default value is 0.
FORMULA:
    
    CASE # 1:   if rate != 0
    ------------------------
    PV(rate,nper,pmt,pv,type):= -[ (pv * c) + (pmt * u) * v ]


    CASE # 2:   if rate == 0
    ------------------------
    
    PV(rate,nper,pmt,pv,type):= -[ pv + (pmt * nper) ]
    
EXAMPLE:
    (pv 0.005,10,-200,-500,1) ==> $2431.4865
    
#endif

TVAL FFinance_PV(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL        ptype;
REAL        rate;                       
REAL        nper;                   
REAL        pmt;                        
REAL        pv;     
REAL        c;
REAL        u;
REAL        v;
REAL        fv;
REAL        q;
StartFrame
DeclareTVAL(argr);
DeclareTVAL(ret);
EndFrame

/*  INITIALIZE THE VARIABLES */

ptype = 0;
pmt = 0.0;
pv = 0.0;
rate = 0.0;
fv = 0.0;

*argr = gCP->Tval_VOID;
*ret = gCP->Tval_VOID;

/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */

if (argc < 3)
	{
	*ret = TERROR("!presentValue: Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 5)
	{
	*ret = TERROR("!presentValue: Too many arguments!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[0], &rate))
	{
	*ret = TERROR("!presentValue: Missing or invalid 'rate' argument!");
	FrameExit(*ret);
	}


if( FFloat_NUMERRCHECK(gCP, gTP, rate) || FFloat_NORMALIZEZERO(gCP, gTP, &rate))
	{
	*ret = TERROR("!presentValue: Missing or invalid 'rate' argument!");
	FrameExit(*ret);
	}
    
if(FFinance_GetArg(gCP, gTP, ret, &argv[1], &nper))
	{
	*ret = TERROR("!presentValue: Missing or invalid 'nperiod' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, nper) || FFloat_NORMALIZEZERO(gCP, gTP, &nper))
	{
	*ret = TERROR("!presentValue: Missing or invalid 'nperiod' argument!");
	FrameExit(*ret);
	}
    
if(FFinance_GetArg(gCP, gTP, ret, &argv[2], &pmt))
	{
	*ret = TERROR("!presentValue: Missing or invalid 'payment' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, pmt) || FFloat_NORMALIZEZERO(gCP, gTP, &pmt))
	{
	*ret = TERROR("!presentValue: Missing or invalid 'payment' argument!");
	FrameExit(*ret);
	}

if (argc > 3)
	{
	if(FFinance_GetArg(gCP, gTP, ret, &argv[3], &fv))
		{
		*ret = TERROR("!presentValue: Invalid 'futureValue' value!");
		FrameExit(*ret);
		}

    if( FFloat_NUMERRCHECK(gCP, gTP, fv) || FFloat_NORMALIZEZERO(gCP, gTP, &fv))
		{
		*ret = TERROR("!presentValue: Invalid 'futureValue' value!");
		FrameExit(*ret);
		}
    }
    
if (argc == 5) 
    {   
    if(FFinance_GetArg(gCP, gTP, ret, &argv[4], &ptype))
		{
		*ret = TERROR("!presentValue: Invalid 'type' value!");
		FrameExit(*ret);
		}

    if( FFloat_NUMERRCHECK(gCP, gTP, ptype) || FFloat_REALTOBINARYVAL(gCP, gTP, &ptype))
		{
		*ret = TERROR("!presentValue: Invalid 'type' value!");
		FrameExit(*ret);
		}
    }

/*  COMPUTE THE PRESENT VALUE FUNCTION   */

if (rate == 0) 
    {
    pv = -(fv + (pmt * nper));
    }
else 
    {
    c = pow(1.0 + rate,nper);
    
    u = pmt * (1.0 + (rate * ptype));
    v = (c - 1.0) / rate;
    
    q = u * v;
    
    pv = -(fv + q) / c;
    }

asTag(argr) = TYREAL;
asReal(argr) = pv;

FrameExit(*argr);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_GetArg

Extract a numeric argument from a tval and return it as a real.

#endif

BOLE FFinance_GetArg(LpXCONTEXT gCP,LpTHREAD gTP, LpTVAL error, LpTVAL arg, LpREAL result)
{
gTP = gTP; // NOOP to hide unused parameter warning message
*error = gCP->TObject_OK;

switch(arg->Tag)
    {
    case TYMONEY:
    case TYREAL:
        *result = asReal(arg);
    break;
    
    case TYNUM:
        *result = asInt(arg);
    break;
    
    case TYBOLE:
        *result = asBool(arg);
    break;
    
    case TYVOID:
        *error = gCP->TObject_ERROR_INVALID_ARGLIST;
        return TRUE;
    break;
    
    default:
        *error = gCP->TObject_ERROR_SYNTAX;
        return TRUE;
    break;
    }
return  FALSE;
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_PPMT

Returns the principal payment for a given period for an investment based on
periodic, constant payments and a constant interest rate.

SYNTAX:     (principalPayment rate,per,nper,pv,{fv},{type})

ARGUMENTS:  rate    :   is the interest rate per period.
            per     :   is the period for which to calculate final interest rate
            nper    :   is the total numbe rof payments period in an annuity
            pmt     :   the payment made each period. It cannot change over
            pv      :   the present value
                        the life of the annuity.
            fv      :   is the future value, or the lump-sum amount that
                        a series of future payments is worth right now.
                        if pv is ommited that default value is zero.
            type    :   is the number 0,1 and indicates when payments are
                        due.  If type is omitted, its default value is 0.
FORMULA:
    
            
EXAMPLE:
    (principalPayment (/ 0.1 12) 1 24 2000) ==> 75.62

#endif

TVAL FFinance_PPMT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL    ptype;
REAL    rate;                       
REAL    nper;   
REAL    per;                
REAL    pmt;                        
REAL    pv;     
REAL    c;
REAL    u;
REAL    v;
REAL    fv;
REAL    q;
REAL    ipmt;
REAL    ppmt;
REAL    p;
LONG    i;
StartFrame
DeclareTVAL(argr);
DeclareTVAL(ret);
EndFrame


/*  INITIALIZE THE VARIABLES EVEN IF IT'S UNNECESSARY */

ptype = 0;
pmt = 0.0;
pv = 0.0;
rate = 0.0;
fv = 0.0;
per =  0.0;
ipmt =  0.0;
ppmt =  0.0;
p = 0.0;

*argr = gCP->Tval_VOID;


/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */


if (argc < 3)
	{
	*ret = TERROR("!principalPayment: Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 5)
	{
	*ret = TERROR("!principalPayment: Too many arguments!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[0], &rate))
	{
	*ret = TERROR("!principalPayment: Missing or invalid 'rate' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, rate) || FFloat_NORMALIZEZERO(gCP, gTP, &rate))
	{
	*ret = TERROR("!principalPayment: Missing or invalid 'rate' argument!");
	FrameExit(*ret);
	}
    
if(FFinance_GetArg(gCP, gTP, ret, &argv[1], &per))
	{
	*ret = TERROR("!principalPayment: Missing or invalid 'period' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, per) || FFloat_NORMALIZEZERO(gCP, gTP, &per))
	{
	*ret = TERROR("!principalPayment: Missing or invalid 'period' argument!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[2], &nper))
	{
	*ret = TERROR("!principalPayment: Missing or invalid 'nperiod' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, nper) || FFloat_NORMALIZEZERO(gCP, gTP, &nper))
	{
	*ret = TERROR("!principalPayment: Missing or invalid 'nperiod' argument!");
	FrameExit(*ret);
	}
    
if(FFinance_GetArg(gCP, gTP, ret, &argv[3], &pv))
	{
	*ret = TERROR("!principalPayment: Missing or invalid 'presentValue' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, pv) || FFloat_NORMALIZEZERO(gCP, gTP, &pv))
	{
	*ret = TERROR("!principalPayment: Missing or invalid 'presentValue' argument!");
	FrameExit(*ret);
	}

if (argc > 4)
	{
	if(FFinance_GetArg(gCP, gTP, ret, &argv[4], &fv))
		{
		*ret = TERROR("!principalPayment: Missing or invalid 'futureValue' argument!");
		FrameExit(*ret);
		}

    if( FFloat_NUMERRCHECK(gCP, gTP, fv) || FFloat_NORMALIZEZERO(gCP, gTP, &fv))
		{
		*ret = TERROR("!principalPayment: Missing or invalid 'futureValue' argument!");
		FrameExit(*ret);
		}
    }
    
if (argc == 6) 
    {   
    if(FFinance_GetArg(gCP, gTP, ret, &argv[5], &ptype))
		{
		*ret = TERROR("!principalPayment: Invalid 'type' value!");
		FrameExit(*ret);
		}

    if( FFloat_NUMERRCHECK(gCP, gTP, ptype))
		{
		*ret = TERROR("!principalPayment: Invalid 'type' value!");
		FrameExit(*ret);
		}

    ptype = (REAL)ceil((double)ptype);
    if( FFloat_ZEROCHECK(gCP, gTP, ptype) || FFloat_NEGCHECK(gCP, gTP, ptype))
		{
		*ret = TERROR("!principalPayment: Invalid 'type' value!");
		FrameExit(*ret);
		}

    }


if (per > nper) 
	{
	*ret = TERROR("!principalPayment: Expecting 'period' value to be less than 'nperiod' value!");
	FrameExit(*ret);
	}

/*  COMPUTE THE PAYMENT APPLIED TOWARDS THE PRINCIPAL OF PV */

if (rate == 0.0) 
    {
    pmt = -(pv + fv) / (REAL)nper;
    }
else 
    {
    c = pow(1.0 + rate,nper);
    u = 1.0 + (rate * ptype);
    v = (c - 1.0) / rate;
    q = u * v;
    pmt = - ((pv * c) + fv) / q;
    }
    
if (ptype == 0) 
    {
    ipmt = -pv * rate;
    ppmt = pmt - ipmt;
    p = pv + ppmt;

    }
else 
    {
    ipmt = 0;
    ppmt = pmt - ipmt;
    p = pv + ppmt;
    }

if (per > 1)
    {
    for (i = 2;i <= per; i++)
        {
        u = -(rate * p);
        ipmt = -(rate * p);
        ppmt = pmt - ipmt;
        p = p + ppmt;
        }
    }

asTag(argr) = TYREAL;
asReal(argr) = ppmt;

FrameExit(*argr);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_PMT

Returns the periodic payment for an annuity based on constant payments and 
a constant interest rate.

SYNTAX:     (payment rate nper pv fv} {type})

ARGUMENTS:  rate    :   is the interest rate per period.
            nper    :   is the total numbe rof payments period in an annuity
            pmt     :   the payment made each period. It cannot change over
                        the life of the annuity.
            fv      :   is the future value, or the lump-sum amount that
                        a series of future payments is worth right now.
                        if pv is ommited that default value is zero.
            type    :   is the number 0,1 and indicates when payments are
                        due.  If type is omitted, its default value is 0.
FORMULA:
    
    [ TBD ]
            
EXAMPLE:
    (payment (/ 0.08 12) 10 10000) ==> -1037.03;
#endif

TVAL FFinance_PMT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL    ptype;
REAL    rate;                       
REAL    nper;                   
REAL    pmt;                        
REAL    pv;     
REAL    c;
REAL    u;
REAL    v;
REAL    fv;
REAL    q;
StartFrame
DeclareTVAL(argr);
DeclareTVAL(ret);
EndFrame

/*  INITIALIZE THE VARIABLES EVEN IF IT'S UNNECESSARY */

ptype = 0;
pmt = 0.0;
pv = 0.0;
rate = 0.0;
fv = 0.0;

*argr = gCP->Tval_VOID;

/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */



if (argc < 3)
	{
	*ret = TERROR("!payment:Not enough arguments!");
	FrameExit(*ret);
	}

/* Check for correct number of arguments */
if (argc > 5)
	{
	*ret = TERROR("!payment:Too many arguments!");
	FrameExit(*ret);
	}

if(FFinance_GetArg(gCP, gTP, ret, &argv[0], &rate))
	{
	*ret = TERROR("!payment:Missing or invalid 'rate' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, rate) || FFloat_NORMALIZEZERO(gCP, gTP, &rate))
	{
	*ret = TERROR("!payment:Missing or invalid 'rate' argument!");
	FrameExit(*ret);
	}
    
if(FFinance_GetArg(gCP, gTP, ret, &argv[1], &nper))
	{
	*ret = TERROR("!payment:Missing or invalid 'nperiod' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, nper) || FFloat_NORMALIZEZERO(gCP, gTP, &nper))
	{
	*ret = TERROR("!payment:Missing or invalid 'nperiod' argument!");
	FrameExit(*ret);
	}
    
if(FFinance_GetArg(gCP, gTP, ret, &argv[2], &pv))
	{
	*ret = TERROR("!payment:Missing or invalid 'presentValue' argument!");
	FrameExit(*ret);
	}

if( FFloat_NUMERRCHECK(gCP, gTP, pmt) || FFloat_NORMALIZEZERO(gCP, gTP, &pmt))
	{
	*ret = TERROR("!payment:Missing or invalid 'presentValue' argument!");
	FrameExit(*ret);
	}


if (argc > 3)
	{
	if(FFinance_GetArg(gCP, gTP, ret, &argv[3], &fv))
		{
		*ret = TERROR("!payment:Invalid 'futureValue' value!");
		FrameExit(*ret);
		}

    if( FFloat_NUMERRCHECK(gCP, gTP, fv) || FFloat_NORMALIZEZERO(gCP, gTP, &fv))
		{
		*ret = TERROR("!payment:Invalid 'futureValue' value!");
		FrameExit(*ret);
		}
    }

if (argc == 5) 
    {       
 	if(FFinance_GetArg(gCP, gTP, ret, &argv[4], &ptype))
		{
		*ret = TERROR("!payment:Invalid 'type' value!");
		FrameExit(*ret);
		}

    if( FFloat_NUMERRCHECK(gCP, gTP, ptype) || FFloat_REALTOBINARYVAL(gCP, gTP, &ptype))
		{
		*ret = TERROR("!payment:Invalid 'type' value!");
		FrameExit(*ret);
		}
    }

/*  COMPUTE THE PAYMENT FUNCTION */

if (rate == 0.0) 
    {
    pmt = -(pv + fv) / (REAL)nper;
    }
else
    {
    c = pow(1.0 + rate,nper);
    u = 1.0 + (rate * ptype);
    v = (c - 1.0) /(REAL) rate;
    q = u * (REAL)v;
    pmt = - ((pv * c) + fv) / (REAL)q;
    }

asTag(argr) = TYREAL;
asReal(argr) = pmt;

FrameExit(*argr);

}


/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_DB

Returns the real depreciation for a specific period fixed declining balance.
Syntax: ( db cost salvage life period {month})

ARGUMENTS:
    
        cost    :   initial cost of the asset
        salvage :   value at the end of the depreciation period
        life    :   number of periods over which the asset is depreciated
        period  :   the period over which you want to calculate the depreciation
        month   :   optional argument indicating the real month. default = 12

ALGORITHM   :
        refer to Excel 4.0, and/or J. Watson`s notes on time-value-of-money
        functions (annuities).

EXAMPLE:
        ( db 1000000 100000 6 1 7)  => $186,083.33

#endif

TVAL FFinance_DB(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{   
REAL            cost;               /*  cost value of the asset         */
REAL            db;                 /*  the declining balance           */
REAL            life;               /*  true life of the asset          */
REAL            month;              /*  month accounted for during calc.*/
REAL            period;             /*  periof for which to calc. dp    */
REAL            rate;               /*  the raw rate value of the asset */
REAL            salvage;            /*  the salvage value of the asset  */
REAL            sum;                /*  tempoary storage used to sum db */
LONG            i;                  /*  internal counter variable       */
LONG            limit;              /*  temporary storate of end period */
StartFrame
DeclareTVAL(argr);
EndFrame

/*  INITIALIZE THE VARIABLES EVEN IF IT'S UNNECESSARY */

*argr = gCP->Tval_VOID;
month = 12.0;
cost = 0.0;
salvage = 0.0;
db = 0.0;

/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */

if ((argc < 4) || (argc > 5)) 
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

switch(argv[0].Tag)
    {
    case TYREAL:
        cost = asReal(&argv[0]);
    break;
    
    case TYNUM:
        cost = asInt(&argv[0]);
    break;
    
    case TYBOLE:
        cost = asBool(&argv[0]);
    break;
    
    case TYVOID:
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    break;
    
    default:
        FrameExit(gCP->TObject_ERROR_SYNTAX);
    break;
    }
    
if( FFloat_NEGCHECK(gCP, gTP, cost) ||  FFloat_NORMALIZEZERO(gCP, gTP, &cost))
    goto BadCleanUp;

switch(argv[1].Tag)
    {
    case TYREAL:
        salvage = asReal(&argv[1]);
    break;
    
    case TYNUM:
        salvage = asInt(&argv[1]);
    break;
    
    case TYBOLE:
        salvage = asBool(&argv[1]);
    break;
    
    case TYVOID:
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    break;
    
    default:
        FrameExit(gCP->TObject_ERROR_SYNTAX);
    break;
    }
    
if( FFloat_NEGCHECK(gCP, gTP, salvage) ||  FFloat_NORMALIZEZERO(gCP, gTP, &salvage))
    goto BadCleanUp;

switch(argv[2].Tag)
    {
    case TYREAL:
        life = asReal(&argv[2]);
    break;
    
    case TYNUM:
        life = asInt(&argv[2]);
    break;
    
    case TYBOLE:
        life = asBool(&argv[2]);
    break;
    
    case TYVOID:
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    break;
    
    default:
        FrameExit(gCP->TObject_ERROR_SYNTAX);
    break;
    }
if( FFloat_NEGCHECK(gCP, gTP, life) ||  FFloat_ZEROCHECK(gCP, gTP, life))
    goto BadCleanUp;

switch(argv[3].Tag)
    {
    case TYREAL: 
        period = asReal(&argv[3]); 
    break;
    
    case TYNUM:
        period = asInt(&argv[3]);
    break;
    
    case TYBOLE:
        period = asBool(&argv[3]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
    
if( FFloat_NUMERRCHECK(gCP, gTP, period))
    goto BadCleanUp;

period =(REAL) ceil((double)period);

if(FFloat_NEGCHECK(gCP, gTP, period) || FFloat_ZEROCHECK(gCP, gTP, period))
    goto BadCleanUp;
    
if (argc == 5) 
    {
    switch(argv[4].Tag)
        {
        case TYREAL: 
            month = asReal(&argv[4]); 
        break;
        
        case TYNUM:
            month = asInt(&argv[4]);
        break;
        
        case TYBOLE:
            month = asBool(&argv[4]);
        break;
        
        case TYVOID: 
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
        break;
        
        default: 
            FrameExit(gCP->TObject_ERROR_SYNTAX); 
        break;
        }
    if(FFloat_NUMERRCHECK(gCP, gTP, month))
        goto BadCleanUp;
    
    month = (REAL)floor((double)month);
    
    if(FFloat_NEGCHECK(gCP, gTP, month) || FFloat_ZEROCHECK(gCP, gTP, month))
        goto BadCleanUp;
    }
    
/*  ERROR CHECK THE SPECIFIED ARGUMENTS FROM ARGV */

if (period > (life + 1))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
    
if ((month > 12) || ((12 == month) && (period > life)))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
    
FFloat_NORMALIZEZERO(gCP,  gTP, &cost);

if (cost == 0.0)
    {
    db = 0.0;
    }
else
    {
    rate = 1.0 - pow(salvage / cost,1.0 / life);
    
    /*  ROUND THE VALUE OF RATE TO THREE DECIMAL PLACES... */
        
    rate *= 1.00E+03;
    rate += 0.5;
    rate = floor(rate);
    rate /= 1.00E+03;
    
    /*  COMPUTE THE DEPRECIATION BALANCE FUCTION */
    
    db = (cost * rate) * (month / 12.0);
    sum = db;
    limit = ((period <= life) ? (period) : (period - 1));
    for (i = 2;i <= limit;i++) 
        { 
        db = (cost - sum) * rate;
        sum += db;
        }
    
    if ((month != 12) && (period == (life + 1)))
        {
        db = (cost - sum) * rate * (12.0 - month) / 12.0;
        }
    
    }

asTag(argr) = TYREAL;
asReal(argr) = db;

FrameExit(*argr);

BadCleanUp:

FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_DDB

FFinance_DDB returns the depreciation of an asset for a specific period using
the double-declining method or some other method user specifed via the
argv list of arguments. It uses the double-declinging method which
computes the depreciation at an accelerated rate.  NOTE: depreciation
should be highest at the initial period, and decreases in successive
periods.
  
SYNTAX: (ddb cost salvage life period {factor})

ARGUMENT:
    cost    :   cost is the initial value of the asset
    salvage :   the value at the end of the depreciation
    life    :   the number of periods over which the asset is being
                depreciated
    period  :   the periof for which user what to calculate the depre-
                ciation. NOTE: period is assume to be same unit as life
    factor  :   the ratre at which the balance declines.  If factor is
                omitted then calculation method is assumed to be 2.0
                which is the double-declining balance)
FORMULA:
    DDB(_) = (cost - [total depreciation from perior periods] * factor) / life

EXAMPLE:
    (ddb 2400 300 6 1 6) ==> $2,100.00
#endif

TVAL FFinance_DDB(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL        cost;               /*  cost value of the asset         */
REAL        ddb;                /*  double-declining balance value  */
REAL        factor;             /*  method to apply on ddb function */
REAL        life;               /*  true life of the asset          */
REAL        period;             /*  periof for which to calc. dp    */
REAL        ratio;              /*  temporary storage as ratio      */
REAL        salvage;            /*  the salvage value of the asset  */
StartFrame
DeclareTVAL(argr);
EndFrame

/*  INITIALIZE THE VARIABLES EVEN IF IT'S UNNECESSARY */

*argr = gCP->Tval_VOID;
factor = 2.0;
cost = 0.0;
salvage = 0.0;
period = 0.0;
life = 0.0;
ddb = 0.0;


/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */

if ((argc < 4) || (argc > 5)) 
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

switch(argv[0].Tag)
    {
    case TYREAL: 
        cost = asReal(&argv[0]); 
    break;
    
    case TYNUM:
        cost = asInt(&argv[0]);
    break;
    
    case TYBOLE:
        cost = asBool(&argv[0]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NANCHECK(gCP,gTP,cost) ||  FFloat_INFCHECK(gCP,gTP,cost) ||  FFloat_NEGCHECK(gCP, gTP, cost))
    goto BadCleanUp;
if(FFloat_ZEROCHECK(gCP, gTP, cost))
    cost = 0.0;

switch(argv[1].Tag)
    {
    case TYREAL: 
        salvage = asReal(&argv[1]); 
    break;
    
    case TYNUM:
        salvage = asInt(&argv[1]);
    break;
    
    case TYBOLE:
        salvage = asBool(&argv[1]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NANCHECK(gCP,gTP,salvage) ||  FFloat_INFCHECK(gCP,gTP,salvage) ||  FFloat_NEGCHECK(gCP, gTP, salvage))
    goto BadCleanUp;
    
if(FFloat_ZEROCHECK(gCP, gTP, salvage))
    salvage = 0.0;
    
switch(argv[2].Tag)
    {
    case TYREAL: 
        life = asReal(&argv[2]); 
    break;
    
    case TYNUM:
        life = asInt(&argv[2]);
    break;
    
    case TYBOLE:
        life = asBool(&argv[2]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
    
if( FFloat_NANCHECK(gCP,gTP,life) ||  FFloat_INFCHECK(gCP,gTP,life) ||  FFloat_NEGCHECK(gCP, gTP, life)
    ||  FFloat_ZEROCHECK(gCP, gTP, life))
    goto BadCleanUp;
    
switch(argv[3].Tag)
    {
    case TYREAL: 
        period = asReal(&argv[3]); 
    break;
    
    case TYNUM:
        period = asInt(&argv[3]);
    break;
    
    case TYBOLE:
        period = asBool(&argv[3]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
    
if( FFloat_NANCHECK(gCP,gTP,period) ||  FFloat_INFCHECK(gCP,gTP,period))
    goto BadCleanUp;
    
period = ceil(period);

if( FFloat_NEGCHECK(gCP, gTP, period) ||  FFloat_ZEROCHECK(gCP, gTP, period))
    goto BadCleanUp;

if (argc == 5) 
    {
    switch(argv[4].Tag)
        {
        case TYREAL: 
            factor = asReal(&argv[4]); 
        break;
        
        case TYNUM:
            factor = asInt(&argv[4]);
        break;
        
        case TYBOLE:
            factor = asBool(&argv[4]);
        break;
        
        case TYVOID: 
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
        break;
        
        default: 
            FrameExit(gCP->TObject_ERROR_SYNTAX); 
        break;
        }
        
    if( FFloat_NANCHECK(gCP,gTP,factor) ||  FFloat_INFCHECK(gCP,gTP,factor) ||  FFloat_NEGCHECK(gCP, gTP, factor)
        ||  FFloat_ZEROCHECK(gCP, gTP, factor))
        goto BadCleanUp;
    }

/*  ERROR CHECK THE SPECIFIED ARGUMENTS FROM ARGV */
    
if (period > life)
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

FFloat_NORMALIZEZERO(gCP,  gTP, &cost);
if (cost == 0.0)
    {
    ddb = 0.0;
    }
else
    {
    /*  COMPUTE THE DEPRECIATION DOUBLE-DECLINING BALANCE FUCTION  */

    ratio = (REAL)factor / life;
    if ( ratio >= 1.0) 
        {
        if (period <= 1.0)
            {
            ddb = cost - salvage;
            }
        else 
            {
            ddb = 0.0;
            }
        }
    else
        {   
        REAL    sum;
        
        sum = cost * (1.0 - pow(1.0 + -ratio, (REAL)period - 1.0));
/*      sum = cost * (1.0 - compound(-ratio, (REAL)period - 1.0)); */
        if (sum > (cost - salvage))
            {
            ddb = 0.0;
            }
        else
            {
            ddb = ratio * (cost - sum);
            if ((sum + ddb) >= (cost - salvage))
                {
                ddb = cost - salvage - sum;
                }
            }
        }
        
}

asTag(argr) = TYREAL;
asReal(argr) = ddb;
FrameExit(*argr);

BadCleanUp:

FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_FV

The FFinance_FV returns the future value of an investment based on periodic,
constant payments and a constant interest rate.

SYNTAX:     ( fv rate nper pmt {pv} {type})

ARGUMENTS:  rate    :   is the interest rate per period.
            nper    :   is the total numbe rof payments period in an annuity
            pmt     :   the payment made each period. It cannot change over
                        the life of the annuity.
            pv      :   is the present value, or the lump-sum amount that
                        a series of future payments is worth right now.
                        if pv is ommited that default value is zero.
            type    :   is the number 0,1 and indicates when payments are
                        due.  If type is omitted, its default value is 0.

FORMULA:
    
    CASE # 1:   if rate != 0
    ------------------------
    FV(rate,nper,pmt,pv,type):= -[ (pv * c) + (pmt * u) * v ]


    CASE # 2:   if rate == 0
    ------------------------
    
    FV(rate,nper,pmt,pv,type):= -[ pv + (pmt * nper) ]

EXAMPLE:
    (fv 0.5 10 -200 -500 1) ==> 62831.543

#endif

TVAL FFinance_FV(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL        ptype;              /*  the type of method of payment   */
REAL        rate;               /*  the interest rate               */
REAL        nper;               /*  number of period                */  
REAL        pmt;                /*  payment on the principal        */
REAL        pv;                 /*  present value or principal      */
REAL        c;                  /*  temporary storage for calc.     */
REAL        u;                  /*  temporary storage for calc.     */
REAL        v;                  /*  temporary storage for calc.     */
REAL        fv;                 /*  the future value                */
StartFrame
DeclareTVAL(argr);
EndFrame


/*  INITIALIZE THE VARIABLES */

ptype = 0.0;
pmt = 0.0;
pv = 0.0;
rate = 0.0;
fv = 0.0;
nper = 0.0;

*argr = gCP->Tval_VOID;

/*  getting the arguments, TYVOID get contributed to computation as zeros. */

if ((argc < 3) || (argc > 5)) 
{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

switch(argv[0].Tag)
    {
    case TYREAL: 
        rate = asReal(&argv[0]); 
    break;
    
    case TYNUM:
        rate = asInt(&argv[0]);
    break;
    
    case TYBOLE:
        rate = asBool(&argv[0]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NUMERRCHECK(gCP, gTP, rate))
    goto BadCleanUp;

switch(argv[1].Tag)
    {
    case TYREAL: 
        nper = asReal(&argv[1]); 
    break;
    
    case TYNUM:
        nper = asInt(&argv[1]);
    break;
    
    case TYBOLE:
        nper = asBool(&argv[1]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NUMERRCHECK(gCP, gTP, nper))
    goto BadCleanUp;

switch(argv[2].Tag)
    {
    case TYREAL: 
        pmt = asReal(&argv[2]); 
    break;
    
    case TYNUM:
        pmt = asInt(&argv[2]);
    break;
    
    case TYBOLE:
        pmt = asBool(&argv[2]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NUMERRCHECK(gCP, gTP, pmt))
    goto BadCleanUp;

if (argc > 3)
    {
    switch(argv[3].Tag)
        {
        case TYREAL: 
            pv = asReal(&argv[3]); 
        break;
        
        case TYNUM:
            pv = asInt(&argv[3]);
        break;
        
        case TYBOLE:
            pv = asBool(&argv[3]);
        break;
        
        case TYVOID: 
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
        break;
        
        default: 
            FrameExit(gCP->TObject_ERROR_SYNTAX); 
        break;
        }
    if( FFloat_NUMERRCHECK(gCP, gTP, pv))
        goto BadCleanUp;
    }

if (argc == 5)
    {
    switch(argv[4].Tag)
        {
        case TYREAL: 
            ptype = asReal(&argv[4]); 
        break;
        
        case TYNUM:
            ptype = asInt(&argv[4]);
        break;
        
        case TYBOLE:
            ptype = asBool(&argv[4]);
        break;
        
        case TYVOID: 
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
        break;
        
        default: 
            FrameExit(gCP->TObject_ERROR_SYNTAX); 
        break;
        }
    if( FFloat_NUMERRCHECK(gCP, gTP, ptype))
        goto BadCleanUp;
    if( FFloat_REALTOBINARYVAL(gCP, gTP, &ptype))
        goto BadCleanUp;    
    }


/*  COMPUTE THE FUTURE VALUE FUNCTION */
    
FFloat_NORMALIZEZERO(gCP,  gTP, &rate);
if(rate == 0.0)
    {
    fv = -(pv + (pmt * nper));
    }
else 
    {
    c  = (REAL)pow(1.0 + rate,nper);
    u = pmt * (1.0 + ((REAL)rate * ptype));
    v = (c - 1.0) / (REAL)rate;
    fv = -(((REAL)pv * c) + ((REAL)u * v));
    }

asTag(argr) = TYREAL;
asReal(argr) = fv;

FrameExit(*argr);

BadCleanUp:

FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FFinance_IPMT

Returns the interest payment for a given period for an investment based on
periodic, constant payments and a constant interest rate.

SYNTAX:     ( ipmt rate per nper pv {fv} {type})

ARGUMENTS:  rate    :   is the interest rate per period.
            per     :   is the period for which to calculate final interest rate
            nper    :   is the total numbe rof payments period in an annuity
            pmt     :   the payment made each period. It cannot change over
            pv      :   the present value
                        the life of the annuity.
            fv      :   is the future value, or the lump-sum amount that
                        a series of future payments is worth right now.
                        if pv is ommited that default value is zero.
            type    :   is the number 0,1 and indicates when payments are
                        due.  If type is omitted, its default value is 0.

FORMULA:
    CASE # 1:   if rate != 0
    ------------------------
    IPMT(rate,nper,pmt,{fv},{type}):= - pmt - ppmt;

    CASE # 2:   if rate == 0
    ------------------------
    
    IPMT(rate,per, nper,pmt,{fv},{type}):= -[ pv + (pmt * nper) ]

ALOGRITHM:
    You must first find the initial principal balance or payment paid towards
    the balance for each period that you are iterating to compute the payment
    applied towards the interest.

EXAMPLE:
    (ipmt (/ 0.1 12) 1 36 8000) ==> -66.67
#endif

TVAL FFinance_IPMT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
LONG        i;                  /*  temporary storage for iteration */
REAL        ptype;              /*  the type of method of payment   */
REAL        rate;               /*  the interest rate               */
REAL        nper;               /*  number of period                */  
REAL        per;                /*  the actual period of calculation*/
REAL        pmt;                /*  payment on the principal        */
REAL        pv;                 /*  present value or principal      */
REAL        c;                  /*  temporary storage for calc.     */
REAL        u;                  /*  temporary storage for calc.     */
REAL        v;                  /*  temporary storage for calc.     */
REAL        fv;                 /*  the future value                */
REAL        q;                  /*  temporary storage for calc.     */
REAL        ipmt;               /*  the payment towards interest    */
REAL        ppmt;               /*  the payment towards principal   */
REAL        p;                  /*  temporary storage for calc.     */
StartFrame
DeclareTVAL(argr);
EndFrame

/*  INITIALIZE THE VARIABLES */

ptype = 0;
pmt = 0.0;
pv = 0.0;
rate = 0.0;
fv = 0.0;
per =  0.0;
ipmt =  0.0;
ppmt =  0.0;
p = 0.0;

*argr = gCP->Tval_VOID;

/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */

if ((argc < 4) || (argc > 6)) 
{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

switch(argv[0].Tag)
    {
    case TYREAL: 
        rate = asReal(&argv[0]); 
    break;
    
    case TYNUM:
        rate = asInt(&argv[0]);
    break;
    
    case TYBOLE:
        rate = asBool(&argv[0]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NUMERRCHECK(gCP, gTP, rate))
    goto BadCleanUp;


switch(argv[1].Tag)
    {
    case TYREAL: 
        per = asReal(&argv[1]); 
    break;
    
    case TYNUM:
        per = asInt(&argv[1]);
    break;
    
    case TYBOLE:
        per = asBool(&argv[1]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NEGCHECK(gCP, gTP, per) || FFloat_ZEROCHECK(gCP, gTP, per) )
    goto BadCleanUp;
    
if (per < 1.0) 
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

switch(argv[2].Tag)
    {
    case TYREAL: 
        nper = asReal(&argv[2]); 
    break;
    
    case TYNUM:
        nper = asInt(&argv[2]);
    break;
    
    case TYBOLE:
        nper = asBool(&argv[2]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NEGCHECK(gCP, gTP, nper) || FFloat_ZEROCHECK(gCP, gTP, nper) )
    goto BadCleanUp;
    
switch(argv[3].Tag)
    {
    case TYREAL: 
        pv = asReal(&argv[3]); 
    break;
    
    case TYNUM:
        pv = asInt(&argv[3]);
    break;
    
    case TYBOLE:
        pv = asBool(&argv[3]);
    break;
    
    case TYVOID: 
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
    break;
    
    default: 
        FrameExit(gCP->TObject_ERROR_SYNTAX); 
    break;
    }
if( FFloat_NUMERRCHECK(gCP, gTP, pv))
    goto BadCleanUp;

if (argc > 4)   /*  GET OPTIONAL ARGUMENT IF SPECIFIED */
    {                       
    switch(argv[4].Tag)
        {
        case TYREAL: 
            fv = asReal(&argv[4]); 
        break;
        
        case TYNUM:
            fv = asInt(&argv[4]);
        break;
        
        case TYBOLE:
            fv = asBool(&argv[4]);
        break;
        
        case TYVOID: 
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
        break;
        
        default: 
            FrameExit(gCP->TObject_ERROR_SYNTAX); 
        break;
        }
    if( FFloat_NUMERRCHECK(gCP, gTP, fv))
        goto BadCleanUp;
    }

if (argc == 6)  /*  GET OPTIONAL ARGUMENT IF SPECIFIED */
    {                       
    switch(argv[5].Tag)
        {
        case TYREAL: 
            ptype = asReal(&argv[5]); 
        break;
        
        case TYNUM:
            ptype = asInt(&argv[5]);
        break;
        
        case TYBOLE:
            ptype = asBool(&argv[5]);
        break;
        
        case TYVOID: 
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST); 
        break;
        
        default: 
            FrameExit(gCP->TObject_ERROR_SYNTAX); 
        break;
        }
    if( FFloat_NUMERRCHECK(gCP, gTP, ptype))
        goto BadCleanUp;
    if( FFloat_REALTOBINARYVAL(gCP, gTP, &ptype))
        goto BadCleanUp;    
    }

if (per > nper) 
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  COMPUTE THE INTEREST PAYMENT AT THE SPECIFIED PERIOD */

if (rate == 0.0) 
    {
    pmt = -(pv + fv) / nper;
    }
else 
    {
    c = pow(1.0 + rate,nper);
    u = 1.0 + ((REAL)rate * ptype);
    v = (c - 1.0) / (REAL)rate;
    q = u * v;
    pmt = - ((pv * c) + fv) / (REAL)q;
    }
    
if (ptype == 0) 
    {
    ipmt = -pv * rate;
    ppmt = pmt - ipmt;
    p = pv + ppmt;
    }
else 
    {
    ipmt = 0;
    ppmt = pmt - ipmt;
    p = pv + ppmt;
    }
    
if (per > 1.0) 
    {
    for (i = 2;i <= per; i++) 
        {
        u = -(rate * p);
        ipmt = -(rate * p);
        ppmt = pmt - ipmt;
        p += ppmt;
        }
    }
    
asTag(argr) = TYREAL;
asReal(argr) = ipmt;

FrameExit(*argr);

BadCleanUp:

FrameExit(gCP->TObject_ERROR_INVALID);
}
