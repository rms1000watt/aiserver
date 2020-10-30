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

#define _C_FDATEFNC
#define _SMARTBASE

#if 0
FDateFnc.c

This source file contains some of the cProcedures which implement the
date time functions supported by the SmartBase server.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fdatefnc.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_Init

Initialize the Logical portion of the SmartLisp function library.  

#endif

TVAL FDateFnc_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,tmpSymbol);
EndFrame
 
if(gCP->FDatefnc_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FDatefnc_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"date",(LpFUNC)&FDateFnc_date);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"julian",(LpFUNC)&FDateFnc_datevalue);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"day",(LpFUNC)&FDateFnc_day);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"month",(LpFUNC)&FDateFnc_month);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"year",(LpFUNC)&FDateFnc_year);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"now",(LpFUNC)&FDateFnc_now);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"today",(LpFUNC)&FDateFnc_today);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"days360",(LpFUNC)&FDateFnc_days360);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"hour",(LpFUNC)&FDateFnc_hour);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"minute",(LpFUNC)&FDateFnc_minute);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"second",(LpFUNC)&FDateFnc_second);
ExitOnError(*ret);

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"time",(LpFUNC)&FDateFnc_time);
ExitOnError(*ret);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_timeconstant

Attempt to parse Text containing any of the following formats and place 
the result in a TYDATE.

hh:mm AMPM
hh:mm:ss AMPM
  
Note : We assume the date is always Jan-1-1AD

#endif

TVAL FDateFnc_timeconstant(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         cn;
NUM         day;
NUM         month;
NUM         year;
NUM         hour;
NUM         minute;
NUM         second;
CHAR        date[384];
CHAR        tokenize[385];
CHAR        tokens[4][64];
LpCHAR      tokenP;
StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,6);

EndFrame

if(argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[0]))
    {
    case TYTEXT:
        strncpy((char*)date, (char*)asText(&argv[0]), (NUM)(sizeof(date) - 1));
    break;

    case TYSTRING:
        strncpy((char*)date, (char*)&atHMChar(asString(ret)->itsCString,0), (NUM)(sizeof(date) - 1));
    break;
    
    case TYSTRINGSUBSTR:
        tokenP = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        strncpy((char*)date, (char*)tokenP, min(SubLen(argv[0]), (NUM)(sizeof(date) - 1)));
        date[min(SubLen(argv[0]), (NUM)(sizeof(date) - 1))] = 0;
    break;
    
    default:
		*ret = FObject_DateAnyCnv(gCP,gTP,TYDATE, argv[0]);
        FrameExit(*ret);
    break;
    }

/*  Terminate the candidate text, and tokenize it. */

date[sizeof(date) - 1] = 0;
strcpy((char*)tokenize, (char*)date);

for(tokenP = (LpCHAR)strtok((char*)tokenize, " :"), cn = 0; cn < 4 && tokenP; cn++, tokenP = (LpCHAR)strtok((char*)NULL, " :"))
    {
    if(strlen((char*)tokenP) < sizeof(tokens[cn]) - 1)
        {
        strcpy((char*)tokens[cn], (char*)tokenP);
        }
    else
        goto BadCleanUp;
    }
    
/*  Init month day and year to special values. */

month = day = year = 1;

hour =  -1;
minute =  -1;
second = 0;

switch(cn)
    {
    case 2:
        /*  Handle hh:mm */
        
        *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
        ExitOnError(*ret);
        hour = asReal(ret);
        
        *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
        ExitOnError(*ret);
        minute = asReal(ret);
    break;
    
    case 3:
        /*  Handle hh:mm AMPM and hh:mm:ss */
        
        if(ISDIGIT((NUM)tokens[0][0]) && ISDIGIT((NUM)tokens[1][0]))
            {
            if(ISDIGIT((NUM)tokens[2][0]))
                {
                /*  Handle hh:mm:ss */
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
                ExitOnError(*ret);
                hour = asReal(ret);
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
                ExitOnError(*ret);
                minute = asReal(ret);
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[2]);
                ExitOnError(*ret);
                second = asReal(ret);
                }
            else
            if(tokens[2][1] == 'M')
                {
                /*   hh:mm AMPM */
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
                ExitOnError(*ret);
                hour = asReal(ret);
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
                ExitOnError(*ret);
                minute = asReal(ret);
                
                if(tokens[2][0] == 'P' )
                    {
                    if(hour < 12 && hour >= 0)
                        hour += 12;
                    else
                    if(hour != 12 )
                        goto BadCleanUp;
                    }
                else
                if(tokens[2][0] != 'A' )
                    goto BadCleanUp;
                }
            else
                goto BadCleanUp;
            }
        else
            goto BadCleanUp;
    break;
    
    case 4:
        if(ISDIGIT((NUM)tokens[0][0]) && ISDIGIT((NUM)tokens[1][0]) && ISDIGIT((NUM)tokens[2][0]))
            {
            if(tokens[3][1] == 'M')
                {
                /*   hh:mm:ss AMPM */
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
                ExitOnError(*ret);
                hour = asReal(ret);
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
                ExitOnError(*ret);
                minute = asReal(ret);
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[2]);
                ExitOnError(*ret);
                second = asReal(ret);
                
                if(tokens[3][0] == 'P' )
                    {
                    if(hour < 12 && hour >= 0)
                        hour += 12;
                    else
                    if(hour != 12 )
                        goto BadCleanUp;
                    }
                else
                if(tokens[3][0] != 'A' )
                    goto BadCleanUp;
                }
            else
                goto BadCleanUp;
            }
        else
            goto BadCleanUp;
    break;
    
    default:
        goto BadCleanUp;
    }
    
if(minute < 0 || minute > 59 || hour < 0 || hour > 23 || second < 0 || second > 59)
    goto BadCleanUp;
    
prmv[0].Tag = TYNUM;
prmv[0].u.Int   = year;

prmv[1].Tag = TYNUM;
prmv[1].u.Int   = month;

prmv[2].Tag = TYNUM;
prmv[2].u.Int   = day;

prmv[3].Tag = TYNUM;
prmv[3].u.Int   = hour;

prmv[4].Tag = TYNUM;
prmv[4].u.Int   = minute;

prmv[5].Tag = TYNUM;
prmv[5].u.Int   = second;

*ret = FDateFnc_date(gCP,gTP,6, &prmv[0]);
FrameExit(*ret);

BadCleanUp:
    
FrameExit(gCP->TObject_ERROR_SYNTAX);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_dateconstant

Attempt to parse Text containing any of the following formats and place
the result in a TYDATE.

d-mmm-yy
mm-d-yy
m/d/y
mmm dd, yy

mmm dd
d-mmm
m/d
m/y
mm-d
  
Note : If no year is specified, assume current year.

#endif

TVAL FDateFnc_dateconstant(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         cn;
NUM         day;
NUM         month;
NUM         year;
CHAR        date[384];
CHAR        tokenize[385];
CHAR        tokens[3][128];
LpCHAR      tokenP;
time_t      now;
struct tm   *datestructP;
StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);

EndFrame


if(argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[0]))
    {
    case TYTEXT:
        strncpy((char*)date, (char*)asText(&argv[0]), sizeof(date) - 1);
    break;

    case TYSTRING:
        strncpy((char*)date, (char*)&atHMChar(asString(ret)->itsCString,0), sizeof(date) - 1);
    break;
    
    case TYSTRINGSUBSTR:
        tokenP = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        strncpy((char*)date, (char*)tokenP, min(SubLen(argv[0]), (NUM)(sizeof(date) - 1)));
        date[min(SubLen(argv[0]), (NUM)(sizeof(date) - 1))] = 0;
    break;
    
    default:
		*ret = FObject_DateAnyCnv(gCP,gTP,TYDATE, argv[0]);
        FrameExit(*ret);
    break;
    }

/*  Terminate the candidate text, and tokenize it. */

date[sizeof(date) - 1] = 0;
strcpy((char*)tokenize, (char*)date);

for(tokenP = (LpCHAR)strtok((char*)tokenize, " -/"), cn = 0; cn < 3 && tokenP; cn++, tokenP = (LpCHAR)strtok((char*)NULL, " -/"))
    {
    if(strlen((char*)tokenP) < sizeof(tokens[cn]) - 1)
        {
        strcpy((char*)tokens[cn], (char*)tokenP);
        }
    else
        goto BadCleanUp;
    }
    
/*  Init month day and year to special values. */

month = day = -1;
now = time( NULL);
datestructP = localtime(&now);
year = _FDateFnc_StartingYear + datestructP->tm_year;

switch(cn)
    {
    case 2:
        if(date[3] == ' ')
            {
            /*  Handle mmm dd */
            
            *ret = FDateFnc_monthNumber(gCP, gTP, tokens[0], &month);
            ExitOnError(*ret);
            
            *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
            ExitOnError(*ret);
            day = asReal(ret);
            }
        else
        if(date[3] == '-')
            {
            /*  Handle mmm-yy */
            
            *ret = FDateFnc_monthNumber(gCP, gTP, tokens[0], &month);
            ExitOnError(*ret);
            
            *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
            ExitOnError(*ret);
            year = asReal(ret);
            day = 1;
            }
        else
        if(!ISDIGIT((NUM)tokens[1][0]))
            {
            /*  Handle d-mmm */
            
            *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
            ExitOnError(*ret);
            day = asReal(ret);

            *ret = FDateFnc_monthNumber(gCP, gTP, tokens[1], &month);
            ExitOnError(*ret);
            }
        else
        if(ISDIGIT((NUM)tokens[0][0]) && ISDIGIT((NUM)tokens[1][0]))
            {
            if(strchr((char*)date, '-') != NULL)
                {
                /*  Handle mm-d */
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
                ExitOnError(*ret);
                month = asReal(ret);
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
                ExitOnError(*ret);
                day = asReal(ret);
                }
            else
            if(strchr((char*)date, '/'))
                {
                /*  Handle m/d m/y */
                
                *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
                ExitOnError(*ret);
                month = asReal(ret);

                *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
                ExitOnError(*ret);
                day = asReal(ret);
                
                if(day < 1 || day > 31)
                    {
                    /*  Assume that it is a year, this is ambiguous!!! */
                    
                    year = day;
                    day = 1;
                    }
                }
            else
                goto BadCleanUp;
                
            }
        else
            goto BadCleanUp;
    break;
    
    case 3:
        
        if(!ISDIGIT((NUM)tokens[1][0]))
            {
            /*  Handle d-mmm-yy */
            
            *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
            ExitOnError(*ret);
            day = asReal(ret);
            
            *ret = FDateFnc_monthNumber(gCP, gTP, tokens[1], &month);
            ExitOnError(*ret);
            
            *ret = FDateFnc_recNumber(gCP, gTP, tokens[2]);
            ExitOnError(*ret);
            year = asReal(ret);
            }
        else
        if(!ISDIGIT((NUM)tokens[0][0]))
            {
            /*  Handle mmm dd, yy */
            
            *ret = FDateFnc_monthNumber(gCP, gTP, tokens[0], &month);
            ExitOnError(*ret);
            
            *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
            ExitOnError(*ret);
            day = asReal(ret);

            *ret = FDateFnc_recNumber(gCP, gTP, tokens[2]);
            ExitOnError(*ret);
            year = asReal(ret);
            }
        else
            {
            /*  Handle mm-d-yy m/d/y */
            
            *ret = FDateFnc_recNumber(gCP, gTP, tokens[0]);
            ExitOnError(*ret);
            month = asReal(ret);

            *ret = FDateFnc_recNumber(gCP, gTP, tokens[1]);
            ExitOnError(*ret);
            day = asReal(ret);
            
            *ret = FDateFnc_recNumber(gCP, gTP, tokens[2]);
            ExitOnError(*ret);
            year = asReal(ret);
            }
    break;
    
    default:
        goto BadCleanUp;
    }
    
if(day < 1 || day > 31 || month < 1 || month > 12)
    goto BadCleanUp;
    
prmv[0].Tag = TYNUM;
prmv[0].u.Int   = year;

prmv[1].Tag = TYNUM;
prmv[1].u.Int   = month;

prmv[2].Tag = TYNUM;
prmv[2].u.Int   = day;

*ret = FDateFnc_date(gCP,gTP,3, (TVAL *)prmv);
FrameExit(*ret);

BadCleanUp:
    
FrameExit(gCP->TObject_ERROR_SYNTAX);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_recNumber

See if this text points to a number, its an error if it does not.
  
#endif

TVAL FDateFnc_recNumber(LpXCONTEXT gCP,LpTHREAD gTP, LpCHAR  textP)
{
NUM     tmpInt;
REAL    tmpReal;
StartFrame
DeclareTVAL(ret);
EndFrame

*ret = FProcedure_recInt(gCP, gTP, &tmpReal, &tmpInt, textP);
ExitOnError(*ret);

if(tmpInt != 0)
    {
    asTag(ret) = TYREAL;
    asReal(ret) = tmpReal;
        
    FrameExit(*ret);
    }
else
    FrameExit(gCP->TObject_ERROR_SYNTAX);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_time

Return the serial time given the hour, minutes, and seconds

(time 12 0 0) ==> 0.5
  
#endif

TVAL FDateFnc_time(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL    hour;
REAL    minute;
REAL    second;
StartFrame
DeclareTVAL(ret);
EndFrame

if (argc != 3) 
	{
	*ret = FDateFnc_timevalue(gCP,gTP,argc,argv);
    FrameExit(*ret);
	}

/*  Set VOID to zero for this function. */

if (asTag(&argv[0]) == TYVOID) 
    { 
    hour = 0;
    }
else
    {
    *ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[0]);
    ExitOnError(*ret);
    hour = asInt(ret);
    }
    
if (asTag(&argv[1]) == TYVOID) 
    { 
    minute = 0;
    }
else
    {
    *ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
    ExitOnError(*ret);
    minute = asInt(ret);
    }
    
if (asTag(&argv[2]) == TYVOID) 
    { 
    second = 0;
    }
else
    {
    *ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[2]);
    ExitOnError(*ret);
    second = asInt(ret);
    }

if (hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 && second >= 0 && second <= 59)
    {
    /*  Format the time as a real fraction. */
    
    asTag(ret) = TYREAL;
    asReal(ret) = ((hour * 3600) + (minute * 60) + second)/ 86400.0;
    }
else
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;

FrameExit (*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_timevalue

Return the julian time given the text time.

(timevalue "12:00:00") ==> 0.5
  
#endif

TVAL FDateFnc_timevalue(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM     tmpNdx;
REAL    julian;
LpCHAR  temp;
CHAR    tmpChar;

StartFrame
DeclareTVAL(ret);
EndFrame

if(argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[0]))
    {
    case TYREAL:
    case TYDATE:
        *ret = argv[0];
    break;

    case TYTEXT:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if(isERROR(ret))
            {
            tmpNdx = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, asText(&argv[0]), &tmpNdx);
            }
    break;

    case TYSTRING:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if(isERROR(ret))
            {
            tmpNdx = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, &atHMChar(asString(ret)->itsCString,0), &tmpNdx);
            }
    break;

    case TYSTRINGSUBSTR:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if (isERROR(ret))
            {
            tmpNdx = 0;
            temp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
            tmpChar = temp[SubLen(argv[0])];
            temp[SubLen(argv[0])] = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, temp, &tmpNdx);
            temp[SubLen(argv[0])] = tmpChar;
            }
    break;
    
    default:
        *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    break;
    }
    
ExitOnError(*ret);
    
julian = fabs(ret->u.Real);
ret->u.Real = julian - floor(julian);
ret->Tag = TYREAL;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_hour

Return the hour given the serial date.

(hour "1:30:45")    ==> 1
(hour .5)           ==> 12
  
#endif

TVAL FDateFnc_hour(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM     tmpNdx;
REAL    julian;
LpCHAR  temp;
CHAR    tmpChar;

StartFrame
DeclareTVAL(ret);
EndFrame

if(argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[0]))
    {
    case TYTEXT:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if(isERROR(ret))
            {
            tmpNdx = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, asText(&argv[0]), &tmpNdx);
            }
    break;

    case TYSTRING:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if(isERROR(ret))
            {
            tmpNdx = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, &atHMChar(asString(&argv[0])->itsCString,0), &tmpNdx);
            }
    break;

    case TYSTRINGSUBSTR:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if (isERROR(ret))
            {
            tmpNdx = 0;
            temp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
            tmpChar = temp[SubLen(argv[0])];
            temp[SubLen(argv[0])] = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, temp, &tmpNdx);
            temp[SubLen(argv[0])] = tmpChar;
            }
    break;
    
    default:
        *ret = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    break;
    }
    
ExitOnError(*ret);
    
julian = asReal(ret);
julian = julian - floor(julian);

/*  Extract the hours */

asTag(ret) = TYNUM;
asInt(ret) = julian * 24.0;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_minute

Return the minute given the serial date.
  
(minute "1:30:45")  ==> 30
(minute .5)         ==> 0

#endif

TVAL FDateFnc_minute(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM     mins;
NUM     tmpNdx;
REAL    julian;
LpCHAR  temp;
CHAR    tmpChar;

StartFrame
DeclareTVAL(ret);
EndFrame

if(argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[0]))
    {
    case TYTEXT:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if(isERROR(ret))
            {
            tmpNdx = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, asText(&argv[0]), &tmpNdx);
            }
    break;

    case TYSTRING:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if(isERROR(ret))
            {
            tmpNdx = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, &atHMChar(asString(&argv[0])->itsCString,0), &tmpNdx);
            }
    break;

    case TYSTRINGSUBSTR:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if (isERROR(ret))
            {
            tmpNdx = 0;
            temp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
            tmpChar = temp[SubLen(argv[0])];
            temp[SubLen(argv[0])] = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, temp, &tmpNdx);
            temp[SubLen(argv[0])] = tmpChar;
            }
    break;
    
    default:
        *ret = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    break;
    }
    
ExitOnError(*ret);
    
julian = fabs(asReal(ret));
julian = julian - floor(julian);

/*  Extract the minutes */

mins = julian * 24.0 * 60.0;
mins = mins % 60;

asTag(ret) = TYNUM;
asInt(ret) = mins;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_second

Return the second given the serial date.
  
(second "1:30:45")  ==> 45
(second .5)         ==> 0

#endif

TVAL FDateFnc_second(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM     hours;
NUM     mins;
NUM     secs;
NUM     tmpNdx;
REAL    tmpReal;
REAL    julian;
LpCHAR  temp;
CHAR    tmpChar;

StartFrame
DeclareTVAL(ret);
EndFrame

if(argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch(asTag(&argv[0]))
    {
    case TYTEXT:
        *ret = FDateFnc_datevalue(gCP, gTP,1, argv);
        if(isERROR(ret))
            {
            tmpNdx = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, asText(&argv[0]), &tmpNdx);
            }
    break;

    case TYSTRING:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if(isERROR(ret))
            {
            tmpNdx = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, &atHMChar(asString(&argv[0])->itsCString,0), &tmpNdx);
            }
    break;

    case TYSTRINGSUBSTR:
        *ret = FDateFnc_datevalue(gCP, gTP, 1, argv);
        if (isERROR(ret))
            {
            tmpNdx = 0;
            temp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
            tmpChar = temp[SubLen(argv[0])];
            temp[SubLen(argv[0])] = 0;
            *ret = FProcedure_cnvTime(gCP, gTP, temp, &tmpNdx);
            temp[SubLen(argv[0])] = tmpChar;
            }
    break;
    
    default:
        *ret = FObject_RealAnyCnv(gCP, gTP, TYREAL, argv[0]);
    break;
    }
    
ExitOnError(*ret);
    
julian = asReal(ret);
julian = julian - floor(julian);

/*  Extract the secs */

tmpReal = julian * 24.0;
hours = tmpReal;
tmpReal = (tmpReal - hours)*60;
mins = tmpReal;
secs = (tmpReal - mins)*60;

asTag(ret) = TYNUM;
asInt(ret) = secs;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_days360

Return the #days between date1 and date2 based on a 30 day/month, 360 day/year.

(days360 "1/1/91" "7/1/91") ==> 180
  
#endif

TVAL FDateFnc_days360(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM     startYear;
NUM     startMonth;
NUM     startDay;
NUM     endYear;
NUM     endMonth;
NUM     endDay;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(startDate);
DeclareTVAL(endDate);
EndFrame

if(argc != 2)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Convert the arguments to TYDATE format */

*startDate = FDateFnc_datevalue(gCP,gTP,1, &argv[0]);
ExitOnError(*startDate);

*endDate = FDateFnc_datevalue(gCP,gTP,1, &argv[1]);
ExitOnError(*endDate);

/*  Extract the year month and day information */

*ret = FDateFnc_year(gCP,gTP,1, startDate);
ExitOnError(*ret);
startYear = asNumIndex(ret);

*ret = FDateFnc_month(gCP,gTP,1, startDate);
ExitOnError(*ret);
startMonth = asNumIndex(ret);

*ret = FDateFnc_day(gCP,gTP,1, startDate);
ExitOnError(*ret);
startDay = asNumIndex(ret);

*ret = FDateFnc_year(gCP,gTP,1, endDate);
ExitOnError(*ret);
endYear = asNumIndex(ret);

*ret = FDateFnc_month(gCP,gTP,1, endDate);
ExitOnError(*ret);
endMonth = asNumIndex(ret);

*ret = FDateFnc_day(gCP,gTP,1, endDate);
ExitOnError(*ret);
endDay = asNumIndex(ret);

/*  Calculate days360 based on this info. */

asTag(ret) = TYNUM;
asInt(ret) = (endYear - startYear)*360 + (endMonth - startMonth)*30 + endDay - startDay;

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_now

Return the serial date.time for the current date and time
  
#endif

TVAL FDateFnc_now(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL        hour;
REAL        minute;
REAL        second;
REAL        timeFraction;
time_t      now;
struct tm   *date;

StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);

EndFrame


argv = argv; // NOOP to hide unused parameter warning message
if(argc)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

now = time( NULL);
date = localtime(&now);

prmv[0].Tag     = TYNUM;
prmv[0].u.Int   = _FDateFnc_StartingYear + date->tm_year;

prmv[1].Tag		= TYNUM;
prmv[1].u.Int    = date->tm_mon + 1;

prmv[2].Tag		= TYNUM;
prmv[2].u.Int    = date->tm_mday;

/*  First we convert to TYDATE */

*ret = FDateFnc_date(gCP,gTP,3, &prmv[0]);
ExitOnError(*ret);

/*  Then we set the type to real for appropriate display. */

ret->Tag = TYREAL;

/*  Then we add the time as a fraction to the date. */

hour    = date->tm_hour;
minute  = date->tm_min;
second  = date->tm_sec;
timeFraction = (hour * 60.0 * 60.0) + (minute * 60.0) + second;
timeFraction /= (24.0 * 60.0 * 60.0);
ret->u.Real += timeFraction;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_today

Return the serial date.time for the current date and time
  
#endif

TVAL FDateFnc_today(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
time_t      now;
struct tm   *date;

StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);

EndFrame

argv = argv; // NOOP to hide unused parameter warning message
if(argc != 0)
	{
	*ret = TERROR("!today: No arguments allowed!" );
    FrameExit(*ret);
	}

now = time( NULL);
date = localtime(&now);

prmv[0].Tag = TYNUM;
prmv[0].u.Int   = _FDateFnc_StartingYear + date->tm_year;

prmv[1].Tag = TYNUM;
prmv[1].u.Int   = date->tm_mon + 1;  /* Fudge Factor need here */

prmv[2].Tag = TYNUM;
prmv[2].u.Int   = date->tm_mday;

/*  First we convert to TYDATE */

*ret = FDateFnc_date(gCP,gTP,3, &prmv[0]);
ExitOnError(*ret);

/*  Then we set the type to real for appropriate display. */

asTag(ret) = TYREAL;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_year

Return the year given the serial date, or a text date.

(year "7/4/4") ==> 4
  
#endif

TVAL FDateFnc_year(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL        tmpReal;
NUM         tmpInt;
NUM         intResult;
CHAR        dateText[128];
LpCHAR      tmpP;
LpCHAR      dateP;
StartFrame
DeclareTVAL(ret);
EndFrame

if (argc != 1)
	{
	*ret = TERROR("!year: Expecting to receive a single argument!" );
	FrameExit(*ret);
	}
    
*ret = gCP->Tval_VOID;

switch(asTag(&argv[0]))
    {
    case TYTEXT:
    case TYSTRING:
    case TYSTRINGSUBSTR:
        /*  Convert to a julian number */
        *ret = FDateFnc_datevalue(gCP,gTP,argc,argv);
    break;
    
    default:
        *ret = FObject_DateAnyCnv(gCP,gTP,TYDATE,argv[0]);
    break;
    }
    
ExitOnError(*ret);

/*  We now have a julian date, we need to extract the day from it. */

asTag(ret) = TYDATE;
*ret = TObject_TextAnyCnv(gCP,gTP,TYTEXT,*ret);
ExitOnError(*ret);

/*  Now we get it into a string buffer */

switch(asTag(ret))
    {
    case TYTEXT:
        strcpy((char*)dateText, (char*)asText(ret));
        break;
        
    case TYSTRING:
        if(asString(ret)->itsMaxItemIndex >= (NUM)(sizeof(dateText)))
            goto BadCleanUp;
        else
            {
            strcpy((char*)dateText, (char*)&atHMChar(asString(ret)->itsCString,0));
            }
    break;
    
    default:
        *ret =  gCP->TObject_ERROR_INVALID;
        goto BadCleanUp;
    break;
    }
    
/*  We now have a string in the format #Mon,day,year(BC) */

dateP = &dateText[1];

if ((tmpP = (LpCHAR)strchr((char*)dateP, ',')) != NULL)
    {
    dateP = tmpP+1;
    if ((tmpP = (LpCHAR)strchr((char*)dateP, ',')) != NULL)
        {
        dateP = tmpP+1;
        
        *ret = FProcedure_recInt(gCP, gTP, &tmpReal, &tmpInt, dateP);
        ExitOnError(*ret);
        
        if(asTag(ret) == TYBOLE && asBool(ret) == TRUE)
            {
            intResult = tmpReal;
            asTag(ret) = TYNUM;
            if(strstr((char*)dateP, "BC"))
                intResult = -intResult;
                
            asInt(ret) = intResult;
            }
        else
            goto BadCleanUp;
        }
    else
        goto BadCleanUp;
    }
else
    goto BadCleanUp;
    
FrameExit(*ret);

BadCleanUp:

if(!isERROR(ret))
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
FrameExit(*ret);
}
/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_monthNumber

Given a month string return the index for the month
  
#endif

TVAL FDateFnc_monthNumber(LpXCONTEXT gCP,LpTHREAD gTP, LpCHAR dateP, LpNUM intResultP)
{
StartFrame
DeclareTVAL(ret);
EndFrame

switch (dateP[0])
    {
    case    'A':
        if ((dateP[1] == 'p') && (dateP[2] == 'r'))
            {*intResultP = 4;}
        else
        if ((dateP[1] == 'u') && (dateP[2] == 'g'))
            {*intResultP = 8;}
        else
            goto Bad;
        break;
        
    case    'D':
        if ((dateP[1] == 'e') && (dateP[2] == 'c'))
            {*intResultP = 12;}
        else
            goto Bad;
        break;
        
    case    'F':
        if ((dateP[1] == 'e') && (dateP[2] == 'b'))
            {*intResultP = 2;}
        else
            goto Bad;
        break;
        
    case    'J':
        if ((dateP[1] == 'a') && (dateP[2] == 'n'))
            {*intResultP = 1;}
        else
        if ((dateP[1] == 'u') && (dateP[2] == 'n'))
            {*intResultP = 6;}
        else
        if ((dateP[1] == 'u') && (dateP[2] == 'l'))
            {*intResultP = 7;}
        else
            goto Bad;
        break;
        
    case    'M':
        if ((dateP[1] == 'a') && (dateP[2] == 'r'))
            {*intResultP = 3;}
        else
        if ((dateP[1] == 'a') && (dateP[2] == 'y'))
            {*intResultP = 5;}
        else
            goto Bad;
        break;
        
    case    'N':
        if ((dateP[1] == 'o') && (dateP[2] == 'v'))
            {*intResultP = 11;}
        else
            goto Bad;
        break;
        
    case    'O':
        if ((dateP[1] == 'c') && (dateP[2] == 't'))
            {*intResultP = 10;}
        else
            goto Bad;
        break;
        
    case    'S':
        if ((dateP[1] == 'e') && (dateP[2] == 'p'))
            {*intResultP = 9;}
        else
            goto Bad;
        break;
        
    default:
        Bad:
        *ret = gCP->TObject_ERROR_INVALID;
        goto BadCleanUp;
        break;
    }

FrameExit(gCP->TObject_OK);

BadCleanUp:

if(!isERROR(ret))
    *ret = gCP->TObject_ERROR_SYNTAX;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_month

Return the month given the serial date, or a text date.

(month "7/4/4") ==> 7
  
#endif

TVAL FDateFnc_month(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         intResult;
CHAR        dateText[128];
LpCHAR      tmpP;
LpCHAR      dateP;
StartFrame
DeclareTVAL(ret);
EndFrame

if(argc != 1)
	{
	*ret = TERROR("!month: Expecting a single argument!" );
	FrameExit(*ret);
	}
    
*ret = gCP->Tval_VOID;

switch(asTag(&argv[0]))
    {
    case TYDATE:
    case TYREAL:
        /*  Convert to a julian number */
        
        *ret =  argv[0];
    break;
    
    case TYTEXT:
    case TYSTRING:
    case TYSTRINGSUBSTR:
        /*  Convert to a julian number */
        *ret = FDateFnc_datevalue(gCP,gTP,argc,argv);
    break;
    
    default:
        *ret = FObject_DateAnyCnv(gCP,gTP,TYDATE,argv[0]);
    break;
    }
    
ExitOnError(*ret);

/*  We now have a julian date, we need to extract the month from it. */

asTag(ret) = TYDATE;
*ret = TObject_TextAnyCnv(gCP,gTP,TYTEXT,*ret);
ExitOnError(*ret);

/*  Now we get it into a string buffer */

switch(asTag(ret))
    {
    case TYTEXT:
        strcpy((char*)dateText, (char*)asText(ret));
        break;
        
    case TYSTRING:
        if(asString(ret)->itsMaxItemIndex >= (NUM)(sizeof(dateText)))
            goto BadCleanUp;
        else
            {
            strcpy((char*)dateText, (char*)&atHMChar(asString(ret)->itsCString,0));
            }
    break;
    
    default:
        *ret =  gCP->TObject_ERROR_INVALID;
        goto BadCleanUp;
    break;
    }
    
/*  We now have a string in the format #Mon,day,year(BC) */

dateP = &dateText[1];

if ((tmpP = (LpCHAR)strchr((char*)dateP, ',')) != NULL)
    {
    *tmpP = 0;
    
    *ret = FDateFnc_monthNumber(gCP, gTP,dateP, &intResult);
    ExitOnError(*ret);

    }
else
    goto BadCleanUp;
    
asTag(ret) = TYNUM;
asInt(ret) = intResult;

FrameExit(*ret);

BadCleanUp:

if(!isERROR(ret))
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_day

Return the day given the serial date, or a text date.

(day "7/4/1992") ==> 4
  
#endif

TVAL FDateFnc_day(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM         tmpInt;
REAL        tmpReal;
CHAR        dateText[128];
LpCHAR      tmpP;
LpCHAR      dateP;
StartFrame
DeclareTVAL(ret);
EndFrame

if(argc != 1)
	{
	*ret = TERROR("!day: Expecting a single argument!" );
	FrameExit(*ret);
	}
    
*ret = gCP->Tval_VOID;

switch(asTag(&argv[0]))
    {
    case TYTEXT:
    case TYSTRING:
    case TYSTRINGSUBSTR:
        /*  Convert to a julian number */
        *ret = FDateFnc_datevalue(gCP,gTP,argc,argv);
    break;
    
    default:
        *ret = FObject_DateAnyCnv(gCP,gTP,TYDATE,argv[0]);
    break;
    }
    
ExitOnError(*ret);

/*  We now have a julian date, we need to extract the day from it. */

asTag(ret) = TYDATE;
*ret = TObject_TextAnyCnv(gCP,gTP,TYTEXT,*ret);
ExitOnError(*ret);

/*  Now we get it into a string buffer */

switch(asTag(ret))
    {
    case TYTEXT:
        strcpy((char*)dateText, (char*)asText(ret));
        break;
        
    case TYSTRING:
        if(asString(ret)->itsMaxItemIndex >= (NUM)(sizeof(dateText)))
            goto BadCleanUp;
        else
            {
            strcpy((char*)dateText, (char*)&atHMChar(asString(ret)->itsCString,0));
            }
    break;
    
    default:
        *ret =  gCP->TObject_ERROR_INVALID;
        goto BadCleanUp;
    break;
    }
    
/*  We now have a string in the format #Mon,day,year(BC) */

dateP = dateText;

if ((tmpP = (LpCHAR)strchr((char*)dateP, ',')) != NULL)
    {
    dateP = tmpP+1;
    if ((tmpP = (LpCHAR)strchr((char*)dateP, ',')) != NULL)
        {
        *tmpP = 0;
        
        *ret = FProcedure_recInt(gCP, gTP, &tmpReal, &tmpInt, dateP);
        ExitOnError(*ret);
        
        if(asTag(ret) == TYBOLE && asBool(ret) == TRUE)
            {
            asTag(ret) = TYNUM;
            asInt(ret) = tmpReal;
            }
        else
            {
            goto BadCleanUp;
            }
        }
    else
        goto BadCleanUp;
    }
else
    goto BadCleanUp;
    
FrameExit(*ret);

BadCleanUp:

if(!isERROR(ret))
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_datevalue

Returns the Julian date for a date specified in text form. We convert the text 
date into a Julian date value that you can use in calculations.

        (julian "7/4/1992") ==> 727733
  
#endif

TVAL FDateFnc_datevalue(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL        tmpReal;
NUM         tmpInt;
NUM         year;
NUM         month;
NUM         day;
CHAR        dayText[16];
CHAR        yearText[62];
CHAR        monthText[16];
CHAR        dateText[128];
LpCHAR      tmpP;
time_t      now;
struct tm   *date;

StartFrame
DeclareTVAL(ret);
EndFrame

if(argc != 1)
	{
	*ret = TERROR("!julian: Expecting a single argument!" );
	FrameExit(*ret);
	}
    
switch(asTag(&argv[0]))
    {
    case TYDATE:
        *ret = argv[0];
        ret->Tag = TYREAL;
        break;
        
    case TYTEXT:
        strcpy((char*)dateText, (char*)asText(&argv[0]));
            
        goto DateValueFromText;
        
        break;
    
    case TYSTRINGSUBSTR:
        tmpP = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        strncpy((char*)dateText, (char*)tmpP, min(SubLen(argv[0]), (NUM)(sizeof(dateText) - 1)));
        dateText[min(SubLen(argv[0]), (NUM)(sizeof(dateText) - 1))] = 0;
        
        goto DateValueFromText;
    
        break;
        
    case TYSTRING:
        if(asString(&argv[0])->itsMaxItemIndex >= (NUM)(sizeof(dateText)))
			{
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
			}
        else
            {
            strcpy((char*)dateText, (char*)&atHMChar(asString(&argv[0])->itsCString,0));
            }
            
        DateValueFromText:
        
        /*  Extract the mm/dd/yyy from the text if possible */
        
        day = month = year = -1;
        
        tmpP = dateText;
        *ret = FProcedure_recInt(gCP, gTP, &tmpReal, &tmpInt, tmpP);
        ExitOnError(*ret);
        
        if(asTag(ret) == TYBOLE && asBool(ret) == TRUE)
            {
            month = tmpReal;
            }
        else
            {
            goto BadCleanUp;
            }
            
        tmpP += tmpInt;
        if(*tmpP == '/')
            {
            tmpP++;
            *ret = FProcedure_recInt(gCP, gTP, &tmpReal, &tmpInt, tmpP);
            ExitOnError(*ret);
            
            if(asTag(ret) == TYBOLE && asBool(ret) == TRUE)
                {
                day = tmpReal;
                }
            else
                {
                goto BadCleanUp;
                }
            }

        tmpP += tmpInt;
        if(*tmpP == '/')
            {
            tmpP++;
            *ret = FProcedure_recInt(gCP, gTP, &tmpReal, &tmpInt, tmpP);
            ExitOnError(*ret);
            
            if(asTag(ret) == TYBOLE && asBool(ret) == TRUE)
                {
                year = tmpReal;
                }
            else
                {
                goto BadCleanUp;
                }
            }
        else
            {
            /*  Force the year to the current year. */
            
            now = time( NULL);
            date = localtime(&now);
            year = _FDateFnc_StartingYear + date->tm_year;
            }
        
        /*  Format the date as a string. */
        
        *ret = FDateFnc_CnvYearToText(gCP, gTP, year, yearText);
        ExitOnError(*ret);
        
        *ret = FDateFnc_CnvMonthToText(gCP, gTP, month, monthText);
        ExitOnError(*ret);
        
        if(day < 33 && day >= 1)
            sprintf((char*)dayText, INTFORMAT, day);
        else
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
            
        sprintf((char*)dateText, "#%s,%s,%s", monthText, dayText, yearText);
        
        tmpInt = 0;
        *ret = FProcedure_cnvDate(gCP, gTP, dateText, &tmpInt);
        
        /* Convert the date to a julian value. */
        if (ret->Tag == TYDATE)
            ret->Tag = TYREAL;
    break;
    
    default:
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    break;
    }
    
FrameExit(*ret);

BadCleanUp:

FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_date
                                    0   1           2                           3   4   5
Return the serial date given the year, month, and day. And optionally also the hour:min:sec
  
#endif

TVAL FDateFnc_date(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM     cn;
NUM     parms[6] = {0, 0, 0, 0, 0, 0};
CHAR    yearText[62];
CHAR    monthText[16];
CHAR    dateText[128];
StartFrame
DeclareTVAL(ret);
EndFrame

if (argc == 1) 
	{
	*ret = FConvert_ToDate(gCP,gTP,argc,argv);
    FrameExit(*ret);
	}
    
if (argc < 3 || argc > 6) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
for(cn = 0; cn < 6; cn++)
    {
    if(cn < argc)
        {
        *ret = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[cn]);
        ExitOnError(*ret);
        
        parms[cn] = asInt(ret);
        }
    else
        parms[cn] = 0;
    }

/* Convert specual case where all parameters are zero to null time. */
if ((parms[0] == 0) && (parms[1] == 0) && (parms[2] == 0) && (parms[3] == 0) && (parms[4] == 0) && (parms[5] == 0))
	{
	ret->u.Real = 0.0;
	ret->Tag = TYDATE;
	FrameExit(*ret);
	}

/*  Format the date as a string. */

*ret = FDateFnc_CnvYearToText(gCP, gTP, parms[0], yearText);
ExitOnError(*ret);

*ret = FDateFnc_CnvMonthToText(gCP, gTP, parms[1], monthText);
ExitOnError(*ret);

if( parms[4] < 0 || parms[4] > 59 || parms[3] < 0 || parms[3] > 24 || 
parms[5] < 0 || parms[5] > 59 || parms[2] < 1 || parms[2] > 31)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

sprintf((char*)dateText, "#%s,"INTFORMAT",%s:"INTFORMAT":"INTFORMAT":"INTFORMAT, monthText, parms[2], yearText, parms[3], parms[4], parms[5]);

cn = 0;
*ret = FProcedure_cnvDate(gCP, gTP, dateText, &cn);
FrameExit (*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_CnvMonthToText

Given an integral month format the text string for that month.
  
#endif

TVAL FDateFnc_CnvMonthToText(LpXCONTEXT gCP,LpTHREAD gTP, NUM month, LpCHAR monthText)
{
    
gTP = gTP; // NOOP to hide unused parameter warning message
switch(month)
    {
    case 1:
        strcpy((char*)monthText, "Jan");
    break;
    
    case 2:
        strcpy((char*)monthText, "Feb");
    break;
    
    case 3:
        strcpy((char*)monthText, "Mar");
    break;
    
    case 4:
        strcpy((char*)monthText, "Apr");
    break;
    
    case 5:
        strcpy((char*)monthText, "May");
    break;
    
    case 6:
        strcpy((char*)monthText, "Jun");
    break;
    
    case 7:
        strcpy((char*)monthText, "Jul");
    break;
    
    case 8:
        strcpy((char*)monthText, "Aug");
    break;
    
    case 9:
        strcpy((char*)monthText, "Sep");
    break;
    
    case 10:
        strcpy((char*)monthText, "Oct");
    break;
    
    case 11:
        strcpy((char*)monthText, "Nov");
    break;
    
    case 12:
        strcpy((char*)monthText, "Dec");
    break;
    
    default:
        return(gCP->TObject_ERROR_INVALID_ARGLIST);
    break;
    }
    
return gCP->TObject_OK;
}
/*--------------------------------------------------------------------------------------- */
#if 0
FDateFnc_CnvYearToText

Given an integral year, format the text string for that year.
  
#endif

TVAL FDateFnc_CnvYearToText(LpXCONTEXT gCP,LpTHREAD gTP, NUM year, LpCHAR yearText)
{
gTP = gTP; // NOOP to hide unused parameter warning message
if(year < 0)
    {
    year *= -1;
    
    sprintf((char*)yearText, INTFORMAT"BC", year);
    }
else
    {
    sprintf((char*)yearText, INTFORMAT"AD", year);
    }
    
return gCP->TObject_OK;
}
