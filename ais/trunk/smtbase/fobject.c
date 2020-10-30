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

#define _C_FOBJECT
#define _SMARTBASE

#if 0
FObject.c
    

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fobject.h"
#include "ffloat.h"		
#include "fproc.h"
#include "tdiction.h"
#include "tdirect.h"
#include "terror.h"
#include "tmatrix.h"
#include "tnummat.h"
#include "tobjvec.h"
#include "tnumvec.h"
#include "tintvec.h"
#include "tbitvec.h"
#include "tbytevec.h"
#include "tfltvec.h"
#include "tcpxvec.h"
#include "tshortvec.h"
#include "tlongvec.h"
#include "tbrick.h"
#include "tpcodvec.h"
#include "tpair.h"


/*--------------------------------------------------------------------------------------- */

#if 0
FObject_Map

#endif

TVAL FObject_Map(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL proc)
{
switch(asTag(&selfTval))
    {
    case TYVECTOR:
        return TVector_Map(gCP,gTP,selfTval,proc);
    break;

    case TYMATRIX:
        return TMatrix_Map(gCP,gTP,selfTval,proc);
    break;

    case TYNUMMATRIX:
        return TNumMatrix_Map(gCP,gTP,selfTval,proc);
    break;

    case TYSTRING:
        return TString_Map(gCP,gTP,selfTval,proc);
    break;

    case TYPAIR:
        return TPair_Map(gCP,gTP,selfTval,proc);
    break;

    case TYSTRUCTURE:
        return TStructure_Map(gCP,gTP,selfTval,proc);
    break;
    
    default:
        return TObject_Map(gCP,gTP,selfTval,proc);
    break;
    }
}


/*--------------------------------------------------------------------------------------- */

#if 0
FObject_Mapc

#endif

TVAL FObject_Mapc(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL proc)
{
switch(asTag(&selfTval))
    {
    case TYVECTOR:
        return TVector_Mapc(gCP,gTP,selfTval,proc);
    break;
    case TYMATRIX:
        return TMatrix_Mapc(gCP,gTP,selfTval,proc);
    break;
    case TYNUMMATRIX:
        return TNumMatrix_Mapc(gCP,gTP,selfTval,proc);
    break;
    case TYSTRING:
        return TString_Mapc(gCP,gTP,selfTval,proc);
    break;
    case TYSYMBOL:
    break;
    case TYPAIR:
        return TPair_Mapc(gCP,gTP,selfTval,proc);
    break;
    case TYSTRUCTURE:
        return TStructure_Mapc(gCP,gTP,selfTval,proc);
    break;

    default:
        return TObject_Mapc(gCP,gTP,selfTval,proc);
    break;
    }
    
return TObject_Mapc(gCP, gTP, selfTval, proc);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_recint

The FObject_recint function recognizes a signed contiguous series of ascii digits as Integers. 
If an integer is recognized, it is returned as either a REAL (32bit this allows very large
integers) or as a NUM (64bit). 

The FObject_recint function returns the length of the integer recognized, or false if no integer is recognized.

#endif

TVAL FObject_recint(LpXCONTEXT gCP, LpTHREAD gTP, LpNUM intResult, LpREAL realResult, LpNUM count, LpCHAR sp)
{
 REAL      sign = 1.0;
 NUM       sinc = 0;
 TVAL	   result;
 
 gTP = gTP;		// NOOP to hide unused parameter warning message
 *count = 0;
 *realResult = 0.0;
 *intResult = 0;

 /* Check for a signed integer */
 if (*sp == '-')
  {
   ++(*count);
   ++sinc;
   ++sp;
   sign = -1.0;
  } else
 if (*sp == '+')
  {
   ++(*count);
   ++sinc;
   ++sp;
   sign = 1.0;
  }

 /* Recognize a continguous series of ascii digits */
 while (ISDIGIT((NUM)*sp))
  {
   *intResult *= 10;
   *realResult *= 10.0;
   *intResult += (NUM)(*(sp)-'0');
   *realResult += (REAL)(*(sp++)-'0');
   *count += (sinc + 1);                
   sinc = 0;            
  }
 
 /* Include the effect of the sign on the Integer */
 if (sign<0) {*intResult = 0 - *intResult; *realResult = 0.0 - *realResult;};

 /* Return the result to the caller */
 /* Note: The result is returned as a REAL if this is a 32bit machine */
 /*       So that the largest possible integer can be expressed.      */
 if (*count > 0)
	{
#ifdef _M64
	result.Tag = TYNUM;
	result.u.Int = *intResult;
#else
	result.Tag = TYREAL;
	result.u.Real = *realResult;
#endif
    return(result);
	}
 else
    return(gCP->TObject_FALSE);
}                                                                           

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_recfrac

The FObject_recfrac function recognizes fractions. If a fraction is recognized, it 
is returned in the specified argument as a REAL. The FObject_recfrac function 
returns the length of the real string recognized, or zero if no real is 
recognized.

#endif

TVAL FObject_recfrac(LpXCONTEXT gCP, LpTHREAD gTP,LpREAL dp,LpNUM count,LpCHAR sp)
{
 REAL         x;
 REAL      sign;
 NUM       sinc;

 gTP = gTP; // NOOP to hide unused parameter warning message

 *count = 0;
 *dp = 0.0;
 x = 1.0;
 sign = 1.0;
 sinc = 0;

 if (*sp == '-')
  {
   ++sinc;
   ++sp;
   sign = -1.0;
  }

 while (ISDIGIT((NUM)*sp))
  {
   x /= 10.0;
   *dp += (x * (REAL)(*(sp++)-'0'));
   ++(*count);      
   *count += sinc;      
   sinc = 0;    
  }
 *dp *= sign;

 if (*count > 0)
    return(gCP->TObject_OK);
 else
    return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_atoi

The FObject_atoi function converts an ascii string into an NUM. 

#endif

TVAL FObject_atoi(LpXCONTEXT gCP, LpTHREAD gTP, LpNUM n,LpCHAR sp)
{
 NUM       sign;
 NUM       sinc;
 NUM       count;
 NUM		  N;                        
 NUM		  digit;                        
 StartFrame
 DeclareTVAL(ec);                       /* TEMPORARY ERROR RETURN VALUE     */
 EndFrame

 gTP = gTP; // NOOP to hide unused parameter warning message
 gCP = gCP; // NOOP to hide unused parameter warning message
 count = 0;
 N = 0;
 sign = 1;
 sinc = 0;

 if (*sp == '-')
  {
   ++(count);
   ++sinc;
   ++sp;
   sign = -1;
  }

 while (ISDIGIT((NUM)*sp))
  {
   N *= 10;
   digit = (NUM)(*(sp++)-'0');
   N += digit;
   count += (sinc + 1);                
   sinc = 0;            
  }

 if (sign < 0) N = 0 - N;

 *n = N;

 if (count > 0)
    FrameExit(gCP->TObject_OK)
 else
    FrameExit(gCP->TObject_ERROR_INVALID);

 FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_atoui

The FObject_atoui function converts an ascii string into an UNUM.

#endif

TVAL FObject_atoui(LpXCONTEXT gCP, LpTHREAD gTP, LpUNUM n, LpCHAR sp)
{
 NUM       sign;
 NUM       sinc;
 NUM       count;
 UNUM		  N;                        
 NUM		  digit;                        
 StartFrame
 DeclareTVAL(ec);                       /* TEMPORARY ERROR RETURN VALUE     */
 EndFrame

 gTP = gTP; // NOOP to hide unused parameter warning message
 gCP = gCP; // NOOP to hide unused parameter warning message
 count = 0;
 N = 0;
 sign = 1;
 sinc = 0;

 if (*sp == '-')
 {
  ++(count);
  ++sinc;
  ++sp;
  sign = -1;
 }
 else 
 if (*sp == '#')
 {
  ++(count);
  ++sinc;
  ++sp;
 }

while (ISDIGIT((NUM)*sp))
  {
   N *= 10;
   digit = (NUM)(*(sp++)-'0');
   N += digit;
   count += (sinc + 1);                
   sinc = 0;            
  }

 if (sign < 0) N = 0 - N;

 *n = N;

 if (count <= 0)
    FrameExit(gCP->TObject_ERROR_INVALID);

 FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_CompareNever

The default compare function which always returns equal.

Note:   This function should never be called for any type.

#endif

TVAL FObject_CompareNever(LpXCONTEXT gCP, LpTHREAD gTP,TVAL left,TVAL right)
{
TVAL		result;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
left = left; // NOOP to hide unused parameter warning message
right = right; // NOOP to hide unused parameter warning message
result.Tag = TYCOMPARE;
result.u.Compare = EQUAL;


return(result);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_SetIV1Never

The default set indexed value function for native types (registered types 
which are not objects).

Note:   This function should never be called for native types.

#endif

TVAL FObject_SetIV1Never(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval,TVAL index1,TVAL newValue)
{
gTP = gTP; // NOOP to hide unused parameter warning message
dstTval = dstTval; // NOOP to hide unused parameter warning message
index1 = index1; // NOOP to hide unused parameter warning message
newValue = newValue; // NOOP to hide unused parameter warning message
return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_SetIV2Never

The default set indexed value function for native types (registered types 
which are not objects).

Note:   This function should never be called for native types.

#endif

TVAL FObject_SetIV2Never(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval,TVAL index1,TVAL index2,TVAL newValue)
{
gTP = gTP; // NOOP to hide unused parameter warning message
dstTval = dstTval; // NOOP to hide unused parameter warning message
index1 = index1; // NOOP to hide unused parameter warning message
index2 = index2; // NOOP to hide unused parameter warning message
newValue = newValue; // NOOP to hide unused parameter warning message
return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_SetIV3Never

The default set indexed value function for native types (registered types 
which are not objects).

Note:   This function should never be called for native types.

#endif

TVAL FObject_SetIV3Never(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval,TVAL index1,TVAL index2,TVAL index3,TVAL newValue)
{
gTP = gTP; // NOOP to hide unused parameter warning message
dstTval = dstTval; // NOOP to hide unused parameter warning message
index1 = index1; // NOOP to hide unused parameter warning message
index2 = index2; // NOOP to hide unused parameter warning message
index3 = index3; // NOOP to hide unused parameter warning message
newValue = newValue; // NOOP to hide unused parameter warning message
return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_GetIV1Never

The default get indexed value function for native types (registered types 
which are not objects).

Note:   This function should never be called for native types.

#endif

TVAL FObject_GetIV1Never(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1)
{
gTP = gTP; // NOOP to hide unused parameter warning message
srcTval = srcTval; // NOOP to hide unused parameter warning message
index1 = index1; // NOOP to hide unused parameter warning message
return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_GetIV2Never

The default get indexed value function for native types (registered types 
which are not objects).

Note:   This function should never be called for native types.

#endif

TVAL FObject_GetIV2Never(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1,TVAL index2)
{
gTP = gTP; // NOOP to hide unused parameter warning message
srcTval = srcTval; // NOOP to hide unused parameter warning message
index1 = index1; // NOOP to hide unused parameter warning message
index2 = index2; // NOOP to hide unused parameter warning message
return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_GetIV3Never

The default get indexed value function for native types (registered types 
which are not objects).

Note:   This function should never be called for native types.

#endif

TVAL FObject_GetIV3Never(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1,TVAL index2,TVAL index3)
{
gTP = gTP; // NOOP to hide unused parameter warning message
srcTval = srcTval; // NOOP to hide unused parameter warning message
index1 = index1; // NOOP to hide unused parameter warning message
index2 = index2; // NOOP to hide unused parameter warning message
index3 = index3; // NOOP to hide unused parameter warning message
return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_VoidAnyCnv

Convert a typed value to a value of type VOID.

#endif

TVAL FObject_VoidAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
gTP = gTP; // NOOP to hide unused parameter warning message
tTarget = tTarget; // NOOP to hide unused parameter warning message
oldValue = oldValue; // NOOP to hide unused parameter warning message
return(gCP->TObject_VOID);
}


/*--------------------------------------------------------------------------------------- */

#if 0
FObject_BoolAnyCnv

Convert a typed value to a value of type BOLE.

#endif

TVAL FObject_BoolAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpCHAR              aStringPtr;
StartFrame
DeclareTVAL(retTval);                       
EndFrame

asTag(retTval) = TYBOLE;
    
if (asTag(&oldValue) == TYTVAL)
	{
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue));
	}
else
if (asTag(&oldValue) == TYTEXT)
    {
    if (strcmp((char*)asText(&oldValue),(const char*)"true") == 0)
        {
        asBool(retTval) = TRUE;
        FrameExit(*retTval);
        }
    else
    if (strcmp((char*)asText(&oldValue),(const char*)"false") == 0)
        {
        asBool(retTval) = FALSE;
        FrameExit(*retTval);
        }
    else
        FrameExit(gCP->TObject_ERROR_INVALID);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    
    if (substrrcmp(aStringPtr, SubLen(oldValue), "true") == 0)
        {
        asBool(retTval) = TRUE;
        FrameExit(*retTval);
        }
    else
    if (substrrcmp(aStringPtr, SubLen(oldValue), "false") == 0)
        {
        asBool(retTval) = FALSE;
        FrameExit(*retTval);
        }
    else
        FrameExit(gCP->TObject_ERROR_INVALID);    
    }
else
if (oldValue.Tag == TYCPX)
	{
	asBool(retTval) = (oldValue.u.Complex->itsReal == 1.0) ? 1 : 0;
	FrameExit(*retTval);
	}
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP,gTP,asObject(&oldValue));
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    
    if (strcmp((char*)aStringPtr,(const char*)"true") == 0)
        {
        asBool(retTval) = TRUE;
        FrameExit(*retTval);
        }
    else
    if (strcmp((char*)aStringPtr,(const char*)"false") == 0)
        {
        asBool(retTval) = FALSE;
        FrameExit(*retTval);
        }
    else
        FrameExit(gCP->TObject_ERROR_INVALID);
    }
else
if ((asTag(&oldValue) == TYBOLE) || (asTag(&oldValue) == TYCHAR))
    {
    asBool(retTval) = (asBool(&oldValue) ? 1 : 0);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYNUM)
    {
    asBool(retTval) = (asInt(&oldValue) ? 1 : 0);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYUNUM)
    {
    asBool(retTval) = (asUInt(&oldValue) ? 1 : 0);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    asBool(retTval) = (asShort(&oldValue) ? 1 : 0);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYREAL) || (asTag(&oldValue) == TYDATE) || (asTag(&oldValue) == TYMONEY))
    {
    asBool(retTval) = (asReal(&oldValue) ? 1 : 0);
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_CharAnyCnv

Convert a typed value to a value of type CHAR.

#endif

TVAL FObject_CharAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpCHAR          aStringPtr;
StartFrame
DeclareTVAL(retTval);                       
EndFrame

retTval->Tag = TYCHAR;
if (asTag(&oldValue) == TYTVAL)
	{
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue));
	}
else
if (asTag(&oldValue) == TYTEXT)
    {
    asChar(retTval) = asText(&oldValue)[0];
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYCHAR)
    {
    asChar(retTval) = asChar(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    
    asChar(retTval) = *aStringPtr;
    FrameExit(*retTval);    
    }
else
if (asTag(&oldValue) == TYBOLE)
    {
    asChar(retTval) = asBool(&oldValue);
    FrameExit(*retTval);
    }
else
if (oldValue.Tag == TYCPX)
	{
	retTval->Tag = TYCHAR;
	retTval->u.Char = (NUM)oldValue.u.Complex->itsReal;
	FrameExit(*retTval);
	}
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    
    asChar(retTval) = *aStringPtr;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYUNUM)
    {
    asChar(retTval) = asUInt(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYNUM)
    {
    asChar(retTval) = asInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    asChar(retTval) = asShort(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYREAL) || (asTag(&oldValue) == TYDATE) || (asTag(&oldValue) == TYMONEY))
    {
    asChar(retTval) = asReal(&oldValue);
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_IntAnyCnv

Convert a typed value to a value of type NUM.

#endif

TVAL FObject_IntAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpCHAR          aStringPtr;
NUM             tmpValue;
StartFrame
DeclareTVAL(retTval);                       
DeclareTVAL(ErrValue);                       
EndFrame

asTag(retTval) = TYNUM;

if (asTag(&oldValue) == TYVOID)
	{
	asInt(retTval) = 0;
	FrameExit(*retTval);
	}
else
if (asTag(&oldValue) == TYTVAL)
	{
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue));
	}
else
if (asTag(&oldValue) == TYTEXT)
    {
    *ErrValue = FObject_atoi(gCP,gTP,&tmpValue,asText(&oldValue));
    ExitOnError(*ErrValue);
    asInt(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
                
    strncpy(gTP->TempBuffer, aStringPtr, SubLen(oldValue));
    gTP->TempBuffer[SubLen(oldValue)] = 0;
    aStringPtr = &gTP->TempBuffer[0];

    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue, aStringPtr);
    ExitOnError(*ErrValue);

    asInt(retTval) = tmpValue;
    FrameExit(*retTval);   
    }
else
if (oldValue.Tag == TYCPX)
	{
	//asInt(retTval) = INTEGER(oldValue.u.Complex->itsReal);
	asInt(retTval) = (NUM)(oldValue.u.Complex->itsReal);
	FrameExit(*retTval);
	}
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);
    
    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    
    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue,aStringPtr);
    ExitOnError(*ErrValue);
    asInt(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYBOLE) || (asTag(&oldValue) == TYCHAR))
    {
    asInt(retTval) = asChar(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYUNUM))
	{
	asInt(retTval) = asUInt(&oldValue);
    FrameExit(*retTval);
	}
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    asInt(retTval) = asInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    asInt(retTval) = asShort(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYREAL) || (asTag(&oldValue) == TYDATE) || (asTag(&oldValue) == TYMONEY))
    {
    retTval->u.Int = (NUM)(oldValue.u.Real);
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_UIntAnyCnv

Convert a typed value to a value of type UNUM.

#endif

TVAL FObject_UIntAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP, TYPE tTarget, TVAL oldValue)
{
LpCHAR          aStringPtr;
UNUM             tmpValue;
StartFrame
DeclareTVAL(retTval);
DeclareTVAL(ErrValue);
EndFrame

asTag(retTval) = TYUNUM;

if (asTag(&oldValue) == TYVOID)
    {
    asUInt(retTval) = 0;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYTVAL)
    {
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue));
    }
else
if (asTag(&oldValue) == TYTEXT)
    {
    *ErrValue = FObject_atoui(gCP,gTP,&tmpValue,asText(&oldValue));
    ExitOnError(*ErrValue);
    asUInt(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

    strncpy(gTP->TempBuffer, aStringPtr, SubLen(oldValue));
    gTP->TempBuffer[SubLen(oldValue)] = 0;
    aStringPtr = &gTP->TempBuffer[0];

    *ErrValue = FObject_atoui(gCP, gTP, &tmpValue, aStringPtr);
    ExitOnError(*ErrValue);
    asUInt(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if (oldValue.Tag == TYCPX)
    {
    asUInt(retTval) = (UNUM)(oldValue.u.Complex->itsReal);
    FrameExit(*retTval);
    }
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

    *ErrValue = FObject_atoui(gCP, gTP, &tmpValue,aStringPtr);
    ExitOnError(*ErrValue);
    asUInt(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYBOLE) || (asTag(&oldValue) == TYCHAR))
    {
    asUInt(retTval) = asChar(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYUNUM)
    {
    asUInt(retTval) = asUInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    asUInt(retTval) = asInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    asUInt(retTval) = asShort(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYREAL) || (asTag(&oldValue) == TYDATE) || (asTag(&oldValue) == TYMONEY))
    {
    asUInt(retTval) = (UNUM)(oldValue.u.Real);
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_ObjAnyCnv

Convert a typed value to a value of type OBJ.

#endif

TVAL FObject_ObjAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{

if (asTag(&oldValue) == TYTVAL)
    return(TObject_Convert(gCP,gTP,tTarget,oldValue));
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT && tTarget == asTag(&oldValue) )
    {
    return(oldValue);
    }
else
    return(gCP->TObject_ERROR_INVALID);
}


/*--------------------------------------------------------------------------------------- */

#if 0
FObject_ShortAnyCnv

Convert a typed value to a value of type SHORT.

#endif

TVAL FObject_ShortAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpCHAR          aStringPtr;
NUM             tmpValue;
StartFrame
DeclareTVAL(retTval);                       
DeclareTVAL(ErrValue);                       
EndFrame

asTag(retTval) = TYSHORT;      

if (asTag(&oldValue) == TYTVAL)
	{
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue));
	}
else
if (asTag(&oldValue) == TYTEXT)
    {
    *ErrValue = FObject_atoi(gCP,gTP,&tmpValue,asText(&oldValue));
    ExitOnError(*ErrValue);

    asShort(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
                
    strncpy(gTP->TempBuffer, aStringPtr, SubLen(oldValue));
    gTP->TempBuffer[SubLen(oldValue)] = 0;
    aStringPtr = &gTP->TempBuffer[0];             

    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue, aStringPtr);
    ExitOnError(*ErrValue);

    asShort(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if (oldValue.Tag == TYCPX)
	{
	retTval->Tag = TYSHORT;
	retTval->u.Short = (NUM)oldValue.u.Complex->itsReal;
	FrameExit(*retTval);
	}
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    
    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue,aStringPtr);
    ExitOnError(*ErrValue);

    asShort(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYBOLE) || (asTag(&oldValue) == TYCHAR))
    {
    asShort(retTval) = asChar(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYUNUM)
    {
    asShort(retTval) = asUInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    asShort(retTval) = asInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYSHORT) || (asTag(&oldValue) == TYTYPE))
    {
    asShort(retTval) = asShort(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYREAL) || (asTag(&oldValue) == TYDATE) || (asTag(&oldValue) == TYMONEY))
    {
    asShort(&retTval) = asReal(&oldValue);
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_RealAnyCnv

Convert a typed value to a value of type REAL.

#endif

TVAL FObject_RealAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpCHAR          aStringPtr;
REAL            tmpValue;
StartFrame
DeclareTVAL(retTval);                       
DeclareTVAL(ErrValue);                       
EndFrame

asTag(retTval) = TYREAL;

if (asTag(&oldValue) == TYTVAL)
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue))
else
if ((asTag(&oldValue) == TYREAL) || (asTag(&oldValue) == TYDATE) || (asTag(&oldValue) == TYMONEY))
    {
	asReal(retTval) = asReal(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYTEXT)
    {
	*ErrValue = TObject_ator(gCP,gTP,&tmpValue,asText(&oldValue));
    ExitOnError(*ErrValue);

    asReal(retTval) = tmpValue;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
                
    strncpy(gTP->TempBuffer, aStringPtr, SubLen(oldValue));
    gTP->TempBuffer[SubLen(oldValue)] = 0;
    aStringPtr = &gTP->TempBuffer[0];
    
    *ErrValue = TObject_ator(gCP, gTP, &tmpValue, aStringPtr);
    ExitOnError(*ErrValue);

    asReal(retTval) = tmpValue;
    FrameExit(*retTval)    
    }
else
if (asTag(&oldValue) == TYCPX)
    {
    asReal(retTval) = oldValue.u.Complex->itsReal;
	FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYBOLE) || (asTag(&oldValue) == TYCHAR))
    {	
    asReal(retTval) = (REAL)asChar(&oldValue);
    FrameExit(*retTval)
    }
else
if (asTag(&oldValue) == TYUNUM)
    {
	asReal(retTval) = (REAL)asUInt(&oldValue);
    FrameExit(*retTval)
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    asReal(retTval) = (REAL)asInt(&oldValue);
    FrameExit(*retTval)
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    asReal(retTval) = (REAL)asShort(&oldValue);
    FrameExit(*retTval)
    }
else
if (asTag(&oldValue) == TYVOID)
    {
    asReal(retTval) = 0.0;
    FrameExit(*retTval)
    }
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
	    FrameExit(gCP->TObject_ERROR_INVALID)

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
    if (aStringPtr == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);
        
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,aStringPtr);
    ExitOnError(*ErrValue);

    retTval->u.Real = tmpValue;
    FrameExit(*retTval)
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID)
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_DateAnyCnv

Convert a typed value to a value of type TYDATE.

#endif

TVAL FObject_DateAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpCHAR          aStringPtr;
NUM             index;
StartFrame
DeclareTVAL(retTval);                       
EndFrame

retTval->Tag = TYDATE;

if (asTag(&oldValue) == TYTVAL)
	{
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue));
	}
else
if (asTag(&oldValue) == TYTEXT)
    {
    aStringPtr = &oldValue.u.Text[0];
    goto ConvertFromText;
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
                
    strncpy(gTP->TempBuffer, aStringPtr, SubLen(oldValue));
    gTP->TempBuffer[SubLen(oldValue)] = 0;
    aStringPtr = &gTP->TempBuffer[0];    
    goto ConvertFromText;
    }
else
if (oldValue.Tag == TYCPX)
	{
	asReal(retTval) = asReal(&oldValue);
	FrameExit(*retTval);
	}
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        
    ConvertFromText:
    index = 0;
    *retTval = FProcedure_cnvDate(gCP, gTP, aStringPtr,&index);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYBOLE)
    {
    asReal(retTval) = asBool(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYCHAR)
    {
    asReal(retTval) = asChar(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYUNUM)
    {
    asReal(retTval) = asUInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    asReal(retTval) = asInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    asReal(retTval) = asShort(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYREAL) || (asTag(&oldValue) == TYDATE) || (asTag(&oldValue) == TYMONEY))
    {
    asReal(retTval) = asReal(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYVOID)
    {
    asReal(retTval) = 0;
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_MoneyAnyCnv

Convert a typed value to a value of type TYMONEY.

#endif

TVAL FObject_MoneyAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpCHAR          aStringPtr;
REAL            tmpValue;
StartFrame
DeclareTVAL(retTval);                       
DeclareTVAL(ErrValue);                       
EndFrame

asTag(retTval) = TYMONEY;

if (asTag(&oldValue) == TYTVAL)
	{
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue));
	}
else
if (asTag(&oldValue) == TYTEXT)
    {
    if (oldValue.u.Text[0] == '$')
        {
        *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&oldValue.u.Text[1]);
        ExitOnError(*ErrValue);
    
        asReal(retTval) = tmpValue;
        FrameExit(*retTval);
        }
    else
        FrameExit(gCP->TObject_ERROR_INVALID);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
                
    strncpy(gTP->TempBuffer, aStringPtr, SubLen(oldValue));
    gTP->TempBuffer[SubLen(oldValue)] = 0;
    aStringPtr = &gTP->TempBuffer[0];

    if (aStringPtr[0] == '$')
        {
        *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&aStringPtr[1]);
        ExitOnError(*ErrValue);
    
        asReal(retTval) = tmpValue;
        FrameExit(*retTval);
        }
    else
        FrameExit(gCP->TObject_ERROR_INVALID);
    }
else
if (oldValue.Tag == TYCPX)
	{
	retTval->Tag = TYMONEY;
	retTval->u.Real = oldValue.u.Complex->itsReal;
	FrameExit(*retTval);
	}
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

    if (aStringPtr[0] == '$')
        {
        *ErrValue = TObject_ator(gCP,gTP,&tmpValue,&aStringPtr[1]);
        ExitOnError(*ErrValue);
    
        asReal(retTval) = tmpValue;
        FrameExit(*retTval);
        }
    else
        FrameExit(gCP->TObject_ERROR_INVALID);
    }
else
if (oldValue.Tag == TYUNUM)
    {
    retTval->u.Real = oldValue.u.UInt;
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    asReal(retTval) = asInt(&oldValue);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYREAL) || (asTag(&oldValue) == TYDATE) || (asTag(&oldValue) == TYMONEY))
    {
    asReal(retTval) = asReal(&oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYVOID)
    {
    asReal(retTval) = 0;
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_ErrorAnyCnv

Convert a typed value to a value of type ERROR.

#endif

TVAL FObject_ErrorAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpCHAR      aStringPtr;
StartFrame
DeclareTVAL(retTval);                       
EndFrame

asTag(retTval) = TYERROR;

if (asTag(&oldValue) == TYTVAL)
	{
    FrameExit(TObject_Convert(gCP,gTP,tTarget,oldValue));
	}
else
if (asTag(&oldValue) == TYVOID)
    {
    asText(retTval)[0] = 0;
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYTEXT)
    {
    FrameExit(oldValue);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
                
    strncpy(gTP->TempBuffer, aStringPtr, SubLen(oldValue));
    gTP->TempBuffer[SubLen(oldValue)] = 0;
    aStringPtr = &gTP->TempBuffer[0];

	*retTval = TERROR(aStringPtr);
	FrameExit(*retTval);
    }
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
   
	*retTval = TERROR(aStringPtr);
	FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_TvalAnyCnv

Convert a typed value to a value of type TVAL.

#endif

TVAL FObject_TvalAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
StartFrame
DeclareTVAL(retTval);                       
DeclareTVAL(ec);                       
EndFrame

if (asTag(&oldValue) == TYTVAL)
    {
    FrameExit(oldValue);
    }
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);
        
    *retTval = FObject_ObjAnyCnv(gCP,gTP,tTarget, oldValue);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYTEXT)  
    {
    FrameExit(oldValue);
    }
else
    {
    *ec = TObject_Convert(gCP,gTP,tTarget,oldValue);
    FrameExit(*ec);
    }
}


/*--------------------------------------------------------------------------------------- */

#if 0
Perm
 
When fPerm is TRUE, makes an object non-garbage-collectable, when FALSE
makes it garbage-collectable, returns TRUE if previously collectable.

Note:   This method should be called for all permanent objects. Remember
        any objects which are referenced by permanent objects are also not
        collected. If no objects are permanent, every object is collected.

#endif

BOLE FObject_Perm(LpXCONTEXT gCP, LpTHREAD gTP, TObject* self, BOLE fPerm)
{

gTP = gTP; // NOOP to hide unused parameter warning message
if (fPerm)
    {
    _TObject_ObjectFlag(self->itsObjectIndex) |= _TObject_OfPERM;
    }
else
    {
    _TObject_ObjectFlag(self->itsObjectIndex) |= _TObject_OfPERM;
    _TObject_ObjectFlag(self->itsObjectIndex) ^= _TObject_OfPERM;
    }
    
return(fPerm);
}
    

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_GetStringPtr

#endif

LpCHAR FObject_GetStringPtr(LpXCONTEXT gCP, LpTHREAD gTP, TObject* self)
{
LpCHAR  theStringPtr;
StartFrame
DeclareTVAL(selfTval);                       
EndFrame

switch(self->itsObjectType)
    {
    case TYSYMBOL:
    case TYSTRING:
        asObject(selfTval) = self;
        asTag(selfTval) = self->itsObjectType;
        theStringPtr = TString_GetStringPtr(gCP,gTP,*selfTval);
    break;

    case TYERROR:
        asObject(selfTval) = self;
        asTag(selfTval) = self->itsObjectType;
        theStringPtr = ErrorArray(*selfTval);
 
    case TYTEXT:
        //theStringPtr = (LpCHAR)selfTval->u.Text[0];
        theStringPtr = (LpCHAR)selfTval->u.Text;
    break;
 
    case TYBYTEVECTOR:
        asObject(selfTval) = self;
        asTag(selfTval) = self->itsObjectType;
        theStringPtr = ByteArray(*selfTval);
    break;

    default:
        theStringPtr = (LpCHAR)NULL;
    break;
    }
    
FrameExit(theStringPtr);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FObject_GetCdr

Return the Lisp tail(cdr) of this object.

#endif

TVAL FObject_GetCdr(LpXCONTEXT gCP, LpTHREAD gTP, TObject* self)
{
StartFrame
DeclareTVAL(selfTval);                       
DeclareTVAL(cdr);                       
EndFrame

asObject(selfTval) = self;
asTag(selfTval) = self->itsObjectType;

switch(self->itsObjectType)
    {
    case TYVECTOR:
        *cdr = selfTval->u.Vector->itsCdr;
    break;
    case TYMATRIX:
        *cdr = selfTval->u.Matrix->itsCdr;
    break;
    case TYNUMMATRIX:
        *cdr = selfTval->u.NumMatrix->itsCdr;
    break;
    case TYPCODEVECTOR:
        *cdr = selfTval->u.PcodeVector->itsCdr;
    break;
    case TYPAIR:
        *cdr = selfTval->u.Pair->itsCdr;
    break;
    case TYSTRUCTURE:
        *cdr = selfTval->u.Structure->itsCdr;
    break;
    case TYDICTIONARY:
        *cdr = selfTval->u.Dictionary->itsCdr;
    break;
    case TYDIRECTORY:
        *cdr = selfTval->u.Directory->itsCdr;
    break;
    case TYOBJVECTOR:
    case TYWORKSPACE:
        *cdr = selfTval->u.ObjVector->itsCdr;
    break;
    case TYSTRING:
    case TYERROR:
    case TYSYMBOL:
    case TYLAMBDA:
        *cdr = gCP->TObject_ERROR_INVALID;
    break;
    case TYNUMVECTOR:
        *cdr = selfTval->u.NumVector->itsCdr;
    break;
    case TYFLTVECTOR:
        *cdr = selfTval->u.FltVector->itsCdr;
    break;
    case TYBITVECTOR:
        *cdr = selfTval->u.BitVector->itsCdr;
    break;
    case TYINTVECTOR:
        *cdr = selfTval->u.IntVector->itsCdr;
    break;
    case TYSHORTVECTOR:
        *cdr = selfTval->u.ShortVector->itsCdr;
    break;
    case TYLONGVECTOR:
        *cdr = selfTval->u.LongVector->itsCdr;
    break;
    case TYBYTEVECTOR:
        *cdr = selfTval->u.ByteVector->itsCdr;
    break;
    case TYBRICK:
        *cdr = selfTval->u.Brick->itsCdr;
    break;
    case TYCPXVECTOR:
        *cdr = selfTval->u.CpxVector->itsCdr;
    break;
    default:
        *cdr = gCP->TObject_ERROR_INVALID;
    break;
    }
    
FrameExit(*cdr);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FObject_SetCdr


#endif

TVAL FObject_SetCdr(LpXCONTEXT gCP, LpTHREAD gTP, TObject* self, TVAL newCdr)
{
StartFrame
DeclareTVAL(selfTval);                       
EndFrame

asObject(selfTval) = self;
asTag(selfTval) = self->itsObjectType;

switch(self->itsObjectType)
    {
    case TYVECTOR:
		selfTval->u.Vector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYMATRIX:
		selfTval->u.Matrix->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYNUMMATRIX:
		selfTval->u.NumMatrix->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYPCODEVECTOR:
		selfTval->u.PcodeVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYPAIR:
		selfTval->u.Pair->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYSTRUCTURE:
		selfTval->u.Structure->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYDIRECTORY:
		selfTval->u.Directory->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYDICTIONARY:
		selfTval->u.Dictionary->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYSTRING:
    case TYERROR:
    case TYSYMBOL:

    case TYLAMBDA:
        FrameExit(gCP->TObject_ERROR_INVALID)
    break;
    case TYOBJVECTOR:
    case TYWORKSPACE:
		selfTval->u.ObjVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;  
    case TYNUMVECTOR:
		selfTval->u.NumVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYFLTVECTOR:
		selfTval->u.FltVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYBITVECTOR:
		selfTval->u.BitVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYINTVECTOR:
		selfTval->u.IntVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYSHORTVECTOR:
		selfTval->u.ShortVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYLONGVECTOR:
		selfTval->u.LongVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYBYTEVECTOR:
		selfTval->u.ByteVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    case TYBRICK:
		selfTval->u.Brick->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
	case TYCPXVECTOR:
		selfTval->u.CpxVector->itsCdr = newCdr;
        FrameExit(*selfTval)
    break;
    default:
    break;
    }
    
FrameExit(gCP->TObject_OK)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FObject_SetMaxIndex


#endif

TVAL FObject_SetMaxIndex(LpXCONTEXT gCP, LpTHREAD gTP, TObject* self, NUM newRepeats)
{
StartFrame
DeclareTVAL(selfTval);                       
EndFrame

asObject(selfTval) = self;
asTag(selfTval) = self->itsObjectType;

switch(self->itsObjectType)
    {
    case TYVECTOR:
        TVector_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYMATRIX:
        TMatrix_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYNUMMATRIX:
        TNumMatrix_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYSTRING:
        TString_SetMaxIndex(gCP,gTP,*selfTval,newRepeats);
    break;
    case TYSYMBOL:
        TString_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;

    case TYPAIR:
    case TYLAMBDA:
    break;
    case TYPCODEVECTOR:
        TPcodeVector_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYSTRUCTURE:
        TStructure_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYCONTINUATION:
    break;
    case TYINTVECTOR:
        TIntVector_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYSHORTVECTOR:
        TShtVector_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYLONGVECTOR:
		TLongVector_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYBYTEVECTOR:
        TByteVector_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYBITVECTOR:
        TBitVector_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYNUMVECTOR:
        TNumVector_SetMaxIndex(gCP,gTP,*selfTval,newRepeats);
    break;
    case TYFLTVECTOR:
        TFltVector_SetMaxIndex(gCP,gTP,*selfTval,newRepeats);
    break;
    case TYOBJVECTOR:
    case TYWORKSPACE:
        TObjVector_SetMaxIndex(gCP,gTP,*selfTval, newRepeats);
    break;
    case TYCPXVECTOR:
        TCpxVector_SetMaxIndex(gCP,gTP,*selfTval,newRepeats);
    break;
	default:
    break;
    }
    
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FObject_ComputeSize

#endif

void FObject_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TObject* self, NUM* aSize)
{
StartFrame
DeclareTVAL(selfTval);                       
EndFrame

asObject(selfTval) = self;
asTag(selfTval) = self->itsObjectType;

/* Use the ComputeSize function vector to access the ComputeSize function for this type */

if (_TObject_TypeComputeSize(self->itsObjectType) != NULL)
    {
    (*((_TObject_TypeComputeSize(self->itsObjectType))))(gCP,gTP,*selfTval, aSize);
    }
FrameReturn;

ALLIGNME(*aSize);
FrameReturn;
}
/*--------------------------------------------------------------------------------------- */
#if 0
FObject_ComputeSizeNever

This is the default computesize function that does nothing

#endif

void FObject_ComputeSizeNever(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, NUM* aSize)
{

self = self; // NOOP to hide unused parameter warning message
aSize = aSize; // NOOP to hide unused parameter warning message
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return;

}

/*--------------------------------------------------------------------------------------- */
#if 0
FObject_Save

#endif

void FObject_Save(LpXCONTEXT gCP, LpTHREAD gTP,TObject* self, HMemory aHMemory)
{
StartFrame
DeclareTVAL(selfTval);                       
EndFrame

asObject(selfTval) = self;
asTag(selfTval) = self->itsObjectType;

/* Use the Save function vector to access the Save function for this type */

if (_TObject_TypeSave(self->itsObjectType) != NULL)
    {
    (*((_TObject_TypeSave(self->itsObjectType))))(gCP, gTP,*selfTval, aHMemory);
    }
FrameReturn;    
}

/*--------------------------------------------------------------------------------------- */
#if 0
FObject_SaveNever

This is the default save function that does nothing

#endif

HMemory FObject_SaveNever(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, HMemory aHMemory)
{

self = self; // NOOP to hide unused parameter warning message
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(aHMemory);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FObject_Doomed

#endif

void FObject_Doomed(LpXCONTEXT gCP, LpTHREAD gTP,TObject* self)
{
TVAL    selfTval; 

asObject(&selfTval) = self;
asTag(&selfTval) = self->itsObjectType;

/* Use the Doomed function vector to access the doomed function for this type */

if (_TObject_TypeDoom(self->itsObjectType) != NULL)
    {
    (*((_TObject_TypeDoom(self->itsObjectType))))(gCP,gTP,selfTval);
    }
}


/*--------------------------------------------------------------------------------------- */
#if 0
FObject_DoomNever

This is the default doom function that is a no-op.

#endif

void FObject_DoomNever(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self)
{

self = self; // NOOP to hide unused parameter warning message
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return;

}


/*--------------------------------------------------------------------------------------- */

#if 0
FObject_Print

#endif

TVAL FObject_Print(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, LpNUM size, LpCHAR buf)
{
if (!_VALIDTVAL(selfTval))
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}

switch(asTag(&selfTval))
    {
    case TYVECTOR:
        return TVector_Print(gCP,gTP,selfTval,size,buf);
    break;
    case TYMATRIX:
        return TMatrix_Print(gCP,gTP,selfTval,size,buf);
    break;
    case TYNUMMATRIX:
        return TNumMatrix_Print(gCP,gTP,selfTval,size,buf);
    break;
    case TYNUMVECTOR:
        return TNumVector_Print(gCP, gTP, selfTval,size,buf);
    break;
    case TYBITVECTOR:
        return TBitVector_Print(gCP, gTP, selfTval,size,buf);
    break;
    case TYINTVECTOR:
        return TIntVector_Print(gCP, gTP, selfTval,size,buf);
    break;
    case TYSHORTVECTOR:
        return TShtVector_Print(gCP, gTP, selfTval,size,buf);
    break;
    case TYLONGVECTOR:
        return TLongVector_Print(gCP, gTP, selfTval,size,buf);
    break;
    case TYOBJVECTOR:
        return TObjVector_Print(gCP, gTP, selfTval,size,buf);
    break;
    case TYFLTVECTOR:
        return TFltVector_Print(gCP, gTP, selfTval,size,buf);
    break;
    case TYSTRING:
        return TString_Print(gCP,gTP,selfTval,size,buf);
    break;
    case TYERROR:
        return TError_Print(gCP,gTP,selfTval,size,buf);
    break;
    case TYSYMBOL:
    break;
    case TYPAIR:
        return TPair_Print(gCP,gTP,selfTval,size,buf);
    break;
    case TYBRICK:
        return TBrick_Print(gCP,gTP,selfTval,size,buf);
	break;
    case TYSTRUCTURE:
        return TStructure_Print(gCP,gTP,selfTval,size,buf);
	break;
    case TYDIRECTORY:
        return TDirectory_Print(gCP,gTP,selfTval.u.Directory,size,buf);
    break;
    case TYDICTIONARY:
        return TDictionary_Print(gCP,gTP,selfTval.u.Dictionary,size,buf);
    break;
    case TYLAMBDA:
        return gCP->TObject_ERROR_INVALID;
    break;
    case TYCPXVECTOR:
        return TCpxVector_Print(gCP, gTP, selfTval,size,buf);
    break;
    default:
    break;
    }
return (gCP->TObject_TRUE);
}


#if 0
FObject_Copy

Creates a copy of the incoming object.

#endif

TVAL FObject_Copy(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TObject,theCopy);
EndFrame

/* Use the Copy function vector to access the Copy function for this type */

theCopy = (*((_TObject_TypeCopy(selfTval.Tag))))(gCP, gTP, selfTval);  

if (theCopy == NULL)	/* a Copy operation was not possible */
	{
    *ret = selfTval; /* returns the original object */
	FrameExit(*ret);
	}
else
	{
	asObject(ret)	=	theCopy;
	asTag(ret)		=	theCopy->itsObjectType;
	FrameExit(*ret);
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FObject_CopyNever

This is the default copy function that does nothing.

#endif

TObject* FObject_CopyNever(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self)
{

self = self; // NOOP to hide unused parameter warning message
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return  NULL;




}
/*--------------------------------------------------------------------------------------- */

#if 0
FObject_GetMaxIndex

#endif

TVAL FObject_GetMaxIndex(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval)
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) =  TYNUM;

switch(asTag(&selfTval))
    {
    case TYVECTOR:
        asInt(ret) =  TVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYMATRIX:
        asInt(ret) =  TMatrix_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYNUMMATRIX:
        asInt(ret) =  TNumMatrix_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYSTRING:
        asInt(ret) =  TString_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYSYMBOL:
        asInt(ret) =  0;
    break;
    case TYPCODEVECTOR:
        asInt(ret) =  TPcodeVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYLAMBDA:
    case TYPAIR:
        asInt(ret) =  0;
    break;
    case TYDICTIONARY:
        asInt(ret) =  TDictionary_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYDIRECTORY:
        asInt(ret) =  TDirectory_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYSTRUCTURE:
        asInt(ret) =  TStructure_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYCONTINUATION:
        asInt(ret) =  0;
    break;
    case TYINTVECTOR:
        asInt(ret) =  TIntVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYSHORTVECTOR:
        asInt(ret) =  TShtVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYLONGVECTOR:
        asInt(ret) =  TLongVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYBYTEVECTOR:
        asInt(ret) =  TByteVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYBITVECTOR:
        asInt(ret) =  TBitVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYNUMVECTOR:
        asInt(ret) =  TNumVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYFLTVECTOR:
        asInt(ret) =  TFltVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    case TYCPXVECTOR:
        asInt(ret) =  TCpxVector_GetMaxIndex(gCP,gTP,selfTval);
    break;
    default:
        asInt(ret) =  0;
    break;
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_AddNewValue

#endif

TVAL FObject_AddNewValue(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, TVAL newValue)
{
switch(asTag(&selfTval))
    {
    case TYSTRING:
    break;
    case TYSYMBOL:
    case TYVECTOR:
        return TVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYLAMBDA:
    break;
    case TYPCODEVECTOR:
        return TPcodeVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYPAIR:
    break;
    case TYSTRUCTURE:
        /*  TStructure_AddNewValue requires a different parm list. */
        /*  perhaps it should be named differently? */
        
        return gCP->TObject_ERROR_INVALID;
    break;
    case TYINTVECTOR:
        return TIntVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYSHORTVECTOR:
        return TShtVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYLONGVECTOR:
        return TLongVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYBYTEVECTOR:
        return TByteVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYBITVECTOR:
        return TBitVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYNUMVECTOR:
        return TNumVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYFLTVECTOR:
        return TFltVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    case TYCPXVECTOR:
        return TCpxVector_AddNewValue(gCP,gTP,selfTval,newValue);
    break;
    default:
        return gCP->TObject_ERROR_INVALID;
    break;
    }
return gCP->TObject_ERROR_INVALID;
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_Destruct

Before deleting the storage allocated for this object, remove the object from the list
of living objects.
 
Then delete this storage allocated for this object.
 
Note:   This method should only be called by mark and sweep garbage collection!
        Disposing of an object outside the scope of mark and sweep can leave
        dangerous invalid object references. 

#endif

void FObject_Destruct(LpXCONTEXT gCP, LpTHREAD gTP, TObject* self)
{
if (_TObject_TypeFlag(self->itsObjectType) == _TObject_TfTOBJECT)
    {
    TObject_Destruct(gCP,gTP,self);
    TObject_OperatorDelete(gCP,gTP,self);
    }
}


/*--------------------------------------------------------------------------------------- */

#if 0
FObject_GetIV1Bits

The default get indexed bit value function for native types (registered types 
which are not objects).

#endif

TVAL FObject_GetIV1Bits(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1)
{
register NUM    tmp;

gTP = gTP; // NOOP to hide unused parameter warning message
/* Commented out because bit indexing is not supported, (for now) */
return(gCP->Tval_VOID);

if ((index1.Tag == TYNUM) && isNumIndex(&srcTval))
    {
    tmp = asNumIndex(&srcTval);
    tmp >>= index1.u.Int;
    tmp &= 1;
    srcTval.Tag = TYNUM;
    srcTval.u.Int = tmp;
    return(srcTval);
    }
    
return(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FObject_SetIV1Bits

The default set indexed bits value function for native types (registered types 
which are not objects).

#endif

TVAL FObject_SetIV1Bits(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval,TVAL index1,TVAL newValue)
{
register NUM    tmp;
register NUM    newv;
register NUM    mask = 1;

gTP = gTP; // NOOP to hide unused parameter warning message
if ((index1.Tag == TYNUM) && isNumIndex(&dstTval) && isNumIndex(&newValue))
    {
    tmp = asNumIndex(&dstTval);
    newv = asNumIndex(&newValue);
    newv &= 1;
    mask <<= index1.u.Int;
    tmp |= mask;
    if (newv == 0) tmp ^= mask;
    dstTval.Tag = TYNUM;
    dstTval.u.Int = tmp;
    return(dstTval);
    }
    
return(gCP->TObject_ERROR_INVALID_ARGLIST);
}

