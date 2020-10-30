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

#define _C_FCONIO
#define _SMARTBASE  
#if 0
FConio.c

This source file contains some of the cProcedures which implement the console io 
functions supported by the SmartLisp interpreter, and also functions called internally
for console io and data formatting.


AUTHORS:            Michael F. Korns

MODIFICATIONS:  

CHANGE HISTORY
Version	Date		Who		Change
		11/26/2007	TMay	modified FConio_fseek and FConio_fresize to allow use of large file support added in SysGlue
		12/15/2006	TMay	modified FConio_saveObject and FConio_loadObject accept 
							TVAL as well as Object targets.
		12/9/2006	TMay	modified FConio_loadObject to accept (loadObject bufferPtr) form.
							Also see TObject_loadFile changes.
		12/9/2006	TMay	modified FConio_saveObject to accept (saveObject object bufferLen bufferPtr)
							form. Also see TObject_saveFile changes.

#endif

#include "fconio.h"
#include "tobject.h"
#include "tstring.h"
#include "tsymbol.h"
#include "tlambda.h"
#include "fproc.h"
#include "tbytevec.h"
#include "twkspace.h"
#include "fmath1.h"
#include "fcompile.h"
#include "futil2.h"
#include "fmake.h"
#include "tdatabas.h"
#include "flisp.h"
#include "terror.h"
#include "fvmscpt.h"
#include "tcpx.h"
#include "fconvert.h"

							/*------------------------------------------------------------*/
/* Array of file handles for use with posix file io functions */
/*------------------------------------------------------------*/
#define	_POSIX 0
#include "stdio.h"
#define	MAXFILEHANDLES	100
#define	MAXFILENAMESIZE	256
#define MAXSLEEPSBEFORIOEERROR 1000
NUM		gFileIOHandles[MAXFILEHANDLES+1];
CHAR 	gFileIONames[MAXFILEHANDLES+1][MAXFILENAMESIZE+1];
NUM     gFileIOTypes[MAXFILEHANDLES+1];
NUM     gFileIOUsers[MAXFILEHANDLES+1];
NUM     gFileIOContexts[MAXFILEHANDLES+1];
NUM     gMaxIOFiles = MAXFILEHANDLES;		


extern  BOLE FFloat_NUMCHECK        (LpXCONTEXT gCP,LpTHREAD gTP,REAL x);

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_Init

Initialize the console io and string formatting portion of the SmartLisp function 
library.  

#endif

TVAL FConio_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
NUM					fileID;
StartFrame
TSymbol*            aSymbol  = (TSymbol*)_TObject_FrameTobj((TSymbol*)&aSymbol);
TSymbol*            alsoSymbol  = (TSymbol*)_TObject_FrameTobj((TSymbol*)&alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FConio_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FConio_Initialized = TRUE;

#if _POSIX
gCP->_Host_Openf         = (LpHOST_OPENF)(FConio_IO_fopen);
gCP->_Host_Readf         = (LpHOST_READF)(FConio_IO_fread);
gCP->_Host_Writef        = (LpHOST_WRITEF)(FConio_IO_fwrite);
gCP->_Host_Seekf         = (LpHOST_SEEKF)(FConio_IO_fseek);
gCP->_Host_Resizef       = (LpHOST_RESIZEF)(FConio_IO_fresize);
gCP->_Host_Closef        = (LpHOST_CLOSEF)(FConio_IO_fclose);

for (fileID=0;fileID<gMaxIOFiles; ++fileID)
	{
	gFileIOHandles[fileID] = 0;
	gFileIONames[fileID][0] = 0;
	gFileIOTypes[fileID] = 0;
	gFileIOUsers[fileID] = 0;
	gFileIOContexts[fileID] = 0;
	}
#endif

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"parse",(LpFUNC)&FConio_Parse);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"run",(LpFUNC)&FConio_RunScript);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"runScript",(LpFUNC)&FConio_RunScript);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"display",(LpFUNC)&FConio_Display);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"error",(LpFUNC)&FConio_Errorcr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"newline",(LpFUNC)&FConio_Newline);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"silent",(LpFUNC)&FConio_Silent);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"writeln",(LpFUNC)&FConio_Writeln);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"appendWriteln",(LpFUNC)&FConio_AppendWriteln); 
ExitOnError(*ec);																				 
alsoSymbol  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"print");
TSymbol_SetGlobalValue(gCP,gTP,alsoSymbol, aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileOpen",(LpFUNC)&FConio_fopen);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileClose",(LpFUNC)&FConio_fclose);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileErase",(LpFUNC)&FConio_ferase);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"saveObject",(LpFUNC)&FConio_saveObject);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"loadObject",(LpFUNC)&FConio_loadObject);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileReadRecord",(LpFUNC)&FConio_FileReadRecord);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"importTab",(LpFUNC)&FConio_ImportTAB);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"exportTab",(LpFUNC)&FConio_ExportTAB);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileSeek",(LpFUNC)&FConio_fseek);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileResize",(LpFUNC)&FConio_fresize);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileRead",(LpFUNC)&FConio_fread);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileWrite",(LpFUNC)&FConio_fwrite);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fdisplay",(LpFUNC)&FConio_FDisplay);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileDisplay",(LpFUNC)&FConio_FDisplay);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fwriteln",(LpFUNC)&FConio_FWriteln);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fileWriteln",(LpFUNC)&FConio_FWriteln);
ExitOnError(*ec);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_pathName

The FConio_pathName function returns the path name as a C string.

Note:   The pathname may be returned as an empty string depending 
		upon the contents of the file name. The file name is never
		returned. The file name and path name are never appended.

#endif

TVAL FConio_pathName(LpXCONTEXT gCP, LpTHREAD gTP, NUM maxLen, LpCHAR buf, TVAL pathName, TVAL fileName)
{
#if _DETECTABSPATH
char ch;
signed char aTag = fileName.Tag;
#endif			// _DETECTABSPATH

LpCHAR apFileName = NULL;
StartFrame
DeclareTVAL(ec);
EndFrame

#if _DETECTABSPATH
if (aTag == TYTEXT)
	apFileName = fileName.u.Text;
else
if (aTag == TYSTRING)
	apFileName = FObject_GetStringPtr(gCP, gTP, asObject(&fileName));
else
if (aTag == TYSTRINGSUBSTR)
    apFileName = TStringSubstringT_GetStringPtr(gCP, gTP, fileName);

if (apFileName != NULL)
{	ch = *apFileName;
	if (ch == '/' || ch == '\\' || (*(apFileName + 1) == ':' &&
		((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'))))
	{	ec->Tag = TYTEXT;
		ec->u.Text[0] = '\0';
		buf[0] = '\0';
		FrameExit(*ec);
	}
}
#endif			// _DETECTABSPATH

/*  Return the path name as a C String. */
*ec = TObject_CnvToText(gCP, gTP, buf, maxLen, pathName);
ExitOnError(*ec);

/*  Return the path name as a TVAL. */
FrameExit(pathName);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_Display

The FConio_Display function displays the arguments on the Console Window.

Note:   The display procedure makes a special effort to detect and to
        drop the quotes which surround text strings before they are
        displayed on the Console.

#endif

TVAL FConio_Display(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 indexOf;
CHAR*               buf;
TVAL				parmv[50];
StartFrame
DeclareTVAL(ec);
DeclareTVAL(displayString);
DeclareTVAL(ret);
EndFrame
 
/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* If there is more than one argument, then append them together */
if (argc > 1)
{
	parmv[0].Tag = TYTEXT;
	parmv[0].u.Text[0] = 0;
	for (indexOf = 0; indexOf < argc; ++indexOf)
	{
		parmv[0] = *displayString;
		if ((argv[indexOf].Tag != TYSTRING) && (argv[indexOf].Tag != TYTEXT))
		{
			parmv[1] = argv[indexOf];
			parmv[2] = gCP->TObject_TRUE;
			parmv[1] = FConvert_ToString(gCP, gTP, 2, &parmv[1]);
			ExitOnError(parmv[1]);
		} 
		else
		{
			parmv[1] = argv[indexOf];
		}

		*displayString = FUtil2_Append(gCP, gTP, 2, parmv);
		ExitOnError(*displayString);
	}
} 
else
if (argc == 1)
{
	if ((argv[0].Tag != TYSTRING) && (argv[0].Tag != TYTEXT))
	{
		parmv[0] = argv[0];
		parmv[1] = gCP->TObject_TRUE;
		*displayString = FConvert_ToString(gCP, gTP, 2, parmv);
		ExitOnError(*displayString);
	}
	else
		*displayString = argv[0];
}
else
{
	FrameExit(gCP->TObject_TRUE);
}

/*  By now displayString contains a Text of String object. */

if (displayString->Tag == TYTEXT)
	buf = &displayString->u.Text[0];
else
if (displayString->Tag == TYSTRING)
	buf = CharArray(*displayString);
else
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Display the display string */
(*gCP->_Host_Display)((POINTER)gCP,gTP,(LpCHAR)&buf[0], 0);

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_Writeln

The FConio_Writeln function displays the arguments on the Console Window,
and terminates the display with a new line.

#endif

TVAL FConio_Writeln(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM                 indexOf;
CHAR*               buf;
TVAL				parmv[50];
StartFrame
DeclareTVAL(ec);
DeclareTVAL(displayString);
DeclareTVAL(ret);
EndFrame
 
/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* If there is more than one argument, then append them together */
if (argc > 1)
{
	parmv[0].Tag = TYTEXT;
	parmv[0].u.Text[0] = 0;
	for (indexOf = 0; indexOf < argc; ++indexOf)
	{
		parmv[0] = *displayString;
		if ((argv[indexOf].Tag != TYSTRING) && (argv[indexOf].Tag != TYTEXT))
		{
			parmv[1] = argv[indexOf];
			parmv[2] = gCP->TObject_TRUE;
			parmv[1] = FConvert_ToString(gCP, gTP, 2, &parmv[1]);
			ExitOnError(parmv[1]);
		} 
		else
		{
			parmv[1] = argv[indexOf];
		}

		*displayString = FUtil2_Append(gCP, gTP, 2, parmv);
		ExitOnError(*displayString);
	}
} 
else
if (argc == 1)
{
	if ((argv[0].Tag != TYSTRING) && (argv[0].Tag != TYTEXT))
	{
		parmv[0] = argv[0];
		parmv[1] = gCP->TObject_TRUE;
		*displayString = FConvert_ToString(gCP, gTP, 2, parmv);
		ExitOnError(*displayString);
	}
	else
		*displayString = argv[0];
}
else
{
	FrameExit(gCP->TObject_TRUE);
}

/*  By now displayString contains a Text of String object. */

if (displayString->Tag == TYTEXT)
	buf = &displayString->u.Text[0];
else
if (displayString->Tag == TYSTRING)
	buf = CharArray(*displayString);
else
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Display the display string */
(*gCP->_Host_Display)((POINTER)gCP,gTP,(LpCHAR)&buf[0], 1);

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FConio_Errorcr

The FConio_Errorcr function converts the first argument into an error value,
and displays the remaining arguments on the Console Window, terminating the 
display with a new line. The current process is terminated.

#endif

TVAL FConio_Errorcr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM                     indexOf;
LpCHAR                  buf = &gTP->TempBuffer[0];
StartFrame	
DeclareTVAL(ret);
EndFrame
 
/*  Convert the first argument into a error value. */

if (argc >= 1)
    {
    buf[0] = 0;
    *ret = FSmartbase_CnvToText(gCP,gTP,&buf[1],sizeof(gTP->TempBuffer)-1,argv[0]);
    buf[0] = '!';
    indexOf = strlen(buf);
    buf[indexOf] = '!';
    buf[++indexOf] = 0;
    *ret = TERROR(buf);
    }
else
    {
    *ret = TERROR("!userError!");
    }

/*  Display the remaining arguments on the console. */

indexOf = 1;
while (indexOf < argc)
    {
    FConio_sprint(gCP,gTP,sizeof(gTP->TempBuffer),&gTP->TempBuffer[0],argv[indexOf++]);
    FConio_printf(gCP,gTP,(LpCHAR)"%s",(LpCHAR)buf);
    }

if (argc > 1)
    FConio_printf(gCP,gTP,(LpCHAR)"%s",(LpCHAR)"\n");

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_Newline

The FConio_Newline function prints a newline character on the Console. 

#endif

TVAL FConio_Newline(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{

StartFrame
DeclareTVAL(ret);
EndFrame

if( argc > 0 && argv[0].Tag != TYVOID )
	FrameExit(TERROR("!arglist in FConio_Newline!"));
/* Intialize the return value */

*ret = TGVALUE("_eol");

/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)"", 1);

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FConio_Silent

The FConio_Silent function tells the SmartBase host system to run with
silent mode on or off. The interpretation of silent mode and its exact
function is a host implementation detail.

#endif

TVAL FConio_Silent(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
/*  We must have only one boolean argument. */

if (argc != 1)
	return(TERROR("!silent: Expecting a single argument!"));
    
if (argv[0].Tag != TYBOLE)
    return(TERROR("!silent: Expecting a Boolean argument!")); /*  Set the SmartBase silent mode. */

gCP->FSmartbase_SilentMode = argv[0].u.Bool;

return(argv[0]);
}



// RCA


/*--------------------------------------------------------------------------------------- */
#if 0
FConio_print

Display an atom in the SmartBase Console window. The FConio_print function 
converts an atom into an ascii string and displays it in the Console window. 
Present in all Lisp dialects, the FConio_print is the fundamental method for 
data display in SmartLisp.

#endif

TVAL        FConio_print(LpXCONTEXT gCP, LpTHREAD gTP, TVAL aThing)
{
NUM                     oldLen;
CHAR*                   buf;
HMChar                  theData;
StartFrame
DeclareTVAL(ret);
EndFrame


 
/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

theData = (HMChar)FMemory_New(gCP, gTP, _FSmartbase_MAXBUFFERLEN+100,TRUE);

buf = &atHMChar(theData,0);
oldLen = gCP->TObject_MaxOutputLen;
gCP->TObject_MaxOutputLen = _FSmartbase_MAXBUFFERLEN;

*ret = FConio_sprint(gCP,gTP,_FSmartbase_MAXBUFFERLEN,buf,aThing);
if (asBool(&ret) != TRUE)
    strcat((char *)buf," ... ");
    
(*gCP->_Host_Display)((POINTER)gCP,gTP,(char*)buf, 1);

FMemory_Free(gCP, gTP, (HMemory)theData);

gCP->TObject_MaxOutputLen = oldLen;

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_sprint

The FConio_sprint function converts an atom into an ascii string. Present in all 
Lisp dialects, the FConio_sprint is the fundamental method for ascii representation 
in SmartLisp.

#endif

TVAL        FConio_sprint(LpXCONTEXT gCP, LpTHREAD gTP, NUM maxLen, LpCHAR buf, TVAL aThing)
{
NUM                     size;
NUM						oldLen;
TVAL					result;
 
buf[0] = 0;
size = 0;
oldLen = gCP->TObject_MaxOutputLen;
gCP->TObject_MaxOutputLen = maxLen;
result = FConio_sprintn(gCP,gTP,buf,&size,aThing);
gCP->TObject_MaxOutputLen = oldLen;
return(result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_sprintn

The FConio_sprintn function appends an atom onto an ascii string. 
The FConio_sprintn function terminates when the size argument is greater than 
TObject_MaxOutputLen.

#endif

TVAL FConio_sprintn(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR buf,LpNUM size,TVAL aThing)
{
StartFrame
DeclareTVAL(ec);
EndFrame

/*  Quit if the output string is already too long */
 
if (!_VALIDTVAL(aThing))
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
	}
else
if (*size > gCP->TObject_MaxOutputLen)
	{ 
	FrameExit(gCP->TObject_FALSE);
	}

if (_TObject_TypePrint(asTag(&aThing)) != NULL)
    {
    *ec = (*((_TObject_TypePrint(asTag(&aThing)))))(gCP,gTP,aThing,size,buf);
    FrameExit(*ec);
    }
else
    {
    *ec = TObject_CnvToText(gCP,gTP,&buf[*size], *size, aThing);
    *size = strlen((char *)buf);
    FrameExit(*ec);
    }

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FConio_printf

The FConio_printf function is the function which prints a formatted string. 
This routine is passed a format string. By scanning the format string until 
a format code is encountered, information can be extracted to determine 
what argument is present on the stack and processing can be handled. 

#endif

TVAL FConio_printf(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR fmt, ...)
{
va_list         args;
NUM             i;
POINTER         argArray[_FSmartbase_MAXARGUMENTS];

/* Gather the variable arguments into an array. */

va_start(args,fmt);
for (i = 0; i < _FSmartbase_MAXARGUMENTS; i++)
    {
    argArray[i] = va_arg(args,POINTER);
    }
va_end(args);
    
/* Format the arguments into the destination string buffer. */

FConio_vsprintf(gCP,gTP,gTP->TempBuffer,fmt,&argArray[0]);
 
/* Display the destination string buffer. */

(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)gTP->TempBuffer, 0);

return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_vsprintf

The FConio_vsprintf function is the internal routine to do the conversion process. 
This routine is passed a format string. By scanning the format string until 
a format code is encountered, information can be extracted to determine 
what argument is present on the stack and processing can be handled. 

The FConio_vsprintf function accepts the following type specifiers in the format
string:

        %{flag}{width}{type}

        {flag}
            %-              LEFT JUSTIFY FLAG
            %               PAD WITH SPACES FLAG
            %0              PAD WITH ZEROS FLAG

        {width}
            nn              WIDTH OF OUTPUT FIELD

        {type}
            a               TVAL
            b               BOLE
            c               CHAR
            s               TEXT
            e               ERROR
            f               FLOAT
            h               HANDLE
            i               NUM
            l               LONG
            o               OBJ
            p               POINTER
            r               REAL
            d               SHORT
            t               TYPE
            x               STRING
            y               SYMBOL



  How the arguments are interpreted is derived from the format string.

#endif

BOLE FConio_vsprintf(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR dest,LpCHAR fmt, LpPOINTER argument)
{
 BOLE           left_justify;       /* FLAG TO INDICATE LEFT JUSTIFICATION  */
 register LpCHAR      format;       /* POINTER TO THE FORMAT STRING         */
 register CHAR             c;       /* CURRENT CHAR UNDER INSPECTION        */
 register NUM          width;       /* FIELD WIDTH AS SPECIFIED BY FORMAT   */
 register NUM        numleft;       /* # OF CHARs TO OUTPUT                 */
 register NUM      num_count;       /* NUMBER OF CHARs ACTUALLY OUTPUT      */
 register LpCHAR  tempformat;       /* TEMP POINTER TO THE FORMAT STRING    */
 CHAR             tempbuffer[_FSmartbase_MAXBUFFERLEN];/* TEMP WORK SPACE   */
 LpCHAR              tempptr;       /* TEMP POINTER TO THE WORK SPACE       */

 CHAR               fillchar;       /* FILL CHAR VALUE                      */

 BOLE                     bx;       /* TEMPORARY WORK SPACE                 */
 CHAR                     cx;       /* TEMPORARY WORK SPACE                 */
 NUM                      nx;       /* TEMPORARY WORK SPACE                 */
 REAL                     rx;       /* TEMPORARY WORK SPACE                 */
 
 StartFrame
 DeclareOBJ(TObject,so);			/* TEMPORARY WORK SPACE                 */
 DeclareOBJ(TString,sx);			/* TEMPORARY WORK SPACE                 */
 DeclareOBJ(TSymbol,sy);			/* TEMPORARY WORK SPACE                 */
 DeclareTVAL(tmpTval);				/* TEMPORARY WORK SPACE                 */
 EndFrame
 /* 
 ** Initialize local variables. 
 */
    
 num_count = 0;                     /* SET # CHARs OUTPUT TO ZERO           */
 format = fmt;                      /* POINT TO THE FORMAT STRING           */

 /* 
 ** SCAN FORMAT STRING UNTIL REACHING THE NIL TERMINATOR. 
 */
 while ((c = *format++))              /* SCAN FORMAT STRING UNTIL NIL         */
  {
   /* 
   ** CHECK FOR A FORMAT SPECIFIER IN THE FORMAT STRING. 
   */
   if (c != '%')                    /* IS THIS NOT A FORMAT SPECIFIER ?     */
    {
     dest[0] = c;                   /* OUTPUT THE FORMAT CHAR               */
     dest[1] = 0;                   /* OUTPUT THE FORMAT CHAR               */
     ++dest;                        /* OUTPUT THE FORMAT CHAR               */
     ++num_count;                   /* PROMOTE COUNT OF OUTPUT CHARs        */
    }
   else
    {
     tempformat     = format;       /* SAVE CURRENT LOCATION                */
     left_justify   = FALSE;        /* RESET LEFT JUSTIFICATION FLAG        */
     fillchar       = ' ';          /* RESET FILL CHAR                      */
 
     /* 
     ** SCAN FORMAT SPECIFIER FOR A FLAG CHARACTER. 
     */
     flagloop:
     switch(c = *format++)          /* RECOGNIZE FORMAT FLAGS               */
      {
       case '-':                    /* MANAGE LEFT JUSTIFICATION FLAG       */                  
        left_justify = TRUE;        /* SET LEFT JUSTIFICATION ON            */      
        goto flagloop;

       case '0':                    /* MANAGE ZERO FILL FLAG                */
        fillchar  = '0';            /* SET FILL CHAR TO '0'                 */
        goto flagloop;

       case ' ':                    /* MANAGE FILL WITH SPACES FLAG         */
        fillchar  = ' ';            /* SET FILL CHAR TO ' '                 */
        goto flagloop;

       default:                     /* MANAGE NOT A FLAG CONDITION          */
        format--;                   /* NEED TO BACK UP IF UNRECOGNIZED      */
        break;                      /* CONTINUE WITH NEXT PARSING STEP      */
      }

     /* 
     ** COMPUTE OUTPUT FIELD WIDTH IN FLAG SPECIFIER. 
     */
     width = 0;                     /* RESET THE OUTPUT FIELD WIDTH         */
     while (ISDIGIT((NUM)*format))       /* ANY DIGIT IS PART OF OPTIONAL WIDTH  */ 
      {
       width *= 10;                 /* SHIFT PREVIOUS WIDTH DIGITS BY 10    */
       width += (*format++)-'0';    /* ADD NEW WIDTH DIGIT                  */
      }
     numleft = width;               /* SET THE OUTPUT FIELD WIDTH           */
            
     /* 
     ** RECOGNIZE TYPE WITHIN FORMAT SPECIFIER. 
     */
     switch(c = *format++)          /* RECOGNIZE FORMAT TYPE                */
      {
       case 'a':                    /* MANAGE TVAL TYPE                     */
        *tmpTval = *(TVAL*)(*argument);
        TObject_CnvToText(gCP, gTP, tempbuffer, sizeof( tempbuffer), *tmpTval);
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'b':                    /* MANAGE BOLE TYPE                     */
        bx = *((LpBOLE)*argument);  /* LOAD ARGUMENT INTO TEMPORARY         */
        if (bx)                     /* IS THE ARGUMENT TRUE ?               */              
            strcpy((char *)tempbuffer,"true");/* CONVERT BOLE TO STRING         */
        else        
            strcpy((char *)tempbuffer,"false");/* CONVERT BOLE TO STRING            */
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'c':                    /* MANAGE CHAR TYPE                     */
        cx = *((LpCHAR)*argument);  /* LOAD ARGUMENT INTO TEMPORARY         */
        tempbuffer[0] = cx;         /* CONVERT CHAR TO STRING               */              
        tempbuffer[1] = 0;          /* CONVERT CHAR TO STRING               */              
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'd':                    /* MANAGE SHORT TYPE                    */
        nx = *((LpSHORT)*argument); /* LOAD ARGUMENT INTO TEMPORARY         */              
        sprintf((char *)tempbuffer,INTFORMAT,nx);/* CONVERT NUM TO STRING           */      
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'h':                    /* MANAGE HANDLE TYPE                   */                  
        nx = *((LpNUM)*argument);  /* LOAD ARGUMENT INTO TEMPORARY         */              
        sprintf((char *)tempbuffer,INTFORMAT,nx);/* CONVERT NUM TO STRING           */      
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'l':                    /* MANAGE LONG TYPE                     */                  
       case 'i':                    /* MANAGE NUM TYPE                      */                  
        nx = *((LpNUM)*argument);   /* LOAD ARGUMENT INTO TEMPORARY         */              
        sprintf((char *)tempbuffer,INTFORMAT,nx);/* CONVERT NUM TO STRING           */
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'p':                    /* MANAGE POINTER TYPE                  */                  
        nx = *((LpNUM)*argument);   /* LOAD ARGUMENT INTO TEMPORARY         */              
        sprintf((char *)tempbuffer,POINTERFORMAT,(void *)nx);/* CONVERT NUM TO STRING       */      
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'f':                    /* MANAGE FLOAT TYPE                    */                  
       case 'r':                    /* MANAGE REAL TYPE                     */                  
        rx = *((LpREAL)*argument);  /* LOAD ARGUMENT INTO TEMPORARY         */              
        TObject_sprintReal(gCP,gTP,(char *)tempbuffer,rx);		/* CONVERT REAL TO STRING  */
        FConio_strblk(gCP,gTP,(LpPOINTER)&tempptr,tempbuffer);  /* DROP LEADING SPACES     */
        strcpy((char *)tempbuffer,(char *)tempptr);				/* DROP LEADING SPACES     */
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'e':                    /* MANAGE K&R ERROR STRING TYPE         */                  
       case 's':                    /* MANAGE K&R STRING TYPE               */                  
        strncpy((char *)tempbuffer,*argument, sizeof(tempbuffer) - 1);/* COPY ARGUMENT STRING  */
        tempbuffer[sizeof(tempbuffer) - 1] = 0;
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'x':                    /* MANAGE STRING TYPE                   */                  
        sx = *((TString**)*argument);/* LOAD ARGUMENT INTO TEMPORARY        */              
        strcpy((char *)tempbuffer,(char *)&atHMChar(sx->itsCString,0));/* COPY STRING TEXT*/
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'y':                    /* MANAGE SYMBOL TYPE                   */                  
        sy = *((TSymbol**)*argument);/* LOAD ARGUMENT INTO TEMPORARY        */
        if (sy->itNeedsBars == TRUE)
            {
            strcpy((char *)tempbuffer,"|");             
            strcat((char *)tempbuffer,(char *)&atHMChar(sy->itsCString,0));
            strcat((char *)tempbuffer,"|");             
            }
        else
            {
            strcpy((char *)tempbuffer,(char *)&atHMChar(sy->itsCString,0));
            }
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 'o':                    /* MANAGE OBJ TYPE                      */                  
        tmpTval->u.Object = *((TObject**)*argument);/* LOAD ARGUMENT         */  
        tmpTval->Tag = tmpTval->u.Object->itsObjectType;
        TObject_CnvToText(gCP, gTP, tempbuffer, sizeof( tempbuffer), *tmpTval);
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       case 't':                    /* MANAGE TYPE TYPE                     */                  
        tmpTval->u.Type = *((LpTYPE)*argument);/* LOAD ARGUMENT              */  
        tmpTval->Tag = TYTYPE;       /* LOAD ARGUMENT TYPE                   */
        TObject_CnvToText(gCP, gTP, tempbuffer, sizeof( tempbuffer), *tmpTval);
        ++argument;                 /* PROMOTE ARGUMENT LIST POINTER        */      
        break;

       default:                     /* MANAGE NOT A FLAG CONDITION          */
        tempbuffer[0] = '%';        /* OUTPUT A % CHAR                      */
        tempbuffer[1] = 0;          /* OUTPUT A % CHAR                      */
        format = tempformat;        /* NEED TO BACK UP IF TYPE UNRECOGNIZED */
        width = 0;                  /* RESET FIELD WIDTH IF TYPE UNKNOWN    */
        break;                      /* CONTINUE WITH NEXT PARSING STEP      */
      }

     /* 
     ** PERFORM RIGHT OR LEFT FILL. 
     */
     nx = strlen((char *)tempbuffer);       /* GET TEMPORARY BUFFER LENGTH ?        */
     if (width > nx)                /* IS RIGHT OR LEFT FILL NECESSARY ?    */
      {
       numleft = width - nx;        /* COMPUTE FILL AMOUNT                  */
       if (left_justify)            /* SHOULD WE LEFT JUSTIFY ?             */
        {
         strcpy((char *)dest,(char *)tempbuffer);/* OUTPUT THE CONVERTED CHARs          */
         dest += nx;                /* PROMOTE THE OUTPUT POINTER           */
         while (numleft-- > 0)      /* RIGHT FILL WITH FILL CHARs           */
          {
           *dest++ = fillchar;      /* RIGHT FILL WITH FILL CHARs           */
           *dest = 0;               /* RIGHT FILL WITH FILL CHARs           */
          }
         num_count += width;        /* PROMOTE OUTPUT CHAR COUNT            */
        }
       else
        {
         while (numleft-- > 0)      /* LEFT FILL WITH FILL CHARs            */
          {
           *dest++ = fillchar;      /* LEFT FILL WITH FILL CHARs            */
           *dest = 0;               /* LEFT FILL WITH FILL CHARs            */
          }
         strcpy((char *)dest,(char *)tempbuffer);/* OUTPUT THE CONVERTED CHARs          */
         dest += nx;                /* PROMOTE THE OUTPUT POINTER           */
         num_count += width;        /* PROMOTE OUTPUT CHAR COUNT            */
        }
      }
     else
      {
       strcpy((char *)dest,(char *)tempbuffer); /* OUTPUT THE CONVERTED CHARs           */
       dest += nx;                  /* PROMOTE THE OUTPUT POINTER           */
       num_count += nx;             /* PROMOTE OUTPUT CHAR COUNT            */
      }
      
    } /* end of else */
  } /* end of while */
        
 FrameExit(TRUE);                      /* RETURN GOOD CONDITION                */
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_sprintf

The FConio_sprintf function is the internal routine to do the conversion process. 
This routine is passed a format string. By scanning the format string until 
a format code is encountered, information can be extracted to determine 
what argument is present on the stack and processing can be handled. 

#endif

BOLE FConio_sprintf(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR dest,LpCHAR fmt, ...)
{
va_list         args;
NUM             i;
POINTER         argArray[_FSmartbase_MAXARGUMENTS];

/* Gather the variable arguments into an array. */

va_start(args,fmt);
for (i = 0; i < _FSmartbase_MAXARGUMENTS; i++)
    {
    argArray[i] = va_arg(args,POINTER);
    }
va_end(args);
    
/* Format the arguments into the destination string buffer. */

return(FConio_vsprintf(gCP,gTP,dest,fmt,&argArray[0]));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_strblk

The FConio_strblk function returns a pointer to the first non-blank character in 
the specified string. If none is found, a value of false is returned.

#endif

BOLE FConio_strblk(LpXCONTEXT gCP, LpTHREAD gTP, register LpPOINTER p, register LpCHAR s)
{

 if(gTP == NULL)
    return(FALSE);
 *p = (POINTER)s;                       /* SET INITIAL RESULT POINTER       */
 while (ISSPACE((NUM)**p)) (*p)++;           /* STOP AT FIRST NON-BLANK CHARACTER*/

 if (**p == 0)                          /* WERE ALL CHARS WHITE SPACE ?     */
    return(FALSE);                      /* YES, THEN RETURN BAD CONDITION   */
 else
    return(TRUE);                       /* NO, THEN RETURN GOOD CONDITION   */
}


/*--------------------------------------------------------------------------------------- */
#if 0
FConio_saveObject

This function saves the specified target and the recursive closure of all 
the objects on which the indicated target depends to a file, predefined buffer or 
automatically created bytevector. The record created contains a record header that
identifies the type of content saved and allows the records to be loaded using
the FConio_loadObject function.

Signatures:
args: fileID, target, spaceUsageSwitch
args: fileID, target
If the spaceUsage switch is true the record is saved to the specified file.
If the spaceUsage switch if false then the record is not saved and only the
size of the record that would have been written is returned. 
If the spaceUsageSwitch is not supplied then it defaults to true.
This form of the function returns the filespace required to save the object closure.

args: target
If a argument is passed then the saveObject function will return a Byte Vector 
containing the record in the same form it would have been written to disk.

args: target, bufferLength, bufferPtr
This form of the function saves the record into a specified buffer. Normally, this buffer
is a section of memory not managed by smtbase.
This form of the function returns the length of the record written into the buffer.

#endif

TVAL    FConio_saveObject(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(aString);
DeclareTVAL(stringCmd);
EndFrame


/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

*stringCmd = TFUNCTION(FMake_String);

if(argc == 0)
	{
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!saveObject: Expecting at least one argument!");
	FrameExit(*ret);
	}
else
if(argc == 3 && (argv[2].Tag != TYPOINTER && argv[2].Tag != TYBOLE))
	{
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!saveObject: Third argument must be Boolean or Pointer!");
	FrameExit(*ret);
	}
if ((argc == 3) && (argv[2].Tag == TYBOLE) &&  (argv[2].u.Bool != 0) && !isNumIndex(&argv[0]))
	{
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!saveObject: First argument must be a fileID!");
	FrameExit(*ret);
	}
else
if (argc == 2 && !isNumIndex(&argv[0]))
	{
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!saveObject: First argument must be a fileID!");
	FrameExit(*ret);
	}

/* Convert immediate tval of type TYTEXT to string object for backward compatibility */
if (argc > 0 && argv[0].Tag == TYTEXT)
	{
		/* copy text into local string object assigned to argv[0] - remember that */
		/* text may be immediate but we want to treat it as an object in most cases.*/
		*aString = FSmartbase_Eval(gCP,gTP,*stringCmd,1,argv[0]);
		ExitOnError(*aString);
		argv[0] = *aString;
	}

if (argc > 1 && argv[1].Tag == TYTEXT)
	{
	/* copy text into local string object assigned to argv[0] - remember that */
	/* text may be immediate but we want to treat it as an object in most cases.*/
	*aString = FSmartbase_Eval(gCP,gTP,*stringCmd,1,argv[1]);
	ExitOnError(*aString);
	argv[1] = *aString;
	}

*ret = TObject_SaveFile(gCP,gTP,argc, argv);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_loadObject
 
This function does an object oriented load from the specified file and optionally 
assigns the result to the globalvalue of an indicated symbol.
    
Examples:

        (loadObject fileID)

        (loadObject vector)


Note:   If the argument is a fileID, then we load the object from the
        specified file (starting at the current file position).
    
Note:   If the argument is a byte vector, then we load the object from the
        specified byte vector.
    
#endif

TVAL    FConio_loadObject(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(loadTval);
EndFrame

/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If the argument is void, return it as is. */

if  ((argc == 1) && (argv[0].Tag == TYVOID))
    {
    FrameExit(gCP->Tval_VOID);
    }

/*  If no user escape was requested, then continue. */

if  ((argc != 1) || 
     (!isNumIndex(&argv[0]) && (argv[0].Tag != TYBYTEVECTOR) && argv[0].Tag != TYPOINTER))
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
else
    {
    *loadTval = TObject_LoadFile(gCP,gTP,1,argv);
    ExitOnError(*loadTval);
    }
 
FrameExit(*loadTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_ferase

The FConio_ferase function attempts to erase the specified file as identified by the path and
file name argument whose interpretation is system specific at this juncture. Returns TRUE on success.

Arguments:

    filePathName  :   file identifier.

Returns:

    TRUE if successful.
    Error if a problem occurs. 

#endif

TVAL FConio_ferase(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM         fileID;
NUM         intResult;
LpCHAR		fname;


/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  Validate the arguments */

if (argc < 1)
	return(TERROR("!ferase: Missing arguments!"));

if (argc > 1)
	return(TERROR("!ferase: Too many arguments!"));

/* Obtain a pointer to the file name. */

if (argv[0].Tag == TYTEXT)
	{
    fname = argv[0].u.Text;
	}
else
if (argv[0].Tag == TYSTRING)
	{
    fname = CharArray(argv[0]);
	}
else
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
	{
    fname = SymbolArray(argv[0]);
	}
else
	{
	return(TERROR("!ferase: filename must be a text, string, or symbol type!"));
	}

/* Open the file then erase it */

fileID = (*gCP->_Host_Openf)((POINTER)gCP,gTP,(char *)fname, 1, 0);
if (fileID < 0)
	{
	return(TERROR("!ferase: problem erasing the file!"));
	}
intResult = (*gCP->_Host_Closef)((POINTER)gCP,gTP, fileID, 0);
if (intResult != 0)
	{
	return(TERROR("!ferase: problem erasing the file!"));
	}
else
	{
 	return(gCP->TObject_TRUE);
	}
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_fclose

The FConio_fclose function attempts to close an open file as identified by the 4-byte integer
argument whose interpretation is system specific at this juncture. Returns TRUE on success.

Arguments:

    fileID  :   NUM file identifier.
    option  :   NUM integer option (0 = erase file after close, 1 = dont erase file after close).

Returns:

    NUM 0 successful.
    Error if a problem occurs. 

#endif

TVAL FConio_fclose(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM         intResult;
/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  Validate the arguments */

if (argc < 2)
	return(TERROR("!fclose: Missing arguments!"));

if (argc > 2)
	return(TERROR("!fclose: Too many arguments!"));

if (!isNumIndex(&argv[0]))
	return(TERROR("!fclose: Expecting argument 1 to be a number!"));

if (!isNumIndex(&argv[1]))
	return(TERROR("!fclose: Expecting argument 2 to be a number!"));

/*  If no errors, close the file */

intResult = (*gCP->_Host_Closef)((POINTER)gCP,gTP, asNumIndex(&argv[0]), asNumIndex(&argv[1]));

if (intResult <  0)
	{
	return(TERROR("!fclose: Unable to close the file!"));
	}
else
	{
 	return(gCP->TObject_TRUE);
	}
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_fopen

The FConio_fopen function attempts to create/open a file as specified in string passed.

Arguments:

filename            The name of the file to be opened. This name may include a full path 
                    specification.

TYNUM  mode         The mode switch. A value of zero (0) requests that only an existing file be 
                    opened. A value of one (1) indicates that a new file should be created, or an 
                    existing file overwritten. A value of two (2), valid for database files only, 
                    indicates that an existing database file be opened for read only transactions 
                    shared with other users.

TYNUM  type         The type switch which determines the type of file to be opened.
                        (0) requests a Smartscript text file .
                        (1) requests a Spreadsheet binary file. 
                        (2) requests a Smarttable binary file. 
                        (3) requests a Workspace binary file. 
                        (4) requests an Object binary file be opened. 
                        (5) requests an Object Database file be opened. 

return(NUM)         The non-zero integer file identifier or zero to indicate an  error condition.

#endif

TVAL FConio_fopen(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM             fileID;
NUM             mode;
NUM             type;
CHAR            name[1024];
CHAR            fname[1024];
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

if (argc == 3 && 
    asTag(&argv[0]) == TYVOID && 
    isNumIndex(&argv[1]) && 
    isNumIndex(&argv[2]))
    {
    /*  If the file name is #void then the _Host_Openf is required to allow */
    /*  the user to select one. */
    
    name[0] = 0;
    mode = asNumIndex(&argv[1]);
    type = asNumIndex(&argv[2]);
    }
else
if (argc == 3 && isNumIndex(&argv[1]) && isNumIndex(&argv[2]))
    {
    mode = asNumIndex(&argv[1]);
    type = asNumIndex(&argv[2]);
    /* Append the file name to the _saveTypes variable (if required) */
    /* Note: Database and ObjectRepository files are appended at new time. */
	*ret = FConio_pathName(gCP, gTP, sizeof(name)-1, name, gCP->TSymbol_Path->itsGlobalValue, argv[0]);
    if (isERROR(ret)) goto CleanUp;
    *ret = TObject_CnvToText(gCP,gTP,fname, sizeof(fname)-1, argv[0]);
    if (isERROR(ret)) goto CleanUp;
	/* Append the file name only if this is NOT a child repository. */
	if (mode == 3)
		strcpy(name,fname);
	else
		strcat(name,fname);
    }
else
if (argc == 2 && isNumIndex(&argv[0]) && isNumIndex(&argv[1]))
    {
    /*  If no file name is specified then the _Host_Openf is required to allow */
    /*  the user to select one. */
    
    name[0] = 0;
    mode = asNumIndex(&argv[0]);
    type = asNumIndex(&argv[1]);
    }
else
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto CleanUp;
    }

fileID = (*gCP->_Host_Openf)((POINTER)gCP,gTP,(char*) name, mode, type);

/*  If file not found in path, search in current directory. */

if ((mode == 0) && (fileID == 0))
    fileID = (*gCP->_Host_Openf)((POINTER)gCP,gTP,(char *)fname, 0, 0);


if (fileID > 0)
    {
    ret->u.Int = fileID;
    ret->Tag = TYNUM;
    }
else
	{
	/* Print out an error message */
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!fileOpen: Cannot open file %s!",fname);
	}


CleanUp:
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_fwrite

The FConio_fwrite function attempts to write the indicated number of bytes from the specified
bytevector to the current file position.

Arguments:

    [0] fileID  :   NUM file identifier.
    [1] offset  :   NUM integer offset specifying the start of bytes to write.
    [2] length  :   NUM integer length specifying the number of bytes to write.
    [3] vector  :   ByteVector for the data to be written.

Returns:

    NUM     number of bytes written.
    Error   if a problem occurs. 

#endif

TVAL FConio_fwrite(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM             fileID;
NUM             intResult;
NUM             intOffset;
NUM             intLength;
NUM             maxLength;
char*           dataPtr;
CHAR			buf[2];
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

/*  Manage the full case where vector, offset, and length are all specified.     */

if (argc == 4 && isNumIndex(&argv[0]) && isNumIndex(&argv[1]) && isNumIndex(&argv[2]) && asTag(&argv[3]) == TYBYTEVECTOR)
    {
    /*  Extract the length and offset and pointer to the ByteVector data. */
    
    fileID = asNumIndex(&argv[0]);
    intOffset = asNumIndex(&argv[1]);
    intLength = asNumIndex(&argv[2]);
    
    /*  Extract the max length and a pointer to the data. */
    
    switch (argv[3].Tag)
        {
        case TYBYTEVECTOR:
            maxLength = ByteVector(argv[3])->itsMaxItemIndex;   
            dataPtr = (char*)&ByteArray(argv[3])[intOffset];
            break;
        
        /* NOTE: These cases will never be reached */
        case TYSTRING:
            maxLength = strlen((char*)CharArray(argv[3]));  
            dataPtr = (char*)&CharArray(argv[3])[intOffset];
            break;
        
        case TYCHAR:
            maxLength = 1;  
            dataPtr = (char*)&argv[3].u.Char;
            break;
        
        case TYTEXT:
            maxLength = MAXAISWORDTEXTLEN-1; 
            dataPtr = (char*)&argv[3].u.Text[intOffset];
            break;
        
        default:
            FrameExit(TERROR("!fileWrite: invalid write buffer data type on file write attempt!"));
            break;
        }

    
    /* Check to make sure all lengths fit within the ByteVector before writing data from the vector. */
        
    if ((maxLength >= (intOffset + intLength)) && (intLength > 0) && (intOffset >= 0))
        {   
        intResult = (*gCP->_Host_Writef)((POINTER)gCP,gTP,fileID, intLength, dataPtr);
		if (intResult < 0)
			{
			FrameExit(TERROR("!fileWrite: i/o error occurred on file write attempt!"));
			}
		else
			{
			ret->u.Int = intLength;
			ret->Tag = TYNUM;
			}
        }
    else
		{
		FrameExit(TERROR("!fileWrite: invalid ByteVector length or offset on file write attempt!"));
		}
    }
else
/*  Manage the full case where only pointer, and length are all specified.     */

if (argc == 3 && isNumIndex(&argv[0]) && isNumIndex(&argv[1]) && isNumIndex(&argv[2]))
    {
    /*  Extract the length and pointer to the data. */
    
    fileID = asNumIndex(&argv[0]);
    intOffset = asNumIndex(&argv[1]);
    intLength = asNumIndex(&argv[2]);
    
    intResult = (*gCP->_Host_Writef)((POINTER)gCP,gTP,fileID, intLength, (char*)intOffset);
	if (intResult < 0)
		{
		FrameExit(TERROR("!fileWrite: i/o error occurred on file write attempt!"));
		}
	else
		{
		ret->u.Int = intLength;
		ret->Tag = TYNUM;
		}
    }
else
/*  Manage the optional case where only the data object specified.   */

if (argc == 2 && isNumIndex(&argv[0]))
    {
    /*  Extract the file Identifier and default the offset to zero. */
    
    fileID = asNumIndex(&argv[0]);
    intOffset = 0;

    /*  Extract the length and a pointer to the data. */
    
    switch (argv[1].Tag)
        {
        case TYBYTEVECTOR:
            intLength = ByteVector(argv[1])->itsMaxItemIndex;   
            dataPtr = (char*)&ByteArray(argv[1])[0];
            break;
        
        case TYSTRING:
            intLength = strlen((char*)CharArray(argv[1]));  
            dataPtr = (char*)&CharArray(argv[1])[0];
            break;
        
        case TYCHAR:
            intLength = 1;  
            buf[0] = argv[1].u.Char;
			buf[1] = 0;
            dataPtr = (char*)&buf[0];
            break;
        
        case TYTEXT:
            intLength = strlen(&argv[1].u.Text[0]); 
            dataPtr = (char*)&argv[1].u.Text[0];
            break;
        
        case TYSTRINGSUBSTR:
            intLength = SubLen(argv[1]);
            dataPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
            if (dataPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
            break;
        
        default:
            FrameExit(TERROR("!fileWrite: invalid write buffer data type on file write attempt!"));
            break;
        }

    
    /* Check to make sure all lengths fit within the ByteVector before writing data from the vector. */
        
    intResult = (*gCP->_Host_Writef)((POINTER)gCP,gTP,fileID, intLength, dataPtr);
	if (intResult < 0)
		{
		FrameExit(TERROR("!fileWrite: i/o error occurred on file write attempt!"));
		}
	else
		{
		ret->u.Int = intLength;
		ret->Tag = TYNUM;
		}
    }
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_fread

The FConio_fread function attempts to read the indicated number of bytes.

Arguments:

    [0] fileID  :   NUM file identifier.
    [1] offset  :   NUM integer offset specifying the start of bytes to read.
    [2] length  :   NUM integer length specifying the number of bytes to read.
    [3] vector  :   ByteVector for the data to be read.

Returns:

    vector  The ByteVector containing the bytes just read.
    Error   if a problem occurs. 

#endif

TVAL FConio_fread(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM             intResult;
NUM             intOffset;
NUM             intLength;
NUM             fileID;
NUM             currentFPos;
NUM             eofFPos;
StartFrame
DeclareTVAL(vector);
DeclareTVAL(tmp);
EndFrame

/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

/*  Manage the full case where vector, offset, and length are all specified.     */

if (argc == 4 && isNumIndex(&argv[0]) && isNumIndex(&argv[1]) && isNumIndex(&argv[2]) && asTag(&argv[3]) == TYBYTEVECTOR)
    {
    /*  Extract the length and offset and pointer to the ByteVector data. */
    
    fileID = asNumIndex(&argv[0]);
    intOffset = asNumIndex(&argv[1]);
    intLength = asNumIndex(&argv[2]);
    *vector = argv[3];
    
    /* Check to make sure all lengths fit within the ByteVector before reading data into the vector. */
        
    if ((ByteVector(argv[3])->itsMaxItemIndex >= (intOffset + intLength)) && (intLength > 0) && (intOffset >= 0))
        {   
        intResult = (*gCP->_Host_Readf)((POINTER)gCP,gTP,fileID, intLength, (char*)&ByteArray(argv[3])[intOffset]);
        }
    else
        intResult = -1;
    

    if (intResult < 0)
        {
        FrameExit(gCP->TObject_ERROR_INVALID);
        }
    else
        {
        FrameExit(*vector);
        }
    }
else
/*  Manage the full case where only pointer, and length are all specified.     */

if (argc == 3 && isNumIndex(&argv[0]) && isNumIndex(&argv[1]) && isNumIndex(&argv[2]))
    {
    /*  Extract the length and pointer to the data. */
    
    fileID = asNumIndex(&argv[0]);
    intOffset = asNumIndex(&argv[1]);
    intLength = asNumIndex(&argv[2]);
    
    intResult = (*gCP->_Host_Readf)((POINTER)gCP,gTP,fileID, intLength, (char*)intOffset);

    if (intResult < 0)
        {
        FrameExit(gCP->TObject_ERROR_INVALID);
        }
    else
        {
        FrameExit(argv[1]);
        }
    }
else
    
/*  Manage the full case where only the length is specified.     */

if (argc == 2 && isNumIndex(&argv[0]) && isNumIndex(&argv[1]))
    {
    /*  Extract the length and create the ByteVector. */
    
    fileID = asNumIndex(&argv[0]);
    intLength = asNumIndex(&argv[1]);
    intOffset = 0;
	*tmp = TSYMBOL("byte");
    *vector = FSmartbase_Eval(gCP,gTP,TGVALUE("makeVector"),2,*tmp,TINT(intLength));
    ExitOnError(*vector);
    
    /* Check to make sure all lengths fit within the ByteVector before reading data into the vector. */
        
    if ((ByteVector(*vector)->itsMaxItemIndex >= (intOffset + intLength)) && (intLength > 0) && (intOffset >= 0))
        {   
        intResult = (*gCP->_Host_Readf)((POINTER)gCP,gTP,fileID, intLength, (char*)&ByteArray(*vector)[intOffset]);
        }
    else
        intResult = -1;
    

    if (intResult < 0)
        {
        FrameExit(gCP->TObject_ERROR_INVALID);
        }
    else
        {
        FrameExit(*vector);
        }
    }
else
    
/*  Manage the full case where only the file identifier is specified.    */

if (argc == 1 && isNumIndex(&argv[0]))
    {
    /*  Compute the length to EOF and create the ByteVector. */
    
    fileID = asNumIndex(&argv[0]);
    currentFPos = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID, 0, 0);
    eofFPos = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID, 0, 2);
    (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID, currentFPos, 1);
    intLength = eofFPos - currentFPos;
    intOffset = 0;
	*tmp = TSYMBOL("byte");
    *vector = FSmartbase_Eval(gCP,gTP,TGVALUE("makeVector"),2,*tmp,TINT(intLength));
    ExitOnError(*vector);
    
    /* Check to make sure all lengths fit within the ByteVector before reading data into the vector. */
        
    if ((ByteVector(*vector)->itsMaxItemIndex >= (intOffset + intLength)) && (intLength > 0) && (intOffset >= 0))
        {   
        intResult = (*gCP->_Host_Readf)((POINTER)gCP,gTP,fileID, intLength, (char*)&ByteArray(*vector)[intOffset]);
        }
    else
        intResult = -1;
    

    if (intResult < 0)
        {
        FrameExit(gCP->TObject_ERROR_INVALID);
        }
    else
        {
        FrameExit(*vector);
        }
    }

FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_fseek

The FConio_fseek function attempts to seek to a specified position in an open file.

Arguments:

    [0] NUM the file reference.
    [1] NUM or REAL offset to seek
    [2] NUM code indicating where to seek from.
            0   =   from current location
            1   =   from start of file
            2   =   from end of file

Returns:

    Resultant file position if successful.
    Error if a problem occurs. 

#endif

TVAL FConio_fseek(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
REAL       realResult;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

if(argc == 3 && isNumIndex(&argv[0]) && isNumIndex(&argv[1]) && isNumIndex(&argv[2]))
    {
    realResult = (*gCP->_Host_Seekf)((POINTER)gCP,gTP, asNumIndex(&argv[0]), asNumIndex(&argv[1]), asNumIndex(&argv[2]));

    if(realResult <  0)
        *ret = gCP->TObject_ERROR_INVALID;
    else
        {
			if (realResult > MAXINT) {
				ret->u.Real = realResult;
				ret->Tag = TYREAL;
			} else {
				ret->u.Int = (NUM)realResult;
				ret->Tag = TYNUM;
			}
        }
    }
else
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_fresize

The FConio_fresize function attempts to resize an open file.

Arguments:

    [0] NUM the file reference.
    [1] NUM or REAL newsize to allocate.

Returns:

    Resultant new end of file position if successful.
    Error if a problem occurs. 

#endif

TVAL FConio_fresize(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
REAL        realResult;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

if(argc == 2 && isNumIndex(&argv[0]) && isNumIndex(&argv[1]))
    {
    realResult = (*gCP->_Host_Resizef)((POINTER)gCP,gTP, asNumIndex(&argv[0]), asNumIndex(&argv[1]));

    if(realResult <  0)
        *ret = gCP->TObject_ERROR_INVALID;
    else
        {
			if (realResult > MAXINT) {
				ret->u.Real = realResult;
		        ret->Tag = TYREAL;
			} else {
				ret->u.Int = (NUM)realResult;
		        ret->Tag = TYNUM;
			}
        }
    }
else
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    }
FrameExit(*ret);
}




/*--------------------------------------------------------------------------------------- */
#if 0
FConio_FileReadRecord

The FConio_FileReadRecord function requires the following arguments:

fileID             A numeric ID assigned to the file by a fileOpen.
buffer             (Optional)The buffer Structure to handle the file data
                   during record extraction.		    

The following optional input arguments are handled as follows:

buffer			  The first call to fileReadRecord will return this buffer.
				  All subsequent calls will return records from this buffer.

Example:

(defun testReadRecord()
	pvars:(n N
		(aFileName "testReadRecord.txt")
		aFile
		)

(defun writeTestFile()
	vars:(n N)
	(setq aFile (fileOpen aFileName 1 0))
	(loop for n from 0 until 10 do
		(fileWriteln aFile n #\tab "some text" )
	) ; end of loop
	(setq aFile (fileClose aFile 1))
true)

(defun readTestFile()
	vars:(n N aEOF (aLine "") aBuffer)
	(setq aFile (fileOpen aFileName 0 0))
    (setq aBuffer (fileReadRecord aFile))
    (while (<> aLine #void) do
       (setq aLine (fileReadRecord aFile aBuffer))
       (writeln aLine)
    ) ; end of while
	(setq aFile (fileClose aFile 1))
true)

;;Main Code

(writeTestFile)
(readTestFile)

true)


Note:   The fileReadRecord function only handles text files where each record is separated by an eol 
		character and each column is separated by a tab character.
#endif

TVAL FConio_FileReadRecord(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 bufferSize = _FSmartbase_MAXIMPORTBUFFERLEN;
BOLE                isSBFfile = FALSE;
NUM                 theFile;
HMChar              bufferHdl;

NUM                 dataSize = 0;
NUM                 fileSize = 1;
NUM                 recordSize = 2;
NUM                 recordCount = 3;
NUM                 columnSize = 4;
NUM                 recordType = 5;
NUM                 readSize = 6;
NUM                 recIndex = 7;
NUM                 curCharIndex = 8;
NUM                 startIndex = 9;
NUM                 oldVectorSize = 10;
NUM                 memorySize = 11;

StartFrame
DeclareOBJ(TByteVector,buffer);
DeclareOBJ(TIntVector,memory);
DeclareOBJ(TLambda,pp);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
DeclareTVAL(theRecord);
DeclareTVAL(theBuffer);
DeclareTVAL(theMemory);
EndFrame

/*  The first argument must be a fileID. */
if ((argc < 1) || !isNumIndex(&argv[0]))
	{
	*theRecord = TERROR("!fileReadRecord: Expecting a valid fileid for argument 1!");
	FrameExit(*theRecord);
	}

/*  The second argument must be a buffer. */
if ((argc > 1) && (argv[1].Tag != TYBYTEVECTOR))
	{
	*theRecord = TERROR("!fileReadRecord: Expecting a valid buffer for argument 2!");
	FrameExit(*theRecord);
	}
else
	{
	*theBuffer = argv[1];
	}

/*  Save the arguments. */
theFile  = asNumIndex(&argv[0]);
*theRecord = gCP->Tval_VOID;

/*  The first call requires that we create a buffer and return it. */
if (argc == 1)
    {
	/* Allocate record buffer as a ByteVector. */
	buffer = TByteVector_New(gCP,gTP);
	asObject(theBuffer) = (TObject*)buffer;
	asTag(theBuffer) = buffer->itsObjectType;
	TByteVector_SetMaxIndex(gCP,gTP,*theBuffer,bufferSize + 100);
	argv[1] = *theBuffer;
	bufferHdl = buffer->itsByteArray;
	/* Allocate persistant memory as an IntVector off the cdr of the buffer. */
	memory = TIntVector_New(gCP,gTP);
	asObject(theMemory) = (TObject*)memory;
	asTag(theMemory) = memory->itsObjectType;
	buffer->itsCdr = *theMemory;
	TIntVector_SetMaxIndex(gCP,gTP,*theMemory,memorySize);
	/* Initialize persistant memory. */
	IntArray(*theMemory)[dataSize] = 0;
	IntArray(*theMemory)[fileSize] = 0;
	IntArray(*theMemory)[recordSize] = 0;
	IntArray(*theMemory)[recordCount] = 0;
	IntArray(*theMemory)[columnSize] = 0;
	IntArray(*theMemory)[recordType] = 0;
	IntArray(*theMemory)[readSize] = 0;
	IntArray(*theMemory)[recIndex] = 0;
	IntArray(*theMemory)[curCharIndex] = 0;
	IntArray(*theMemory)[startIndex] = 0;
	IntArray(*theMemory)[recordSize] = 0;
	IntArray(*theMemory)[oldVectorSize] = 0;
	/*  Figure how much data we have to read and set the file pointer to the beginning of file*/
	IntArray(*theMemory)[startIndex] = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,0,0);
	IntArray(*theMemory)[fileSize] = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,0,2);
	(*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,IntArray(*theMemory)[startIndex],1);
	*ec = FConio_Fill32KBuffer(gCP,gTP,&IntArray(*theMemory)[fileSize], &IntArray(*theMemory)[dataSize], theFile, bufferHdl);
	ExitOnError(*ec);
	FrameExit(*theBuffer);
	}
    
/*  Request a handle for reading the maximum possible record from the file. */

buffer = theBuffer->u.ByteVector;
bufferHdl = buffer->itsByteArray;
*theMemory = buffer->itsCdr;
if (bufferHdl == NULL) goto BadCleanUp;
if (theMemory->Tag != TYINTVECTOR)
    {
	*theRecord = TERROR("!fileReadRecord: Expecting a valid memory Integer Vector!");
	FrameExit(*theRecord);
	}
memory = theMemory->u.IntVector;

/* If the fileSize and the data size are both zero, the */
/* we have reached the end of the file and we return an empty record. */

if ((IntArray(*theMemory)[fileSize] <= 0) && (IntArray(*theMemory)[dataSize] <= 0))
	{
	FrameExit(gCP->Tval_VOID);
	}

/* fileSize is initially set to the size of the file. If the file	 */
/* is larger than the buffer size, this code will call the */
/* FConio_Fill32KBuffer function to fill the buffer in chunks. */

if ((IntArray(*theMemory)[fileSize] > 0) && (IntArray(*theMemory)[dataSize] <= 0))
	{
	IntArray(*theMemory)[recIndex] = 0;
	IntArray(*theMemory)[curCharIndex] = 0;
	IntArray(*theMemory)[startIndex] = 0;
	*ec = FConio_Fill32KBuffer(gCP,gTP,&IntArray(*theMemory)[fileSize], &IntArray(*theMemory)[dataSize], theFile, bufferHdl);
	ExitOnError(*ec);
	}

/* Scan the data buffer for a record */
FConio_GetRecord(bufferHdl, &IntArray(*theMemory)[recordSize], 
							&IntArray(*theMemory)[recIndex], 
							&IntArray(*theMemory)[recordType], 
							&IntArray(*theMemory)[recordCount], 
							isSBFfile);

/* Process the columns for this record (also known as row) */
/* Initialize the row and column specifier for the first column of this new record */
/* Convert the record number (row number) to a zero based index  */
IntArray(*theMemory)[dataSize] -= IntArray(*theMemory)[recordSize];

/* Call the setImport method of the Lambda to store the imported row text. */
*theRecord = FSmartbase_CnvFromText(gCP,gTP,&atHMChar(bufferHdl,IntArray(*theMemory)[curCharIndex]));
ExitOnError(*theRecord);
IntArray(*theMemory)[curCharIndex] += IntArray(*theMemory)[recordSize];
IntArray(*theMemory)[recordSize] = 0;

/* We have now finished importing all the columns for this record.*/
/* Update the data buffer index (recIndex) to point to the	*/
/* beginning of the next record */

IntArray(*theMemory)[recIndex] = IntArray(*theMemory)[curCharIndex];

    
/*  Return the record just read. */

BadCleanUp:

FrameExit(*theRecord);
 
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_LexBuffer

This method lexes a buffer for a single cell and creates any objects it needs to.

Note:   If the item begins with an = sign, the text is treated as a formula.

#endif

TVAL    FConio_LexBuffer(LpXCONTEXT gCP, LpTHREAD gTP, HMChar bufferHdl, NUM start, NUM end)
{
LpCHAR				strPtr = &atHMChar(bufferHdl,0);
NUM                 tmpInt;
REAL                aReal;
CHAR                firstCh;
CHAR                tmpCh;
CHAR                savCh;
NUM					newStart = start;
NUM					newEnd = end;
TVAL				aNumber;
BOLE				generateDebugInfoSW;
StartFrame
DeclareOBJ(TVector,debugSourceVector);
DeclareOBJ(TStructure,debugListLines);
DeclareTVAL(ret);
DeclareTVAL(text);
DeclareTVAL(result);
EndFrame

/*  Save the current compiler debugger information and switches. */
/*  Note: This function recursively calls the Lisp parser.       */

generateDebugInfoSW = gTP->FCompile_GenerateDebugInfo;
debugSourceVector = gTP->FCompile_DebugSourceVector;
debugListLines = gTP->FCompile_DebugListLines;
gTP->FCompile_GenerateDebugInfo = FALSE;
gTP->FCompile_DebugSourceVector = NIL;
gTP->FCompile_DebugListLines = NIL;

/*  Save the character after the end. */

savCh = atHMChar(bufferHdl,end);
atHMChar(bufferHdl,end) = 0;

*result = gCP->TObject_ERROR_INVALID;

/*  Eliminate leading and trailing whitespace. */

for (; ((newStart < newEnd) && (atHMChar(bufferHdl,newStart) == 32)) ; ++newStart) {}
for (--newEnd; ((newEnd > newStart) && (atHMChar(bufferHdl,newEnd) == 32)) ; --newEnd) {}
++newEnd;

/*  Restore the old ending character and save the new ending character. */

atHMChar(bufferHdl,end) = savCh;
savCh = atHMChar(bufferHdl,newEnd);
atHMChar(bufferHdl,newEnd) = 0;

/*	Check the first non whitespace character of the input. */

firstCh = atHMChar(bufferHdl,newStart);
switch (firstCh)
    {
    case 0:
        *result = gCP->Tval_VOID;
    break;
    
    case '=':
        goto RecognizeString;
    break;
    
    case '#':
        if (strcmp((char *)&atHMChar(bufferHdl,newStart),"#void") == 0)
            {
            *result = gCP->Tval_VOID;
            }
        else
            {
            tmpInt = newStart;
            FProcedure_recDate(gCP,gTP,bufferHdl, &tmpInt, ret);
            if (ret->Tag == TYERROR)
				{
                goto RecognizeString;
				}
            else
				{
               /* if (tmpInt < newEnd) */
                if (strPtr[tmpInt] != 0)
					{		 
					goto RecognizeString;
					}
                else
					{
                    *result = *ret;
					}
				}
            }
    break;
    
    case '"':
		if (atHMChar(bufferHdl,newStart+1) == '#')
			{
            tmpInt = newStart+1;
            FProcedure_recDate(gCP,gTP,bufferHdl, &tmpInt, ret);
            if ((ret->Tag == TYDATE) && (strPtr[tmpInt] == '"'))
				{
                *result = *ret;
                goto Last;
				}
			else
				{
				goto Retry;
				}
 			}
		else
			{
		 Retry:
			while(atHMChar(bufferHdl,newStart) == '"') newStart++;
			tmpInt = newStart;
		 Again:
			while ((atHMChar(bufferHdl,tmpInt) != '"') &&
				   (atHMChar(bufferHdl,tmpInt) != '\\') &&
				   (atHMChar(bufferHdl,tmpInt) != 0) &&
				   (++tmpInt < newEnd)) {}
        
			if (atHMChar(bufferHdl,tmpInt) == '\\')
				{tmpInt += 2; goto Again;}
			else
			if ((atHMChar(bufferHdl,tmpInt) == '"') && (atHMChar(bufferHdl,tmpInt+1) == '"'))
				{tmpInt += 2; goto Again;}
			else
			if (atHMChar(bufferHdl,tmpInt) == '"')
				goto Convert;
			else
			if (atHMChar(bufferHdl,tmpInt) >= 32)
				goto RecognizeString;

		Convert:
			*result = FSmartbase_CnvFromSubstring(gCP,gTP,&atHMChar(bufferHdl,newStart),0L,(tmpInt - newStart));
			}
    break;
    
    case '$':
        tmpInt = newStart + 1;
        *ret = FProcedure_recNumber(gCP,gTP,bufferHdl,&tmpInt,&aReal,&aNumber);
        if (asBool(ret) == TRUE) 
            {
       /*     if (tmpInt < newEnd) */
            if (strPtr[tmpInt] != 0)				 
              goto RecognizeString;
            result->Tag = TYMONEY;
            result->u.Real = aReal;
            }
        else
            goto RecognizeString; 
    break;
    
    case 'f':
        if (strcmp((char *)&atHMChar(bufferHdl,newStart),"false") == 0)
            {
            result->Tag = TYBOLE;
            result->u.Bool = FALSE;
            }
        else
            goto RecognizeString; 
    break;

    case 'F':
        if (strcmp((char *)&atHMChar(bufferHdl,newStart),"FALSE") == 0)
            {
            result->Tag = TYBOLE;
            result->u.Bool = FALSE;
            }
        else
            goto RecognizeString; 
    break;

    case 'T':
        if (strcmp((char *)&atHMChar(bufferHdl,newStart),"TRUE") == 0)
            {
            result->Tag = TYBOLE;
            result->u.Bool = TRUE;
            }
        else
            goto RecognizeString; 
    break;
    
    case 't':
        if (strcmp((char *)&atHMChar(bufferHdl,newStart),"true") == 0)
            {
            result->Tag = TYBOLE;
            result->u.Bool = TRUE;
            }
        else
            goto RecognizeString; 
    break;
    
    case '-':
    case '+':
    case '.' :
        if(newStart + 1 < newEnd)
            {
            tmpCh = atHMChar(bufferHdl,newStart+1);
            if((tmpCh < '0' || tmpCh > '9') && tmpCh != '.')
                goto RecognizeString;
            }
        else
            goto RecognizeString;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        tmpInt = newStart;
        *ret = FProcedure_recNumber(gCP,gTP,bufferHdl,&tmpInt,&aReal,&aNumber);
        if (asBool(ret) == TRUE)
            {
            /*if (tmpInt < newEnd) */
            if (strPtr[tmpInt] != 0)			 
                goto RecognizeString;
            newStart = tmpInt;
			*result = aNumber;
            }
        else
            goto RecognizeString; 
    break;  
    
    default:
        RecognizeString:
        *result = TObject_CnvFromText(gCP,gTP,&atHMChar(bufferHdl,newStart));
		if (!ISALNUM((NUM)atHMChar(bufferHdl,newStart)))
			{
			*text = *result;
			*result = FLisp_Lisp(gCP,gTP,1,text);
			if (result->Tag == TYLAMBDA) *result = FSmartbase_Ref(gCP,gTP,2,result->u.Lambda->Interfaces,TOBJ(gCP->FCompile_dbgParseTreeSYM));
			if ((result->Tag == TYPAIR) && (result->u.Pair->itsCdr.Tag == TYVOID))
				*result = result->u.Pair->itsCar;
			else
				*result = *text;
			}
    break;
    
    }
    
/*  Restore displaced character and exit. */

Last:
gTP->FCompile_GenerateDebugInfo = generateDebugInfoSW;
gTP->FCompile_DebugSourceVector = debugSourceVector;
gTP->FCompile_DebugListLines = debugListLines;

atHMChar(bufferHdl,newEnd) = savCh;
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_Fill32KBuffer



The FConio_Fill32KBuffer reads a maximum of 32k bytes from a file and puts the data into
a buffer.  If the last character in the buffer is not a CR or LF, scan backwards 
until the CR or LF is found and position the file pointer to the byte after the CR 
or LF to force the next read at the beginning of a new record boundary.


#endif

TVAL FConio_Fill32KBuffer(LpXCONTEXT gCP,LpTHREAD gTP,LpNUM fileSize, LpNUM readSize, NUM theFile, HMChar bufferHdl)
{
NUM         bufferSize = _FSmartbase_MAXIMPORTBUFFERLEN+100;
NUM			tempFileSize, tempSeekPos;
char*       bufptr = &atHMChar(bufferHdl,0);
StartFrame
DeclareTVAL(result);
EndFrame

 /* The readSize variable is used to compute the amount of data to read */
 /* The fileSize variable keeps track the remaining bytes to read from the file */ 

if (*fileSize > bufferSize)	/* is the file size larger than our buffer ? */
    {
    *readSize = bufferSize;	/* limit the number of bytes to read to the buffer size */
    }
else
    {
    *readSize = *fileSize;	/* read all the remaining bytes of the file into the buffer */
    }


/* Read from the file */
(*gCP->_Host_Readf) ((POINTER)gCP,gTP,theFile,*readSize,(char*)&bufptr[0]);

/* Determine if the last record was partially read by checking if the last char read was a CR or LF */

if (*fileSize - bufferSize > 0)
    {
    /*  Parse back to the last CR, LF set the file position to the end of the last whole record */
    /*  This insures that we always end a buffer on a record boundary and start the next read */
    /*  on the beginning of a record boundary */
    
	/* ignore null byte at the end of read buffer */											
	--(*readSize);						
	/* skip past any CR LF fragments at end of buffer to prevent split record condition */		
    while ((*readSize > 0) && ((bufptr[*readSize] == 10) || (bufptr[*readSize] == 13))  )
		{
		--(*readSize);
		}																					

    /*  Parse back to the last CR, LF set the file position to the end of the last whole record */
    /*  This insures that we always end a buffer on a record boundary and start the next read */
    /*  on the beginning of a record boundary */
    
    while ((*readSize > 0) && ((bufptr[*readSize] != 10) && (bufptr[*readSize] != 13))  )
		{
		--(*readSize);
		}

    if (*readSize <= 0 && ((bufptr[*readSize] != 10) && (bufptr[*readSize] != 13)) )
		{
		*fileSize = 0;
		/* return error */
		*result = TERROR("!fileIOError: read text file encountered record larger than buffer size!");
		FrameExit(*result);
		}

	/* Compute the number of remaining bytes in the file left un-read */
    *fileSize -= (*readSize + 1);
	tempSeekPos = -(*fileSize);
	/* Position the file pointer to the byte after the record terminator in the read buffer */
    tempFileSize =  (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,tempSeekPos,SEEK_END);
	
    }
else
	{
	/* Compute the number of remaining bytes in the file left un-read */
	*fileSize = 0;
	--(*readSize); /* ignore null byte at the end of read buffer */
	}

FrameExit(gCP->Tval_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_GetRecord


A record may be terminated by a single CF or a single LF or a combination of CR & LF.
The FConio_GetRecord scans the read buffer bufferHdl for a record terminator: CR & LF, CR, LF.
The CR & LF characters are replaced with the Null Character '0'.
The curCharIndex pointer is advanced to the character following the CR & LF.
The dataSize argument is reduced by the number of characters found before the CR & LF.

The FConio_GetRecord function will also apply some simple heuristics to determine if the
current record is a header record only found in .SBF files.  The first 3 record types are 
special header records that contain information needed to construct the smarttable. The 
header records are not counted in the record count.


The valid types for .SBF files:

Type 
1		Column name record	-- Text description of the column
2		Type Record			-- Describes the type of the column G = General
3		Key Record			-- Set to 'p' if it is a key column, set to 'n' for non-key column.
4		Data Record			-- A spreadsheet row or a smarttable row.

The following record type is valid for .SBF and .TAB files
4		Data Record			-- A spreadsheet row or a smarttable row.


#endif

void FConio_GetRecord(HMChar bufferHdl, register LpNUM recordSize, register LpNUM curCharIndex,
					  LpNUM recordType, LpNUM recordCount, BOLE isSBFfile)
{
char	saveChar;
NUM		start = *curCharIndex; 
char *  bufptr = &atHMChar(bufferHdl,0);


*recordSize = 0;

/* Find a record delimiter */
while ( !(ISLINEBREAK(bufptr[*curCharIndex]) || (bufptr[*curCharIndex] == 0)) )
	{
	++(*curCharIndex);
	++(*recordSize);
	}


/* if this is an SBF file, determine record type */
if (isSBFfile)
	{
	/* If we have gone beyond the header records always assume it is a data record*/
	if (*recordCount > 0)
		{
		(*recordCount)++;
		*recordType = DATAREC;
		goto Finish;
		}
		
	/* If it is the very first record, assume it is the column name record */
	if (*recordCount ==  0 && start == 0)
		{
		*recordType = COLUMNREC;
		goto Finish;
		}

	/* Check if the first column contains a single character field */
	if (*recordCount == 0  && isalpha(bufptr[start]) && atHMChar(bufferHdl,start + 1) == '\t' )
		{

		saveChar = bufptr[start];

		/* Check if the first column contains  the string "G" */
		if (toupper(saveChar) == 'G')
			{
			/* make the assumption, that it is the type record */
			*recordType = TYPEREC;
			goto Finish;
			}
 
		/* Check if the first column the string "P" or "N" */
		if ((toupper(saveChar) == 'P') || (toupper(saveChar) == 'N'))
			{
			/* Make the assumption, that it is the type record */
			*recordType = KEYDEFREC;
			goto Finish;
			}
		}
		
	/* This is .SBF record that did not qualify as one of the headers */
	/* It will default to a  DATAREC */
	(*recordCount)++;
	*recordType = DATAREC;
	}
else
	/* This a .TAB file, so it is always a DATAREC */
	{
	(*recordCount)++;
	*recordType = DATAREC;
	}

Finish:


/* Check if there are three record delimiters (CR, CR, LF) */

if ((bufptr[*curCharIndex+0] == 13) &&		
	(bufptr[*curCharIndex+1] == 13) &&	
	(bufptr[*curCharIndex+2] == 10))
    {
	bufptr[*curCharIndex] = 0;	/* replace the first CR character with a null to create a K&R string */
    ++(*curCharIndex);			/* move past the first CR */
	bufptr[*curCharIndex] = 0;	/* replace the second CR character with a null to create a K&R string */
    ++(*curCharIndex);			/* move past the second CR */
	bufptr[*curCharIndex] = 0;	/* replace the LF character with a null to create a K&R string */
    ++(*curCharIndex);			/* move past the LF */
	*recordSize += 3;
    }
else
/* Check if there are two record delimiters (CR, LF) or (LF, CR) */

if (((bufptr[*curCharIndex+1] == 10) || (bufptr[*curCharIndex+1] == 13)) &&							
    (bufptr[*curCharIndex] != bufptr[*curCharIndex+1]))		
    {
	bufptr[*curCharIndex] = 0;	/* replace the CR or LF character with a null to create a K&R string */	
    ++(*curCharIndex);			/* move past the CR or LF */
	bufptr[*curCharIndex] = 0;	/* replace the CR or LF character with a null to create a K&R string */	
    ++(*curCharIndex);					/* move past the CR or LF */
	*recordSize += 2;
    }
else
    {
	bufptr[*curCharIndex] = 0;	/* replace the CR or LF character with a null to create a K&R string */	

    ++(*curCharIndex);					/*  move past the CR or LF */
	++(*recordSize);
    }

/* curCharIndex points to the byte after the record terminator */
/* recordSize will represent the number of bytes in the record */
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_GetColumn



The FConio_GetColumn scans the current record for a column terminator:Tab or Null byte
(The Null Byte indicates that the column being parsed is the last column of a record)
The curCharIndex pointer is advanced to the character following the Tab or Null.

#endif

void FConio_GetColumn(HMChar bufferHdl, register LpNUM recordSize, register LpNUM curCharIndex)
{
while (*recordSize > 0 && (atHMChar(bufferHdl,*curCharIndex) != 9) && (atHMChar(bufferHdl,*curCharIndex) != 0)) 
	{
	++(*curCharIndex);	/* update the current character index */
	--(*recordSize);	/* reduce the number of bytes to scan in this record */
	}


if (*recordSize > 0	&& (atHMChar(bufferHdl,*curCharIndex) == 9))
	{
	atHMChar(bufferHdl,*curCharIndex) = 0;	/* replace the tab character with a null to create a K&R string */
	++(*curCharIndex);					/* move the current character index past the tab */
	--(*recordSize);					/* reduce the number of bytes to scan in this record */
	}


/* If the column occurs at the end of a record, the record terminator might be one or two null bytes */
if (atHMChar(bufferHdl,*curCharIndex) == 0 ) 
	{
	++(*curCharIndex);		/* move the current character index past the second null byte */
	--(*recordSize);		/* reduce the number of bytes to scan in this record */
	if (atHMChar(bufferHdl,*curCharIndex) == 0 ) 
		{
		++(*curCharIndex);		/* move the current character index past the second null byte */
		--(*recordSize);		/* reduce the number of bytes to scan in this record */
		}
	}

}



/*--------------------------------------------------------------------------------------- */
#if 0
FConio_Parse

(gCP,gTP,parse string)


The parse cProcedure parses a string and returns either a parsed
value or the original string. The parse function may also receive
a string pointer, a start index, and an end index.

For example

    (parse "34.56") =>  34.56

	-or-

	(setq X "34.56")
	(setq ptr X)
	(setq len (length X))
    (parse ptr 0 len) =>  34.56

#endif

TVAL FConio_Parse(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
LpCHAR          temp;

/*  Check for the correct number of arguments. */
    
switch (argv[0].Tag)
    {
    case TYTEXT:
	    if (argc != 1) return(gCP->TObject_ERROR_INVALID_ARGLIST);
        temp = &argv[0].u.Text[0];
        return(FConio_LexBuffer(gCP, gTP, (HMChar)((char*)&temp),0L,strlen(argv[0].u.Text)));
        break;
    
    case TYSTRINGSUBSTR:
        if (argc != 1) return(gCP->TObject_ERROR_INVALID_ARGLIST);
        temp = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (temp == NULL) return(gCP->TObject_ERROR_INVALID);
        
        return(FConio_LexBuffer(gCP, gTP, (HMChar)((char*)&temp), 0L, SubLen(argv[0])));
        break;
    
    case TYSTRING:
	    if (argc != 1) return(gCP->TObject_ERROR_INVALID_ARGLIST);
        return(FConio_LexBuffer(gCP, gTP, (HMChar)asString(&argv[0])->itsCString,0L,strlen((char*)*asString(&argv[0])->itsCString)));
        break;
    
    case TYBYTEVECTOR:
	    if (argc != 1) return(gCP->TObject_ERROR_INVALID_ARGLIST);
        return(FConio_LexBuffer(gCP, gTP, (HMChar)argv[0].u.ByteVector->itsByteArray,0L,strlen((char*)*argv[0].u.ByteVector->itsByteArray)));
        break;
    
    case TYNUM:
	    if ((argc == 3) && (argv[1].Tag == TYNUM) && (argv[2].Tag == TYNUM) && (argv[1].u.Int < argv[2].u.Int))
		    {
		    return(FConio_LexBuffer(gCP, gTP,(HMChar)((char*)&argv[0].u.Pointer),(NUM)argv[1].u.Int,(NUM)argv[2].u.Int));
		    }
	    else
	    if (argc == 1)
		    {
		    return(argv[0]);
		    }
	    else
		    {
		    return(gCP->TObject_ERROR_INVALID_ARGLIST);
		    }
        break;
    
    default:
    return(argv[0]);
    break;
    }
}


/*--------------------------------------------------------------------------------------- */
#if 0
FConio_FDisplay

The FConio_FDisplay function displays the arguments to the specified file.

Note:   The display procedure makes a special effort to detect and to
        drop the quotes which surround text strings before they are
        displayed to the file.

#endif

TVAL FConio_FDisplay(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 indexOf;
HMChar              theData = NULL;
CHAR*               buf = NULL;
NUM                 oldLen;
NUM                 fileID;
NUM                 intOffset;
NUM                 intLength;
NUM                 intResult;
CHAR*               dataPtr;
CHAR				cbuf[2];
StartFrame
DeclareTVAL(ec);
DeclareTVAL(ret);
EndFrame
 
/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

oldLen = gCP->TObject_MaxOutputLen;
gCP->TObject_MaxOutputLen = _FSmartbase_MAXBUFFERLEN;

asTag(ret) = TYBOLE;
asBool(ret) = TRUE;
indexOf = 0;

if (argc >= 2 && isNumIndex(&argv[0]))
    {
    /*  Extract the file Identifier and default the offset to zero. */
    
    fileID = asNumIndex(&argv[indexOf]);
    intOffset = 0;

    while (++indexOf < argc)
        {
        /*  Extract the length and a pointer to the data. */
        
        switch (argv[indexOf].Tag)
            {
            case TYBYTEVECTOR:
                intLength = ByteVector(argv[indexOf])->itsMaxItemIndex; 
                dataPtr = (char*)&ByteArray(argv[indexOf])[0];
                break;
            
            case TYSTRING:
                intLength = strlen((char*)CharArray(argv[indexOf]));    
                dataPtr = (char*)&CharArray(argv[indexOf])[0];
                break;
            
            case TYCHAR:
                intLength = 1;  
                cbuf[0] = argv[indexOf].u.Char;
                cbuf[1] = 0;
                dataPtr = &cbuf[0];
                break;
            
            case TYTEXT:
                intLength = strlen(&argv[indexOf].u.Text[0]);   
                dataPtr = (char*)&argv[indexOf].u.Text[0];
                break;
            
            case TYSTRINGSUBSTR:
                intLength = SubLen(argv[indexOf]);
                dataPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[indexOf]);
                break;
            
            default:
                if (theData == NULL)
                    { 
                    theData = (HMChar)FMemory_New(gCP, gTP, (NUM)_FSmartbase_MAXBUFFERLEN,TRUE);
                    buf = &atHMChar(theData,0);
                    }

                *ec = FSmartbase_CnvToText(gCP,gTP,buf,gCP->TObject_MaxOutputLen,argv[indexOf]);
                ExitOnError( *ec);
                dataPtr = buf;
                intLength = strlen(dataPtr);
                break;
            }

        /*  Write the converted text which is now in the file buffer. */
        /* Check to make sure all lengths fit within the buffer before writing data to the file. */
            
		if (intLength > 0)
			{
			intResult = (*gCP->_Host_Writef)((POINTER)gCP,gTP,fileID, intLength, dataPtr);
			}
		else
			{
			intResult = intLength;
			}
        
        if (intResult < 0)
            {
            *ret = gCP->TObject_ERROR_INVALID;
            goto Last;
            }

        }
    }

Last:
if (theData != NULL)
    FMemory_Free(gCP, gTP, (HMemory)theData);

gCP->TObject_MaxOutputLen = oldLen;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_FWrileln

The FConio_FWrileln function displays the arguments to the specified file,
and terminates the line with the host end of line characters.

#endif

TVAL FConio_FWriteln(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ec);
DeclareTVALArray(prmv,2);

EndFrame

*ec = FConio_FDisplay(gCP,gTP,argc,argv); /* Display the line */
ExitOnError(*ec);

prmv[0] = argv[0];
prmv[1] = TGVALUE("_eol");		/* Add the eol character or characters */

*ec = FConio_FDisplay(gCP,gTP,2,&prmv[0]);
FrameExit(*ec);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FConio_AppendWriteln

The FConio_AppendWriteln function appends the arguments as a string in the specified
Byte Vector.


(setq v (new Vector: byte: 4096))
(setq v[0] 0)
(appendWriteln v "This value of this number is " 234)


#endif

TVAL FConio_AppendWriteln(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM				argIndex;
NUM				size = 0;
LpCHAR			bufPtr = 0;
NUM				intLength;
LpCHAR			dataPtr;
CHAR			tmpBuf[100];
TByteVector*	self = argv[0].u.ByteVector;
TVAL			selfTval = argv[0];
StartFrame
DeclareTVAL(err);
EndFrame
 
/* Check the first argument to make sure it is a Byte Vector. */

if (argc < 1)
	{
	*err = TERROR("!appendWriteln: Expecting at least one argument!");
	FrameExit(*err);
	}

/* Check the first argument to make sure it is a Byte Vector. */

if (selfTval.Tag != TYBYTEVECTOR)
	{
	*err = TERROR("!appendWriteln: Expecting argument 1 to be a Byte Vector!");
	FrameExit(*err);
	}

/* Point to the Byte Array and obtain its length. */

bufPtr = ByteArray(selfTval);
if ((bufPtr[0] == 0) || (self->itsMaxItemIndex == 0))
	{
	self->itsMemoLength = size = 0;
	}
else
if ((self->itsMemoLength >= 0) && (self->itsMemoLength < self->itsMaxItemIndex) && (bufPtr[self->itsMemoLength] == 0))
	{
	size = self->itsMemoLength;
	}
else
	{
	size = strlen(bufPtr);
	self->itsMemoLength = size;
	}

/* Set the maximum upper limit on sprintn display size. */

gCP->TObject_MaxOutputLen = ByteVector(selfTval)->itsMaxItemIndex - 6;

/* Display each of the items as a string in the Byte Vector. */

for (argIndex = 1; argIndex < argc; ++argIndex)
    {
    /* Make sure we do not exceed the target length. */

	if (size >= gCP->TObject_MaxOutputLen)
		{
		OutOfMemory:
		*err = TERROR("!appendWriteln: Exceeded maximum size of vector !");
		FrameExit(*err);
		}
    
	/*  Extract the length and a pointer to the data. */

    switch (argv[argIndex].Tag)
        {
		case TYBOLE:
			if (argv[argIndex].u.Bool == TRUE)
				{
				dataPtr = (char *)"true";
				}
			else
				{
				dataPtr = (char *)"false";
				}
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size += intLength;
			break;
        
        case TYBYTEVECTOR:
            dataPtr = (char*)&ByteArray(argv[argIndex])[0];
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size += intLength;
            break;
        
        case TYCHAR:
            dataPtr = (char*)&argv[argIndex].u.Text[0];
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			bufPtr[size] = (CHAR)argv[argIndex].u.Int;
			if (bufPtr[size] != 0) bufPtr[++size] = 0;
            break;
        
		case TYCPX:
			*err = TCpx_Print(gCP,gTP,argv[argIndex],&size,&bufPtr[size]);
            ExitOnError(*err);
			break;

        case TYDATE:
			TObject_CnvDateToText(gCP,gTP,tmpBuf,argv[argIndex]);
            dataPtr = tmpBuf;
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size += intLength;
            break;
        
        case TYNUM:
        case TYPOINTER:
			sprintf((char *)tmpBuf,INTFORMAT,argv[argIndex].u.Int);
            dataPtr = tmpBuf;
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size += intLength;
            break;
        
        case TYREAL:
			TObject_sprintReal(gCP,gTP,(char *)tmpBuf,argv[argIndex].u.Real);
            dataPtr = tmpBuf;
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size += intLength;
            break;
        
        case TYMONEY:
			tmpBuf[0] = '$';
            dataPtr = tmpBuf;
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size += intLength;
            break;

        case TYSTRING:
            intLength = strlen((char*)CharArray(argv[argIndex]));
            dataPtr = (char*)&CharArray(argv[argIndex])[0];
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size = strlen(bufPtr);
            break;
   
        case TYSYMBOL:
        case TYQUOTEDSYMBOL:
            dataPtr = (char*)&SymbolArray(argv[argIndex])[0];
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size += intLength;
            break;

        case TYTEXT:
            dataPtr = (char*)&argv[argIndex].u.Text[0];
            intLength = strlen(dataPtr);
			if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
			strcat(&bufPtr[size],dataPtr);
			size += intLength;
            break;
        
        case TYSTRINGSUBSTR:
            intLength = SubLen(argv[argIndex]);
            if ((size + intLength) >= gCP->TObject_MaxOutputLen) goto OutOfMemory;
            dataPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[argIndex]);
            if (dataPtr != NULL)
                {
                strncat(&bufPtr[size],dataPtr,intLength);
                size += intLength;
                }
            break;
        
        default:
			/*  Produce a deep string conversion of all other argument types.*/
			*err = FConio_sprint(gCP,gTP,gCP->TObject_MaxOutputLen - size,&bufPtr[size],argv[argIndex]);
            ExitOnError(*err);
            intLength = strlen(&bufPtr[size]);
			size += intLength;
            break;	
		}
	}

/* Return the Byte Vector with the next text. */

self->itsMemoLength = size;

FrameExit(selfTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_RunScript

The runScript cProcedure evaluates the script file whose file name is
the specified argument. This procedure always returns true.

An example follows.

    (runScript "My Script File")       =>  true

#endif

TVAL FConio_RunScript(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
NUM                 fileID;
CHAR                fileName[1024];
CHAR                fileFName[1024];
NUM                 length;
HMChar              hmChar;
StartFrame
DeclareTVALArray(prmv,2);
DeclareTVAL(fileTval);
DeclareTVAL(tmpTval);
DeclareTVAL(ScriptValue);

EndFrame

/*  Check for a user escape request before continuing. */

if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

/*  There must be exactly one argument. */

asTag(fileTval) = TYBOLE;
asBool(fileTval) = TRUE;

if (argc > 1)
	{
    *tmpTval = TERROR("!runScript: Too many arguments!");
	FrameExit(*tmpTval);
	}

/*  If no file name is specified, then let the user select a file. */
if (argc == 0)
    {
    *tmpTval = TERROR("!runScript: Needs exactly one argument!");
	FrameExit(*tmpTval);
    }
else
/*  If a file name is specified, then evaluate the specified file. */
    {
	*tmpTval = FConio_pathName(gCP, gTP, sizeof(fileName)-1, fileName, gCP->TSymbol_Path->itsGlobalValue, argv[0]);
    ExitOnError(*tmpTval);
    *tmpTval = TObject_CnvToText(gCP,gTP,fileFName, sizeof(fileFName)-1, argv[0]);
    ExitOnError(*tmpTval);
    strcat(fileName,fileFName);
    
    fileID = (*gCP->_Host_Openf)((POINTER)gCP,gTP,(char *)fileName, 0, 0);

    /*  If file not found in path, search in current directory. */
    
    if (fileID == 0)
        fileID = (*gCP->_Host_Openf)((POINTER)gCP,gTP,(char *)fileFName, 0, 0);


    RunTheScript:
    
    /*  Evaluate the specified script file. */

    if (fileID != 0)
        {
        /*  Figure out how large the file is, and then reset file to start. */
        
        length = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID, 0, 2);
        (*gCP->_Host_Seekf)((POINTER)gCP,gTP,fileID, 0, 1);
        
        /* Allocate a buffer and read it in. */
        
        if(length < 0)
            {
			*tmpTval = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!runScript: Invalid file length for file %s!",fileName);
			FrameExit(*tmpTval);
            }
        else
        if(length > 0)
            {
            hmChar = (HMChar)FMemory_New(gCP, gTP, length + 1,TRUE);
                
            if(hmChar != NULL)
                {
                (*gCP->_Host_Readf)  ((POINTER)gCP,gTP,fileID, length, (char*)&atHMChar(hmChar, 0));
                
                /*  Interpret the temporary script. */
                
                atHMChar(hmChar,length) = 0;
                *ScriptValue = FSmartbase_Evals(gCP,gTP,&atHMChar(hmChar,0),FALSE);
                
                FMemory_Free(gCP, gTP, (HMemory)hmChar);
                }
            }

        /*  Close the file and exit. */
        
        (*gCP->_Host_Closef)((POINTER)gCP,gTP,fileID, 1);
        FrameExit(*ScriptValue);
        }

    }


/* Print out an error message */
*tmpTval = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!runScript: Cannot open script %s!",fileName);
FrameExit(*tmpTval);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_ImportTAB

The FConio_ImportTAB function requires the following arguments:

fileID             A numeric ID assigned to the file by a fileOpen.
anLambda            The name of an Lambda to handle the import.		    

The following optional input arguments are handled as follows:

recordsOnly:	  Treat each record as a string and pass it to the target Lambda
recordVectors:	  Parse each field from the record and enter each field into a Vector
				  which is passed to the Lambda




Note:   The importTab function only handles text files where each record is separated by an eol 
		character.
#endif

TVAL FConio_ImportTAB(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 bufferSize = _FSmartbase_MAXIMPORTBUFFERLEN;
BOLE                isSBFfile = TRUE;
BOLE                isRecordsOnly = FALSE;
BOLE                isRecordVectors = FALSE;
NUM                 maxColN;
NUM                 theFile;
HMChar              bufferHdl;
NUM                 dataSize;
NUM                 fileSize;
NUM                 recordSize;
NUM                 recordCount;
NUM                 recordType;
NUM                 recIndex;
NUM                 curCharIndex;
NUM                 startIndex;
StartFrame
DeclareOBJ(TByteVector,buffer);
DeclareOBJ(TLambda,pp);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
DeclareTVAL(recordVector);
DeclareTVAL(result);
DeclareTVAL(ret);
DeclareTVAL(col);
DeclareTVAL(row);
DeclareTVAL(ivTval);
DeclareTVAL(theTarget);
DeclareTVALArray(prmv,4);

EndFrame

/*  Check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  The optional third argument must be a of the following valid option. */
if ((argc == 3) && 
    (argv[2].Tag == TYSYMBOL) &&
    (strcmp(SymbolArray(argv[2]),"recordsOnly") == 0))
    {
    argc = 2;
    isSBFfile = FALSE;
    isRecordsOnly = TRUE;
    isRecordVectors = FALSE;
    }
else
if ((argc == 3) && 
    (argv[2].Tag == TYSYMBOL) &&
    (strcmp(SymbolArray(argv[2]),"recordVectors") == 0))
    {
    argc = 2;
    isSBFfile = FALSE;
    isRecordsOnly = FALSE;
    isRecordVectors = TRUE;
    }

/*  The first argument must be a fileID. */
if (argc != 2 || !isNumIndex(&argv[0]))
	{
	*ret = TERROR("!ImportTab: Expecting a valid fileid for argument 1!");
	FrameExit(*ret);
	}

/*  Save the target argument. */
*theTarget = argv[1];

if (theTarget->Tag == TYLAMBDA)
    {
    pp = (TLambda*)asObject(theTarget);
    }
else
	{	
	*ret = TERROR("!ImportTab: Expecting an Lambda for argument 2!");
    goto BadCleanUp;
}
    
/*  Request a handle for reading the maximum possible record from the file. */

theFile  = asNumIndex(&argv[0]);
buffer = TByteVector_New(gCP,gTP);
asObject(ivTval) = (TObject*)buffer;
asTag(ivTval) = buffer->itsObjectType;
TByteVector_SetMaxIndex(gCP,gTP,*ivTval,bufferSize + 100);

bufferHdl = buffer->itsByteArray;
if (bufferHdl == NULL)
    goto BadCleanUp;

/* Initialize the row and col TVALs */
asTag(row) = TYNUM;
asTag(col) = TYNUM;
asInt(row) = asInt(col) = 0;

/*  Figure how much data we have to read and set the file pointer to the beginning of file*/
startIndex = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,0,0);
fileSize = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,0,2);
(*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,startIndex,1);


/* The import function will read the .TAB file records into Spreadsheet cells. */
/* or read the .SBF file records into the Smarttable */

dataSize = 0;
maxColN = 0;
recordCount = 0;

while (TRUE)
    {

	/* fileSize is initially set to the size of the file. If the file	 */
	/* is larger than 32k, this loop will call the FConio_Fill32KBuffer  */
	/* function to fill the bufferHdl buffer in 32k chunks. */

	if (fileSize > 0)
		{
		startIndex = recIndex = curCharIndex = 0;	/* reset all buffer indices */
		*ec = FConio_Fill32KBuffer(gCP,gTP,&fileSize, &dataSize, theFile, bufferHdl);
		ExitOnError(*ec);
		}
	else 
		goto CleanUp;

	while (dataSize > 0)
	{
    /* If we are returning records only, return the record string here. */
    if (isRecordsOnly)
        {
		/* Scan the data buffer for a record */
		FConio_GetRecord(bufferHdl, &recordSize, &recIndex, &recordType, &recordCount, isSBFfile);

		/* Process the columns for this record (also known as row) */
		/* Initialize the row and column specifier for the first column of this new record */
		/* Convert the record number (row number) to a zero based index  */
		asInt(row) = recordCount - 1;
		asInt(col) = 0;
		dataSize -= recordSize;

		/* Call the setImport method of the Lambda to store the imported row text. */
		*result = TLambda_GetPvMember(gCP,gTP,*theTarget,"setImport");
		ExitOnError(*result);
        prmv[0] = *row;
        prmv[1] = FSmartbase_CnvFromText(gCP,gTP,&atHMChar(bufferHdl,curCharIndex));
		*recordVector = FSmartbase_Evalv(gCP,gTP,*result,2,&prmv[0]);
		ExitOnError(*recordVector);
        curCharIndex += recordSize;
        recordSize = 0;

		/* We have now finished importing all the columns for this record.*/
		/* Update the data buffer index (recIndex) to point to the	*/
		/* beginning of the next record */

		recIndex = curCharIndex;
		}
    else
    /* If we are returning record vectors, make a new record vector here. */
    if (isRecordVectors)
        {
		/* Scan the data buffer for a record */
		FConio_GetRecord(bufferHdl, &recordSize, &recIndex, &recordType, &recordCount, isSBFfile);

		/* Process the columns for this record (also known as row) */
		/* Initialize the row and column specifier for the first column of this new record */
		/* Convert the record number (row number) to a zero based index  */
		asInt(row) = recordCount - 1;
		asInt(col) = 0;
		dataSize -= recordSize;

		/* Call the refImport method of the Lambda to get the new row vector. */
		*result = TLambda_GetPvMember(gCP,gTP,*theTarget,"refImport");
		ExitOnError(*result);
        prmv[0] = *row;
		*recordVector = FSmartbase_Evalv(gCP,gTP,*result,1,&prmv[0]);
		ExitOnError(*recordVector);

		/* Parse each tab delimited field and set the parsed field data into */
		/* the empty record container provided by the Lambda (see refImport above). */
		while (recordSize > 0)
			{
			FConio_GetColumn(bufferHdl, &recordSize, &curCharIndex);
			/* curCharIndex points to the byte after the column delimiter */
			/* The number of bytes in the column will be deducted from recordSize */
			switch (recordType)
				{
				case COLUMNREC:

					/*  Convert the name to a Symbol */
					aSymbol = TSymbol_MakeUnique(gCP,gTP,&atHMChar(bufferHdl, startIndex));
					/* Add a new column, with this name, to the Lambda. */
					*result = TLambda_GetPvMember(gCP,gTP,*theTarget,"setColName");
					ExitOnError(*result);
					prmv[0] = *col;
					prmv[1].u.Object = (TObject*)aSymbol;
					prmv[1].Tag = aSymbol->itsObjectType;
					*result = FSmartbase_Evalv(gCP,gTP,*result,2,&prmv[0]);
					ExitOnError(*result);

					/* For .SBF files we need to count the number of column names in the column record */
					maxColN += 1;
					break;

				case TYPEREC:
					/* ignore */	
					break;

				case KEYDEFREC:
					/* ignore */	
					break;
					
				case DATAREC:
				default:
 					/* For SBF files, truncate excess columns */
					if (isSBFfile && (asInt(col) >= maxColN))
						{
						break;
						}
					/* Set the column and row value into the Lambda. */
					/* Handle the case where the column is empty */
					if (startIndex == curCharIndex )
						{
						/* If we are passing recordVectors, */
						/* set into the record vector here. */
						if (isRecordVectors)
							{
							prmv[0] = *recordVector;
							prmv[1] = *col;
							prmv[2] = gCP->Tval_VOID;
							*result = FSmartbase_Setv(gCP,gTP,3,&prmv[0]);
							ExitOnError(*result);
							}
						else
							{
							/* No data is treated as a #void. */
							prmv[0] = *theTarget;
							prmv[1] = *col;
							prmv[2] = *row;
							prmv[3]= gCP->Tval_VOID;			
							*result = FSmartbase_Setv(gCP,gTP,4,&prmv[0]);
							ExitOnError(*result);
							}
						}
					else
						{
						/* Parse the string to get the data. */
						prmv[0] = *theTarget;
						prmv[1] = *col;
						prmv[2] = *row;
						prmv[3] = FConio_LexBuffer(gCP, gTP, bufferHdl, startIndex, curCharIndex - 1);			
						ExitOnError(prmv[3]);
						/* Here we special case the "" value, because */
						/* export will output "" as a single tab char */
						/* and import will reimport as a #void. */
						if ((prmv[3].Tag == TYTEXT) && (prmv[3].u.Text[0] == 0))
							prmv[3]= gCP->Tval_VOID;    
 						/* If we are passing recordVectors, */
						/* set into the record vector here. */
						if (isRecordVectors)
							{
							prmv[0] = *recordVector;
							prmv[1] = *col;
							prmv[2] = prmv[3];
							*result = FSmartbase_Setv(gCP,gTP,3,&prmv[0]);
							/* We do not check for errors here */
							/* because we want to ignore extra data. */
							}
						else
							{
							/* For fieldsOnly processing, just return the field */
							*result = FSmartbase_Setv(gCP,gTP,4,&prmv[0]);
							ExitOnError(*result);
							}
						}
				break;

				}	/* End switch */

				/* Update startIndex so that it points to the new column position */
				startIndex = curCharIndex;

				/* Update the column count */
				++asInt(col);
   
			} /* End while loop for processing columns */

		/* We have now finished importing all the columns for this record.*/
		/* Update the data buffer index (recIndex) to point to the	*/
		/* beginning of the next record */

		recIndex = curCharIndex;

		/* Pass the filled record container to the Lambda here. */
 		*result = TLambda_GetPvMember(gCP,gTP,*theTarget,"setImport");
		ExitOnError(*result);
        prmv[0] = *row;
        prmv[1] = *recordVector;
		*result = FSmartbase_Evalv(gCP,gTP,*result,2,&prmv[0]);
		ExitOnError(*result);
		} /* End if for recordVectors processing */
	else
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}


	
	}	/* End while loop for processing records  */
} /* End while loop for processing file */


    
/*  Dispose of the 32K handle after the file has been written. */

CleanUp:


/* return the number of rows that were read */
FrameExit(TINT(recordCount));

BadCleanUp:

FrameExit(*ret);
 
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_ExportTAB

(gCP,gTP,exportTab file Lambda mode)

fileID			The fileID from a previous fileOpen function.
Lambda           The Lambda from which we are exporting (or an Object Repository).
mode:			The type of export: 
									recordVectors:
									recordsOnly:

#endif

TVAL FConio_ExportTAB(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 bufferSize = _FSmartbase_MAXBUFFERLEN;
LpCHAR              bufferPtr;
HMChar              bufferHdl;
BOLE                isRecordsOnly = FALSE;
BOLE				isRecordVectors = FALSE;
NUM                 dataSize;
NUM                 colIndex;
NUM                 rowIndex;
NUM                 maxCols;
NUM                 i;
TVAL                theTarget;
LpCHAR              eolPtr;
NUM                 eolLen;
BOLE				continueExporting = TRUE;
StartFrame
DeclareOBJ(TByteVector,buffer);
DeclareOBJ(TLambda,pp);
DeclareOBJ(TObjVector,ov);  
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(result);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareTVALArray(prmv,4);
DeclareTVAL(recordVector);
DeclareTVAL(recordsOnly);
DeclareTVAL(eol);
DeclareTVAL(lengthCmd);

EndFrame

/*  Check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* Perform some miscellaneous initialization */
*eol= TGVALUE("_eol");
*lengthCmd = TFUNCTION(FUtil2_Length);

/*  The optional third argument must be one of the following valid options. */
if ((argc == 3) && 
    (argv[0].Tag == TYNUM) &&
    (argv[2].Tag == TYSYMBOL) &&
    (strcmp(SymbolArray(argv[2]),"recordsOnly") == 0))
    {
    isRecordsOnly = TRUE;
    isRecordVectors = FALSE;
    }
else
if ((argc == 3) && 
    (argv[0].Tag == TYNUM) &&
    (argv[2].Tag == TYSYMBOL) &&
    (strcmp(SymbolArray(argv[2]),"recordVectors") == 0))
    {
    isRecordsOnly = FALSE;
    isRecordVectors = TRUE;
    }
else
	{
	goto ExitNow;
	}

/*  Save the target argument. */
theTarget = argv[1];

if (theTarget.Tag == TYLAMBDA)
    {
    pp = (TLambda*)asObject(&theTarget);
    }
else
   {
   ExitNow:
   *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
   FrameExit(*ret);
   }
    
/*  Get length of and pointer to end of line string data. */
FSmartbase_StringPtr(gCP,gTP,eol,&eolPtr,&eolLen);

/*  Request a handle for writing the maximum possible record to the file. */
buffer = TByteVector_New(gCP,gTP);
tmp->u.Object = (TObject*)buffer;
tmp->Tag = buffer->itsObjectType;
TByteVector_SetMaxIndex(gCP,gTP,*tmp,bufferSize + 100);

bufferHdl = buffer->itsByteArray;
if (bufferHdl == NULL)
   {
   *ret = gCP->TObject_ERROR_INVALID;
   FrameExit(*ret);
   }
bufferPtr = (LpCHAR)*bufferHdl;

/* Loop forever reading records from the Lambda and exporting them to the file. */
/* Note: When the Lambda returns FALSE (instead of a record), we quit exporting. */

rowIndex = 0;
while (continueExporting)
    {
    /*  Clear the ascii record for the next row's data values. */
    dataSize = 0;
    bufferPtr[0] = 0;

   /* If we are exporting records only, get the record string here. */
    if (isRecordsOnly)
        {
        /* Request a record string from the Lambda. */
		*result = TLambda_GetPvMember(gCP,gTP,theTarget,"refExport");
		ExitOnError(*result);
        prmv[0] = TINT(rowIndex);
		*recordsOnly = FSmartbase_Evalv(gCP,gTP,*result,1,&prmv[0]);
		ExitOnError(*recordsOnly);
		if ((recordsOnly->Tag == TYBOLE) && (recordsOnly->u.Bool == FALSE)) 
			{
			continueExporting = FALSE;
			goto Quit; 
			}
		TObject_CnvToText(gCP,gTP,&bufferPtr[dataSize],(bufferSize - dataSize),*recordsOnly);            
        }
	else
    if (isRecordVectors)
		{
		/* We are exporting record vectors, ask the Lambda for a single vector */
		/* Call the refExport method of the Lambda to get the new row vector. */
		*result = TLambda_GetPvMember(gCP,gTP,theTarget,"refExport");
		ExitOnError(*result);
		prmv[0] = TINT(rowIndex);
		*recordVector = FSmartbase_Evalv(gCP,gTP,*result,1,&prmv[0]);
		ExitOnError(*recordVector);
		if ((recordVector->Tag == TYBOLE) && (recordVector->u.Bool == FALSE))
			{
			continueExporting = FALSE;
			goto Quit; 
			}

		/* We are exporting record vectors, ask the vector for its length. */
		/* Note: We use generic calls to support any type of Vector, Structure, or Dictionary. */
		*result = FSmartbase_Evalv(gCP,gTP,*lengthCmd,1,recordVector);
		ExitOnError(*result);
		maxCols = asNumIndex(result);
		colIndex = 0;

		/*  Export a value referenced by a column and row attribute */
		while (colIndex < maxCols)
			{
			/* If we have received a recordVector, */
            /* get a single item (a column item )  */
			/* and copy into the output buffer.    */
			prmv[0] = *recordVector;
			prmv[1] = TINT(colIndex);
			*result = FSmartbase_Refv(gCP,gTP,2,&prmv[0]);
            ExitOnError(*result);

			FConio_sprint(gCP,gTP,(bufferSize-dataSize),&bufferPtr[dataSize],*result);
			dataSize = strlen((char *)&bufferPtr[0]) + 1;

			/*  Append a tab to the end of each column data	*/
			bufferPtr[dataSize - 1] = '\t';
			bufferPtr[dataSize] = 0;
			++colIndex;
			} /* End while loop to process row & column data */
		} /* End if for recordVectors processing */

    dataSize = strlen((char *)&bufferPtr[0]);
	/* This caused the last character to be overwritten */
	/* dataSize = dataSize == 0 ? 0 : dataSize - 1; */

    /*  Append an _eol (END OF LINE) to the end of the data row  */
    for (i = 0; i < eolLen; i++)
        {
        bufferPtr[dataSize++] = eolPtr[i];
        bufferPtr[dataSize] = 0;
        }

    /*  Write the record to the file. */
    (*gCP->_Host_Writef)((POINTER)gCP,gTP, asNumIndex(&argv[0]), strlen((char *)&bufferPtr[0]), (char *)&bufferPtr[0]);

    ++rowIndex;
    } /* End while loop to process exported Lambda data rows */

/*  Return the number of rows exported. */
Quit:
FrameExit(TINT(rowIndex));
}

/*-------------------------*/
/* Posix file IO functions */
/*-------------------------*/

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_IO_fclose

(gCP,gTP,fileID,opcode);

fileID		The fileID returned from a previous FConio_IO_fopen invocation.
opcode	    The file close option. A value of: 
				(0) = erase file after closing, 
				(1) = don't erase file after closing).

#endif

NUM	 FConio_IO_fclose(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM opcode)
{
NUM		result = 0;
FILE*	fileHDL;

if ((fileID<=0) || (fileID>=MAXFILEHANDLES) || (gFileIOHandles[fileID] == 0))
	{
	return(-1); /* Invalid fileID argument */
	}

/* Issue the posix file close command */
fileHDL = (FILE*)gFileIOHandles[fileID];
if (fileHDL!=NULL) result = fclose(fileHDL);
if (opcode==0) remove(&gFileIONames[fileID][0]);

/* Empty the posix file handle in the fileID slot and return the result */
gFileIOHandles[fileID] = 0;
gFileIONames[fileID][0] = 0;
gFileIOTypes[fileID] = 0;
gFileIOUsers[fileID] = 0;
gFileIOContexts[fileID] = 0;
return(result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_IO_fremove

(gCP,gTP,char* name);

name		The path and file name of the file to be erased.

#endif

NUM	 FConio_IO_fremove(LpXCONTEXT gCP,LpTHREAD gTP,char* name)
{

/* Erase the specified file */
remove(name);

return(0);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_IO_fread

(gCP,gTP,NUM fileID, NUM length, char* bufptr)

fileID		The fileID returned from a previous FConio_IO_fopen invocation.
length	    The maximum number of bytes to read from the file. 
bufptr		The location to place any bytes read.

#endif

NUM	 FConio_IO_fread(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM length, char* bufptr)
{
NUM		result = 0;
FILE*	fileHDL;

if ((fileID<=0) || (fileID>=MAXFILEHANDLES) || (gFileIOHandles[fileID] == 0))
	{
	return(-1); /* Invalid fileID argument */
	}

/* Issue the posix file read command */
fileHDL = (FILE*)gFileIOHandles[fileID];
result = (NUM)fread(bufptr, length, 1, fileHDL);
if (result < 0)
	{
	return(result);
	}
return(0);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_IO_fwrite

(gCP,gTP,NUM fileID, NUM length, const char* bufptr)

fileID		The fileID returned from a previous FConio_IO_fopen invocation.
length	    The maximum number of bytes to write to the file. 
bufptr		The location from which to write any bytes.

#endif

NUM	 FConio_IO_fwrite(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM length, const char* bufptr)
{
#define		errFileWrite        20002
NUM			result = 0;
FILE*		fileHDL;

if ((fileID<=0) || (fileID>=MAXFILEHANDLES) || (gFileIOHandles[fileID] == 0))
	{
	return(-1); /* Invalid fileID argument */
	}

/* Issue the posix file write command */
fileHDL = (FILE*)gFileIOHandles[fileID];
result = fwrite(bufptr, length, 1, fileHDL);
if (result != 1) 
	{
	FSmartbase_Throw(gCP,gTP,(const NUM)errFileWrite);
	}
/* Empty the posix file handle in the fileID slot and return the result */
return(0);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_IO_fseek

(gCP,gTP,fileID,adjustment,opcode);

fileID		The fileID returned from a previous FConio_IO_fopen invocation.
adjustment	The file adjustment, in bytes, to make to the read/write position of the file. 
code	    The file seek option. A value of: 
				(0)=add offset to current location; 
				(1)=add offset to start of file; 
				(2)=add offset to end of file

#endif

REAL	FConio_IO_fseek(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID,REAL adjustment,NUM code)
{
REAL	result = 0.0;
FILE*	fileHDL;
long	offset;
long	new_offset;
fpos_t	new_pos;
int		whence;

if ((fileID<=0) || (fileID>=MAXFILEHANDLES) || (gFileIOHandles[fileID] == 0))
	{
	return(-1); /* Invalid fileID argument */
	}

/* Seek in the file from the location specified by code */
switch(code)
    {
    case 0:
        whence = SEEK_CUR;
    break;

    case 1:
        whence = SEEK_SET;
    break;

    case 2:
        whence = SEEK_END;
    break;

    default:
        /*  Invalid seek code. */
        return(-1);
    break;
	}

/* Perform the seek and return the result */
fileHDL = (FILE*)gFileIOHandles[fileID];
offset = (long)adjustment;
fseek(fileHDL, offset, whence);
fgetpos(fileHDL, &new_pos);
#ifdef _LINUX
new_offset = new_pos.__pos;
#else
new_offset = new_pos;
#endif
return (new_offset);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_IO_fresize

(gCP,gTP,fileID,newsize);

fileID		The fileID returned from a previous FConio_IO_fopen invocation.
newsize		The new file size, in bytes, after resizing of the file. 

#endif

REAL	FConio_IO_fresize(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID,REAL newsize)
{
REAL	result = 0.0;
FILE*	fileHDL;
long	offset;
int		whence= SEEK_SET;

return(0);

if ((fileID<=0) || (fileID>=MAXFILEHANDLES) || (gFileIOHandles[fileID] == 0))
	{
	return(-1); /* Invalid fileID argument */
	}

/* Issue the posix file close command */
fileHDL = (FILE*)gFileIOHandles[fileID];
offset = (long)newsize;
result = fseek(fileHDL,offset,whence);
fputc(EOF,fileHDL);

/* Empty the posix file handle in the fileID slot and return the result */
return(result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FConio_IO_fopen

(gCP,gTP,name,mode,type)

name		The full path and file name of the file to be opened.
mode	    The mode switch. A value of: 
             (0) requests that only an existing file be opened; 
             (1) indicates that a new file will be created. If a file already exists with this name, it will be destroyed and a new virgin file will be created; 
			 (2) valid for database files only, indicates that an existing database file be opened for read only transactions shared with other users.
type		The file type switch. A value of: 
			 (0) requests that a text file be opened; 
			 (1) requests that a Spreadsheet binary file be opened;
			 (2) requests that a Smarttable binary file be opened; 
			 (3) requests that a Workspace binary file be opened; 
			 (4) requests that an Object binary file be opened; 
			 (5) requests that an Object Database file be opened.

Notes on posix file mode codes.

The mode string consists of: 
1) A "r", "w" or "a", followed by 
2) An optional "b", followed by 
3) An optional "+".

Here are the meanings of the different options:

r Open the file for reading. 
w Open the file for overwriting. Creates a new file if it doesn't already exist. 
a Open the file for appending (i.e. new data is added at the end). Creates a new file if it doesn't already exist. 
b Opens the file as a binary file (we always open ALL files as binary). 
+ Open the file for reading and writing. 


#endif

NUM	 FConio_IO_fopen(LpXCONTEXT gCP,LpTHREAD gTP,char* pathName, NUM mode, NUM type)
{
BOLE            resizeSW = FALSE;
NUM				foundASlot;
NUM             slotIndex;
NUM				slotIndex2;
NUM				rt;
NUM				fileHandle;		/* file handle returned from os file open command */
char            modeString[2][8];
NUM				sleepCounter = MAXSLEEPSBEFORIOEERROR;
NUM				numModes = 0;
NUM				m;
char			name[MAXFILENAMESIZE + 1];
TVAL			sleep = TGVALUE("sleep");

/* Check the arguments to make sure they are correct */
if ((strlen(pathName) >= MAXFILENAMESIZE) || (mode < 0) || (mode > 3) || (type < 0) || (type > 5))
    return(0);

/* Convert filename to consistent system full path format */
for (m=0; (m<MAXFILENAMESIZE) && (pathName[m] != '\0'); ++m)
{
#ifdef _WIN
	if (pathName[m] == '/') 
		name[m] = '\\';
	else
		name[m] = pathName[m];
#else
	if (pathName[m] == '\\')
		name[m]  = '/';
	else
		name[m] = pathName[m];
#endif
}
name[m] = '\0';
fileHandle = 0;

/* Set the modeString[] by file type and mode. The modeString array contains
   one or more elements that specify a possible way to open the file.
   Usually there are only two possible modes with the objective of giving 
   the user some level of access if full access can not be granted.
   This nested switch also makes it easier to spot bad modes etc. The old code
   in ansShell did not catch bad modes for the various file types. */
numModes = 0;
switch(type) {
	case 0: /* Text file */
#ifdef _WIN
		if (mode == 0) { /* Open an existing file */
			strcpy(modeString[numModes++],"rb+"); /* open existing binary for read/write */
			strcpy(modeString[numModes++],"rb");  /* open existing binary for read only */
			resizeSW = FALSE;
			}
		else 
		if (mode == 1) { /* Open a new file */
			strcpy(modeString[numModes++],"wb+"); /* open new binary file for for read/write */
			strcpy(modeString[numModes++],"wb");  /* open new binary file for write only */
			resizeSW = TRUE;
			}
		else 
			return(0);
#else
		if (mode == 0) { /* Open an existing file */
			strcpy(modeString[numModes++],"r+"); /* open existing text file for read/write */
			strcpy(modeString[numModes++],"r");	/* open existing text file for read only */
			resizeSW = FALSE;
			}
		else 
		if (mode == 1) { /* Open a new file */
			strcpy(modeString[numModes++],"w+"); /* open new text file for read/write */
			strcpy(modeString[numModes++],"w");	 /* open new text file for write only */
			resizeSW = TRUE;
			}
		else 
			return(0);
#endif
		break;
	case 1: /* Spreadsheet binary file */
	case 2: /* Smarttable binary file */
	case 3: /* Workspace binary file */
	case 4: /* Object binary file */
		switch(mode) {
			case 0:  /* Open an existing file */
				strcpy(modeString[numModes++],"rb+");	/* open existing binary file for read/write */
				strcpy(modeString[numModes++],"rb");	/* open existing binary file for read only */
				resizeSW = FALSE;
				break;
			case 1:  /* Open a new file */
				strcpy(modeString[numModes++],"wb+");	/* open new binary file for read/write */
				strcpy(modeString[numModes++],"wb");	/* open new binary file for write only */
				resizeSW = TRUE;
				break;
			default:
			return(0);
			} /* end of mode switch */
		break;
	case 5: /* Object Database file */
		switch(mode) {
			case 0:  /* Open an existing database file */
				strcpy(modeString[numModes++],"rb+");	/* open existing binary file for read/write */
				resizeSW = FALSE;
				break;
			case 1: /* open an existing or new database file */
				strcpy(modeString[numModes++],"rb+");	/* open existing binary file for read/write */
				strcpy(modeString[numModes++],"wb+");	/* open new binary file for read/write */
				/* SPECIAL MODE CASE -- must check on open to see how to set resizeSW!!! */
				break;
			case 2: /* open an existing database for retrieval only */
				strcpy(modeString[numModes++],"rb");	/* open existing binary for read only */
				resizeSW = FALSE;
				break;
			case 3: /* Open an inserted database file */
				strcpy(modeString[numModes++],"rb+");	/* open existing binary for read/write */
				resizeSW = FALSE;
				break;
			} /* end of mode switch */
		break;
	} /* end of file type switch */

/* Find the next open file slot */
switch(type) {
    case 0: /* Text file */
	case 1: /* Spreadsheet binary file */
	case 2: /* Smarttable binary file */
	case 3: /* Workspace binary file */
	case 4: /* Object binary file */
		/* find the next open file slot */
		foundASlot = 0;
		for (rt = 0; (rt < 2) && (foundASlot == 0); rt++) { /* retries */

			/* see if filename is already in use */
			for(slotIndex2=1; (slotIndex2 <= gMaxIOFiles) ; ++slotIndex2) {
#ifdef _WIN
				if ((_stricmp(gFileIONames[slotIndex2],name) == 0) && (gFileIOHandles[slotIndex2] != 0)) {
#else
				if ((strcmp(gFileIONames[slotIndex2],name) == 0) && (gFileIOHandles[slotIndex2] != 0)) {
#endif
					return(0); /* Application error: file is already in use by another context */

				}
			}

			for (slotIndex=1; (slotIndex <= gMaxIOFiles) && (foundASlot == 0); slotIndex++) { /* file slots */
				if (gFileIOHandles[slotIndex] == 0) 
					{ /* see if slot is used */

					/* Make sure filename is not already in use */
					for(slotIndex2=1; (slotIndex2 <= gMaxIOFiles) ; ++slotIndex2) {
#ifdef _WIN
						if ((_stricmp(gFileIONames[slotIndex2],name) == 0) && (gFileIOHandles[slotIndex2] != 0)) {
#else
						if ((strcmp(gFileIONames[slotIndex2],name) == 0) && (gFileIOHandles[slotIndex2] != 0)) {
#endif
							return(0); /* Application error! Filename already in use */
							}
					}

					gFileIOHandles[slotIndex] = 1;
					foundASlot = 1; /* flag to exit rt and slotIndex loop */
					break;

					} /* end if (gFileIOHandles[slotIndex] == 0) */
				} /* slotIndex */
			} /* rt */

		if (foundASlot == 0) 
			{
			return(0); /* Application error! No open slots */
			}

		/*********************************************/
		/* if we got here we have an empty file slot */
		/*********************************************/

        /*  Open the file, return if there is an error. */
		m = 0; /* try modeString[0] first */
		while (m < numModes) {
			if ((fileHandle = (NUM)fopen(name,modeString[m++])) != 0) 
				break; /* found a slot! */
			}

		if (fileHandle == 0) { /* Could not open file so fail gracefully if possible */
			gFileIOHandles[slotIndex] = 0; /* clear the place holder */
			return(0);
			}
 
        /*  Add this file to the list of open files. */
        gFileIOHandles[slotIndex] = fileHandle;
        gFileIOTypes[slotIndex] = type;
		gFileIOUsers[slotIndex] = 1;
        strcpy(gFileIONames[slotIndex],name);
        gFileIOContexts[slotIndex] = (NUM)gCP;
        return(slotIndex);

	break;
    
	case 5:  /* open database file */
		/* Look to see if this database file is already open.	*/
		/* Note: If so, we piggy back on this file if the open  */
		/* call came from the same context that has it open.	*/
		/* We Never open the database file twice!				*/

		DatabaseOpenrt:
		for(slotIndex=1; slotIndex <= gMaxIOFiles; slotIndex++) 
			{
			if ((gFileIOHandles[slotIndex] != 0) &&  /* Open file */
				(gFileIOTypes[slotIndex] == 5) &&  /* Database file */
#ifdef _WIN
				(_stricmp(gFileIONames[slotIndex],name) == 0)) /* Database file name */
#else
				(strcmp(gFileIONames[slotIndex],name) == 0)) /* Database file name */
#endif
				{
				/*  Never open database files from two different contexts.  */
				/*  Note: If a database file is open in one context, it is  */
				/*        unavailable to all other contexts. A database can */
				/*        only be open in one thread at a time.             */
                if (gFileIOContexts[slotIndex] != (NUM)gCP) {
					/* If the database file is open in another context then */
					/* rt for a bit to see if we can get access */

					while ((gFileIOHandles[slotIndex] != 0) && 
						   (gFileIOTypes[slotIndex] == 5) && 
						   (strcmp(gFileIONames[slotIndex],name) == 0)) {
						--sleepCounter;
						if (sleepCounter <= 0) 
							return(0);
						FSmartbase_Eval(gCP,gTP,sleep,1,TINT(1));
						} 
					goto DatabaseOpenrt;
					}
				else {
					/* We piggy back on any previous database file opens from this context */
					if ((gFileIOHandles[slotIndex] != 0) &&
						(gFileIOTypes[slotIndex] == 5) &&
						(strcmp(gFileIONames[slotIndex],name) == 0)) {
						++gFileIOUsers[slotIndex];
						return(slotIndex); /* We are riding piggy back, ye ha! */
						}
					else
						goto DatabaseOpenrt;
					}
				}
			}

		/* If we got here we need to open the database */

		/* Check again that no other context has gotten in and opened the database file */
		/* This time we do not sleep between checks as this would block all contexts from */
		/* doing database file opens - a bad idea mate! */
		for(slotIndex=1; slotIndex <= gMaxIOFiles; slotIndex++) 
			{
			if ((gFileIOHandles[slotIndex] != 0) &&  /* Open existing file */
				(gFileIOTypes[slotIndex] == 5) &&  /* Database files only */
				(strcmp(gFileIONames[slotIndex],name) == 0)) {
                if (gFileIOContexts[slotIndex] != (NUM)gCP) {
					return(0); /* Some other context got in and opened the db */
					}
				/* If we got here somthing is seriously wrong. It would mean that the current */
				/* context was able to open the database. This would mean there is an error */
				/* somewhere above this point in this routine. */
				return(0); /* This should never happen. TM! maybe this should be a throw? */
				}
			}

		/* find the next open file slot so we can use the empty slot */
		foundASlot = 0;
		for (rt = 0; (rt < 2) && (foundASlot == 0); rt++) 
			{
			for (slotIndex=1; (slotIndex <= gMaxIOFiles) && (foundASlot == 0); slotIndex++) 
				{
				if (gFileIOHandles[slotIndex] == 0) 
					{ /* try to lock this slot */
					gFileIOHandles[slotIndex] = 1; /* place holder until we get a real filehandle */
					foundASlot = 1; /* flag to exit rt and slotIndex loop */
					break;
					}
				} /* slotIndex */
			} /* rt */

		if (foundASlot == 0) {
			return(0); /* Application error! No open slots */
			}

        /* Make sure we set the mode switches correctly. */
        if (mode == 0) 
			{
            /*  Open an existing database file for update and access. */
            fileHandle = (NUM)fopen(name,"rb+");
            if (fileHandle == 0) 
				{
				gFileIOHandles[slotIndex] = 0;
				return(0); /* Application error! No file of that name */
				}
            }
        else        
        if (mode == 1) 
			{
            /*  Create and open a database file for new. */
            fileHandle = (NUM)fopen(name,"rb+");
            if (fileHandle == 0) 
				{
                fileHandle = (NUM)fopen(name,"wb+");
                if (fileHandle == 0) 
					{
					gFileIOHandles[slotIndex] = 0;
					return(0); /* fopen for new file failed for filename */ 
					}
                }
            else {
                resizeSW = TRUE;
                }

           }
        else
        if (mode == 2) 
			{
            /*  Open an existing file for retrieval only (no update allowed). */
            fileHandle = (NUM)fopen(name,"rb");
            if (fileHandle == 0) 
				{
				gFileIOHandles[slotIndex] = 0;
				return(0);  /* fopen for existing file failed for filename. */
				}
            }
        else
        if (mode == 3) 
			{
            /*  Open an inserted database file for update and access. */

            fileHandle = (NUM)fopen(name,"rb+");
            if (fileHandle == 0) 
				{
				gFileIOHandles[slotIndex] = 0;
				return(0);  /* fopen failed for inserted database with filename */
				}
            }
        else {
            /*  Anything else is an invalid mode. */
			return(0);  /* Application error! Invalid mode */
            }

        /*  Add this file to the list of open files. */
        gFileIOHandles[slotIndex] = fileHandle;
        gFileIOTypes[slotIndex] = type;
        gFileIOUsers[slotIndex] = 1;
        strcpy(gFileIONames[slotIndex],name);
        gFileIOContexts[slotIndex] = (NUM)gCP;
 
        /*  Reset database size to zero. */
        if (resizeSW) {
				/* POSIX does not support releasing file space so this is a no op */
			}

		return(slotIndex);
		break;
    
    default:
        /*  We don't recognize this mode type. */
        return(0); /* Application error! Invalid mode */
		break;
    }
}

