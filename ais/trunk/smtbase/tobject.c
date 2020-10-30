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

#define _C_TOBJECT
#define _SMARTBASE
#if 0
TObject.c

Methods for the typed object class which is at the root of the typing hierarchy.
TObject is the class which supports runtime typing and automatic garbage collection.

Note:   This class contains the support for the C++ extensions such as: automated
        mark and sweep garbage collection, manifest types, and oodbms save and load.

AUTHORS:            Michael F. Korns

CHANGE HISTORY
Version	Date		Who		Change
		01/29/2006	TMay	rewrote TObject_savefile to be more maintainable and
							ehanced the sizeof behavior to use a buffer only as large
							as the largest single object reached in the object
							closure.
		12/15/2006	TMay	modified TObject_saveFile and TObject_loadFile to accept
							TVAL as well as Object targets.
		12/9/2006	TMay	modified TOjbect_loadFile to accept bufferPtr argument.
							Also see FConio_loadObject changes.
		12/9/2006	TMay	modified TObject_saveFile to accept args: object, bufferLen, bufferPtr
							Also see FConio_saveObject changes.

#endif

#include "tobject.h"
#include "fobject.h"
#include "ffloat.h"
#include "flisp.h"
#include "futil3.h"
#include "fpred2.h"
#include "fmake.h"
#include "tdatabas.h"
#include "fdatefnc.h"
#include "terror.h"
#include "tbytevec.h"

#define _TObject_SaveLoadBuffer 65535

extern TVAL     TCpx_Print      (LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);
extern void     TCpxVector_Init       (LpXCONTEXT gCP,LpTHREAD gTP);

HMemory TObject_NewLoadObject(NUM theFile, HMemory anHMemory, NUM aFileID, NUM bResolve );


/*  Define the new type variables for this Class.    */

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_Init

Initialize the host operating system and the C++ object oriented extensions.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TObject_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
NUM             indexOf;

/*  Don't initialize more than once. */
if (gCP->TObject_Initialized) return;
gCP->TObject_Initialized = TRUE;

gTP->TObject_ErrorTree = gCP->Tval_VOID;
gCP->TObject_GarbageON = TRUE;
gCP->TObject_GarbageCollectInProgress = FALSE;
gCP->TObject_GarbageInhibit = 0;
gCP->TObject_MaxOutputLen = _FSmartbase_MAXBUFFERLEN;
gTP->ObjStackIdx = 0;

/* DEBUG FSmartbase_Log(gCP, gTP, "TObject_Init(), Initialize CHAR test macro\r\n"); */			/* NDEBUG */
/* Initialize CHAR test macro array. */
for (indexOf = 0; indexOf < 256; indexOf++)
    gCP->TObject_Ctype[indexOf] = 0;

gCP->TObject_Ctype[0]    = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[1]    = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[2]    = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[3]    = CHRCNTRL|CHRASCII|CHRSPACE;
gCP->TObject_Ctype[4]    = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[5]    = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[6]    = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[7]    = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[8]    = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[9]    = CHRCNTRL|CHRASCII|CHRSPACE;
gCP->TObject_Ctype[10]   = CHRCNTRL|CHRASCII|CHRSPACE;
gCP->TObject_Ctype[11]   = CHRCNTRL|CHRASCII|CHRSPACE;
gCP->TObject_Ctype[12]   = CHRCNTRL|CHRASCII|CHRSPACE;
gCP->TObject_Ctype[13]   = CHRCNTRL|CHRASCII|CHRSPACE;
gCP->TObject_Ctype[14]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[15]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[16]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[17]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[18]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[19]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[20]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[21]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[22]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[24]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[25]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[26]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[27]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[28]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[29]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[30]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[31]   = CHRCNTRL|CHRASCII;
gCP->TObject_Ctype[32]   = CHRSPACE|CHRASCII;
gCP->TObject_Ctype[33]   = CHRASCII|CHRPUNCT|CHRSYMBOL;
gCP->TObject_Ctype[34]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[35]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[36]   = CHRASCII|CHRPUNCT|CHRSYMBOL;
gCP->TObject_Ctype[37]   = CHRASCII|CHRPUNCT|CHRSYMBOL;
gCP->TObject_Ctype[38]   = CHRASCII|CHRPUNCT|CHRSYMBOL;
gCP->TObject_Ctype[39]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[40]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[41]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[42]   = CHRASCII|CHRSYMBOL|CHROPER;
gCP->TObject_Ctype[43]   = CHRASCII|CHRSYMBOL|CHROPER;
gCP->TObject_Ctype[44]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[45]   = CHRASCII|CHRSYMBOL|CHROPER;
gCP->TObject_Ctype[46]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[47]   = CHRASCII|CHRSYMBOL|CHROPER;
gCP->TObject_Ctype[48]   = CHRDIGIT|CHRHEX|CHROCTAL|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[49]   = CHRDIGIT|CHRHEX|CHROCTAL|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[50]   = CHRDIGIT|CHRHEX|CHROCTAL|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[51]   = CHRDIGIT|CHRHEX|CHROCTAL|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[52]   = CHRDIGIT|CHRHEX|CHROCTAL|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[53]   = CHRDIGIT|CHRHEX|CHROCTAL|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[54]   = CHRDIGIT|CHRHEX|CHROCTAL|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[55]   = CHRDIGIT|CHRHEX|CHROCTAL|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[56]   = CHRDIGIT|CHRHEX|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[57]   = CHRDIGIT|CHRHEX|CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[58]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[59]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[60]   = CHRASCII|CHRSYMBOL|CHROPER;
gCP->TObject_Ctype[61]   = CHRASCII|CHRSYMBOL|CHROPER;
gCP->TObject_Ctype[62]   = CHRASCII|CHRSYMBOL|CHROPER;
gCP->TObject_Ctype[63]   = CHRASCII|CHRPUNCT|CHRSYMBOL;
gCP->TObject_Ctype[64]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[65]   = CHRALPHA|CHRHEX|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[66]   = CHRALPHA|CHRHEX|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[67]   = CHRALPHA|CHRHEX|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[68]   = CHRALPHA|CHRHEX|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[69]   = CHRALPHA|CHRHEX|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[70]   = CHRALPHA|CHRHEX|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[71]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[72]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[73]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[74]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[75]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[76]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[77]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[78]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[79]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[80]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[81]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[82]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[83]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[84]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[85]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[86]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[87]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[88]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[89]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[90]   = CHRALPHA|CHRASCII|CHRUPPER|CHRSYMBOL;
gCP->TObject_Ctype[91]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[92]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[93]   = CHRASCII|CHRPUNCT;
gCP->TObject_Ctype[94]   = CHRASCII|CHRPUNCT|CHRSYMBOL;
gCP->TObject_Ctype[95]   = CHRASCII|CHRSYMBOL;
gCP->TObject_Ctype[96]   = CHRALPHA|CHRASCII|CHRHEX|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[97]   = CHRALPHA|CHRASCII|CHRHEX|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[98]   = CHRALPHA|CHRASCII|CHRHEX|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[99]   = CHRALPHA|CHRASCII|CHRHEX|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[100]  = CHRALPHA|CHRASCII|CHRHEX|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[101]  = CHRALPHA|CHRASCII|CHRHEX|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[102]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[103]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[104]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[105]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[106]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[107]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[108]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[109]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[110]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[111]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[112]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[113]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[114]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[115]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[116]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[117]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[118]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[119]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[120]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[121]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[122]  = CHRALPHA|CHRASCII|CHRLOWER|CHRSYMBOL;
gCP->TObject_Ctype[123]  = CHRPUNCT;
gCP->TObject_Ctype[124]  = CHRPUNCT;
gCP->TObject_Ctype[125]  = CHRPUNCT;
gCP->TObject_Ctype[126]  = CHRPUNCT;
gCP->TObject_Ctype[127]  = CHRPUNCT|CHRCNTRL;

/*  Initialize the C++ object list extensions. */
/* DEBUG FSmartbase_Log(gCP, gTP, "TObject_Init(), Initialize C++ object list extensions\r\n"); */			/* NDEBUG */
gCP->TObject_MaxObjectCount  = gCP->TObject_initMaxObjects;
gCP->TObject_UsedObjectCount = 0;
gCP->TObject_FreeObjectIndex = 1;
gCP->TObject_ObjectFlag =(HMChar)FMemory_New(gCP, gTP, (LONG)gCP->TObject_MaxObjectCount,TRUE);
gCP->TObject_MainObjectList = (HMObj)FMemory_New(gCP, gTP, (LONG)gCP->TObject_MaxObjectCount * sizeof(TObject *),TRUE);
gCP->TObject_MainObjectHeaderArray = (HMObj)FMemory_New(gCP, gTP, (LONG)gCP->TObject_MaxObjectCount * _FSmartbase_ObjectHeaderMaxSize,TRUE);

/*  Set up initial NIL object and free object list. */
/* DEBUG FSmartbase_Log(gCP, gTP, "TObject_Init(), Setup initial NIL object and free object list.\r\n"); */			/* NDEBUG */
_TObject_MainObjectList(0) = NIL;
_TObject_ObjectFlag(0) = _TObject_OfVOID;
for(indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    _TObject_MainObjectList(indexOf) = (OBJ)indexOf+1;
    _TObject_ObjectFlag(indexOf) = _TObject_OfVOID;
	}
_TObject_MainObjectList(gCP->TObject_MaxObjectCount-1) = 0;

/* DEBUG FSmartbase_Log(gCP, gTP, "TObject_Init(), Initialize T_Object* values\r\n"); */			/* NDEBUG */
gCP->TObject_VOID		  = gCP->Tval_VOID;
asTag(&gCP->TObject_OK)    = TYBOLE;    asBool(&gCP->TObject_OK) = TRUE;
asTag(&gCP->TObject_TRUE)  = TYBOLE;    asBool(&gCP->TObject_TRUE) = TRUE;
asTag(&gCP->TObject_FALSE) = TYBOLE;    asBool(&gCP->TObject_FALSE) = FALSE;
asTag(&gCP->TObject_HIGH)  = TYCOMPARE; asCompare(&gCP->TObject_HIGH) = HIGH;
asTag(&gCP->TObject_EQUAL) = TYCOMPARE; asCompare(&gCP->TObject_EQUAL) = EQUAL;
asTag(&gCP->TObject_LOW)   = TYCOMPARE; asCompare(&gCP->TObject_LOW) = LOW;
asTag(&gCP->TObject_NAN)   = TYREAL;
((LpINT32)&gCP->TObject_NAN.u.Real)[0] = (INT32)NAN_64_LO;
((LpINT32)&gCP->TObject_NAN.u.Real)[1] = (INT32)NAN_64_HI;

/* DEBUG FSmartbase_Log(gCP, gTP, "TObject_Init(), Initialize TObject_ERROR_* values\r\n"); */			/* NDEBUG */

gCP->TObject_ERROR_OUT_OF_MEMORY	= TPERROR("!memory!");
gCP->TObject_ERROR_FRAME_ERROR		= TPERROR("!frame!");
gCP->TObject_ERROR_INVALID			= TPERROR("!badarg!");
gCP->TObject_ERROR_DIVIDE_BY_ZERO	= TPERROR("!divzero!");
gCP->TObject_ERROR_INVALID_ARGLIST	= TPERROR("!arglist!");
gCP->TObject_ERROR_RUNTIME			= TPERROR("!runtime!");
gCP->TObject_ERROR_SYNTAX			= TPERROR("!syntax!");
gCP->TObject_ERROR_BADOPERATOR		= TPERROR("!badoper!");
gCP->TObject_ERROR_BADSYMBOL		= TPERROR("!badsymbol!");
gCP->TObject_ERROR_BADINDEX			= TPERROR("!badIndex!");
gCP->TObject_ERROR_BADTYPE			= TPERROR("!badType!");
gCP->TObject_ERROR_BADMSG			= TPERROR("!badMsg!");
gCP->TObject_ERROR_BADCONTINUATION	= TPERROR("!badContinue!");
gCP->TObject_ERROR_CONTINUATION		= TPERROR("!ConInProg!");  /* Special constant for compiler */
gCP->TObject_ERROR_WINDOWACTIVE		= TPERROR("!windowActive!");
gCP->TObject_ERROR_SYMBOLMUSTFOLLOW = TPERROR("!bad#!");
gCP->TObject_ERROR_PAIRINDEXREQ		= TPERROR("!pairIdx!");
gCP->TObject_ERROR_SYMBOLINDEXREQ	= TPERROR("!symbolIdx!");
gCP->TObject_ERROR_NUMINDEXREQ		= TPERROR("!numberIdx!");
gCP->TObject_ERROR_BADIDXORKEY		= TPERROR("!badIdxOrKey!");
gCP->TObject_ERROR_BADIVAL			= TPERROR("!badIVal!");
gCP->TObject_ERROR_ACCESSDENIED		= TPERROR("!noaccess!");

/*  Make sure basic types are registered in the proper order. */
/*  Note:   For speed, the FVMSCRIPT files use hard coded types (like TYNUM) */
/*          in switch statements. These hard coded basic type codes must be */
/*          coordinated with the actual dynamically registered types codes. */
/*          The final comparison is designed to abend if these types are not */
/*          fully synchronized.  */
/* FSmartbase_Log(gCP, gTP, "TObject_Init(), Register basic types.\r\n"); */			/* NDEBUG */
TVector_Init(gCP,gTP);
TBitVector_Init(gCP,gTP);
TByteVector_Init(gCP,gTP);
TCpxVector_Init(gCP,gTP);
TIntVector_Init(gCP,gTP);
TNumVector_Init(gCP,gTP);
TFltVector_Init(gCP,gTP);
TObjVector_Init(gCP,gTP);
TPcodeVector_Init(gCP,gTP);
TString_Init(gCP,gTP);
TSymbol_Init(gCP,gTP);
TStructure_Init(gCP,gTP);
TPair_Init(gCP,gTP);

/*  Create the necessary support objects. */
gTP->TObject_SaveVector = TObjVector_New(gCP,gTP);
gTP->TObject_SaveNdx = TIntVector_New(gCP,gTP);

TLambda_Init(gCP, gTP);

}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_Error

Return the specified message as an error.

#endif

TVAL TObject_Error(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR theMsg)
{

return(FSmartbase_Error(gCP, gTP, theMsg));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_sprintReal

Converts a real number to text in the most appropriate format.

#endif

NUM TObject_sprintReal(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR buf,REAL aReal)
{
NUM			i;
NUM			bufLen;
BOLE		decimal;


gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/* Convert the real number maybe without a decimal. */

sprintf(buf,"%.13G",aReal);

/* Make sure the output has a decimal */

bufLen = strlen(buf);
decimal = FALSE;
for (i = 0; i < bufLen; ++i)
	{
	if ((buf[i] == '.') || 	(buf[i] == 'E') || (buf[i] == 'e') || (buf[i] == 'n') || (buf[i] == 'N') ||
		(buf[i] == 'f') || (buf[i] == 'F'))
		{
		decimal = TRUE;
		break;
		}
	}

/* NAN - Not a Number */
/* INF - Infinity */

if (!decimal)
	{
	buf[i] = '.';
	buf[++i] = '0';
	buf[++i] = 0;
	}

return(TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TObject_SaveFile

This function saves the specified target and the recursive closure of all
the objects on which the indicated target depends to a file, predefined buffer or
automatically created bytevector. The record created contains a record header that
identifies the type of content saved and allows the records to be loaded using
the FConio_loadObject function.

Arguments:
	fileId, target
	fileId, target, true
			Write record to file.
			Return length of record.

	object
	fileId, target, false
			Return a byte vector containing the record

	target, bufferLen, buffer
			Write record to supplied buffer. Ensure we do not
			write past bufferLen! Return length of record.

	NOTE
    As a prefix to the file data, we store the recordHeader which contains
    an object record signature and the length of the object data to follow.

#endif

TVAL TObject_SaveFile(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
OBRECORDHEADER  recordHeader;
OBRECORDHEADER* pRecordHeader;
MHANDLEINFO     aMHandle;
HMemory         anHMemory = NULL;
LONG            lSize;
LONG            lTmp;
LONG			recordPos = 0;
NUM             curItem;
NUM             itemSize;
NUM             totalSize;
NUM             bufferSize;
NUM             bufferUsedSize;
NUM				recordLength;
NUM             theFile;
NUM             objIdx;
BOLE			tvalYes;
BOLE            indexYes;
TVAL            objLength;
TVAL            aCmdFMake_Vector = TFUNCTION(FMake_Vector);
TVAL            aCmdTObject_SaveFile = TFUNCTION(TObject_SaveFile);
TVAL            aCmdFDateFnc_now = TFUNCTION(FDateFnc_now);
TVAL			target;

typedef enum  { eFileTarget, eVectorTarget, eBufferTarget, eNoTarget } eTarget;
eTarget			aTarget;

StartFrame
DeclareOBJ(TObject,saveObject);
DeclareOBJ(TObject,theObject);
DeclareTVAL(vec);
DeclareTVAL(tmpTval);
EndFrame

theFile = -1;
tvalYes = FALSE;
indexYes = FALSE;


/*  Check for One, Two, or Three arguments*/
switch(argc)
{
case 1: /* args: target */
    aTarget = eVectorTarget; /* Returning a byte vector containing the object closure record */
	if (_TObject_TypeFlag(argv[0].Tag) == _TObject_TfTOBJECT)
		{ /* Get Object closure size */
		theObject = argv[0].u.Object;
		if (!_VALIDOBJ(theObject)) /*  We can only save valid objects. */
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		//Call TObject_SaveFile to get size of object closure including recordheader
		objLength = FSmartbase_Eval(gCP,gTP,aCmdTObject_SaveFile,3,TINT(0),argv[0],TBOOL(FALSE));
		ExitOnError(objLength);
		bufferSize = objLength.u.Int;
		}
	else /* target is an immediate TVAL */
	if (_TObject_TypeFlag(argv[0].Tag) == _TObject_TfIOBJECT)
		{
		/* For immediate types, we must save the reference object as well. */
		theObject = (TObject*)_TObject_MainObjectList(ObjIdx(argv[0]));
		if (!_VALIDOBJ(theObject)) /*  We can only save valid objects. */
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);

		tmpTval->u.Object = theObject;
		tmpTval->Tag = theObject->itsObjectType;

		/* Get the size of the reference object */
		objLength = FSmartbase_Eval(gCP,gTP,aCmdTObject_SaveFile,3,TINT(0),*tmpTval,TBOOL(FALSE));
		ExitOnError(objLength);

		target = argv[0];
		indexYes = TRUE;

		/* Include the immediate TVAL in the length */
		/* The length of the Object Header is already included in objLength */
		bufferSize = objLength.u.Int + sizeof(TVAL);
		}
	else /* target is an immediate TVAL */
		{
		target = argv[0];
		tvalYes = TRUE;
		bufferSize = sizeof(OBRECORDHEADER) + sizeof(TVAL);
		}

	*vec = FSmartbase_Eval(gCP,gTP,aCmdFMake_Vector,2,TSYMBOL("byte"),TINT(bufferSize));
	ExitOnError(*vec);
	anHMemory = (HMemory)ByteVector(*vec)->itsByteArray;
	pRecordHeader = (OBRECORDHEADER*)ByteArray(*vec); //Pointer to beginning of data in structure
	break;

case 3: /* args: fileId, target, saveFlag or args: target,bufferlen, bufferPtr */
	if (argv[2].Tag == TYPOINTER)
		{ /* args: target,bufferlen, bufferPtr */
		  /* Writing into a file and returning the length of object closure record written */
		aTarget = eBufferTarget;
		if (_TObject_TypeFlag(argv[0].Tag) == _TObject_TfTOBJECT)
			{
			theObject = argv[0].u.Object;
			if (!_VALIDOBJ(theObject)) /*  We can only save valid objects. */
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
			}
		else
		if (_TObject_TypeFlag(argv[0].Tag) == _TObject_TfIOBJECT)
		    {
		    /* For immediate types, we must save the reference object as well. */
		    theObject = (TObject*)_TObject_MainObjectList(ObjIdx(argv[0]));
		    if (!_VALIDOBJ(theObject)) /*  We can only save valid objects. */
			    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);

		    target = argv[0];
		    indexYes = TRUE;
		    }
		else /* target is immediate TVAL */
			{
			target = argv[0];
			tvalYes = TRUE;
			aTarget = eBufferTarget;
			}
		bufferSize = argv[1].u.Int;		/* size of supplied memory buffer */
		anHMemory = (HMemory)&argv[2].u.Pointer; /* anHMemory is a pointer to a pointer to the data buffer */
	    pRecordHeader = (OBRECORDHEADER*)*anHMemory;
		break;
		}
	else
	if (argv[2].Tag != TYBOLE) /* The 3rd argument must be a pointer or a bool */
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	else
	if (argv[2].u.Bool == FALSE)
		{ /* args: fileId, target, saveFlag==FALSE */
		aTarget = eNoTarget; /* we will only return size of object closure */
		if (_TObject_TypeFlag(argv[1].Tag) == _TObject_TfTOBJECT)
			{
			theObject = argv[1].u.Object;
			if (!_VALIDOBJ(theObject)) /*  We can only save valid objects. */
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
			theFile = -1; /* eNoTarget so we ignore the fileID argument */
			pRecordHeader = (OBRECORDHEADER*)&recordHeader;

			bufferSize = _TObject_SaveLoadBuffer;
			anHMemory = FMemory_New(gCP, gTP, (LONG)bufferSize, TRUE);
			break;
			}
		else
		if (_TObject_TypeFlag(argv[1].Tag) == _TObject_TfIOBJECT)
		    {
		    /* For immediate types, we must save the reference object as well. */
		    theObject = (TObject*)_TObject_MainObjectList(ObjIdx(argv[1]));
		    if (!_VALIDOBJ(theObject)) /*  We can only save valid objects. */
			    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);

			theFile = -1; /* eNoTarget so we ignore the fileID argument */
			pRecordHeader = (OBRECORDHEADER*)&recordHeader;
			bufferSize = _TObject_SaveLoadBuffer;
			anHMemory = FMemory_New(gCP, gTP, (LONG)bufferSize, TRUE);

			target = argv[1];
			indexYes = TRUE;
			break;
		    }
		else /* target is an immediate TVAL - we can return a value right away */
			{
			bufferSize = sizeof(OBRECORDHEADER) + sizeof(TVAL);
			FrameExit(TINT(bufferSize));
			}
		break;
		}
	/* NOTE: Fall through for saveFlag == TRUE condition */
	/* args: fileId, target, saveFlag where saveFlag == TRUE */

case 2: /* args: fileId, target */
	aTarget = eFileTarget; /* Saving the target to the specified file */
	if (_TObject_TypeFlag(argv[1].Tag) == _TObject_TfTOBJECT)
		{
		theObject = argv[1].u.Object;
		if (!_VALIDOBJ(theObject)) /*  We can only save valid objects. */
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		theFile = argv[0].u.Int;
		/*  We allocate a large buffer for the buffering of disk writes. */
		bufferSize = _TObject_SaveLoadBuffer;
		anHMemory = FMemory_New(gCP, gTP, (LONG)bufferSize, TRUE);
		}
    else
    if (_TObject_TypeFlag(argv[1].Tag) == _TObject_TfIOBJECT)
        {
        theObject = (TObject*)_TObject_MainObjectList(ObjIdx(argv[1]));
        if (!_VALIDOBJ(theObject)) /*  We can only save valid objects. */
	        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
        theFile = argv[0].u.Int;

		/*  We allocate a large buffer for the buffering of disk writes. */
		bufferSize = _TObject_SaveLoadBuffer;
		anHMemory = FMemory_New(gCP, gTP, (LONG)bufferSize, TRUE);

		target = argv[1];
		indexYes = TRUE;
        }
	else /* target is an immediate TVAL */
		{
		theFile = argv[0].u.Int;
		target = argv[1];
		tvalYes = TRUE;
		bufferSize = sizeof(OBRECORDHEADER) + sizeof(TVAL);
		}
	pRecordHeader = (OBRECORDHEADER*)&recordHeader;
	break;

default:
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	break;
    } /* end of switch on argc */

/* set record header attributes */
strcpy(pRecordHeader->AisIdentifier,"AIS");
sprintf(pRecordHeader->VersionIdentifier,"%d",_VERSION);
pRecordHeader->recordLength = 0;

if (indexYes)
    pRecordHeader->recordType = _INDEXRECORDTYPE;
else
if (tvalYes)
    pRecordHeader->recordType = _TVALRECORDTYPE;
else
    pRecordHeader->recordType = _OBJECTRECORDTYPE;

pRecordHeader->recordVersion = _VERSION;
pRecordHeader->SaveTimeStamp = (FSmartbase_Eval(gCP,gTP,aCmdFDateFnc_now,0,NIL)).u.Real;
totalSize = 0;
bufferUsedSize = 0;
recordLength = 0;

if (tvalYes)
{
    /* process immediate tval */
	pRecordHeader->recordLength = sizeof(TVAL);
	if (eFileTarget == aTarget)
		{
		(*gCP->_Host_Writef)((POINTER)gCP,gTP,theFile, sizeof(OBRECORDHEADER),(char*)pRecordHeader);
		(*gCP->_Host_Writef)((POINTER)gCP,gTP,theFile, sizeof(TVAL),(char*)&target);
		}
	else
		memcpy((char*)&atHMemory(anHMemory,sizeof(OBRECORDHEADER)),(char *)&target,sizeof(TVAL));

	totalSize = sizeof(OBRECORDHEADER) + sizeof(TVAL);

	if (aTarget == eVectorTarget)
		FrameExit(*vec);

	FrameExit(TINT(totalSize));
}

/* The following processing is only necesasry for object closures */
/*  Zero the vector which will hold all the objects which need */
/*  to be saved along with the "top level" object. These are */
/*  the objects included in the "object closure" of the top level */
/*  object about to be saved.  */

curItem = 0;
FObject_SetMaxIndex(gCP,gTP,(TObject*)gTP->TObject_SaveVector, 0);

/*  Resize the vector to hold the object id xref index for all  */
/*  the objects which need to be saved along with the top level */
/*  object. These are the objects included in the "object closure"  */
/*  of the top level object about to be saved. */

for(lSize = lTmp = 0; lTmp < gCP->TObject_MaxObjectCount; lTmp++)
	{
	/*  Figure the maximum "live" object id. */
	if( _TObject_ObjectFlag(lTmp) != _TObject_OfVOID)
		lSize = max(lSize, lTmp);
	}
/*  Make sure to include the "last" object id. */
++lSize;

asObject(tmpTval) = (TObject*)gTP->TObject_SaveNdx;
asTag(tmpTval) = gTP->TObject_SaveNdx->itsObjectType;
TIntVector_SetMaxIndex(gCP,gTP,*tmpTval,lSize);

/*  Initialize
the vector to hold the object id xref index for all  */
/*  the objects which need to be saved. */

for(lTmp = 0; lTmp < lSize; lTmp++)
	{
	/*  Init the cross-reference indices to -1. */
	atHMInt(gTP->TObject_SaveNdx->itsIntArray,lTmp) = -1;
	}

if (!indexYes)
    {
    switch(aTarget)
	    {
	    case eFileTarget:
	        recordPos = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,0,0);
            (*gCP->_Host_Writef)((POINTER)gCP,gTP,theFile, sizeof(OBRECORDHEADER), (char*)pRecordHeader);
	        bufferUsedSize = 0;
	        break;

	    case eVectorTarget:
	    case eBufferTarget:
	        bufferUsedSize = sizeof(OBRECORDHEADER);
	        break;

	    case eNoTarget:
	        bufferUsedSize = 0;
	        break;
	    } /* end of switch */
    }

/* process object closure */
/*  Register the top level object to the save vector. This will place */
/*  one object (the top level object to be saved) in the save vector */
/*  and its object id in the xref object id vector. */

objIdx = TObject_RegisterObject(gCP,gTP,theObject);

if (indexYes)
    {
    /* process index types */

    /* get the correct object index */
    ObjIdx(target) = objIdx;

    /* record length does not include record header */
    pRecordHeader->recordLength = sizeof(TVAL);

    if (eFileTarget == aTarget)
	    {
	    /* save the position of the object record header */
	    recordPos = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,0,0);
	    (*gCP->_Host_Writef)((POINTER)gCP,gTP,theFile,sizeof(OBRECORDHEADER),(char*)pRecordHeader);
	    (*gCP->_Host_Writef)((POINTER)gCP,gTP,theFile,sizeof(TVAL),(char*)&target);
	    }
    else
	    memcpy((char*)&atHMemory(anHMemory,sizeof(OBRECORDHEADER)),(char*)&target,sizeof(TVAL));

    switch (aTarget)
        {
        case eFileTarget:
            bufferUsedSize = 0;
            break;

        case eVectorTarget:
        case eBufferTarget:
            /* object data will be written after the object record header and the index TVAL */
            bufferUsedSize = sizeof(OBRECORDHEADER) + sizeof(TVAL);
            break;

        case eNoTarget:
            bufferUsedSize = 0;
            break;
        } /* end of switch */
    }

/*  Starting with the top level object (which is the only object in the */
/*  save vector) ask it to (a) compute its size, (b) ready its data in */
/*  a package for saving to disk, and (c) add any objects upon which it */
/*  depends to the save object vector. */
/*  Loop through all objects which become registered to the save vector */
/*  and save them all to disk. */

do
	{
	/*  Get the next object to be saved. */
	saveObject = atHMObject(gTP->TObject_SaveVector->itsObjectArray,curItem);
	/* CompupteSize returns the "package" size required to save the object */
	/* This includes a TObjectOnDisk header and the object data but not the
	   size of any objects on which it may depend. */
	FObject_ComputeSize(gCP,gTP,saveObject,&itemSize);

	if (bufferSize - bufferUsedSize < itemSize)
		{ /* remaining buffer size exceeded so write and resize if necessary */

		if (aTarget == eVectorTarget || aTarget == eBufferTarget)
			{ /* We should never get here as the buffer should be large enouth */
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BADBUFFERSIZE);
			}

		/*  Write out the current packages to disk. */
		if (aTarget == eFileTarget && bufferUsedSize)
			(*gCP->_Host_Writef)((POINTER)gCP,gTP,theFile,bufferUsedSize,(char*)TObjectOnDiskPtr(anHMemory,0));

		totalSize += bufferUsedSize;
		pRecordHeader->recordLength += bufferUsedSize;
		bufferUsedSize = 0;

		if (itemSize > bufferSize)
			{ /* Resize the buffer if we have an object bigger than the default (or latest resized) size */
			bufferSize = itemSize;
			anHMemory = FMemory_Resize(gCP, gTP, anHMemory,(LONG)bufferSize);
			}
		}

	if (aTarget == eNoTarget)
		{ /* write into begining of buffer - the contents of the buffer is discarded for eNoTartget */
		aMHandle.fwd = (char*)&atHMemory(anHMemory,0);
		/* save current object and register any objects it depends on */
		FObject_Save(gCP, gTP, saveObject, (HMemory)&aMHandle);
		if ((TObjectOnDiskPtr(anHMemory,0)->itsObjectType < TYVOID) ||
			(TObjectOnDiskPtr(anHMemory,0)->itsObjectType > TYMAXVALIDTYPE))
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
			}
		}
	else
		{ /* write into buffer past current position */
		/* The first thing in an object package is the TObjectOnDisk header record. The  */
		/* itsVariableLength element is that portion of the total package size */
		/* containing the object data. */
		TObjectOnDiskPtr(anHMemory,bufferUsedSize)->itsVariableLength = itemSize - SIZEOF_TObjectOnDisk;
		aMHandle.fwd = (char*)&atHMemory(anHMemory,bufferUsedSize);
		/* save current object and register any objects it depends on */
		FObject_Save(gCP, gTP, saveObject, (HMemory)&aMHandle);
		if ((TObjectOnDiskPtr(anHMemory,bufferUsedSize)->itsObjectType < TYVOID) ||
			(TObjectOnDiskPtr(anHMemory,bufferUsedSize)->itsObjectType > TYMAXVALIDTYPE))
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
			}
		}
	bufferUsedSize += itemSize;

	/*  We continue with the next object in the object to be saved vector. */
	/*  We quit when we save the last object and it doesn't register any */
	/*  other objects which need to be saved.        */

	curItem++;
	} while (curItem < gTP->TObject_SaveVector->itsMaxItemIndex);


/*  Write out any previous object packages remaining in the file buffer. */
if (bufferUsedSize && (aTarget == eFileTarget))
	(*gCP->_Host_Writef)((POINTER)gCP,gTP,theFile, bufferUsedSize, (char*)TObjectOnDiskPtr(anHMemory,0));

totalSize += bufferUsedSize;
if (aTarget == eNoTarget || aTarget == eFileTarget)
    {
	totalSize += sizeof(OBRECORDHEADER);

	/* add the length of index type to the total length */
	if (indexYes)
	    totalSize += sizeof(TVAL);
	}

/*  Update the record header */
if (aTarget != eNoTarget)
	pRecordHeader->SaveTimeStamp = (FSmartbase_Eval(gCP,gTP,aCmdFDateFnc_now,0,NIL)).u.Real;

pRecordHeader->recordLength = totalSize - sizeof(OBRECORDHEADER);

if (aTarget == eFileTarget)
	{ /* Rewrite file header if eFileTarget */
    lTmp = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,0,0); /* get current position */
    (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,recordPos,1); /* move to start of record */
    (*gCP->_Host_Writef)((POINTER)gCP,gTP,theFile, sizeof(OBRECORDHEADER), (char*)pRecordHeader);
	(*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,lTmp,1); /* move back to end of record */
	}

/*  Free all allocated temporary resources */
FObject_SetMaxIndex(gCP,gTP,(TObject*)gTP->TObject_SaveVector, 0);
asObject(tmpTval) = (TObject*)gTP->TObject_SaveNdx;
asTag(tmpTval) = gTP->TObject_SaveNdx->itsObjectType;
TIntVector_SetMaxIndex(gCP,gTP,*tmpTval, 0);

/* Select proper return value based on type of return Target */
switch (aTarget)
{
case eVectorTarget:
    FrameExit(*vec);
	break;
case eBufferTarget:
	FrameExit(TINT(totalSize));
	break;
case eFileTarget:
case eNoTarget:
    FMemory_Free(gCP, gTP, anHMemory);
    FrameExit(TINT(totalSize));
	break;
}
FrameExit(TINT(totalSize));
}
/*--------------------------------------------------------------------------------------- */
#if 0
TObject_LoadFile

Perform a recursive file load for the given object from the given file, bytevector or
buffer.

#endif

TVAL TObject_LoadFile(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
OBRECORDHEADER  recordHeader;
NUM             theFile;
NUM             itemSize;
NUM             totalSize;
NUM             usedSize;
NUM             remainingSize;
LONG            lTmp;
LONG            lMark;
LONG            lPosFirstObjectOnDisk = 0;
LONG            lEndPos = 0;
TObjectOnDisk*  theObjectOnDiskP;
TObject*        obj;
HMChar          hmChar = NULL;
MHANDLEINFO     aMHandle;
NUM             totalFileSize = 0;
NUM             totalObjectCount = 0;
BOLE            fileYes;
BOLE			bufferYes;
BOLE			tvalYes;
BOLE            indexYes;
TVAL			myTval;
StartFrame
DeclareTVAL(gcsafe);
DeclareTVAL(retTval);
EndFrame

argc = argc; // NOOP to hide unused parameter warning message
/*  Initialize and identify the input argument. */

if (argv[0].Tag == TYNUM)
    {
    theFile = argv[0].u.Int;
    fileYes = TRUE;
	bufferYes = FALSE;
    }
else
if (argv[0].Tag == TYBYTEVECTOR)
    {
    theFile = NIL;
    fileYes = FALSE;
	bufferYes = FALSE;
    }
else
if (argv[0].Tag == TYPOINTER)
	{
	theFile = NIL;
	fileYes = FALSE;
	bufferYes = TRUE;
	if (argv[0].u.Pointer == NULL)
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

*gcsafe = argv[0];
*retTval = gCP->TObject_VOID;
lMark = 0;
usedSize = 0;

/*  Read the header record of the first ObjectOnDisk. */
/*  Ignore any records which are not object data (recordType == _OBJECTRECORDTYPE). */
/*  Save the position for the start of the first ObjectOnDisk  */
/*  record, and compute the total record length. */

if (!fileYes)
	{
	if(bufferYes)
		hmChar = (HMChar)&argv[0].u.Pointer; /* pointer to pointer required */
	else
		hmChar = (HMChar)ByteVector(argv[0])->itsByteArray;
	}

ReadRecordHeader:
if (!fileYes)
    {
	memcpy((char*)&recordHeader,(char*)&atHMemory(hmChar,usedSize),sizeof(OBRECORDHEADER));
    usedSize += sizeof(OBRECORDHEADER);
    }
else
if ((*gCP->_Host_Readf)((POINTER)gCP,gTP,theFile, sizeof(OBRECORDHEADER), (char*)&recordHeader) != 0)
    {
    FrameExit(gCP->TObject_ERROR_BADTYPE);
    }

tvalYes = (recordHeader.recordType == _TVALRECORDTYPE);
indexYes = (recordHeader.recordType == _INDEXRECORDTYPE);

if (recordHeader.recordVersion != _VERSION)
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_WRONG_VERSION);
    }
else
if (recordHeader.recordType == _OBJECTRECORDTYPE || tvalYes || indexYes)
    {
	FObject_SetMaxIndex(gCP,gTP,(TObject*)gTP->TObject_SaveVector, 0);

    if (fileYes)
        {
        lEndPos = recordHeader.recordLength;
        lPosFirstObjectOnDisk = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,0,0);
        lEndPos += lPosFirstObjectOnDisk;
        }
    else
        {
        lEndPos = recordHeader.recordLength;
        lPosFirstObjectOnDisk = usedSize;
        lEndPos += lPosFirstObjectOnDisk;
        }

    if (indexYes)
        {
        /* for index types, skip the index TVAL area */
        lPosFirstObjectOnDisk += sizeof(TVAL);
        }
    }
else
    { /* Skip unknown record type */
    if (fileYes)
        {
        (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile, recordHeader.recordLength, 0);
        }
    else
        {
        usedSize += recordHeader.recordLength;
        }
    goto ReadRecordHeader;
    }

if (tvalYes || indexYes)
	{ /* Read in object data */
	if (fileYes)
		{
		(*gCP->_Host_Readf) ((POINTER)gCP,gTP,theFile,sizeof(TVAL),(char*)retTval);
		}
	else
		{
		memcpy((char*)&myTval,(char*)&atHMemory(hmChar,usedSize),sizeof(TVAL));
		memcpy((char*)retTval,(char*)&atHMemory(hmChar,usedSize),sizeof(TVAL));
		}

    if (indexYes)
        {
        /* read the object part after reading the index TVAL */
        goto LoadObjects;
        }
    }
else
	{
LoadObjects:
	/* Register all objects in an object closure from a disk file (with buffering). */
	if (fileYes)
		{
		totalSize = _TObject_SaveLoadBuffer;
		hmChar = (HMChar)FMemory_New(gCP, gTP, (LONG)totalSize, TRUE);

		/*  Make pass one to set up and register the objects to be loaded. */

		for (lTmp = 0, lMark = lPosFirstObjectOnDisk; lMark < lEndPos;)
			{
			/*  Limit last chunk to correct size (We are loading from a file). */

			if (lMark + totalSize > lEndPos)
				{
				totalSize = lEndPos - lMark;

				hmChar = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)hmChar, totalSize);
				}

			/*  Read object data from the file into the file buffer. */

			(*gCP->_Host_Readf) ((POINTER)gCP,gTP,theFile,totalSize,(char*)&atHMemory(hmChar,0));
			usedSize = 0;


			/*  Make a pass through the file buffer registering each object in */
			/*  the buffer until we run off the end of the file buffer. */
			/*  Note: Usually we run out of file buffer in the middle of an object, */
			/*        sometimes even in the middle of the object header. */

			while (totalSize - usedSize >= SIZEOF_TObjectOnDisk)
				{
				/*  Adjust the object on disk pointer to next object in the file buffer. */

				theObjectOnDiskP = (TObjectOnDisk*)&atHMemory(hmChar,usedSize);
				if ((theObjectOnDiskP->itsObjectType < TYVOID) ||
					(theObjectOnDiskP->itsObjectType > TYMAXVALIDTYPE))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				itemSize = SIZEOF_TObjectOnDisk + theObjectOnDiskP->itsVariableLength;
				totalFileSize += itemSize;
				++totalObjectCount;

				/*  Have we run out of buffer space in the middle of an object? */

				if (itemSize > totalSize - usedSize)
					{
					/*  If we run out of buffer space in the middle of an object, */
					/*  then we must back up and read the whole object over again. */
					/*  Seek back to reread this whole object as the first object */
					/*  in the new file buffer. */

					(*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,-(totalSize - usedSize),0);

					/*  If the object is too large for the current file buffer, */
					/*  then we must resize the file buffer to hold this object. */

					if (itemSize > totalSize)
						{
						totalSize = itemSize;
						hmChar = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)hmChar, totalSize);
						}
					break;
					}
				else
					{
					/* Advance to the next object in the file buffer and register it. */
					/* We reset the SmartLisp Stack and the garbage collection frame */
					/* immediately after each registration in case the object's load */
					/* function failed to do its duty. */

					usedSize += SIZEOF_TObjectOnDisk;
					aMHandle.fwd = (char*)&atHMemory(hmChar,usedSize);
					_TObject_RecordFrame
					(*(LpFNLOAD)_TObject_TypeLoad(theObjectOnDiskP->itsObjectType))(gCP,gTP,(HMemory)&aMHandle,(NUM)lTmp++,(NUM)FALSE);
					_TObject_CheckFrame
					usedSize += theObjectOnDiskP->itsVariableLength;
					lMark += itemSize;
					}
				}

			/* If we have run out of file buffer space in the middle of an object */
			/* header, then we must back up enough to read the object header. */

			remainingSize = totalSize - usedSize;
			if ((0 < remainingSize)  && (remainingSize < SIZEOF_TObjectOnDisk))
				{
				lMark = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,-remainingSize,0);
				}
			}
		}
	else
	/* Register all objects in an object closure from a buffer in memory. */
		{

		usedSize = lPosFirstObjectOnDisk;

		/*  Make pass one to set up and register the objects to be loaded. */
		/*  Used size is set past the record header portion of the buffer. */

		for (lTmp = 0, lMark = usedSize; lMark < lEndPos;)
			{
			/*  Make a pass through the buffer registering each object.  */

			theObjectOnDiskP = (TObjectOnDisk*)&atHMemory(hmChar,usedSize);

			if ((theObjectOnDiskP->itsObjectType < TYVOID) ||
				(theObjectOnDiskP->itsObjectType > TYMAXVALIDTYPE))
				{
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				}
			itemSize = SIZEOF_TObjectOnDisk + theObjectOnDiskP->itsVariableLength;
			totalFileSize += itemSize;
			++totalObjectCount;

			/* Advance to the next object in the file buffer and register it. */
			/* We reset the SmartLisp Stack and the garbage collection frame */
			/* immediately after each registration in case the object's load */
			/* function failed to do its duty. */

			usedSize += SIZEOF_TObjectOnDisk;
			aMHandle.fwd = (char*)&atHMemory(hmChar,usedSize);

			_TObject_RecordFrame
			(*(LpFNLOAD)_TObject_TypeLoad(theObjectOnDiskP->itsObjectType))(gCP,gTP,(HMemory)&aMHandle,(NUM)lTmp++,(NUM)FALSE);
			_TObject_CheckFrame

			usedSize += theObjectOnDiskP->itsVariableLength;
			lMark += itemSize;
			}
		}
	}

/* Load an object closure from a disk file (with buffering). */
if (!tvalYes || indexYes)
	{
	if (fileYes)
		{
		(*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,lPosFirstObjectOnDisk,1);
		totalSize = _TObject_SaveLoadBuffer;
		hmChar = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)hmChar, totalSize);

		/*  Make pass two to actually load the objects in the disk file. */

		for (lTmp = 0, lMark = lPosFirstObjectOnDisk; lMark < lEndPos;)
			{
			/*  Limit last chunk to correct size. */

			if (lMark + totalSize > lEndPos)
				{
				totalSize = lEndPos - lMark;

				hmChar = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)hmChar, totalSize);
				}

			/*  Read object data from the file into the file buffer. */

			(*gCP->_Host_Readf) ((POINTER)gCP,gTP,theFile,totalSize,(char*)&atHMemory(hmChar,0));
			usedSize = 0;

			/*  Make a pass through the file buffer loading each object in */
			/*  the buffer until we run off the end of the file buffer. */
			/*  Note: Usually we run out of file buffer in the middle of an object, */
			/*        sometimes even in the middle of the object header. */

			while (totalSize - usedSize >= SIZEOF_TObjectOnDisk)
				{
				/*  Adjust the object on disk pointer to next object in the file buffer. */

				theObjectOnDiskP = (TObjectOnDisk*)&atHMemory(hmChar,usedSize);
				itemSize = SIZEOF_TObjectOnDisk + theObjectOnDiskP->itsVariableLength;
				totalFileSize += itemSize;

				/*  Have we run out of buffer space in the middle of an object? */

				if (itemSize > totalSize - usedSize)
					{
					/*  If we run out of buffer space in the middle of an object, */
					/*  then we must back up and read the whole object over again. */
					/*  Seek back to reread this whole object as the first object */
					/*  in the new file buffer. */

					(*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,-(totalSize - usedSize),0);

					/*  If the object is too large for the current file buffer, */
					/*  then we must resize the file buffer to hold this object. */

					if (itemSize > totalSize)
						{
						totalSize = itemSize;
						hmChar = (HMChar)FMemory_Resize(gCP, gTP, (HMemory)hmChar, totalSize);
						}
					break;
					}
				else
					{
					/* Advance to the next object in the file buffer and load it. */
					/* We reset the SmartLisp Stack and the garbage collection frame */
					/* immediately after each object load in case the object's load */
					/* function failed to do its duty. */

					usedSize += SIZEOF_TObjectOnDisk;
					aMHandle.fwd = (char*)&atHMemory(hmChar,usedSize);
					_TObject_RecordFrame
					(*(LpFNLOAD)_TObject_TypeLoad(theObjectOnDiskP->itsObjectType))(gCP,gTP,(HMemory)&aMHandle, (NUM)lTmp++, (NUM)TRUE);
					_TObject_CheckFrame
					usedSize += theObjectOnDiskP->itsVariableLength;
					lMark += itemSize;
					}
				}

			/* If we have run out of file buffer space in the middle of an object */
			/* header, then we must back up enough to read the object header. */

			remainingSize = totalSize - usedSize;
			if ((0 < remainingSize)  && (remainingSize < SIZEOF_TObjectOnDisk))
				{
				lMark = (*gCP->_Host_Seekf)((POINTER)gCP,gTP,theFile,-remainingSize,0);
				}
			}

		FMemory_Free(gCP, gTP, (HMemory)hmChar);
		}
	else
	/* Load an object closure from a buffer in memory. */
		{
		usedSize = lPosFirstObjectOnDisk;

		/*  Make pass two actually load the objects in the byte vector. */

		for (lTmp = 0, lMark = usedSize; lMark < lEndPos;)
			{
			/*  Make a pass through the file buffer loading each object in */
			/*  the buffer until we run off the end of the file buffer. */
			/*  Adjust the object on disk pointer to next object in the file buffer. */

			theObjectOnDiskP = (TObjectOnDisk*)&atHMemory(hmChar,usedSize);

			itemSize = SIZEOF_TObjectOnDisk + theObjectOnDiskP->itsVariableLength;
			totalFileSize += itemSize;

			/* Advance to the next object in the file buffer and load it. */
			/* We reset the SmartLisp Stack and the garbage collection frame */
			/* immediately after each object load in case the object's load */
			/* function failed to do its duty. */

			usedSize += SIZEOF_TObjectOnDisk;
			aMHandle.fwd = (char*)&atHMemory(hmChar,usedSize);

			_TObject_RecordFrame
			(*(LpFNLOAD)_TObject_TypeLoad(theObjectOnDiskP->itsObjectType))(gCP,gTP,(HMemory)&aMHandle, (NUM)lTmp++, (NUM)TRUE);
			_TObject_CheckFrame

			usedSize += theObjectOnDiskP->itsVariableLength;
			lMark += itemSize;
			}
		}
	}


if (indexYes)
    {
    /* update the object index */
    obj = (TObject*)TObject_CheckRegistration(gCP,gTP,asObjIdx(retTval));
    asObjIdx(retTval) = obj->itsObjectIndex;
    }
else
if(!tvalYes)
	{
	/*  Set up the return value, always the first item saved */
	asTag(retTval) = (atHMObject(gTP->TObject_SaveVector->itsObjectArray,0))->itsObjectType;
	asObject(retTval) = atHMObject(gTP->TObject_SaveVector->itsObjectArray,0);
	/*  Free any allocated resources */
	FObject_SetMaxIndex(gCP,gTP,(TObject*)gTP->TObject_SaveVector, 0);
	}

FrameExit(*retTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TObject_LoadObject

Perform an object file load for the given object from the given file.

Note:   This function expects to be called sequentially for each file ID 0...n. It
        expects that there will be NO intervening file operations.

#endif

TObject* TObject_LoadObject(LpXCONTEXT gCP,LpTHREAD gTP,NUM theFile, NUM aFileID, NUM bResolve )
{
TObjectOnDisk   theObjectOnDisk;
HMemory         anHMemory;
StartFrame
DeclareTVAL(tmpTval);
EndFrame

anHMemory = FMemory_New(gCP, gTP, (LONG)0,TRUE);

(*gCP->_Host_Readf) ((POINTER)gCP,gTP,theFile,SIZEOF_TObjectOnDisk,(char*)&theObjectOnDisk);

anHMemory = FMemory_Resize(gCP, gTP, anHMemory,(LONG)(theObjectOnDisk.itsVariableLength));

(*gCP->_Host_Readf) ((POINTER)gCP,gTP,theFile,theObjectOnDisk.itsVariableLength,(char*)TObjectOnDiskPtr(anHMemory,0));

_TObject_RecordFrame
*tmpTval = (*(LpFNLOAD)_TObject_TypeLoad(theObjectOnDisk.itsObjectType))(gCP,gTP,anHMemory, aFileID, bResolve);
_TObject_CheckFrame

FMemory_Free(gCP, gTP, anHMemory);

if(!isERROR(tmpTval))
    {
    FrameReset;
    return(asObject(tmpTval));
    }
else
    {
    /*PTC   Handle error condition */
    FrameReset;
    return(NULL);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_LoadTval

We must redirect all object references on save and load, this would be best accomplished
with a registration vector, but could be handled inline as we have chosen for now.

#endif

TVAL TObject_LoadTval(LpXCONTEXT gCP,LpTHREAD gTP,TVAL  aTval)
{
TYPE            tag = asTag(&aTval);
StartFrame
DeclareOBJ(TObject,obj);
DeclareTVAL(retTval);
EndFrame

if (_TObject_TypeFlag(tag) == _TObject_TfTOBJECT)
    {
	*retTval = aTval;
    retTval->Tag = tag;
    retTval->u.Object = TObject_CheckRegistration(gCP,gTP,(NUM)aTval.u.Object);
    }
else
if (_TObject_TypeFlag(tag) == _TObject_TfIOBJECT)
    {
	*retTval = aTval;
    retTval->Tag = tag;
    obj = (TObject*)TObject_CheckRegistration(gCP,gTP,ObjIdx(aTval));
    asObjIdx(retTval) = obj->itsObjectIndex;
    }
else
    *retTval = aTval;

FrameExit(*retTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_LoadNever

The noop function for all types which are vectored to do nothing at load time.

#endif

TVAL TObject_LoadNever(LpXCONTEXT gCP, LpTHREAD gTP,HMemory anHMemory, NUM theFileID, NUM bResolve)
{
gTP = gTP; // NOOP to hide unused parameter warning message
anHMemory = anHMemory; // NOOP to hide unused parameter warning message
theFileID = theFileID; // NOOP to hide unused parameter warning message
bResolve = bResolve; // NOOP to hide unused parameter warning message
return gCP->TObject_TRUE;
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_RegisterLoad

This function will add an item to the Save Vector and return its file id.

#endif

TVAL TObject_RegisterLoad(LpXCONTEXT gCP,LpTHREAD gTP,NUM aFileID, TObject* theObject)
{
StartFrame
DeclareTVAL(indexTval);
DeclareTVAL(objectTval);
DeclareTVAL(ivTval);
EndFrame


if(theObject == NULL)
	{
    FrameExit(gCP->TObject_ERROR_INVALID);
	}
else
    {

    if(aFileID+1 > gTP->TObject_SaveVector->itsMaxItemIndex)
        FObject_SetMaxIndex(gCP,gTP,(TObject*)gTP->TObject_SaveVector, aFileID+1);

    asTag(indexTval) = TYNUM;
    asInt(indexTval) = aFileID;
    asTag(objectTval) = theObject->itsObjectType;
    asObject(objectTval) = theObject;

    asObject(ivTval) = (TObject*)gTP->TObject_SaveVector;
    asTag(ivTval) = gTP->TObject_SaveVector->itsObjectType;
    (*_TObject_TypeSetIV1(asTag(ivTval)))(gCP,gTP, *ivTval, *indexTval, *objectTval);
    FrameExit(gCP->TObject_TRUE);
    }
}


/*--------------------------------------------------------------------------------------- */
#if 0
TObject_CheckRegistration

This function will check the save vector and return the Object Ptr if the obj has already
been created.

#endif

TObject* TObject_CheckRegistration(LpXCONTEXT gCP,LpTHREAD gTP,NUM aFileID)
{
gCP = gCP; // NOOP to hide unused parameter warning message
if(aFileID > gTP->TObject_SaveVector->itsMaxItemIndex)
    {
    return (TObject*)NULL;
    }
else
if (aFileID != (NUM)_TObject_NILSavedObject)
    return (atHMObject(gTP->TObject_SaveVector->itsObjectArray,aFileID));
else
    return (TObject*)NULL;
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_RegisterObject

This function will add an item to the Save Vector and return its file id.

#endif

NUM TObject_RegisterObject(LpXCONTEXT gCP,LpTHREAD gTP,TObject* anObject)
{
NUM     ndx;
StartFrame
DeclareTVAL(tmpTval);
DeclareTVAL(argTval);
EndFrame

if(anObject == NULL)
	{
    FrameExit((NUM)_TObject_NILSavedObject);
	}
else
if (!_VALIDOBJ(anObject))
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    FrameExit(0);
    }
else
if (anObject->itsObjectIndex >= gTP->TObject_SaveNdx->itsMaxItemIndex)
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    FrameExit(0);
    }
else
    {
    /*  Check the current list to see if object is already registered */

    ndx = atHMInt(gTP->TObject_SaveNdx->itsIntArray,anObject->itsObjectIndex);
    if(ndx >= 0)
        {
        FrameExit(ndx);
        }
    else
        {
        /*  Add new items to the save vector */

        asTag(tmpTval) = anObject->itsObjectType;
        asObject(tmpTval) = anObject;
        atHMInt(gTP->TObject_SaveNdx->itsIntArray,anObject->itsObjectIndex) = gTP->TObject_SaveVector->itsMaxItemIndex;

        asTag(argTval) = gTP->TObject_SaveVector->itsObjectType;
        asObject(argTval) = (TObject*)gTP->TObject_SaveVector;
        TObjVector_AddNewValue(gCP,gTP,*argTval, *tmpTval);
        FrameExit(gTP->TObject_SaveVector->itsMaxItemIndex - 1);
        }
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_RegisterTval

We must redirect all object references on save and load, this would be best accomplished
with a registration vector, but could be handled inline as we have chosen for now.

#endif

TVAL TObject_RegisterTval(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval)
{
TYPE            tag = asTag(&aTval);
StartFrame
DeclareOBJ(TObject,objPtr);
EndFrame

if (_TObject_TypeFlag(tag) == _TObject_TfTOBJECT)
    {
    asObj(&aTval) = TObject_RegisterObject(gCP,gTP,asObject(&aTval));
    }
else
if (_TObject_TypeFlag(tag) == _TObject_TfIOBJECT)
    {
	objPtr = (TObject*)_TObject_MainObjectList(ObjIdx(aTval));
    ObjIdx(aTval) = TObject_RegisterObject(gCP,gTP,objPtr);
    }

FrameExit(aTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_MarkAndSweep

Perform mark and sweep garbage collection.

#endif

void TObject_MarkAndSweep(LpXCONTEXT gCP,LpTHREAD gTP)
{
NUM                     indexOf;
NUM                     lengthOf;
TVAL                    tmpTval;
TVAL                    symbolTable;
LpOBJ					symbolTablePtr;
TByteVector*			bvec;
BOLE					OldDebugJitOn;

/*  Do NOT allow recursive garbage collection. */
if (gCP->TObject_GarbageCollectInProgress == TRUE)
	{
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_FRAME_ERROR);
	}
else
	{
	gCP->TObject_GarbageCollectInProgress = TRUE;
	}

/*  Inhibit garbage collection if the garbage inhibitor counter is greater than zero. */
if (gCP->TObject_GarbageON == FALSE) return;
if (gCP->TObject_GarbageInhibit > 0) return;

/*  Turn off the JIT when performing garbage collection */
/*  Note: The JIT requires allocation of a new vector   */
/*        during compilation. This memory use can cause */
/*        a cascading garbage collection recursion bug  */
/*        when executing an EvalWhenDoomed Lambda, which */
/*        is uncompiled, when memory is low. The VM     */
/*        emulator does not require new memory to run   */
OldDebugJitOn = gTP->DebugJitOn;
gTP->DebugJitOn = FALSE;


/*  Unmark all objects. */
/*  Note:   The nil object is never garbage collected. */
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if (_TObject_ObjectFlag(indexOf) != _TObject_OfVOID)
        {
        _TObject_ObjectFlag(indexOf) |= _TObject_OfMARK;
        _TObject_ObjectFlag(indexOf) ^= _TObject_OfMARK;
        }
    }

/*  Mark message the Symbol Table but NOT the Symbol objects it contains.   */
_TObject_ObjectFlag(gCP->TSymbol_SymbolTable->itsObjectIndex) |= _TObject_OfMARK;

/*  Send Mark message to all debugger objects */
_TObject_RecordFrame
TObject_MarkObj(gCP,gTP,(TObject*)gTP->DebugBreakProc);
TObject_MarkTval(gCP,gTP,gTP->DebugBreakExp);
TObject_MarkTval(gCP,gTP,gTP->TObject_ErrorTree);
TObject_MarkObj(gCP,gTP,(TObject*)gTP->FCompile_DebugSourceVector);
TObject_MarkObj(gCP,gTP,(TObject*)gTP->FCompile_DebugListLines);
TObject_MarkTval(gCP,gTP,gTP->FCompile_DebugInterfaces);
TObject_MarkTval(gCP,gTP,gTP->FCompile_DebugCurrentSourceLine);
TObject_MarkTval(gCP,gTP,gTP->FCompile_DebugCurrentSourceLineIndex);
_TObject_CheckFrame

/*  Send Mark message to all permanent objects, including all Symbol */
/*  objects which are either locked or which have non-void global values. */
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if (_TObject_ObjectFlag(indexOf) & _TObject_OfPERM)
        {
        _TObject_RecordFrame
        TObject_MarkObj(gCP,gTP,_TObject_ObjectByIndex(indexOf));
        _TObject_CheckFrame
        }
    else
    if ((_TObject_ObjectFlag(indexOf) & _TObject_OfOBJECT) &&
        (_TObject_ObjectByIndex(indexOf)->itsObjectType == TYSYMBOL))
        {
        if ((((TSymbol*)_TObject_ObjectByIndex(indexOf))->itsGlobalLock == TRUE) ||
            (((TSymbol*)_TObject_ObjectByIndex(indexOf))->itsGlobalValue.Tag != TYVOID) ||
            (((TSymbol*)_TObject_ObjectByIndex(indexOf))->itsCProcedure != (LpFUNC)TObject_NilFunction))
            {
            _TObject_RecordFrame
            TObject_MarkObj(gCP,gTP,_TObject_ObjectByIndex(indexOf));
            _TObject_CheckFrame
            }
        }
	else
	if ((_TObject_ObjectFlag(indexOf) & _TObject_OfOBJECT) &&
		(_TObject_ObjectByIndex(indexOf)->itsObjectType == TYOBJREPOSITORY))
		{
		/* If we have an open Repository, pointing to a disk database file, */
		/* with a transaction in progress, we do not want to garbage collect. */

		if (((TDatabase*)_TObject_ObjectByIndex(indexOf))->itsOdbID != NIL)
			{
			bvec = ((TDatabase*)_TObject_ObjectByIndex(indexOf))->itsOdbID;
			if (bvec->itsCdr.Tag == TYNUM)
				{
				_TObject_RecordFrame
				TObject_MarkObj(gCP,gTP,_TObject_ObjectByIndex(indexOf));
				_TObject_CheckFrame
				}
			}
		}
    }

/*  Send Mark message to all Global objects. */
for (indexOf = 0; indexOf < gCP->TObject_MaxTypes; ++indexOf)
    {
    if(_TObject_TypeGlobalMark(indexOf) != TObject_GlobalMarkNever)
        {
        _TObject_RecordFrame
        (*(LpFGMARK)_TObject_TypeGlobalMark(indexOf))(gCP,gTP);
        _TObject_CheckFrame
        }
    }

/*  Check for system stack or recursion overflow. */

if(gTP->ObjStackIdx >= gTP->MaxObjStackSize)
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_FRAME_ERROR);
else
if (((TopOfStack - 20) >= gTP->MaxTvalStackSize) || (TopOfStack < 0))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_STACK);

/*  Clear all items on the TVAL stack which are not in use */
/*  Send Mark messages to those which are still being used. */
for (indexOf = 0; indexOf < gTP->MaxTvalStackSize; ++indexOf)
    {
    tmpTval = gTP->TvalStack[indexOf];
    if (asTag(&tmpTval) != TYVOID && indexOf < TopOfStack)
        {
        _TObject_RecordFrame
        TObject_MarkTval(gCP,gTP,tmpTval);
        _TObject_CheckFrame
        }
    else
        gTP->TvalStack[indexOf] = gCP->Tval_VOID;
    }

/*  Send Mark message to all objects in the current frame. */
for (indexOf = 0; indexOf < gTP->ObjStackIdx; ++indexOf)
    {
    _TObject_RecordFrame
    TObject_MarkObj(gCP,gTP,_TObject_FramedObject(indexOf));
    _TObject_CheckFrame
    }

/*  Send mark messages to all bound methods in the type table. */

for (indexOf = 0; indexOf < gCP->TObject_MaxTypes; ++indexOf)
    {
    if (_TObject_TypeMethods(indexOf).Tag != TYVOID)
        {
        _TObject_RecordFrame
        TObject_MarkTval(gCP,gTP,_TObject_TypeMethods(indexOf));
        _TObject_CheckFrame
        }
    }

/*  Send mark messages to all structure fields in the type table. */

for (indexOf = 0; indexOf < gCP->TObject_MaxTypes; ++indexOf)
    {
    if (_TObject_TypeFields(indexOf).Tag != TYVOID)
        {
        _TObject_RecordFrame
        TObject_MarkTval(gCP,gTP,_TObject_TypeFields(indexOf));
        _TObject_CheckFrame
        }
    }

/*  Warn all unmarked Lambdas that they are soon to be killed. */
/*  Note:   Each condemned Lambda is warned before it is killed. */

for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if ((!(_TObject_ObjectFlag(indexOf) & _TObject_OfMARK)) &&
		(_TObject_ObjectFlag(indexOf) != _TObject_OfVOID) &&
        (_TObject_ObjectByIndex(indexOf)->itsObjectType == TYLAMBDA))
            {
            _TObject_RecordFrame
            FObject_Doomed(gCP,gTP,_TObject_ObjectByIndex(indexOf));
            _TObject_CheckFrame
            }
    }

/*  Warn all unmarked non-Lambda objects that they are soon to be killed. */
/*  Note:   Each condemned non-Lambda object is warned before it is killed. */

for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if ((!(_TObject_ObjectFlag(indexOf) & _TObject_OfMARK)) &&
		(_TObject_ObjectFlag(indexOf) != _TObject_OfVOID) &&
        (_TObject_ObjectByIndex(indexOf)->itsObjectType != TYLAMBDA))
            {
            if (_TObject_ObjectByIndex(indexOf) == (TObject*)gCP->TSymbol_SymbolTable)
				{
				/* We should never condemn the Symbol Table. */
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				}
            _TObject_RecordFrame
            FObject_Doomed(gCP,gTP,_TObject_ObjectByIndex(indexOf));
            _TObject_CheckFrame
            }
    }

/*  Kill all unmarked objects. */
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if ((!(_TObject_ObjectFlag(indexOf) & _TObject_OfMARK)) &&
        (_TObject_ObjectFlag(indexOf) != _TObject_OfVOID))
        {
        if (_TObject_ObjectByIndex(indexOf) == (TObject*)gCP->TSymbol_SymbolTable)
			{
			/* We should never kill the Symbol Table. */
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
			}
        _TObject_RecordFrame
        FObject_Destruct(gCP,  gTP, _TObject_ObjectByIndex(indexOf));
        _TObject_CheckFrame
        }
    }

/*  Reset the JIT to its previous setting */
gTP->DebugJitOn = OldDebugJitOn;

/*  Reset the GC to its previous setting */
gCP->TObject_GarbageCollectInProgress = FALSE;

/*  Do we need to deallocate extra space from the Symbol Table?  */
if ((gCP->TSymbol_SymbolTable->itsMaxItemIndex - gCP->TSymbol_SymbolTable_ActiveCount) >= gCP->TSymbol_SymbolTable_MaxEmptyCount)
	{
	symbolTable.Tag = TYOBJVECTOR;
	symbolTable.u.ObjVector = gCP->TSymbol_SymbolTable;
	TObjVector_SetMaxIndex(gCP,gTP,symbolTable,gCP->TSymbol_SymbolTable_ActiveCount+gCP->TSymbol_SymbolTable_NewAllocationSize);
	symbolTablePtr = (LpOBJ)*gCP->TSymbol_SymbolTable->itsObjectArray;
	lengthOf = gCP->TSymbol_SymbolTable->itsMaxItemIndex;
	for (indexOf = gCP->TSymbol_SymbolTable_ActiveCount;indexOf < lengthOf; ++indexOf)
		{
		symbolTablePtr[indexOf] = NIL;
		}
	}
}


/*--------------------------------------------------------------------------------------- */
#if 0
TObject_MarkTval

Mark the specified type of data. This function is usually called during a mark
and sweep phase.

#endif

void TObject_MarkTval(LpXCONTEXT gCP, LpTHREAD gTP,TVAL aTval )
{
LpFMARK     aFunction;
TObject*	obj;

/* If the type is a descendent of TObject send it a mark message. */
/* otherwise, call its appropriate mark function from the Mark vector. */

if ((aTval.Tag < TYVOID) || (aTval.Tag > TYMAXVALIDTYPE))
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    }
if (_TObject_TypeFlag(aTval.Tag) == _TObject_TfTOBJECT)
    {
    /*  Don't mark the nil object */
    if (aTval.u.Object == NIL)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
    else
    /*  Mark the object */
		{
        TObject_MarkObj(gCP,gTP,aTval.u.Object);
		}
    }
else
if (_TObject_TypeFlag(aTval.Tag) == _TObject_TfIOBJECT)
    {
    /*  Don't mark the nil object */
    if (ObjIdx(aTval) == NIL)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
    else
    /*  Mark the object from its index */
		{
		obj = (TObject*)_TObject_MainObjectList(ObjIdx(aTval));
        TObject_MarkObj(gCP,gTP,obj);
		}
    }
else
    {
    aFunction = (LpFMARK)_TObject_TypeMark(aTval.Tag);

    if (aFunction != TObject_MarkNever)
        {
        (*aFunction)(gCP,gTP, aTval);
        }
    }
}


/*--------------------------------------------------------------------------------------- */
#if 0
TObject_MarkObj

Mark the specified type of data. This function is usually called during a mark
and sweep phase.

#endif

void TObject_MarkObj(LpXCONTEXT gCP,LpTHREAD gTP,TObject*   anObject )
{
TVAL        aTval;
LpFMARK     aFunction;

/* Don't mark the nil object */

if (anObject == NULL)
    {
    return;
    }
else
if (((LpCHAR)anObject < (LpCHAR)gCP->FMemory_ParentPtr) ||
    ((LpCHAR)anObject > (LpCHAR)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated)) ||
    (anObject->itsObjectType < TYVOID) ||
    (anObject->itsObjectType > TYMAXVALIDTYPE) ||
    (anObject->itsObjectIndex < 0) ||
    (anObject->itsObjectIndex > gCP->TObject_MaxObjectCount) ||
    (_TObject_TypeFlag(anObject->itsObjectType) != _TObject_TfTOBJECT))
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    }
else
if (!_TObject_IsMarked(anObject))
    {
    _TObject_MarkFlag(anObject) |= _TObject_OfMARK;

    aTval.u.Object = anObject;
    aTval.Tag = anObject->itsObjectType;
    aFunction = (LpFMARK)_TObject_TypeMark(aTval.Tag);

    if (aFunction != TObject_MarkNever)
        {
        (*aFunction)(gCP,gTP, aTval);
        }
    }
}


/*--------------------------------------------------------------------------------------- */
#if 0
TObject_FrameError

Manage the situation when the TObject garbage collection pointer frame is
in an error condition. This usually occurs, during development, when a
programmer incorrectly applies unmatched _TObject_SetFrame / FrameReset
macros.

#endif

void TObject_FrameError(LpXCONTEXT gCP,LpTHREAD gTP)
{
FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_FRAME_ERROR);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_Convert

Convert a source type into a target type.

#endif

TVAL TObject_Convert(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
LpFCONVERT      aFunction;

aFunction = (LpFCONVERT)_TObject_TypeConvert(tTarget);

return((*aFunction)(gCP,gTP,tTarget,oldValue));
}

// RCA


/*--------------------------------------------------------------------------------------- */
#if 0
TObject_Compare

Compare a source type with a target type and return the following codes:

    HIGH                        1
    EQUAL                       0
    LOW                         -1

#endif

TVAL TObject_Compare(LpXCONTEXT gCP,LpTHREAD gTP,TVAL leftTval, TVAL rightTval)
{
LpF2TVALS       aFunction;

aFunction = (LpF2TVALS)_TObject_TypeCompare(asTag(&leftTval));

return((*aFunction)(gCP,gTP,leftTval,rightTval));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_NewCompare

Register a new compare function with the typing system.

#endif

void TObject_NewCompare(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget,LpF2TVALS aCmpFunction)
{
gTP = gTP; // NOOP to hide unused parameter warning message
_TObject_TypeCompare(tTarget) = aCmpFunction;
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_NewConvert

Register a new convert function with the typing system.

#endif

void TObject_NewConvert(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget,LpFCONVERT aCnvFunction)
{
gTP = gTP; // NOOP to hide unused parameter warning message
_TObject_TypeConvert(tTarget) = aCnvFunction;
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_NewNever

The default object creation function for native types (registered types
which are not objects).

#endif

TVAL TObject_NewNever(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
gTP = gTP; // NOOP to hide unused parameter warning message
argc = argc; // NOOP to hide unused parameter warning message
argv = argv; // NOOP to hide unused parameter warning message
return(gCP->TObject_ERROR_ACCESSDENIED);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_NewSetIndexedValue

Register a new SetIV1 function with the typing system.

#endif

void TObject_NewSetIndexedValue(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget,LpF3TVALS aSetIV1Function)
{
gTP = gTP; // NOOP to hide unused parameter warning message
_TObject_TypeSetIV1(tTarget) = aSetIV1Function;
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_MarkNever

The default marking function for native types (registered types
which are not objects).

Note:   This function acts as a noop.

#endif

void    TObject_MarkNever(LpXCONTEXT gCP, LpTHREAD gTP,TVAL aTval)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
aTval = aTval; // NOOP to hide unused parameter warning message
return;
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_GlobalMarkNever

The default marking function for types which have no associated global objects to be
marked.

Note:   This function acts as a noop.

#endif

TVAL TObject_GlobalMarkNever(LpXCONTEXT gCP, LpTHREAD gTP)
{
gTP = gTP; // NOOP to hide unused parameter warning message
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_MapDefault

The default map function. This function returns a vector object with
the results of the map operation.

#endif

TVAL TObject_MapDefault(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self,TVAL proc)
{
NUM             i;
NUM             cn;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVAL(lengthTval);
DeclareTVAL(length);
DeclareTVAL(makeVector);
EndFrame


*length =		TGVALUE("length");
*makeVector = TGVALUE("makeVector");

/*  Get length of object to map. */

*lengthTval = FSmartbase_Eval(gCP,gTP,*length,1,self);
if (lengthTval->Tag != TYNUM)
    {
    FrameExit(gCP->TObject_ERROR_INVALID);
    }
else
    cn = lengthTval->u.Int;

/*  Create the result vector object. */

*ret = FSmartbase_Eval(gCP,gTP,*makeVector,1,*lengthTval);
ExitOnError(*ret);

/*  Map the original object to the result vector. */

for (i = 0; i < cn; ++i)
    {
    *tmp = FSmartbase_Ref(gCP,gTP,2,self,TINT(i));
    ExitOnError(*tmp);
    *tmp = FSmartbase_Eval(gCP,gTP,proc,1,*tmp);
    ExitOnError(*tmp);
    *tmp = FSmartbase_Set(gCP,gTP,3,*ret,TINT(i),*tmp);
    ExitOnError(*tmp);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_PrintDefault

The default Print function for all types.

#endif

TVAL TObject_PrintDefault(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
NUM         txtLen;
LpCHAR      aTextPtr;
StartFrame
DeclareTVAL(ec);
EndFrame

*ec = TObject_TextAnyCnv(gCP,gTP,TYTEXT,self);

if(isERROR(ec) && !isERROR(&self))
    {
    FrameExit(gCP->TObject_FALSE);
    }
else
    {
    if (asTag(ec) == TYTEXT)
        aTextPtr = asText(ec);
    else
    if (asTag(ec) == TYSTRING)
          aTextPtr = CharArray(*ec);
    else
    if (asTag(ec) == TYERROR)
         aTextPtr = ErrorArray(*ec);
   else
       {
        /*  error... */

        aTextPtr = (LpCHAR)"!Print!";
        }

    txtLen = strlen((char*)aTextPtr);
    if(*size +  txtLen + 1 > gCP->TObject_MaxOutputLen)
		{
        FrameExit(gCP->TObject_FALSE);
		}
    else
        {
        strcat((char*)&buf[*size], (const char*)aTextPtr);
        *size += txtLen;
        }
    }

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_PrintNever

The noop Print function.

#endif

TVAL TObject_PrintNever(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
gTP = gTP; // NOOP to hide unused parameter warning message
self = self; // NOOP to hide unused parameter warning message
size = size; // NOOP to hide unused parameter warning message
buf = buf; // NOOP to hide unused parameter warning message
return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_SetIV1

#endif

TVAL TObject_SetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval,TVAL index1,TVAL newValue)
{
LpF3TVALS       aFunction;

aFunction = (LpF3TVALS)_TObject_TypeSetIV1(asTag(&dstTval));

return((*aFunction)(gCP,gTP, dstTval,index1,newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_SetIV2

#endif

TVAL TObject_SetIV2(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval,TVAL index1,TVAL index2,TVAL newValue)
{
LpF4TVALS       aFunction;

aFunction = (LpF4TVALS)_TObject_TypeSetIV2(asTag(&dstTval));

return((*aFunction)(gCP,gTP, dstTval,index1,index2,newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_SetIV3

#endif

TVAL TObject_SetIV3(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval,TVAL index1, TVAL index2, TVAL index3,TVAL newValue)
{
LpF5TVALS       aFunction;

aFunction = (LpF5TVALS)_TObject_TypeSetIV3(asTag(&dstTval));

return((*aFunction)(gCP,gTP,dstTval,index1,index2,index3,newValue));
}


// RCA


/*--------------------------------------------------------------------------------------- */
#if 0
TObject_SetIVText

Set the indexed value in the repeating portion of a TYTEXT or TYSTRING object.

#endif

TVAL TObject_SetIVText(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
TVAL            err;
CHAR            aChar;
CHAR*           aTextPtr;
NUM             indexOf;
NUM             fillIndex;
NUM				currentLen;
NUM             maxDataIndex;
StartFrame
DeclareTVAL(retString);
EndFrame

/*  We only accept numeric indices. */

if (!isNumIndex(&index1))
	{
    FrameExit(gCP->TObject_ERROR_NUMINDEXREQ);
	}
else
	{
    indexOf = asNumIndex(&index1);
	}

*retString = selfTval;

/*  We only allow chars as elements of strings. */

err = TObject_Convert(gCP,gTP,TYCHAR,newValue);
ExitOnError(err);
aChar = asChar(&err);

/*  Now determine whether this is a TYTEXT or a TYSTRING */
if(retString->Tag == TYTEXT)
    {
	*retString = TERROR("!Cannot set characters in a Text. Must be converted to a String first. Use makeString function!");
	FrameExit(*retString);

    aTextPtr = retString->u.Text;
	maxDataIndex = MAXAISWORDTEXTLEN-1;
	currentLen = strnlen(aTextPtr,maxDataIndex);

	/*  Make sure string index is in range. */

	if (indexOf < 0)
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	else
	if (indexOf >= currentLen)
		{
		if (indexOf < (MAXAISWORDTEXTLEN - 1))
			{
			for (fillIndex = currentLen; fillIndex <= indexOf; ++fillIndex)
				{
				aTextPtr[fillIndex] = ' ';
				}
			aTextPtr[fillIndex] = 0;
			}
		else
			{
			*retString = gCP->Tval_VOID;
			retString->u.Object = (TObject*)TString_MakeUnique(gCP,gTP,aTextPtr);
			retString->Tag = TYSTRING;
			goto StringManager;
			}
		}
    }
else
if(retString->Tag == TYSTRING)
    {
	StringManager:
    aTextPtr = CharArray(*retString);
	maxDataIndex = String(*retString)->itsMaxItemIndex;
	currentLen = strnlen(aTextPtr,maxDataIndex);

	/*  Make sure string index is in range. */

	if (indexOf < 0)
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	else
	if (indexOf >= currentLen)
		{
		if (indexOf < (maxDataIndex - 1))
			{
			for (fillIndex = currentLen; fillIndex <= indexOf; ++fillIndex)
				{
				aTextPtr[fillIndex] = ' ';
				}
			aTextPtr[fillIndex] = 0;
			}
		else
			{
			TString_SetMaxIndex(gCP,gTP,*retString, indexOf+2);
			aTextPtr = CharArray(*retString);
			maxDataIndex = String(*retString)->itsMaxItemIndex;
			currentLen = strnlen(aTextPtr,maxDataIndex);
			for (fillIndex = currentLen; fillIndex <= indexOf; ++fillIndex)
				{
				aTextPtr[fillIndex] = ' ';
				}
			aTextPtr[fillIndex] = 0;
			}
		}
    }
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}


/*  Now we set the specified indexed value */

aTextPtr[indexOf] = aChar;

FrameExit(*retString);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_GetIVText

Get an indexed value from the repeating portion of a TYTEXT object.

#endif

TVAL TObject_GetIVText(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1)
{
CHAR*           aTextPtr;
NUM             indexOf;
NUM             maxDataIndex;
StartFrame
DeclareTVAL(retTval);
EndFrame

/*  We only accept numeric indices. */

if (!isNumIndex(&index1))
	{
    FrameExit(gCP->TObject_VOID);
	}
else
	{
    indexOf = asNumIndex(&index1);
	}

/*  Now determine whether this is a TYTEXT or a TYSTRING */
if(asTag(&srcTval) == TYTEXT)
    {
    aTextPtr = asText(&srcTval);
    }
else
if(asTag(&srcTval) == TYSTRING)
    {
    aTextPtr = &atHMChar(asString(&srcTval)->itsCString,0);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

    /*  Make sure string index is in range. */

maxDataIndex = strlen((char*)aTextPtr);

if (indexOf < 0)
	{
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}
else
if (indexOf >= maxDataIndex)
	{
    FrameExit(gCP->TObject_VOID);
	}

/*  Now we create the return TVAL */

asTag(retTval) = TYCHAR;
asChar(retTval) = aTextPtr[indexOf];

FrameExit(*retTval);

}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_Print

Look up the appropriate Print function and call it.

#endif

TVAL TObject_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size,  LpCHAR buf)
{
LpFPRINT        aFunction;

aFunction = (LpFPRINT)_TObject_TypePrint(asTag(&self));

return((*aFunction)(gCP, gTP, self, size, buf));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_PrintObj

Look up the appropriate Print function and call it.

#endif

TVAL TObject_PrintObj(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
if (_TObject_TypeFlag(asTag(&self)) == _TObject_TfTOBJECT)
    {
    return(FObject_Print(gCP, gTP, self, size, buf));
    }
else
    {
    return(gCP->TObject_ERROR_INVALID);
    }
}


/*--------------------------------------------------------------------------------------- */

#if 0
TObject_Invalid

The default function for undefined comparisons or conversions.

#endif

TVAL TObject_Invalid(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget,TVAL oldValue)
{
gTP = gTP; // NOOP to hide unused parameter warning message
tTarget = tTarget; // NOOP to hide unused parameter warning message
oldValue = oldValue; // NOOP to hide unused parameter warning message
return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_NilFunction

For undefined CProcedures.

#endif

TVAL    TObject_NilFunction(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
argc = argc; // NOOP to hide unused parameter warning message
argv = argv; // NOOP to hide unused parameter warning message
return(TERROR("!NilFunction!"));
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_VoidAnyCmp

Compare a typed value to a value of type VOID.

#endif

TVAL TObject_VoidAnyCmp(LpXCONTEXT gCP,LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
gTP = gTP; // NOOP to hide unused parameter warning message
if (asTag(&leftVal) == TYVOID)
    return(gCP->TObject_EQUAL);
else
if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NIL)
        return(gCP->TObject_EQUAL);
    else
        return(gCP->TObject_ERROR_INVALID);
    }
else
    return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_CharPrint

Format printed output for TYCHAR.

#endif

TVAL TObject_CharPrint(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
CHAR    tmpBuf[128];
NUM     nLen;

gTP = gTP; // NOOP to hide unused parameter warning message
if(asTag(&self) == TYCHAR )
    {
    switch(asChar(&self))
        {
        case 0x08:
            strcpy((char*)tmpBuf, "#\\backspace");
        break;

        case 0x1b:
            strcpy((char*)tmpBuf, "#\\escape");
        break;

        case 0x0a:
            strcpy((char*)tmpBuf, "#\\newline");
        break;

        case 0x0c:
            strcpy((char*)tmpBuf, "#\\page");
        break;

        case 0x0d:
            strcpy((char*)tmpBuf, "#\\return");
        break;

        case 0x7f:
            strcpy((char*)tmpBuf, "#\\rubout");
        break;

        case 0x20:
            strcpy((char*)tmpBuf, "#\\space");
        break;

        case 0x09:
            strcpy((char*)tmpBuf, "#\\tab");
        break;

        default:
            sprintf((char*)tmpBuf, "#\\%c", (int)asChar(&self));
        break;

        }
    nLen = strlen((char*)tmpBuf);

    if( *size + nLen + 1 < gCP->TObject_MaxOutputLen)
        {
        strcat((char*)&buf[*size], (const char*)tmpBuf);
        *size += nLen;
        return(gCP->TObject_TRUE);
        }
    }
return(gCP->TObject_FALSE);
}


/*--------------------------------------------------------------------------------------- */

#if 0
TObject_TextPrint

Convert a typed value to a value of type TEXT.

#endif

TVAL TObject_TextPrint(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf)
{
char cTmp[MAXAISWORDTEXTLEN+10];

gTP = gTP; // NOOP to hide unused parameter warning message
if (*size > gCP->TObject_MaxOutputLen)
    return(gCP->TObject_FALSE);

if(asTag(&self) == TYTEXT)
    {
    sprintf(cTmp, "\"%s\"", asText(&self) );
    if ((*size + (NUM)strlen(cTmp) + 5) > gCP->TObject_MaxOutputLen)
        return(gCP->TObject_FALSE);
    strcat((char*)buf, cTmp);
    *size = strlen((char*)buf);
    return(gCP->TObject_TRUE);
    }
return(gCP->TObject_FALSE);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_strcmp

The TObject_strcmp function compares two strings and returns a TVAL result.

#endif

TVAL TObject_strcmp(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pTarget,LpCHAR pSource)
{
StartFrame
DeclareTVAL(result);
EndFrame

asTag(result) = TYCOMPARE;
asCompare(result) = strcmp((char*)pTarget,(const char*)pSource);
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_strncmp

The TObject_strncmp function compares two strings and returns a TVAL result.

#endif

TVAL TObject_strncmp(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pTarget,LpCHAR pSource,NUM Length)
{
StartFrame
DeclareTVAL(result);
EndFrame

asTag(result) = TYCOMPARE;
asCompare(result) = strncmp((char*)pTarget,(const char*)pSource,(size_t)Length);
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_ator

The TObject_ator function converts an ascii string into a real number. Any leading
spaces in the string are removed.

#endif

TVAL TObject_ator(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL rs,LpCHAR sp)
{
NUM      count;                        /* TEMPORARY DIGITS RECOGNIZED COUNT*/
NUM          n;                        /* TEMPORARY INPUT STREAM COUNT     */
NUM         sn;                        /* TEMPORARY EXPONENT SIGN          */
NUM          i;                        /* TEMPORARY INTEGER RECOGNIZED     */
REAL         x;                        /* TEMPORARY INTEGER RECOGNIZED     */
REAL         y;                        /* TEMPORARY INTEGER RECOGNIZED     */
REAL         e;                        /* TEMPORARY EXPONENT CALCULATED    */
REAL         f;                        /* TEMPORARY EXPONENT CALCULATED    */
REAL      sign;                        /* TEMPORARY REAL NUMBER SIGN VALUE */
StartFrame
DeclareTVAL(ec);
EndFrame

  /* Skip any leading spaces.  */

 while (*sp == ' ')                     /* IS THIS CHARACTER A SPACE ?      */
  ++sp;                                 /* SKIP ALL LEADING SPACES          */

  /* Test for signed quantity.  */

 if (*sp == '-')                        /* IS THIS A SIGNED NUMBER ?        */
  {
   sign = -1.0;                         /* SET SIGN TO NEGATIVE             */
   ++sp;                                /* SKIP OVER THE SIGN               */
  }
 else
 if (*sp == '+')                        /* IS THIS A SIGNED NUMBER ?        */
  {
   sign = 1.0;                          /* SET SIGN TO POSITIVE             */
   ++sp;                                /* SKIP OVER THE SIGN               */
  }
 else
   sign = 1.0;                          /* SET SIGN TO POSITIVE             */

  /* A real numeric constant may begin with an integer.  */

 *ec = FObject_recint(gCP,gTP,&i,&x,&count,sp);  /* DO WE HAVE AN INTEGER ?          */
 if (!_TObject_IsError(*ec))             /* DO WE HAVE AN INTEGER ?          */
   sp += count;                         /* PROMOTE PAST CONSTANT DIGITS     */
 else
   x = 0.0;                             /* SET INTEGER VALUE TO ZERO        */

  /* All real constants are sandwiched with a '.'.  */

 if (*sp == '.')                        /* IS THIS A REAL CONSTANT ?        */
  {
   ++count;                             /* PROMOTE DIGITS RECOGNIZED COUNT  */
   *ec = FObject_recfrac(gCP,gTP,&y,&n,++sp);    /* DO WE HAVE A FRACTION ?          */
   if (!_TObject_IsError(*ec))           /* DO WE HAVE A FRACTION ?          */
    {
     sp += n;                           /* PROMOTE PAST FRACTION DIGITS     */
     count += n;                        /* PROMOTE PAST FRACTION DIGITS     */
     x += y;                            /* ADJUST INTEGER BY FRACTION       */
     if ((*sp == 'e') || (*sp == 'E'))  /* SCIENTIFIC NOTATION ?            */
      {
       ++count;                         /* PROMOTE PAST EXPONENT SYMBOL     */
       sn = 1.0;                        /* SET EXPONENT SIGN POSITIVE       */
       if (*(++sp) == '-')              /* IS THERE A SIGNED EXPONENT ?     */
        {
         ++count;                       /* PROMOTE PAST EXPONENT SIGN       */
         sn = -1.0;                     /* SET EXPONENT SIGN NEGATIVE       */
         ++sp;                          /* ADJUST INPUT FOR EXPONENT SIGN   */
        }
       *ec = FObject_recint(gCP,gTP,&i,&y,&n,sp);   /*DO WE HAVE AN EXPONENT ?          */
       if (!_TObject_IsError(*ec))       /*DO WE HAVE AN EXPONENT ?          */
        {
         sp += n;                       /* PROMOTE PAST EXPONENT DIGITS     */
         count += n;                    /* PROMOTE PAST EXPONENT DIGITS     */
         if (y != 0.0)                  /* IS THERE A NON ZERO EXPONENT ?   */
          {
           e = 10.0;                    /* CALCULATE REAL EXPONENT          */
           f = (REAL)sn;                /* CALCULATE REAL EXPONENT          */
           y *= f;                      /* CALCULATE REAL EXPONENT          */
           f = log(e);                  /* CALCULATE REAL EXPONENT          */
           e = f * y;                   /* CALCULATE REAL EXPONENT          */
           e = exp(e);                  /* CALCULATE REAL EXPONENT          */
           x *= e;                      /* ADJUST REAL BY EXPONENT          */
          }
        }
      }
    }
   else
     y = 0.0;                           /* SET FRACTION VALUE TO ZERO       */
  }

 if (count > 0)                         /* HAVE WE RECOGNIZED ANYTHING ?    */
    {
    *rs = x * sign;                     /* RETURN THE REAL RESULT           */
    FrameExit(gCP->TObject_OK);                 /* RETURN GOOD CONDITION            */
    }
 else
    {
    *rs = 0.0;                          /* RETURN THE REAL RESULT           */
    FrameExit(gCP->TObject_ERROR_INVALID);      /* RETURN BAD CONDITION             */
    }
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_atob

The TObject_atob function converts an ascii string into a BOLE.

#endif

TVAL TObject_atob(LpXCONTEXT gCP,LpTHREAD gTP,LpBOLE b,LpCHAR sp)
{
gTP = gTP; // NOOP to hide unused parameter warning message
if (strcmp((char*)sp,"true"))
    {
    *b = TRUE;
    return(gCP->TObject_OK);
    }
else
if (strcmp((char*)sp,"false"))
    {
    *b = FALSE;
    return(gCP->TObject_OK);
    }
else
    return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_GlobalMark

The marking function for global objects associated with TObject.

#endif

TVAL TObject_GlobalMark(LpXCONTEXT gCP,LpTHREAD gTP)
{
/*  Send a mark message to the TObject_SaveVector object. */

TObject_MarkObj(gCP,gTP,(TObject*)gTP->TObject_SaveVector);
TObject_MarkObj(gCP,gTP,(TObject*)gTP->TObject_SaveNdx);

return gCP->TObject_OK;
}
/*--------------------------------------------------------------------------------------- */
#if 0
TObject_MapText

Make a copy of the passed TVAL and call the given proc on each element,
storing the result in place.

#endif

TVAL TObject_MapText(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc)
{
NUM                 index;
NUM                 maxIndex;
StartFrame
DeclareTVAL(copy);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/* Make a copy of myself since map returns an object of the same type */

*copy = self;
maxIndex = strlen((char*)asText(&copy));

asTag(tmp) = TYCHAR;

for(index=0; index < maxIndex; index++)
    {
    asChar(tmp) = asText(&copy)[index];
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        asText(&copy)[index] = asChar(ret);
    }
FrameExit(*copy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_MapcText

Loop through the text in the tval and call the specified function.

#endif

TVAL TObject_MapcText(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc)
{
NUM                 index;
NUM                 maxIndex;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

maxIndex = strlen((char*)asText(&self));
asTag(tmp) = TYCHAR;

for(index=0; index < maxIndex; index++)
    {
    asChar(tmp) = asText(&self)[index];
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if (isERROR(ret))
        break;
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_BoolAnyCmp

Compare a typed value to a value of type BOLE.

#endif

TVAL TObject_BoolAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
LpCHAR aStringPtr = NULL;

if (asTag(&rightVal) == TYTVAL)
    return(FPredicate2_FullCompare(gCP, gTP, leftVal, rightVal));
else
if (asTag(&rightVal) == TYTEXT)
    {
    if (asBool(&leftVal) == TRUE)
        return(TObject_strcmp(gCP, gTP, (LpCHAR)"true",asText(&rightVal)));
    else
        return(TObject_strcmp(gCP, gTP, (LpCHAR)"false",asText(&rightVal)));
    }
else
if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NIL)
        return(gCP->TObject_ERROR_INVALID);

    if (rightVal.Tag == TYSTRINGSUBSTR)
        {
        aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
        if (aStringPtr == NULL) return(gCP->TObject_ERROR_INVALID);

        if (asBool(&leftVal) == TRUE)
            return(TString_substrlcmp(gCP, gTP, (LpCHAR)"true",aStringPtr,SubLen(rightVal)));
        else
            return(TString_substrlcmp(gCP, gTP, (LpCHAR)"false",aStringPtr,SubLen(rightVal)));
        }
    else
        {
        aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&rightVal));
        if (aStringPtr == NIL)
            return(gCP->TObject_ERROR_INVALID);

        if (asBool(&leftVal) == TRUE)
            return(TObject_strcmp(gCP, gTP, (LpCHAR)"true",aStringPtr));
        else
            return(TObject_strcmp(gCP, gTP, (LpCHAR)"false",aStringPtr));
        }
    }
else
if ((asTag(&rightVal) == TYBOLE) || (asTag(&rightVal) == TYCHAR))
    {
    if (asBool(&leftVal) == (asBool(&rightVal) ? 1 : 0))
        return(gCP->TObject_EQUAL);
    else
    if (asBool(&leftVal) < (asBool(&rightVal) ? 1 : 0))
        return(gCP->TObject_LOW);
    else
        return(gCP->TObject_HIGH);
    }
else
if (asTag(&rightVal) == TYUNUM)
    {
    if (asBool(&leftVal) == (asUInt(&rightVal) ? 1 : 0))
        return(gCP->TObject_EQUAL);
    else
    if (asBool(&leftVal) < (asUInt(&rightVal) ? 1 : 0))
        return(gCP->TObject_LOW);
    else
        return(gCP->TObject_HIGH);
    }
else
if (asTag(&rightVal) == TYNUM)
    {
    if (asBool(&leftVal) == (asInt(&rightVal) ? 1 : 0))
        return(gCP->TObject_EQUAL);
    else
    if (asBool(&leftVal) < (asInt(&rightVal) ? 1 : 0))
        return(gCP->TObject_LOW);
    else
        return(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYCOMPARE) || (asTag(&rightVal) == TYTYPE))
    {
    if (asBool(&leftVal) == asShort(&rightVal))
        return(gCP->TObject_EQUAL);
    else
    if (asBool(&leftVal) < asShort(&rightVal))
        return(gCP->TObject_LOW);
    else
        return(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYREAL) || (asTag(&rightVal) == TYDATE) || (asTag(&rightVal) == TYMONEY))
    {
    if (asBool(&leftVal) == (asReal(&rightVal) ? 1 : 0))
        return(gCP->TObject_EQUAL);
    else
    if (asBool(&leftVal) < (asReal(&rightVal) ? 1 : 0))
        return(gCP->TObject_LOW);
    else
        return(gCP->TObject_HIGH);
    }
else
    return(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_CharAnyCmp

Compare a typed value to a value of type CHAR.

#endif

TVAL TObject_CharAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
LpCHAR      aStringPtr = NULL;
NUM         tmpValue;
StartFrame
DeclareTVAL(ErrValue);
EndFrame

if (asTag(&rightVal) == TYTVAL)
	{
    FrameExit(FPredicate2_FullCompare(gCP, gTP, leftVal, rightVal));
	}
else
if ((asTag(&rightVal) == TYBOLE) || (asTag(&rightVal) == TYCHAR))
    {
    if (asChar(&leftVal) ==  asChar(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asChar(&leftVal) < asChar(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
if (asTag(&rightVal) == TYTEXT)
    {
    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue, asText(&rightVal));
    _TObject_ErrorChk(*ErrValue);
    if (asChar(&leftVal) == (char)tmpValue)
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asChar(&leftVal) < tmpValue)
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NIL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    if (rightVal.Tag == TYSTRINGSUBSTR)
        {
        aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
        if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

        strncpy(gTP->TempBuffer, aStringPtr, SubLen(rightVal));
        gTP->TempBuffer[SubLen(rightVal)] = 0;
        aStringPtr = &gTP->TempBuffer[0];
        }
    else
        {
        aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&rightVal));
        if (aStringPtr == NIL)
            FrameExit(gCP->TObject_ERROR_INVALID);
        }

    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue, aStringPtr);
    _TObject_ErrorChk(*ErrValue);
    if (asChar(&leftVal) == (char)tmpValue)
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asChar(&leftVal) < tmpValue)
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYNUM) || (asTag(&rightVal) == TYPOINTER))
    {
    if (asChar(&leftVal) == asInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asChar(&leftVal)  < asInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (asTag(&rightVal) == TYUNUM)
    {
    if (asChar(&leftVal) == asUInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asChar(&leftVal)  < asUInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYCOMPARE) || (asTag(&rightVal) == TYTYPE))
    {
    if (asChar(&leftVal)  == asShort(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asChar(&leftVal)  < asShort(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYREAL) || (asTag(&rightVal) == TYDATE) || (asTag(&rightVal) == TYMONEY))
    {
    if (asChar(&leftVal)  == asReal(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asChar(&leftVal)  < asReal(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_IntAnyCmp

Compare a typed value to a value of type NUM.

#endif

TVAL TObject_IntAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
LpCHAR      aStringPtr = NULL;
NUM         tmpValue;
StartFrame
DeclareTVAL(ErrValue);
EndFrame

if (asTag(&rightVal) == TYTVAL)
	{
    FrameExit(FPredicate2_FullCompare(gCP, gTP, leftVal, rightVal));
	}
else
if (asTag(&rightVal) == TYTEXT)
    {
    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue, asText(&rightVal));
    _TObject_ErrorChk(*ErrValue);
    if (asInt(&leftVal) == tmpValue)
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asInt(&leftVal) < tmpValue)
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    if (rightVal.Tag == TYSTRINGSUBSTR)
        {
        aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
        if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

        strncpy(gTP->TempBuffer, aStringPtr, SubLen(rightVal));
        gTP->TempBuffer[SubLen(rightVal)] = 0;
        aStringPtr = &gTP->TempBuffer[0];
        }
    else
        {
        aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&rightVal));
        if (aStringPtr == NIL)
            FrameExit(gCP->TObject_ERROR_INVALID);
        }

    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue, aStringPtr);
    _TObject_ErrorChk(*ErrValue);
    if (asInt(&leftVal) == tmpValue)
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asInt(&leftVal) < tmpValue)
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYBOLE) || (asTag(&rightVal) == TYCHAR))
    {
    if (asInt(&leftVal) == asChar(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asInt(&leftVal) < asChar(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (rightVal.Tag == TYUNUM)
    {
    if (leftVal.u.UInt == rightVal.u.UInt)
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (leftVal.u.UInt < rightVal.u.UInt)
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYNUM) || (asTag(&rightVal) == TYPOINTER))
    {
    if (asInt(&leftVal) == asInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asInt(&leftVal) < asInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYCOMPARE) || (asTag(&rightVal) == TYTYPE))
    {
    if (asInt(&leftVal) == asShort(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asInt(&leftVal) < asShort(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYREAL) || (asTag(&rightVal) == TYDATE) || (asTag(&rightVal) == TYMONEY))
    {
    if (asInt(&leftVal) == asReal(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asInt(&leftVal) < asReal(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}


/*--------------------------------------------------------------------------------------- */


#if 0
TObject_ShortAnyCmp

Compare a typed value to a value of type SHORT.

#endif

TVAL TObject_ShortAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
LpCHAR      aStringPtr = NULL;
NUM         tmpValue;
StartFrame
DeclareTVAL(ErrValue);
EndFrame

if (asTag(&rightVal) == TYTVAL)
	{
    FrameExit(FPredicate2_FullCompare(gCP, gTP, leftVal, rightVal));
	}
else
if (asTag(&rightVal) == TYTEXT)
    {
    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue, asText(&rightVal));
    _TObject_ErrorChk(*ErrValue);

    if (asShort(&leftVal) == tmpValue)
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asShort(&leftVal) < tmpValue)
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);

    if (rightVal.Tag == TYSTRINGSUBSTR)
        {
        aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
        if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

        strncpy(gTP->TempBuffer, aStringPtr, SubLen(rightVal));
        gTP->TempBuffer[SubLen(rightVal)] = 0;
        aStringPtr = &gTP->TempBuffer[0];
        }
    else
        {
        aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&rightVal));
        if (aStringPtr == NIL)
            FrameExit(gCP->TObject_ERROR_INVALID);
        }

    *ErrValue = FObject_atoi(gCP, gTP, &tmpValue, aStringPtr);
    _TObject_ErrorChk(*ErrValue);

    if (asShort(&leftVal) == tmpValue)
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asShort(&leftVal) < tmpValue)
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
if ((asTag(&rightVal) == TYBOLE) || (asTag(&rightVal) == TYCHAR))
    {
    if (asShort(&leftVal) == asChar(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asShort(&leftVal) < asChar(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (asTag(&rightVal) == TYUNUM)
    {
    if (asShort(&leftVal) == asUInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asShort(&leftVal) < asUInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYNUM) || (asTag(&rightVal) == TYPOINTER))
    {
    if (asShort(&leftVal) == asInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asShort(&leftVal) < asInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYCOMPARE) || (asTag(&rightVal) == TYTYPE))
    {
    if (asShort(&leftVal) == asShort(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asShort(&leftVal) < asShort(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYREAL) || (asTag(&rightVal) == TYDATE) || (asTag(&rightVal) == TYMONEY))
    {
    if (asShort(&leftVal) == asReal(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asShort(&leftVal) < asReal(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

// RCA


/*--------------------------------------------------------------------------------------- */

#if 0
TObject_RealAnyCmp

Compare a typed value to a value of type REAL.

Note:   This function also operates for left values of type DATE and MONEY.

#endif

TVAL TObject_RealAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
StartFrame
DeclareTVAL(ErrValue);
EndFrame

if (asTag(&rightVal) == TYTVAL)
	{
    FrameExit(FPredicate2_FullCompare(gCP, gTP, leftVal, rightVal));
	}
else
if ((rightVal.Tag == TYREAL) || (rightVal.Tag == TYMONEY) || (rightVal.Tag == TYDATE))
    {
    if (asReal(&leftVal) == asReal(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asReal(&leftVal) < asReal(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYTEXT) || (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT))
    {
    *ErrValue = FObject_RealAnyCnv(gCP,gTP,leftVal.Tag,rightVal);
    _TObject_ErrorChk(*ErrValue);

    if (leftVal.u.Real == ErrValue->u.Real)
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (leftVal.u.Real < ErrValue->u.Real)
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYBOLE) || (asTag(&rightVal) == TYCHAR))
    {
    if (asReal(&leftVal) == asChar(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asReal(&leftVal) < asChar(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (asTag(&rightVal) == TYUNUM)
    {
    if (asReal(&leftVal) == asUInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asReal(&leftVal) < asUInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYNUM) || (asTag(&rightVal) == TYPOINTER))
    {
    if (asReal(&leftVal) == asInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asReal(&leftVal) < asInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYCOMPARE) || (asTag(&rightVal) == TYTYPE))
    {
    if (asReal(&leftVal) == asShort(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (asReal(&leftVal) < asShort(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_FloatAnyCmp

Compare a typed value to a value of type FLOAT.

Note:   This function assumes that the left value is of type FLOAT.

#endif

TVAL TObject_FloatAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
StartFrame
EndFrame

switch (asTag(&rightVal))
	{
	case TYREAL:
	case TYDATE:
	case TYMONEY:
		if (*((LpFLOAT)&leftVal.u.Real) < rightVal.u.Real) FrameExit(gCP->TObject_LOW);
		if (*((LpFLOAT)&leftVal.u.Real) == rightVal.u.Real) FrameExit(gCP->TObject_EQUAL);
		FrameExit(gCP->TObject_HIGH);
    break;

	case TYFLOAT:
		if (*((LpFLOAT)&leftVal.u.Real) < *((LpFLOAT)&rightVal.u.Real)) FrameExit(gCP->TObject_LOW);
		if (*((LpFLOAT)&leftVal.u.Real) == *((LpFLOAT)&rightVal.u.Real)) FrameExit(gCP->TObject_EQUAL);
		FrameExit(gCP->TObject_HIGH);
    break;

	case TYUNUM:
		if (*((LpFLOAT)&leftVal.u.Real) < rightVal.u.UInt) FrameExit(gCP->TObject_LOW);
		if (*((LpFLOAT)&leftVal.u.Real) == rightVal.u.UInt) FrameExit(gCP->TObject_EQUAL);
		FrameExit(gCP->TObject_HIGH);
    break;

	case TYNUM:
	case TYBOLE:
	case TYCHAR:
		if (*((LpFLOAT)&leftVal.u.Real) < rightVal.u.Int) FrameExit(gCP->TObject_LOW);
		if (*((LpFLOAT)&leftVal.u.Real) == rightVal.u.Int) FrameExit(gCP->TObject_EQUAL);
		FrameExit(gCP->TObject_HIGH);
    break;

	case TYSHORT:
		if (*((LpFLOAT)&leftVal.u.Real) < rightVal.u.Short) FrameExit(gCP->TObject_LOW);
		if (*((LpFLOAT)&leftVal.u.Real) == rightVal.u.Short) FrameExit(gCP->TObject_EQUAL);
		FrameExit(gCP->TObject_HIGH);
    break;

	case TYCOMPARE:
		if (*((LpFLOAT)&leftVal.u.Real) < rightVal.u.Compare) FrameExit(gCP->TObject_LOW);
		if (*((LpFLOAT)&leftVal.u.Real) == rightVal.u.Compare) FrameExit(gCP->TObject_EQUAL);
		FrameExit(gCP->TObject_HIGH);
    break;

	default:
		if (leftVal.Tag < rightVal.Tag) FrameExit(gCP->TObject_LOW);
		if (leftVal.Tag == rightVal.Tag) FrameExit(gCP->TObject_EQUAL);
		FrameExit(gCP->TObject_HIGH);
    break;
	}

FrameExit(gCP->TObject_ERROR_INVALID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_DateAnyCmp

Compare a typed value to a value of type TYDATE.

#endif

TVAL TObject_DateAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
return(TObject_RealAnyCmp(gCP,gTP,leftVal,rightVal));
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_MoneyAnyCmp

Compare a typed value to a value of type TYMONEY.

#endif

TVAL TObject_MoneyAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
return(TObject_RealAnyCmp(gCP,gTP,leftVal,rightVal));
}


/*--------------------------------------------------------------------------------------- */

#if 0
TObject_TextAnyCmp

Compare a typed value to a value of type TEXT.

#endif

TVAL TObject_TextAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
REAL        tmpValue;
NUM			leftIntValue;
UNUM		leftUIntValue;
LpCHAR      aStringPtr = NULL;
LpCHAR      leftStringPtr;
StartFrame
DeclareTVAL(ErrValue);
EndFrame

/* Get the left value */
if(asTag(&leftVal) == TYTEXT)
    leftStringPtr = asText(&leftVal);
else
if(asTag(&leftVal) == TYSTRING)
    leftStringPtr = &atHMChar(asString(&leftVal)->itsCString,0);
else
if(asTag(&leftVal) == TYSTRINGSUBSTR)
    {
    FrameExit(TStringSubstringT_SubstringAnyCmp(gCP, gTP, leftVal, rightVal));
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);

/* Get the right value */
if (asTag(&rightVal) == TYTVAL)
	{
    FrameExit(FPredicate2_FullCompare(gCP, gTP, leftVal, rightVal));
	}
else
if (asTag(&rightVal) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);

    FrameExit(TString_substrlcmp(gCP, gTP, leftStringPtr, aStringPtr, SubLen(rightVal)));
    }
else
if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NIL)
		{
        FrameExit(gCP->TObject_ERROR_INVALID);
		}

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&rightVal));
    if (aStringPtr == NIL) FrameExit(gCP->TObject_ERROR_INVALID);

    FrameExit(TObject_strcmp(gCP, gTP, leftStringPtr,aStringPtr));
    }
else
if (asTag(&rightVal) == TYTEXT)
    {
    FrameExit(TObject_strcmp(gCP, gTP, leftStringPtr,asText(&rightVal)));
    }
else
if (asTag(&rightVal) == TYBOLE)
    {
    if (asBool(&rightVal) == TRUE)
		{
        FrameExit(TObject_strcmp(gCP, gTP, leftStringPtr,(LpCHAR)"true"));
		}
    else
        FrameExit(TObject_strcmp(gCP, gTP, leftStringPtr,(LpCHAR)"false"));
    }
else
if (asTag(&rightVal) == TYCHAR)
    {
    if (*leftStringPtr == asChar(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (*leftStringPtr < asChar(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (asTag(&rightVal) == TYUNUM)
    {
	*ErrValue = FObject_atoui(gCP,gTP,&leftUIntValue,leftStringPtr);
    _TObject_ErrorChk(*ErrValue);

    if (leftUIntValue == asUInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (leftUIntValue < asUInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYNUM) || (asTag(&rightVal) == TYPOINTER))
    {
	*ErrValue = FObject_atoi(gCP,gTP,&leftIntValue,leftStringPtr);
    _TObject_ErrorChk(*ErrValue);

    if (leftIntValue == asInt(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (leftIntValue < asInt(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if ((asTag(&rightVal) == TYCOMPARE) || (asTag(&rightVal) == TYTYPE))
    {
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,leftStringPtr);
    _TObject_ErrorChk(*ErrValue);

    if (tmpValue == asShort(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (tmpValue < asShort(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
if (asTag(&rightVal) == TYREAL)
    {
    *ErrValue = TObject_ator(gCP,gTP,&tmpValue,leftStringPtr);
    _TObject_ErrorChk(*ErrValue);

    if (tmpValue == asReal(&rightVal))
		{
        FrameExit(gCP->TObject_EQUAL);
		}
    else
    if (tmpValue < asReal(&rightVal))
		{
        FrameExit(gCP->TObject_LOW);
		}
    else
        FrameExit(gCP->TObject_HIGH);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}


/*--------------------------------------------------------------------------------------- */

#if 0
TObject_ErrorAnyCmp

Compare a typed value to a value of type ERROR.

#endif

TVAL TObject_ErrorAnyCmp(LpXCONTEXT gCP,LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
LpCHAR aStringPtr = NIL;

if (asTag(&rightVal) == TYTVAL)
    return(FPredicate2_FullCompare(gCP, gTP, leftVal, rightVal));
else
if (asTag(&rightVal) == TYTEXT)
    {
    return(TObject_strcmp(gCP, gTP, asText(&leftVal),asText(&rightVal)));
    }
else
if (asTag(&rightVal) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, rightVal);
    if (aStringPtr == NULL) return(gCP->TObject_ERROR_INVALID);

    return(TString_substrlcmp(gCP, gTP, asText(&leftVal), aStringPtr, SubLen(rightVal)));
    }
else
if (_TObject_TypeFlag(asTag(&rightVal)) == _TObject_TfTOBJECT)
    {
    if (asObject(&rightVal) == NULL)
        return(gCP->TObject_ERROR_INVALID);

    aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&rightVal));
    if (aStringPtr == NULL)
        return(gCP->TObject_ERROR_INVALID);

    return(TObject_strcmp(gCP, gTP, asText(&leftVal),aStringPtr));
    }
else
    return(gCP->TObject_ERROR_INVALID);
}


/*--------------------------------------------------------------------------------------- */

#if 0
TObject_TvalAnyCmp

Compare a typed value to a value of type TVAL.

#endif

TVAL TObject_TvalAnyCmp(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal)
{
return(FPredicate2_FullCompare(gCP, gTP, leftVal, rightVal));
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_CnvFromText

Convert from a c string into a TVAL.

#endif

TVAL TObject_CnvFromText(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR inputText)
{
StartFrame
DeclareTVAL(retTval);
EndFrame

if (strlen((char*)inputText) >= MAXAISWORDTEXTLEN)
    {
    *retTval = gCP->Tval_VOID;
    asObject(retTval) = (TObject*)TString_MakeUnique(gCP,gTP,inputText);
    asTag(retTval) = TYSTRING;
    }
else
    {
    asTag(retTval) = TYTEXT;
    // This is causing a buffer overflow in GCC 4.x
    //strcpy((char*)asText(retTval), (const char*)inputText);
    memcpy((void*)asText(retTval), (const void*)inputText, strlen((char*)inputText) + 1);
    }

FrameExit(*retTval);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_CnvToText

Convert from a TVAL to a c string.

#endif

TVAL TObject_CnvToText(LpXCONTEXT gCP,LpTHREAD gTP, LpCHAR buf, NUM maxLen, TVAL source)
{
NUM     oldLen;
NUM     size = 0;
LpCHAR  aStringPtr = NULL;
CHAR	typeName[256];
CHAR	tmpBuf[1024];
StartFrame
DeclareTVAL(ec);
EndFrame


if (asTag(&source) == TYTEXT)
	{
    aStringPtr = asText(&source);
    if ((NUM)strlen((char*)aStringPtr) < (maxLen - 4))
        {
        strcpy((char*)buf, (const char*)aStringPtr);
        }
    else
    if ((maxLen - 4) > 0)
        {
        strncpy((char*)buf, (const char*)aStringPtr, maxLen-4);
        strcpy((char*)&buf[maxLen-4], "...");
        }
    else
        {
        buf[0] = 0;
        }
	}
else
if ((asTag(&source) == TYSYMBOL) ||
	(asTag(&source) == TYQUOTEDSYMBOL) ||
	(asTag(&source) == TYSTRING))
	{
    aStringPtr = &atHMChar(asSymbol(&source)->itsCString,0);
    if ((NUM)strlen((char*)aStringPtr) < (maxLen - 4))
        {
        strcpy((char*)buf, (const char*)aStringPtr);
        }
    else
    if ((maxLen - 4) > 0)
        {
        strncpy((char*)buf, (const char*)aStringPtr, maxLen-4);
        strcpy((char*)&buf[maxLen-4], "...");
        }
    else
        {
        buf[0] = 0;
        }
	}
else
if (source.Tag == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, source);
    if (aStringPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
    size = SubLen(source);

    if (size < (maxLen - 4))
        {
        strncpy((LpCHAR)buf, (const char*)aStringPtr, size);
        }
    else
    if ((maxLen - 4) > 0)
        {
        strncpy((LpCHAR)buf, (const char*)aStringPtr, maxLen - 4);
        strcpy((LpCHAR)&buf[maxLen - 4], "...");
        }
    else
        {
        buf[0] = 0;
        }
    }
else
if (asTag(&source) == TYBYTEVECTOR)
	{
    aStringPtr = &atHMChar(((TByteVector*)asObject(&source))->itsByteArray,0);
    oldLen = ((TByteVector*)asObject(&source))->itsMaxItemIndex;
    if (oldLen < maxLen)
        {
        strncpy((char*)buf, (const char*)aStringPtr,oldLen);
        buf[oldLen] = 0;
        }
    else
    if ((maxLen - 4) > 0)
        {
        strncpy((char*)buf, (const char*)aStringPtr, maxLen-4);
        strcpy((char*)&buf[maxLen-4], "...");
        }
    else
        {
        buf[0] = 0;
        }
	}
else
if ((source.Tag == TYCPROCEDURE) || (source.Tag == TYVMEVALUATOR))
	{
	FProcedure_PrintcProcedure(gCP,gTP,source,&size,buf);
	}
else
if (source.Tag == TYCPX)
	goto PrintValue;
else
if (source.Tag == TYBRICKROW)
    {
    strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&source)));
    sprintf((char*)tmpBuf,"#<Brick "SHORTFORMAT">["SHORTFORMAT"]", ObjIdx(source), RowIdx(source));

    strcpy((char*)buf, (const char*)tmpBuf);
    FrameExit(gCP->TObject_OK);
    }
else
if (source.Tag == TYBRICKFIELD)
    {
    strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&source)));
    sprintf((char*)tmpBuf,"#<Brick "SHORTFORMAT">["SHORTFORMAT"]["SHORTFORMAT"]", ObjIdx(source), RowIdx(source), FldIdx(source));

    strcpy((char*)buf, (const char*)tmpBuf);
    FrameExit(gCP->TObject_OK);
    }
else
if (source.Tag == TYMATRIXROW)
    {
    strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&source)));
    if (FldIdx(source) >= 0)
        sprintf((char*)tmpBuf,"#<Matrix "SHORTFORMAT">["SHORTFORMAT"]["SHORTFORMAT"]", ObjIdx(source), RowIdx(source), FldIdx(source));
    else
        sprintf((char*)tmpBuf,"#<Matrix "SHORTFORMAT">["SHORTFORMAT"]", ObjIdx(source), RowIdx(source));

    strcpy((char*)buf, (const char*)tmpBuf);
    FrameExit(gCP->TObject_OK);
    }
else
if (source.Tag == TYNUMMATRIXROW)
    {
    strcpy((char*)typeName, (const char*)_TObject_TypeName(asTag(&source)));
    if (FldIdx(source) >= 0)
        sprintf((char*)tmpBuf,"#<NumMatrix "SHORTFORMAT">["SHORTFORMAT"]["SHORTFORMAT"]", ObjIdx(source), RowIdx(source), FldIdx(source));
    else
        sprintf((char*)tmpBuf,"#<NumMatrix "SHORTFORMAT">["SHORTFORMAT"]", ObjIdx(source), RowIdx(source));

    strcpy((char*)buf, (const char*)tmpBuf);
    FrameExit(gCP->TObject_OK);
    }
else
if (_TObject_TypeFlag(asTag(&source)) == _TObject_TfTOBJECT)
    {
    strcpy((char *)typeName, (const char *)_TObject_TypeName(asTag(&source)));
    sprintf((char *)tmpBuf,"#<%s "INTFORMAT">",(const char *)typeName,asObject(&source)->itsObjectIndex);

    strcpy((char*)buf, (const char*)tmpBuf);
    FrameExit(gCP->TObject_OK);
    }
else
	{
PrintValue:
    if (_TObject_TypePrint(asTag(&source)) != NULL)
        {
        oldLen = gCP->TObject_MaxOutputLen;
        gCP->TObject_MaxOutputLen = maxLen;
        buf[0] = 0;
        *ec = (*((_TObject_TypePrint(asTag(&source)))))(gCP,gTP,source,&size,buf);
        gCP->TObject_MaxOutputLen = oldLen;
        FrameExit(*ec);
        }
    else
        FrameExit(TObject_Error(gCP,gTP,"!cantcnvtotext!"));
    }

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_Destruct

Before deleting the storage allocated for this object, remove the object from the list
of living objects.

Note:   This method should only be called by mark and sweep garbage collection!
        destroying an object outside the scope of mark and sweep can leave
        dangerous invalid object references.

#endif

void TObject_Destruct(LpXCONTEXT gCP,LpTHREAD gTP,TObject* self)
{
NUM			previousFreeObjectIndex;

gTP = gTP; // NOOP to hide unused parameter warning message
--gCP->TObject_UsedObjectCount;
previousFreeObjectIndex = gCP->TObject_FreeObjectIndex;
gCP->TObject_FreeObjectIndex = self->itsObjectIndex;
_TObject_MainObjectList(self->itsObjectIndex) = previousFreeObjectIndex;
gCP->TObject_FreeObjectIndex = self->itsObjectIndex;
_TObject_ObjectFlag(self->itsObjectIndex) = _TObject_OfVOID;
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_OperatorNew

Create a new blank object header, adding the new object header to the list of live objects.

#endif

void*   TObject_OperatorNew(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TObject,self);
EndFrame


/*  Grow the maximum number of objects (if necessary). */
if (gCP->TObject_FreeObjectIndex == 0)
    {
    TObject_MarkAndSweep(gCP,gTP);
    if (gCP->TObject_FreeObjectIndex == 0)
        {
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_TOO_MANY_OBJECTS);
        }
    }

/*  Allocate main memory for the new object. */
/*  Note: We always allocate the fixed size for all object headers. */
/* DEBUG FSmartbase_Log(gCP, gTP, "TObject_OperatorNew, Allocate main memory for the new object\r\n"); */			/* NDEBUG */
self = (TObject*)((*((char **)gCP->TObject_MainObjectHeaderArray))+(gCP->TObject_FreeObjectIndex*_FSmartbase_ObjectHeaderMaxSize));
_FMemory_memset(self,0,_FSmartbase_ObjectHeaderMaxSize);	/* sizeof(TLambda) = 208 bytes */

/*  Link the new object to the mark and sweep garbage collection table. */
++gCP->TObject_UsedObjectCount;
gCP->TObject_MaxUsedObjectCount = max(gCP->TObject_MaxUsedObjectCount,gCP->TObject_UsedObjectCount);
self->itsObjectIndex = gCP->TObject_FreeObjectIndex;
gCP->TObject_FreeObjectIndex = _TObject_MainObjectList(self->itsObjectIndex);
_TObject_MainObjectList(self->itsObjectIndex) = (OBJ)self;
_TObject_ObjectFlag(self->itsObjectIndex) = _TObject_OfOBJECT;
/* DEBUG FSmartbase_Log(gCP, gTP, "TObject_OperatorNew, Finished.\r\n"); */		/* NDEBUG */

FrameExit((void*)self);
}

/*--------------------------------------------------------------------------------------- */

#if 0
TObject_OperatorDelete

Delete this storage allocated for this object.

Note:   This method should only be called by mark and sweep garbage collection!
        Disposing of an object outside the scope of mark and sweep can leave
        dangerous invalid object references.

#endif

void    TObject_OperatorDelete(LpXCONTEXT gCP,LpTHREAD gTP,TObject* self)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
self->itsObjectIndex = NIL;
self->itsObjectType = NIL;
self->itsObjectFlag = NIL;
}



/*--------------------------------------------------------------------------------------- */

#if 0
TObject_TextAnyCnv

Convert a typed value to a value of type TEXT.

#endif

TVAL TObject_TextAnyCnv(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue)
{
NUM             size;
NUM             savsize;
CHAR            typeName[256];
CHAR            tmpBuf[1024];
LpCHAR          aStringPtr = NULL;
StartFrame
DeclareTVAL(retTval);
EndFrame

tTarget = tTarget; // NOOP to hide unused parameter warning message
/*  Make sure we set default return value to empty text. */
retTval->Tag         = TYTEXT;
retTval->u.Text[0]   = 0;

if( asTag(&oldValue) == TYQUOTEDPAIR)
    {
    savsize  = gCP->TObject_MaxOutputLen;
    gCP->TObject_MaxOutputLen = sizeof(tmpBuf) - 1;
    size = 0;
    tmpBuf[0] = 0;
    *retTval = TPair_PrintQuoted(gCP,gTP,oldValue, &size, tmpBuf);
    gCP->TObject_MaxOutputLen = savsize;
    if(asTag(retTval) == TYBOLE && asBool(retTval) == TRUE)
        *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if( asTag(&oldValue) == TYQUOTEDSYMBOL)
    {
    savsize  = gCP->TObject_MaxOutputLen;
    gCP->TObject_MaxOutputLen = sizeof(tmpBuf) - 1;
    size = 0;
    tmpBuf[0] = 0;
    *retTval = TSymbol_PrintQuoted(gCP, gTP, oldValue, &size, tmpBuf);
    gCP->TObject_MaxOutputLen = savsize;
    if(asTag(retTval) == TYBOLE && asBool(retTval) == TRUE)
        *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if( asTag(&oldValue) == TYSYMBOL)
    {
    tmpBuf[0] = 0;
    if (asSymbol(&oldValue)->itNeedsBars == TRUE)
        sprintf((char*)tmpBuf,"|%s|",(char*)*(asSymbol(&oldValue)->itsCString));
    else
        strcpy((char*)tmpBuf,(const char*)*(asSymbol(&oldValue)->itsCString));
    *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYCPX)
    {
	size = 0;
    tmpBuf[0] = 0;
    TCpx_Print(gCP,gTP, oldValue, &size, tmpBuf);
    *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYSTRINGSUBSTR)
    {
    aStringPtr = TStringSubstringT_GetStringPtr(gCP, gTP, oldValue);
    if (aStringPtr == NULL)
        {
        strcpy((char *)typeName, (const char *)_TObject_TypeName(asTag(&oldValue)));
        sprintf((char *)tmpBuf,"#<%s "INTFORMAT">",(const char *)typeName,asObject(&oldValue)->itsObjectIndex);
        aStringPtr = tmpBuf;
        size = strlen(aStringPtr);
        }
    else
        size = SubLen(oldValue);

    asObject(retTval) = (TObject*)TString_SubString_MakeUnique(gCP,gTP,aStringPtr,0,size);
    asTag(retTval) = TYSTRING;
    FrameExit(*retTval);
    }
else
if (_TObject_TypeFlag(asTag(&oldValue)) == _TObject_TfTOBJECT)
    {
    if (asObject(&oldValue) == NULL)
		aStringPtr = (LpCHAR)"#void";
	else
        {
        aStringPtr = FObject_GetStringPtr(gCP, gTP, asObject(&oldValue));
        }

    if (aStringPtr == NULL)
        {
        strcpy((char *)typeName, (const char *)_TObject_TypeName(asTag(&oldValue)));

        sprintf((char *)tmpBuf,"#<%s "INTFORMAT">",(const char *)typeName,asObject(&oldValue)->itsObjectIndex);
        }
    else
    if(strlen((char *)aStringPtr) < sizeof(tmpBuf) - 1)
        strcpy((char*)tmpBuf,(const char *)aStringPtr);
    else
        {
        /*  error ... */

        FrameExit(gCP->TObject_ERROR_BADIVAL);
        }

    *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYTEXT)
    {
    oldValue.Tag = TYTEXT;
    FrameExit(oldValue);
    }
else
if (asTag(&oldValue) == TYVOID)
    {
    strcpy((char *)asText(retTval),(const char *)"#void");
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYLEX)
    {
    strcpy((char *)asText(retTval),(const char *)"!lexicalform!");
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYBOLE)
    {
    if (asBool(&oldValue) == TRUE)
        strcpy((char *)asText(retTval),(const char *)"true");
    else
        strcpy((char *)asText(retTval),(const char *)"false");
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYCHAR)
    {
    sprintf((char *)asText(retTval), "%c", (int)asChar(&oldValue));
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYUNUM)
    {
	tmpBuf[0] = '#';
    sprintf((char *)&tmpBuf[1],UINTFORMAT,asUInt(&oldValue));
    *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYNUM) || (asTag(&oldValue) == TYPOINTER))
    {
    sprintf((char *)tmpBuf,INTFORMAT,asInt(&oldValue));
    *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYUNUM)
    {
    sprintf((char *)tmpBuf,UINTFORMAT,asUInt(&oldValue));
    *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if ((asTag(&oldValue) == TYCOMPARE) || (asTag(&oldValue) == TYTYPE))
    {
    sprintf((char *)tmpBuf,SHORTFORMAT,asShort(&oldValue));
    *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYREAL)
    {
    TObject_sprintReal(gCP,gTP,(char *)tmpBuf,asReal(&oldValue));
    *retTval = TObject_CnvFromText(gCP, gTP, tmpBuf);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYMONEY)
    {
    tmpBuf[0] = '$';
    TObject_sprintReal(gCP,gTP,(char *)&tmpBuf[1],asReal(&oldValue));
    *retTval = TObject_CnvFromText(gCP,gTP,tmpBuf);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) == TYDATE)
    {
	TObject_CnvDateToText(gCP, gTP, tmpBuf, oldValue);
    /* Convert the final text into a tval. */
    *retTval = TObject_CnvFromText(gCP,gTP,tmpBuf);
    FrameExit(*retTval);
    }
else
if (asTag(&oldValue) < gCP->TObject_MaxTypes)
    {
    sprintf((char*)tmpBuf,"#<%s>", _TObject_TypeName(asTag(&oldValue)));
    *retTval = TObject_CnvFromText(gCP,gTP,tmpBuf);
    FrameExit(*retTval);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID);
}


/*--------------------------------------------------------------------------------------- */

#if 0
TObject_CnvDateToText

Convert a date value to a value of type TEXT.

#endif

TVAL TObject_CnvDateToText(LpXCONTEXT gCP, LpTHREAD gTP,LpCHAR tmpBuf, TVAL dateValue)
{
REAL            julian;
REAL            julianFrac;
BOLE            bc;
NUM             leapyear;
NUM             year400th;
NUM             year100th;
NUM             year4th;
NUM             day;
REAL            truncatedleapyearcycles;
REAL            leapyearcycles;
NUM             year;
StartFrame
EndFrame

/*  Make sure we receive and return the correct values. */

if (dateValue.Tag != TYDATE) FrameExit(gCP->TObject_ERROR_INVALID);

if (FFloat_NUMCHECK(gCP, gTP, dateValue.u.Real))
    {
    /* Normalize the date to julian. */

    julian = floor(dateValue.u.Real);
    julianFrac = dateValue.u.Real - julian;

    /* Compute the date for all dates after the birth of Christ. */
    if (julian >= 0)
        {
        bc = FALSE;
        /*  Compute the number of years since Christ. Every 4th year is a leap year. */
        /*  Every 100th year is NOT a leap year. Every 400th year is a leap year. */
        /*  Identify whether this is a leap year, and compute the number of days */
        /*  since the start of the year. */

		year400th = 0;
		year100th = 0;
		year4th = 0;
		if (julian < 365)
			{
			year = 0;
			}
		else
			{
			year = 1;
			julian -= 365;
			truncatedleapyearcycles = floor(julian / 146097);
			leapyearcycles = truncatedleapyearcycles;
			julian -= (leapyearcycles * 146097);
			year += leapyearcycles * 400;
			if (julian >= 145731) year400th = 1;

			truncatedleapyearcycles = floor(julian / 36524);
			leapyearcycles = truncatedleapyearcycles;
			julian -= (leapyearcycles * 36524);
			year += (leapyearcycles * 100);
			if ((julian >= 36159) && (year400th == 0)) year100th = 1;

			truncatedleapyearcycles = floor(julian / 1461);
			leapyearcycles = truncatedleapyearcycles;
			julian -= (leapyearcycles * 1461);
			year += (leapyearcycles * 4);
			if ((julian >= 1096) && (year400th == 0) && (year100th == 0)) year4th = 1;
			}

		leapyear = 0;
		if ((year400th == 1) || (year4th == 1)) leapyear = 1;

		if ((julian == 0) && (year400th == 1))
			{
			--year;
			julian = 365;
			}
		else
        if (julian < 365)
            {
            year += 0;
            }
        else
        if (julian < 730)
            {
            year += 1;
            julian -= 365 + leapyear;
            }
        else
        if (julian < 1095)
            {
            year += 2;
            julian -= 730;
            }
        else
            {
            year += 3;
            julian -= 1095;
            }
        }
    /* Compute the date for all dates before the birth of Christ. */
    else
        {
        bc = TRUE;
        /*  Compute the number of years before Christ. Every 4th year is a leap year. */
        /*  Every 100th year is NOT a leap year. Every 400th year is a leap year. */
        /*  Identify whether this is a leap year, and compute the number of days */
        /*  since the start of the year. */
		julian = -julian;
		--julian; /* reset #Dec,31,1BC to zero BC */
		year400th = 0;
		year100th = 0;
		year4th = 0;
		truncatedleapyearcycles = floor(julian / 146097);
		leapyearcycles = truncatedleapyearcycles;
		julian -= (leapyearcycles * 146097);
		year = leapyearcycles * 400;
		if (julian >= 145731) year400th = 1;

		truncatedleapyearcycles = floor(julian / 36524);
		leapyearcycles = truncatedleapyearcycles;
		julian -= (leapyearcycles * 36524);
		year += (leapyearcycles * 100);
		if ((julian >= 36159) && (year400th == 0)) year100th = 1;

		truncatedleapyearcycles = floor(julian / 1461);
		leapyearcycles = truncatedleapyearcycles;
		julian -= (leapyearcycles * 1461);
		year += (leapyearcycles * 4);
		if ((julian >= 1096) && (year400th == 0) && (year100th == 0)) year4th = 1;

		leapyear = 0;
		if ((year400th == 1) || (year4th == 1)) leapyear = 1;

		if ((julian == 0) && (year400th == 1))
			{
			julian = 0;
			}
		else
        if (julian <= 364)
            {
            year += 1;
            julian = 364 - julian;
            }
        else
        if (julian <= 729)
            {
            year += 2;
            julian = 729 - julian;
            }
        else
        if (julian <= 1094)
            {
            year += 3;
            julian = 1094 - julian;
            }
        else
            {
            year += 4;
            julian = (1459 + leapyear) - julian;
            }
        }
    /*  Compute the month of the year (every leap years has Feb 29th). */
    if (julian < 31)
        {
        day = ++julian;
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Jan,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (59 + leapyear))
        {
        day = ++julian - 31;
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Feb,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (90 + leapyear))
        {
        day = ++julian - (59 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Mar,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (120 + leapyear))
        {
        day = ++julian - (90 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Apr,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (151 + leapyear))
        {
        day = ++julian - (120 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#May,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (181 + leapyear))
        {
        day = ++julian - (151 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Jun,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (212 + leapyear))
        {
        day = ++julian - (181 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Jul,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (243 + leapyear))
        {
        day = ++julian - (212 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Aug,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (273 + leapyear))
        {
        day = ++julian - (243 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Sep,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (304 + leapyear))
        {
        day = ++julian - (273 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Oct,%i,%i",(LpCHAR)&day,&year);
        }
    else
    if (julian < (334 + leapyear))
        {
        day = ++julian - (304 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Nov,%i,%i",(LpCHAR)&day,&year);
        }
    else
        {
        day = ++julian - (334 + leapyear);
        FConio_sprintf(gCP,gTP,tmpBuf,(LpCHAR)"#Dec,%i,%i",(LpCHAR)&day,&year);
        }
    /* Add BC suffix to the final text (if necessary). */
    if (bc)
        strcat((char*)tmpBuf,(const char*)"BC");

    if(julianFrac)
        {
        REAL    tmpReal;
        NUM     hours;
        NUM     mins;
        NUM     secs;
        CHAR    timeBuf[64];

        tmpReal = julianFrac * 24.0;
        hours = tmpReal;
        tmpReal = (tmpReal - hours)*60;
        mins = tmpReal;
        secs = (tmpReal - mins)*60;

        sprintf((char*)timeBuf, ":%02ld:%02ld:%02ld", hours, mins, secs);
        strcat((char*)tmpBuf,(const char*)timeBuf);
        }
    }
else
    {
    strcpy(tmpBuf,"!matherror!");
    }

FrameExit(gCP->Tval_TRUE);
}


/*  CConversions */

/*--------------------------------------------------------------------------------------- */
#if 0
Mapc

Loop through each of my members and apply the specified procedure to each
one of them. Return the result of the final application on my last member.

#endif

TVAL    TObject_Mapc(LpXCONTEXT gCP, LpTHREAD gTP,TVAL tval, TVAL proc)
{
NUM         index;
NUM         maxIndex;
StartFrame
DeclareOBJ(TObject,self);
DeclareTVAL(ivTval);
DeclareTVAL(tmpIndex);
EndFrame

if ((*_TObject_TypeMapc(asTag(&tval))) != TObject_Mapc)
    return((*_TObject_TypeMapc(asTag(&tval)))( gCP, gTP, tval, proc));

// Set maxIndex to same size as input object
*tmpIndex = FObject_GetMaxIndex(gCP, gTP, tval);
maxIndex = tmpIndex->u.Int;
tmpIndex->Tag = TYNUM;

for(index=0; index < maxIndex; ++index)
{	// Get next element from input, process it.
	tmpIndex->u.Int = index;
	*ivTval = (*_TObject_TypeGetIV1(tval.Tag))(gCP,gTP, tval, *tmpIndex);
	if(isERROR(ivTval))
		FrameExit(*ivTval);
	*ivTval = FSmartbase_Evalv(gCP,gTP, proc, 1, ivTval);
	if(isERROR(ivTval))
		FrameExit(*ivTval);
}
FrameExit(*ivTval);
}

/*--------------------------------------------------------------------------------------- */

#if 0
ComputeSize

The oodbms is trying to compute the size of Handle required to store all of your data.
Add your size requirements to the input size argument.

#endif

void    TObject_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,long* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */

#if 0
GetClassType

Return the type of the class to which the object belongs.

#endif

TYPE TObject_GetClassType(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TObject*    self = asObject(&selfTval);
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsObjectType);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Map

Make a copy of me and apply the given procedure on each of my members.
Return my copy whose members are the result of each application.

#endif

TVAL    TObject_Map(LpXCONTEXT gCP,LpTHREAD gTP,TVAL tval, TVAL proc)
{
NUM         maxIndex;
NUM         index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmpIndex);
DeclareTVAL(tmp);
DeclareTVAL(ivTval);
EndFrame

if ((*_TObject_TypeMap(asTag(&tval))) != TObject_Map)
    return((*_TObject_TypeMap(asTag(&tval)))(gCP,gTP,tval,proc));

// Make a copy of myself since map returns an object of the same type as me.
*ret = FObject_Copy(gCP, gTP, tval);
// Set maxIndex to same size as input object
*tmpIndex = FObject_GetMaxIndex(gCP, gTP, tval);
maxIndex = tmpIndex->u.Int;
tmpIndex->Tag = TYNUM;

for(index=0; index < maxIndex; ++index)
{	// Get next element from input, process it, set corresponding output value.
	tmpIndex->u.Int = index;
	*ivTval = (*_TObject_TypeGetIV1(tval.Tag))(gCP,gTP, tval, *tmpIndex);
	if(isERROR(ivTval))
		FrameExit(*ivTval);
	*ivTval = FSmartbase_Evalv(gCP,gTP, proc, 1, ivTval);
	if(isERROR(ivTval))
		FrameExit(*ivTval);
	*tmp = (*_TObject_TypeSetIV1(ret->Tag))(gCP,gTP, *ret,
		*tmpIndex, *ivTval);
	if (isERROR(tmp))
		FrameExit(*tmp);
}
FrameExit(*ret);
}

TVAL TObject_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL srcTval, TVAL index1)
{
LpF2TVALS       aFunction;

aFunction = (LpF2TVALS)_TObject_TypeGetIV1(asTag(&srcTval));

return((*aFunction)(gCP,gTP,srcTval,index1));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_GetIV2

#endif

TVAL TObject_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL srcTval, TVAL index1,TVAL index2)
{
LpF3TVALS       aFunction;

aFunction = (LpF3TVALS)_TObject_TypeGetIV2(asTag(&srcTval));

return((*aFunction)(gCP,gTP,srcTval,index1,index2));
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_GetIV3

#endif

TVAL TObject_GetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL srcTval, TVAL index1, TVAL index2, TVAL index3)
{
LpF4TVALS       aFunction;

aFunction = (LpF4TVALS)_TObject_TypeGetIV3(asTag(&srcTval));

return((*aFunction)(gCP,gTP,srcTval, index1,index2,index3));
}



/*--------------------------------------------------------------------------------------- */
#if 0
TObject_MapObject

Verify the data type and call the appropriate method.

#endif

TVAL TObject_MapObject(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc)
{
if (_TObject_TypeFlag(asTag(&self)) == _TObject_TfTOBJECT)
    {
    return FObject_Map(gCP,gTP,self, proc);
    }
else
    {
    return(gCP->TObject_ERROR_INVALID);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_MapcObject

Verify the data type and call the appropriate method.

#endif

TVAL TObject_MapcObject(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc)
{
if (_TObject_TypeFlag(asTag(&self)) == _TObject_TfTOBJECT)
    {
    return(FObject_Mapc(gCP,gTP,self, proc));
    }
else
    {
    return(gCP->TObject_ERROR_INVALID);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
TObject_New

Create a new TObject.

#endif

TObject*    TObject_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TObject,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TObject_Initialized) TObject_Init(gCP,gTP);

self = (TObject*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYOBJ;
FrameExit(self);
}


/*--------------------------------------------------------------------------------------- */
#if 0
strnlen

Return the null delimited length of a K&R C string up to the maximim length specified. If
the maximum length is exceeded, return the maximum length minus one AND place a null
character at the maximum length location.

#endif

#if defined(_GCC) && defined(_WIN)
NUM   strnlen(LpCHAR aString,NUM aMaxLength)
{
NUM		indexOf;

/*  If the maximum length is zero or greater than zero, then return zero. */

if (aMaxLength <= 0) return(0);

/*  Search for a null character anywhere within the maximum length of the string. */

for (indexOf = 0; indexOf < aMaxLength; ++indexOf)
	{
	if (aString[indexOf] == 0) return(indexOf);
	}

/*  We did not find a null character, so we will insert a null character. */

aString[aMaxLength-1] = 0;

return(aMaxLength - 1);
}
#endif
