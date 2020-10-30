/**********************************************************************************
	The Smartbase High Speed Analytic Database Engine 
	
	The Smartbase Engine is the core data analysis facility of the 
	Analytic Information Server. Smartbase contains the context memory
	manager, the core virtual machine, the Lisp compiler, the runtime environment,
	the Object Repository manager, the basic file I/O Inteface handler, 
	and the MySQL Interface handler.

***********************************************************************************/
/**********************************************************************************
    Copyright (C) 2013 Analytic Research Foundation.

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
FSmtbase.h

Declarations  of the top level API procedures required for the Smartbase
High Speed Analytic Database Engine.

AUTHORS:            Michael F. Korns

REFERENCES:

    [1]     "THINK C (5.0) Object Oriented Programming Manual", Philip Borenstein
            & Jeff Mattson, Symantec Corporation, Cupertino California, 1991.

    [2]     "THINK C (5.0) Standard Libraries Reference", Philip Borenstein
            & Jeff Mattson, Symantec Corporation, Cupertino California, 1991.

    [3]     "THINK C (5.0) User`s Manual", Philip Borenstein & Jeff Mattson,
            Symantec Corporation, Cupertino California, 1991.

    [4]     "Structure and Interpretation of Computer Programs", Harold Abelson
            Gerald Sussman & Julie Sussman, McGraw-Hill Book Company, New York,
            1985.

    [5]     "Revised(3) Report on the Algorithmic Language Scheme", Harold Abelson
            et. al., MIT AI Laboratory, AI Memo 848a, September 1986.

    [6]     "Common Lisp: The Reference", Franz Inc., Addison-Wesley Publishing
            Company, Menlo Park California, 1988.

    [7]     "Inside Macintosh Vols I - V", Apple Inc., Addison-Wesley Publishing
            Company, Menlo Park California, 1985.

    [8]     "Techniques Of Artificial Intelligence", Stuart C. Shapiro, D Van Nostrand
            Company, New York, 1979.

    [9]     "Lisp", Patrick Winston & Berthold Horn, Addison-Wesley Publishing Company,
            Menlo Park California, 1981.

    [10]    "Common Lisp: A Tutorial", Wendy L. Milner, Prentice Hall, Englewood Cliffs
            New Jersey, 1988.

    [11]    "Natural Language Processing in Lisp", Gerald Gazdar & Chris Mellish,
            Addison-Wesley Publishing Company, Menlo Park California, 1989.


CHANGE HISTORY
Version	 Date		Who		Change
6.000    04/30/2013 mfk     Added a large number of unsigned integer instructions.
5.005    11/16/2008 rca     Added vmregRefWord,vmregRefXWord,vmregSetWord,vmregSetXWord instructions.
5.0000   8/20/2008  fchua   MySQL Support.
5.0000   8/20/2008  fchua   New object types (TBrickRow, TBrickField, TStringSubstring).
5.0000   8/20/2008  fchua   New immediate types (TBrickRowT, TBrickFieldT, TStringSubstringT).
5.000    8/20/2008  mfk     Updated TLambda, TDatabase structure, Object Disk Header definitions.
3.0000	 6/26/2007	mfk		Beta Release New Class Lisp language features for 32bit architectures
2.0001	 1/3/2007	tlw		Add MAXINT, MININT, MAXLONG, MINLONG defines.
1.0101	 8/03/2006	tlw		Define BIGENDIAN, bit-sizes for AMD64
		 12/9/2006  TM		Added FSMARTBASE_ERR_BADBUFFERSIZE

Compiler/Build switches -- please set these in the makefile (or IDE), dont set them here as it makes
cross platform version control difficult. You must set one of each to be assured of predictable behavior

OS selection Switches
_MACOS, _SUN, _LINUX, _WIN

Compiler/Toolchain Switches
_MSVC, _GCC

CPU Selection Switches
_PENTIUM, _AMD64, _UNKNOWN32

AIS Configuration Switches
_JIT		// Include Just In Time Compiler code

Typical Configurations By Target Environment
WindowsXP with MSVC
_WIN _MSVC _PENTIUM _JIT	// Windows XP Pro 32 with MS Visual Studio 2003
_WIN _MSVC _AMD64 _JIT		// Windows XP Pro 64 with MS Visual Studio 2005

_LINUX _GCC _AMD64       	// SUSE Linux 10.1 with GCC, no JIT
_LINUX _GCC _AMD64 _JIT  	// SUSE Linux 10.1 with GCC, with JIT

#endif

/*  Make sure that we skip this include file if it has already been seen. */
/*  Also, make sure that all common variables are declared "extern" if */
/*  the common keyword has not explicitly been declared. */

#ifndef _H_FSmartbase
#define _H_FSmartbase

#ifdef _LINUX
#define __int32 long
#endif

/*  SmartBase Engine Extended Configuration Options. */
/*  Notes:  A value of 1 indicates the specified extended */
/*          configuration be included in this build.  */

#define __EXNEURALNET       1       /* NeuralNet Lambda Support */
#define __EXFINANCE         1       /* Financial Functions Support */
#define __EXSIMULATE        1       /* Simulation Support */
#define __EXLEXICON         1       /* Lexicon Text Retrieval Support */
#define __EXMYSQL	        1       /* MySQL Database Support */

/*  SmartBase Disk File Version Id. */
/*  Notes:  First digit is major release Number. */
/*          Second is revision number. */
/*          The Disk File Version is not meant */
/*          to match the Smartbase version and */
/*          release numbers because not every */
/*          release changes the disk signature.  */

// NOTE: THIS FOR _BASEVERSION SHOULD ALWAYS BE DIVISIBLE BY 4 AND MUST BE BETWEEN 000 AND 400!
#define _BASEVERSION 60

#ifdef _LINUX

#ifdef _M64
// NOTE: THIS VALUE IS FOR LINUX 64-bit and SHOULD ALWAYS BE EQUAL TO THE _BASEVERSION
#define _VERSION     _BASEVERSION + 0
#else
// NOTE: THIS VALUE IS FOR LINUX 32-bit and SHOULD ALWAYS BE +1 OF THE _BASEVERSION
#define _VERSION     _BASEVERSION + 1
#endif
#else
#ifdef _M64
// NOTE: THIS VALUE IS FOR WINDOWS 64-bit and SHOULD ALWAYS BE +2 OF THE _BASEVERSION
#define _VERSION     _BASEVERSION + 2
#else
// NOTE: THIS VALUE IS FOR WINDOWS 32-bit and SHOULD ALWAYS BE +3 OF THE _BASEVERSION
#define _VERSION     _BASEVERSION + 3
#endif
#endif


/*  SmartBase Engine Version ID. */
/*  Notes:  Returned by the (version) function. (Beta Release New Class Lisp language features for 32bit architectures)  */
#ifdef _M64
#define CURRENT_VERSION		"6.0000-64bit"
#else
#define CURRENT_VERSION		"6.0000-32bit"
#endif


/*  Host system external symbol definition (How API symbols are exported). */
#ifndef PUBLIC
#ifdef _MSVC
#define PUBLIC              extern _declspec(dllexport)
#else
#define PUBLIC              extern
#endif
#endif


/*  Declare the bitsize of the register data types */
#ifdef _M32
#define BITSIZEOFFLOAT		2
#define BITSIZEOFNUM32		2
#define BITSIZEOFNUM		2
#define BITSIZEOFREAL		3
#define BITSIZEOFAISWORD	4
#define BITSIZEOFSHORT		1
#define _FSmartbase_HeapBlockSize	128
#elif defined _M64
#define BITSIZEOFFLOAT		2
#define BITSIZEOFNUM32		2
#define BITSIZEOFNUM		3
#define BITSIZEOFREAL		3
#define BITSIZEOFAISWORD	4
#define BITSIZEOFSHORT		1
#define _FSmartbase_HeapBlockSize	256
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif

/*  Declare the arithmetic overflow macros for each machine. */
/*  Note:   These macros must reflect the machine language */
/*          check for the host machine. The default is not to */
/*          check for arithmetic overflow. */
#define JumpNoOverflow(label)		goto label;
#define JumpOnOverflow(label)
#ifdef _MSVC
#define JumpOverflowSet 1
#ifdef _M32
#undef JumpNoOverflow
#define JumpNoOverflow(label)		_asm { jno label }
#undef JumpOnOverflow
#define JumpOnOverflow(label)		_asm { jo label }
#elif defined _M64
#define JumpNoOverflow(label)		goto label;
#define JumpOnOverflow(label)
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif
#endif


/* Declare some derived types */
#undef BIGENDIAN		// So far, we support no big endian platforms.

/*  Host system pragma declarations (How Needless warnings are turned off). */
#ifdef _MSVC
#pragma warning(disable: 4001)	// nonstandard extension 'single line comment' was used
#pragma warning(disable: 4100)	// unreferenced formal parameter
#pragma warning(disable: 4101)	// unreferenced local variable
#pragma warning(disable: 4201)	// nameless unions are part of C++
#pragma warning(disable: 4209)	// nonstandard extension used : benign typedef redefinition
#pragma warning(disable: 4214)	// long double is the same precision as double
#pragma warning(disable: 4699)	// Creating new precompiled header WinDebug/SMTBASE.pch
#endif

/*  To avoid prepending the current Global path to a file spec. that is already an absolute
	path, uncomment the following definition. Otherwise, smtbase works as it always has. */
#define _DETECTABSPATH 1

/*  Include these header files for ANSI Standard C on most platforms. */
/*  Set compiler on ANSII settings, but turn off check pointer types */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#ifndef __cplusplus
	#include <math.h>
#endif
#include <float.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <time.h>

#ifdef MEMTEST
#include <assert.h>	/* TM -- added to help check new FMemory.c */
// #include <crtdbg.h>
#endif

/*  MAXIMUM NUMBER OF ARGUMENTS */
/* */
/*  FSmartbase supports function calls between C programs and Lambdas. */
/*  This defines the maximum number of arguments in a function call. */

#define __MAXPARMS			50		/* Maximum number of arguments in function calls */


/*  COMPILER ALLIGNMENT FACTOR */
/* */
/*  FSmartbase supports automatic calculation of the C compiler allignment */
/*  factor for the current compilation. Each host computer may require   */
/*  allignment of structures to one, two, four, or eight byte boundries. */

typedef struct {char x;double y;} allignment;
#define ALLIGNMENTFACTOR  (sizeof(allignment) - sizeof(double))
#define ALLIGNME(isize)   isize = (((isize + ALLIGNMENTFACTOR - 1) / ALLIGNMENTFACTOR) * ALLIGNMENTFACTOR)

/*  When _FVMScript_ESCAPECHECK may be defined as an integer counter indicative of the number of */
/*  instructions which may execute between calling the host to check for an escape (user-abort) */
/*  for the current operation. */
/*  Note: On the Macintosh the new escape code is (command-period). */
#define _FSMARTBASE_ESCAPECHECK  1000

/*  CATCH & THROW ERROR RETURN CODE MACROS */
/* */
/*  FSmartbase supports catch & throw macros. These macros allow resumption */
/*  of execution from a standard reference point after an error condition. */
/* */
/*  Note:   Do not return status codes of 0 or 1 with the THROW macro!!!  */
/*          While our product uses these codes, ANSII C does not */
/*          support them with CATCH and THROW. */

#define FSMARTBASE_ERR_OUT_OF_MEMORY					1001
#define FSMARTBASE_ERR_FRAME_ERROR						1002
#define FSMARTBASE_ERR_INVALID							1003
#define FSMARTBASE_ERR_STACK							1004
#define FSMARTBASE_ERR_ESCAPE							1005
#define FSMARTBASE_ERR_PCODE							1006
#define FSMARTBASE_ERR_BAD_DATATYPE						1007
#define FSMARTBASE_ERR_RECURSION						1008
#define FSMARTBASE_ERR_FRAME_RELEASE					1009
#define FSMARTBASE_ERR_STACK_RELEASE					1010
#define FSMARTBASE_ERR_RECURSION_RELEASE				1011
#define FSMARTBASE_ERR_QUIT								1012
#define FSMARTBASE_ERR_WRONG_VERSION					1013
#define FSMARTBASE_ERR_ENGINE_BUSY						1014
#define FSMARTBASE_ERR_REPOSITORY_GC					1015
#define FSMARTBASE_ERR_REPOSITORY_LOCKOUT				1016
#define FSMARTBASE_ERR_MEMORY_REQUEST_TOO_LARGE			1017
#define FSMARTBASE_ERR_OUT_OF_MEMORY_FRAGMENTED			1018
#define FSMARTBASE_ERR_TOO_MANY_OBJECTS					1019
#define FSMARTBASE_REQUEST_SUSPEND						1020
#define FSMARTBASE_ERR_BADBUFFERSIZE					1021


/*  Maximum size (in bytes) of all temporary variables in any C function. */
/*  Note: The Smartbase engine is an Lambda oriented repository written in */
/*		  portable C code. Because this is a repository engine managing */
/*		  large volumes of data, any request for a large context workspace, */
/*        will also cause the engine to request a C stack that is quite large, */
/*		  to support a maximum possible number of Lisp and C function recursions */
/*        that is also quite large. This define states the maximum size (in bytes) */
/*		  allowed for temporary variables in any single C function so that the */
/*        maximum allowed recursions will not exceed the size of the requested C stack. */

#define		_FSmartbase_C_Temporaries		2000

/* Maximum number of OS provide memory allocation blocks for a context */
#define		FSMARTBASE_MAXCONTEXTMEMORYBLOCKS	20
#define		FSMARTBASE_MINBLOCKSIZE				10000000
#define		FSMARTBASE_BLOCKDECREMENT			10000000

/*  PORTABLE TYPES */
/* */
/*  FSmartbase supports a number of portable types for Lisp and C. */
/* */
/*  Note: When porting or changing the size of NUM, LONG, REAL, or TOKEN  */
/*        make sure to also change the necessary code in the FSmartbase  */
/*        subsystems. */

#define NIL             0
typedef char*           CPTR,   * PCPTR;
typedef CPTR*           HDL,    * PHDL;
#define HNIL            (HDL)0L
#define PNIL            (CPTR)0L
#define LpNIL           (char *)0L
#define LhNIL           (char **)0L
#define VOID            void
typedef void                    * LpVOID;           /*  Really means void: a nothing indicator */
typedef char			BOLE,   * LpBOLE;
typedef short           COMPARE,* LpCOMPARE;
typedef short           ERR,    * LpERR;
typedef char            CHAR,   * LpCHAR;
typedef unsigned char   UCHAR,  * LpUCHAR;
typedef unsigned short  UINT16, * LpUINT16;
typedef unsigned int    UINT32, * LpUINT32;
typedef short			INT16,  * LpINT16;
typedef int				INT32,  * LpINT32;
#ifdef _M64
#ifdef _GCC
typedef unsigned long   UNUM,   * LpUNUM;
typedef int             NUM32,  * LpNUM32;
typedef long            NUM,    * LpNUM;
typedef long            OBJ,    * LpOBJ;            /*  Really refers to any TObject* or descendent */
typedef unsigned long	ULONG;
typedef long			LONG;
#elif defined _MSVC
typedef unsigned __int64 UNUM,  * LpUNUM;
typedef __int32	        NUM32,  * LpNUM32;
typedef __int64         NUM,    * LpNUM;
typedef long			LONG;
typedef unsigned long	ULONG;
typedef __int64			OBJ,    * LpOBJ;            /*  Really refers to any TObject* or descendent */
#else
#error "The toolset for this build is not defined in the preprocessor defines for this configuration"
#endif
#define MAXNUM			 9223372036854775807L
#define MINNUM			-9223372036854775808L
#define _MAXNUM			 9223372036854775807.0
#define _MINNUM			-9223372036854775808.0
#elif defined _M32
typedef unsigned long   UNUM,   * LpUNUM;
typedef long            NUM32,  * LpNUM32;
typedef long            NUM,    * LpNUM;
typedef unsigned __int32 ULONG;
typedef __int32			 LONG;
typedef long            OBJ,    * LpOBJ;            /*  Really refers to any TObject* or descendent */
#define MAXNUM			 2147483647
#define MINNUM			-2147483648
#define _MAXNUM			 2147483647.0
#define _MINNUM			-2147483648.0
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif
typedef short			FLAG;
typedef char*           POINTER;
typedef POINTER         * LpPOINTER;
typedef long*           PNTR;
typedef PNTR            * LpPNTR;
typedef float           FLOAT,  * LpFLOAT;
typedef double          REAL,   * LpREAL;
typedef short           SHORT,  * LpSHORT;
typedef char			TEXT,   * LpTEXT;
typedef short           TYPE,   * LpTYPE;
typedef char            NAME[20];
typedef char            STREAM[1];					/*  Type delimited variable length data stream */
typedef STREAM			* LpSTREAM;
typedef NAME            * LpNAME;
#ifdef _M64
#ifdef _MSVC
#define INTFORMAT       "%I64d"
#define UINTFORMAT      "%I64u"
#define POINTERFORMAT   "0x%p"
#elif defined _GCC
#define INTFORMAT       "%ld"
#define UINTFORMAT      "%lu"
#define POINTERFORMAT   "%p"
#else
#error "The toolset for this build is not defined in the preprocessor defines for this configuration"
#endif
#elif defined _M32
#define INTFORMAT       "%ld"
#define UINTFORMAT      "%lu"
#define POINTERFORMAT   "0x%p"
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif

#define REALFORMAT      "%.12e"
#define SHORTFORMAT     "%d"
#define HEXFORMAT       "%lx"
#define MAXINT			 2147483647
#define MININT			-2147483648
#ifdef _M64
#define MAXLONG			 9223372036854775807L
#define MINLONG			-9223372036854775808L
#define _MAXLONG		 9223372036854775807.0
#define _MINLONG		-9223372036854775808.0
#elif defined _M32
#define MINLONG			0x80000000
#define MAXLONG			0x7fffffff
#define _MAXLONG		 2147483647.0
#define _MINLONG		-2147483648.0
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif
#define _MAXREAL        DBL_MAX
#define _MINREAL        DBL_MIN
#define _MAXDIMENSIONS  3

#ifndef LONG_MAX
#define LONG_MAX MAXNUM
#endif

#define NAN_32    0x7FC00000
#define NAN_64_HI 0x7FF80000
#define NAN_64_LO 0x00000000
#define INF_64_HI 0x7FF00000
#define INF_64_LO 0x00000000

#define REALCHECK(r)    ((_MAXREAL < (r)) ? _MAXREAL : ((-_MAXREAL > (r)) ? -_MAXREAL : (((-_MINREAL < (r)) && (_MINREAL > (r))) ? 0.0 : (r))))
#define INTEGER(r)		((_MAXLONG < (r)) ? (MAXLONG) : ((_MINLONG > (r)) ? (MINLONG) : ((NUM)(r))))

/*  FSMARTBASE ROOT CLASS DISK LAYOUT */
/*  FSmartbase supports a root class known as TObject. */
/*  The TObject root class forms the basis for all other   */
/*  FSmartbase class structures. These definitions include */
/*  both the memory and the disk root class structure layout. */

typedef struct
    {
    LONG                itsVariableLength;  /*  Inclusive disk size */
    TYPE                itsObjectType;      /*  Dependent on init order! */
    LONG                itsObjectData[1];
    }TObjectOnDisk;
#define TObjectOnDiskPtr(h,n)   ((TObjectOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TObjectOnDisk    ((NUM)&((TObjectOnDisk*)0)->itsObjectData)


// DEBUG
#define _FSMTBASE_LOG(msg) {FILE *apFile;if ((apFile = fopen("SysLog.txt", "a"))!= NULL){fwrite(msg,1,strlen(msg),apFile);fclose(apFile);}}

/*  Define the memory handle structure used by the memory manager */

#define		_MEMMAXFRAMES           500L
// TLW extend the marker in mh to an unsigned.  Since pointers are on 4 or 8 byte boundaries, no more space is consumed.
#define		_MARKERVALUE			(unsigned)0x89abcdef

typedef struct mh
    {
        POINTER         fwd;            /*  Pointer to next free handle (free), to data (busy)	*/
#if MEMTEST
        unsigned	marker;				/*  Marker used in test for overwrite */
        unsigned	marker2;			/*  Second marker used in test for overwrite */
#endif
        struct mh*      back;           /*  Pointer to previous free frame handle (free)		*/
        struct mh*      next;           /*  Pointer to next allocated block						*/
        struct mh*      prev;           /*  Pointer to previously allocated block				*/
        LONG            userSize;       /*  Buffer size as last requested by user (<= true size)*/
        SHORT	        Frame;          /*  Frame Index For Handle								*/
        BOLE            Free;           /*  Free Switch (true iff block is free)				*/
        BOLE            Fixed;          /*  Fixed frame Switch (true iff block is fixed)		*/
        LONG            data[1];        /*  Data Block											*/
     }   MHANDLEINFO, *LpMHANDLEINFO;
/* The following macro returns the size of the header up to but */
/* not including any portion of the data[] element */
/* Note: Please keep this total size to an even multiple of eight bytes. */
#define SIZEOF_MHANDLEINFO  ((NUM)&((MHANDLEINFO*)0)->data)


/* CLASS CONSTANT DECLARATIONS */

/* The TObject CLASS supports several CLASS constants for use */
/* in marking, locking, and pinning the object list entries.  */

#define _TObject_OfVOID             0
#define _TObject_OfOBJECT           1
#define _TObject_OfMARK             2
#define _TObject_OfLOCK             4
#define _TObject_OfPERM             8
#define _TObject_OfIGNORE           16

#define _TObject_TfNATIVE           0
#define _TObject_TfTOBJECT          1
#define _TObject_TfCOBJECT          2
#define _TObject_TfIOBJECT          3

#define _TObject_NILSavedObject     -1


/*  LINEBREAKS */
/* */
/*  FSmartbase supports a a portable test for ascii line breaks. */
/* */
/*  Note: When porting between MSDOS, Unix, Windows, Macintosh, etc.  */
/*        one must make sure to check for either a line feed (10) or  */
/*        a carriage return (13). */

#define LINEBREAK       10
#define RETURNCHR       13
#define ISLINEBREAK(c)  (((c) == 10) || ((c) == 13))

/*  BOLE VALUES */
/* */
/*  SmartBase contains its own values for the BOLE data type. The values for  */
/*  BOLE include not only true and false, but also comparison and error */
/*  status values. These values are declared herein to maximize portability. */

#ifndef TRUE
#define TRUE            1
#endif
#ifndef FALSE
#define FALSE           0
#endif
#define YES             1
#define NO              0
#define ON              1
#define OK              1
#define OFF             0
#define HIGH            1
#define EQUAL           0
#define LOW             -1

/*  *********************************************************************   
    AISWORD DATA TYPE                                                       
 																		    
 	The Smartbase High Speed Analytic Database Engine contains the context
	memory manager designed to handle gigbytes of context memory during
	analytic procession tasks. Smartbase subdivides all context memory
	into an atomic word data type which is the union of a number of    
    native data types. The format of the word data type is defined below.   
    Note: The word data type is known as a AISWORD because WORD has too     
          many C language macro collisions with Microsoft Windows, and      
          other legacy systems. The old term TVAL stands for Typed VALue    
          and the old term TVAL is deprecated in version 6.0 and later.     
    *********************************************************************  */

#define MAXREGISTERLEN		16
#define MAXREGISTERCNT		50
#define MAXAISWORDTEXTLEN	10
typedef struct {union {NUM						Bool;
                       NUM						Char;
                       COMPARE					Compare;
                       NUM32					Index[2];		/* The Modifier and Offset can be overloaded to become a third index (see asTail)	*/
                       NUM						Int;
                       UNUM                     UInt;
                       NUM32					Long;
                       FLAG						Flag[4];
                       OBJ						Obj;
					   POINTER					Pointer;
                       REAL						Real;
                       SHORT					Short;
                       CHAR						Text[8];		/* Text may extend into the TextOverflow and QuoteCnt areas (see MAXAISWORDTEXTLEN)	*/
                       TYPE						Type;
                       REAL						Void;
                       struct TLambda*			Lambda;
                       struct TBitVector*		BitVector;
                       struct TByteVector*		ByteVector;
                       struct TContinuation*	Continuation;
					   struct TCpx*				Complex;
					   struct TCpxVector*		CpxVector;
                       struct TDatabase*		Repository;
                       struct TDictionary*		Dictionary;
                       struct TDirectory*		Directory;
                       struct TError*			Error;
                       struct TFltVector*		FltVector;
                       struct TIntVector*		IntVector;
                       struct TLongVector*		LongVector;
                       struct TMatrix*			Matrix;
                       struct TNumMatrix*		NumMatrix;
                       struct TNumVector*		NumVector;
                       struct TObject*			Object;
                       struct TObjVector*		ObjVector;
                       struct TPair*			Pair;
                       struct TPcodeVector*		PcodeVector;
                       struct TBrick*			Brick;
                       struct TBrickRow*        BrickRow;
                       struct TBrickField*      BrickField;
                       struct TShtVector*		ShortVector;
                       struct TString*			String;
                       struct TStructure*		Structure;
                       struct TSymbol*			Symbol;
                       struct TVector*			Vector;
                       struct TWorkspace*		Workspace;
                      } u;
                unsigned char			TextOverflow;			/* Text may extend into the TextOverflow bytes (see MAXAISWORDTEXTLEN) */
                unsigned char			QuoteCnt;				/* A word may not be evaluated until its quote count is reduced to zero (Note: text may also extend into the QuoteCnt field making the QuoteCnt zero). */
                unsigned short			Modifier;				/* The Modifier and Offset can be overloaded to become the Tail (see asTail) */
                unsigned short			Offset;					/* The Modifier and Offset can be overloaded to become the Tail (see asTail) */
                signed char				DeclaredType;			/* This word was declared with the specified type. */
                signed char				Tag;					/* This word currently contains a value of the specified type. */
                } TVAL, *LpTVAL, AISWORD, *LpWORD;

#define     asBool(a)			(((LpWORD)(a))->u.Bool)
#define     asChar(a)			(((LpWORD)(a))->u.Char)
#define     asCompare(a)		(((LpWORD)(a))->u.Compare)
#define     asData(a)			(((LpWORD)(a))->u)
#define     asQuoteCnt(a)		(((LpWORD)(a))->QuoteCnt)
#define     asGoto(a)			(((LpWORD)(a))->u.Field[2])
#define     asInt(a)			(((LpWORD)(a))->u.Int)
#define     asUInt(a)			(((LpWORD)(a))->u.UInt)
#define     asLong(a)			(((LpWORD)(a))->u.Long)
#define     asError(a)			(((LpWORD)(a))->u.Error)
#define     asObj(a)			(((LpWORD)(a))->u.Obj)
#define     asObject(a)			(((LpWORD)(a))->u.Object)
#define     asPointer(a)		(((LpWORD)(a))->u.Pointer)
#define     asReal(a)			(((LpWORD)(a))->u.Real)
#define     asShort(a)			(((LpWORD)(a))->u.Short)
#define     asText(a)			(((LpWORD)(a))->u.Text)
#define     asType(a)			(((LpWORD)(a))->u.Type)
#define     asFunction(a)		((LpFUNC)((LpWORD)(a))->u.Pointer)
#define     asEvaluator(a)		((LpVMEVALUATOR)((LpWORD)(a))->u.Pointer)
#define     asLambda(a)			(((LpWORD)(a))->u.Lambda)
#define     asProcedure(a)		(((LpWORD)(a))->u.Lambda)
#define		Lambda(aisword)		((aisword).u.Lambda)
#define		Procedure(aisword)	((aisword).u.Lambda)

#define     asTag(a)			(((LpWORD)(a))->Tag)
#define     asTail(a)			(*((LpINT32)(&((LpWORD)(a))->Modifier)))
#define     asMemo(a)			(*((LpINT32)(&((LpWORD)(a))->Modifier)))
#define		asOffset(a)			(((LpWORD)(a))->Offset)
#define		asModifier(a)		(((LpWORD)(a))->Modifier)
#define     asDeclaredType(a)	(((LpWORD)(a))->DeclaredType)
#define     asQuoteCnt(a)		(((LpWORD)(a))->QuoteCnt)
#define     asQuoteTag(a)		(*((LpUINT16)((&((LpWORD)(a))->Tag)-1)))

#define     asObjIdx(a)			(((LpWORD)(a))->u.Index[0])
#define     asRowIdx(a)			(((LpWORD)(a))->u.Index[1])
#define     asFldIdx(a)			(*((LpINT32)(&((LpWORD)(a))->Modifier)))

#define     ObjIdx(aisword)		((aisword).u.Index[0])
#define     RowIdx(aisword)		((aisword).u.Index[1])
#define     FldIdx(aisword)		(*((LpINT32)(&(aisword).Modifier)))

#define     asSubOff(a)			(((LpWORD)(a))->u.Index[1])
#define     asSubLen(a)			(*((LpINT32)(&((LpWORD)(a))->Modifier)))
#define     SubOff(aisword)		((aisword).u.Index[1])
#define     SubLen(aisword)		(*((LpINT32)(&(aisword).Modifier)))

#define     asNumIndex(a)		((a)->Tag==TYREAL?(a)->u.Real:(a)->Tag==TYNUM?(a)->u.Int:(a)->Tag==TYUNUM?(a)->u.UInt:(a)->Tag==TYSHORT?(a)->u.Short:(a)->Tag==TYCHAR?((unsigned char)(a)->u.Char):(a)->Tag==TYMONEY?(a)->u.Real:(a)->Tag==TYDATE?(a)->u.Real:(a)->Tag==TYCPX?(a)->u.Complex->itsReal:0)
#define     isNumIndex(a)		((asTag(a)==TYREAL) || (asTag(a)==TYNUM) || (asTag(a)==TYUNUM) || (asTag(a)==TYSHORT) || (asTag(a)==TYCHAR) || (asTag(a)==TYCPX))

#define     asRealIndex(a)		((a)->Tag==TYFRAME?(a)->u.Real:(a)->Tag==TYREAL?(a)->u.Real:0.0)
#define     isRealIndex(a)		((asTag(a)==TYFRAME) || (asTag(a)==TYREAL))

#define     isNullTval(a)		(((LpWORD)(a))->Tag == TYVOID)
#define		isObject(a)			(_TObject_TypeFlag((a)->Tag) == _TObject_TfTOBJECT)
#define     isCompareEQ(a)		((asTag(a) == TYCOMPARE) && (asCompare(a) == EQUAL))
#define     isCompareNE(a)		((asTag(a) == TYCOMPARE) && (asCompare(a) != EQUAL))
#define     isCompareLT(a)		((asTag(a) == TYCOMPARE) && (asCompare(a) == LOW))
#define     isCompareLE(a)		((asTag(a) == TYCOMPARE) && (asCompare(a) < HIGH))
#define     isCompareGT(a)		((asTag(a) == TYCOMPARE) && (asCompare(a) == HIGH))
#define     isCompareGE(a)		((asTag(a) == TYCOMPARE) && (asCompare(a) > LOW))

#define     isTRUE(a)			((asTag(a) == TYBOLE) && (asBool(a) == TRUE))
#define     isFALSE(a)			((asTag(a) == TYBOLE) && (asBool(a) == FALSE))

#define     isERROR(a)			(asTag(a) == TYERROR)

#define     inRange(v,s,e)		(((v) >= (s)) &&((v) < (e)))

#ifndef _MIN
	#define     _MIN(a,b)       ((a < b) ? a : b)
#endif

#ifndef _MAX
	#define     _MAX(a,b)       ((a > b) ? a : b)
#endif

/*  MAXIMUM VM INSTRUCTIONS DECLARATION */
/*  This key macro is ALSO defined in "tpcodvec.h".	*/
/*  WARNING: Keep these two definitions identical	*/
#define     _VMMAXINSTRS    297	/* WARNING!!! Must be set equal to value of _VMMAXINSTRS in tpcodevec.h!!! */

/*  MEMORY BLOCK DECLARATIONS */
/*   */
/*  The data structures, macros, and coding conventions used in  */
/*  this source file are designed to isolate and protect the  */
/*  Smartbase High Speed Engine from the low level differences */
/*  between the Macintosh, Windows 3.1, Windows NT, and Unix  */
/*  memory management structures.  */

typedef struct
    {
    AISWORD				Value;              /*  Binding value   (result) */
    struct TObject*		Key;                /*  Binding key     (index) */
    } BIND, *LpBIND;

typedef struct
    {
    AISWORD				Value;              /*  Binding value   (result) */
    AISWORD				Key;                /*  Binding key     (index) */
    } PBIND, *LpPBIND;

typedef struct  {
                CHAR        Char[1];
                } FMemory, **HMemory;
#define _HMemoryPtr(h)      (*(char **)(h))
#define atHMemory(h,n)      ((*(HMemory)(h))->Char[(n)])

typedef struct  {
                CHAR        Char[1];
                } FMChar, **HMChar;
#define atHMChar(h,n)       ((*(HMChar)(h))->Char[(n)])

typedef struct  {
                BIND        Bind[1];
                } FMBind, **HMBind;
#define atHMBind(h,n)       ((*(HMBind)(h))->Bind[(n)])

typedef struct  {
                PBIND       PBind[1];
                } FMPBind, **HMPBind;
#define atHMPBind(h,n)      ((*(HMPBind)(h))->PBind[(n)])

typedef struct  {
                BOLE        Bool[1];
                } FMBool, **HMBool;
#define atHMBool(h,n)       ((*(HMBool)(h))->Bool[(n)])

typedef struct  {
                COMPARE     Compare[1];
                } FMCompare, **HMCompare;
#define atHMCompare(h,n)    ((*(HMCompare)(h))->Compare[(n)])

typedef struct  {
                POINTER      Handle[1];
                } FMHandle, **HMHandle;
#define atHMHandle(h,n)     ((*(HMHandle)(h))->Handle[(n)])

typedef struct  {
                NUM         Int[1];
                } FMInt, **HMInt;
#define atHMInt(h,n)        ((*(HMInt)(h))->Int[(n)])

typedef struct  {
                UNUM        UInt[1];
                } FMUInt, **HMUInt;
#define atHMUInt(h,n)        ((*(HMUInt)(h))->UInt[(n)])

typedef struct  {
                NUM32       Long[1];
                } FMLong, **HMLong;
#define atHMLong(h,n)       ((*(HMLong)(h))->Long[(n)])

typedef struct  {
                SHORT       Short[1];
                } FMShort, **HMShort;
#define atHMShort(h,n)      ((*(HMShort)(h))->Short[(n)])

typedef struct  {
                NAME        Name[1];
                } FMName, **HMName;
#define atHMName(h,n)       ((*(HMName)(h))->Name[(n)])

typedef struct  {
                OBJ         Obj[1];
                } FMObj, **HMObj;
#define atHMObj(h,n)        ((*(HMObj)(h))->Obj[(n)])

typedef struct  {
                struct TObject* Object[1];
                } FMObject, **HMObject;
#define atHMObject(h,n)     ((*(HMObject)(h))->Object[(n)])

typedef struct  {
                OBJ*        Objpntr[1];
                } FMObjpntr, **HMObjpntr;
#define atHMObjpntr(h,n)    ((*(HMObjpntr)(h))->Objpntr[(n)])

typedef struct  {
                POINTER     Pointer[1];
                } FMPointer, **HMPointer;
#define atHMPointer(h,n)    ((*(HMPointer)(h))->Pointer[(n)])

typedef struct  {
                REAL        Real[1];
                } FMReal, **HMReal;
#define atHMReal(h,n)       ((*(HMReal)(h))->Real[(n)])

typedef struct  {
                FLOAT       Float[1];
                } FMFloat, **HMFloat;
#define atHMFloat(h,n)      ((*(HMFloat)(h))->Float[(n)])

typedef struct  {
                TYPE        Type[1];
                } FMType,   **HMType;
#define atHMType(h,n)       ((*(HMType)(h))->Type[(n)])

typedef struct  {
                AISWORD     Tval[1];
                } FMTval,   **HMTval;
#define atHMTval(h,n)       ((*(HMTval)(h))->Tval[(n)])

#define	AISWORDARRAYITEMSIZE		sizeof(FMTval)
#define	BINDARRAYITEMSIZE			sizeof(FMBind)


/*  Define some fundamental SmartBase engine parametrics */

#define _FSmartbase_MAXIMPORTBUFFERLEN 2500000	/* Max size of import buffer */
#define _FSmartbase_MAXBUFFERLEN        512000	/* Max size of working buffer */
#define _FSmartbase_MAXARGUMENTS        40      /* Max number of arguments */

/*  Thread Structure  */
//  The following structure contains the variables necessary to perserve a thread

typedef struct {AISWORD*			TvalStack;									/* Smartbase operations stack */
				NUM					TvalStackIdx;								/* Operations stack current index */
				NUM					MaxTvalStackSize;							/* Maximum size of the operations stack */
				BOLE				Initialized;
				NUM					SessionID;									/* Caller's session identifier */
				NUM					RequestID;									/* Caller's request identifier */
				NUM					RecursionCount;								/* Current recursion count */
				NUM					MaxRecursions;								/* Maximum recursions allowed */
				NUM					RequiredHostStackSpace;						/* Space required for host stack */
				NUM					ObjStackIdx;								/* Protected object stack current index */
				NUM					MaxObjStackSize;							/* Maximum size of the protected object stack */
				OBJ**				ObjStack;									/* Smartbase protected object stack */
				struct TLambda*		DebugBreakProc;								/* Current breakpoint is set in this Lambda object */
				AISWORD				DebugBreakExp;								/* Current breakpoint is set with this until expression (or Void) */
				NUM					DebugBreakIP;								/* Current breakpoint is set at this instruction address (DRM pcode) */
				NUM					DebugBreakCount;							/* Current breakpoint is set with this counter value */
				NUM					DebugBreakOpcode;							/* Current breakpoint this instruction opcode (DRM pcode) */
				NUM					DebugFlags;
				BOLE				DebugJitOn;
				BOLE				DebugJitAvailableForHost;
				BOLE				DebugSuspended;								/* Host should not change debug flags - engine has them in a temporary state */
				NUM					DebugTraceOn;
				BOLE				EngineStateChanged;							/* set by calls to FDebug_Debug */
				CHAR				TempBuffer[_FSmartbase_MAXBUFFERLEN+2];		/* Temporary task scratchpad buffer */
				BOLE				FCompile_GenerateDebugInfo;					/* Compile function and Lisp parser should generate debugging information. */
				struct TVector*		FCompile_DebugSourceVector;					/* Current Source Vector object being compiled with debugging information. */
				struct TStructure*	FCompile_DebugListLines;					/* Current List ==> Line Numbers Structure being compiled with debugging information. */
				AISWORD				FCompile_DebugInterfaces;					/* Current Interfaces Structure being compiled with debugging information. */
				AISWORD				FCompile_DebugCurrentSourceLine;			/* Current source line providing debug information to the compile function. */
				AISWORD				FCompile_DebugCurrentSourceLineIndex;		/* Index of current source line providing debug information to the compile function. */
				NUM					FCompile_tmpCount;
				NUM					FCompile_lambdaSwitch;
				NUM					FCompile_LambdaNest;
				NUM					FCompile_LastProcID;
				NUM					FCompile_LastProcDisp;
				BOLE				FCompile_OptimizeSW;
				AISWORD				FDatabas_IOerr;
				AISWORD				FDatabas_OutOfMemory;
				AISWORD				FDatabas_PageSize;
				char*				FDebug_fmtStr77;
				BOLE				FDebug_ShowShort;
				REAL				FMath2_realResult;
				REAL				FMath2_imagResult;
				NUM					FMath2_intTotal;
				BOLE				FMath2_boolInitial;
				AISWORD				FMath2_CallBackTval;
				NUM					FOpt2_labelCounter;
				NUM					FVmscript_facs[8];
				BOLE				FVmscript_definiteArgs;						/*  Definite argument switch */
				NUM					FVmscript_SizeOfFormals;					/*  Number of formal arguments */
				CHAR				FVmscript_targetAm;							/*  Assignment target modifier */
				NUM					FVmscript_escapeCheck;						/*  Escape check counter */
				NUM					FVmscript_nArgument;						/*  Integer argument operand */
				REAL				FVmscript_Integer;							/*  Modulus math integer part */
				REAL				FVmscript_Fraction;							/*  Modulus math fraction part */
				NUM                 FVmscript_Modifier;							/*  Modifier pointer for push */
				AISWORD             FVmscript_isource;							/*  Tval source operand */
				AISWORD				FVmscript_iargument;						/*  Tval argument operand */
				AISWORD				FVmscript_iindex;							/*  Tval index operand */
				struct TSymbol*		FVmscript_aSymbol;							/*  Temporary symbol variable */
				BOLE				FVmscript_NestedDebugSwt;					/*  Nested debug required switch */
				NUM					FVmscript_cacheIndex;						/*  Temporary cache index */
				TYPE				FVmscript_type;								/*  Temporary type variable */
				NUM					FVmscript_Selection;						/*  Debugger source line selection */
				NUM					FVmscript2_DisassemblyStyle;
				NUM					FVmscript2_OldDisassemblyStyle;
				NUM					FVmscript2_VariablesToShow;
				struct TLambda*		FVmscript2_OldSelf;							/*  Saved copy of self */
				struct TLambda*		TLambda_TheProc;
				struct TLambda*		TLambda_saveProc;
				struct TLambda*		TLambda_CurrentProcedure;
				struct TContinuation*  TLambda_TheContinuation;
				AISWORD				TLambda_ThePropList;
				BOLE				TObject_ErrorSwt;
				AISWORD				TObject_ErrorTree;
				struct	TObjVector*	TObject_SaveVector;
				struct	TIntVector*	TObject_SaveNdx;
				REAL                startOfMemoryBlocks[1];
				 /* The array of memory blocks follows the,    */
				 /* last of the thread blocks. The variable is */
				 /* REAL for alignment purposes only.          */
                } THREAD, *LpTHREAD;
#define THREADBLOCKLENGTH         (NUM)(sizeof(THREAD))

/*  Declare the SmartLisp Lisp Native execution functions here. */
/*  Note:   These variables and macros can be used to access the  */
/*          SmartLisp native execution environment at run time  */
/*          from compiled Lambdas which are not interpreted.  */

typedef     AISWORD         (*LpFUNTVAL)	(POINTER gCP,LpTHREAD gTP, ...);
typedef     AISWORD         (*LpFUNWORD)	(POINTER gCP,LpTHREAD gTP, ...);
typedef     AISWORD*        (*LpFUNTVALPTR)	(POINTER gCP,LpTHREAD gTP, ...);
typedef     AISWORD*        (*LpFUNWORDPTR)	(POINTER gCP,LpTHREAD gTP, ...);
typedef     POINTER	        (*LpFUNPOINTER)	(POINTER gCP,LpTHREAD gTP, ...);
typedef     UNUM            (*LpATOUI)      (CHAR * s, CHAR ** ptr, int radix);
typedef		NUM             (*LpATOI)       (CHAR * s);
typedef		LONG			(*LpATOL)       (CHAR * s);
typedef		REAL			(*LpATOF)       (CHAR * s);
typedef		REAL			(*LpMODF)       (REAL v,LpNUM n);

/*  Execution Context Structure  */
//  The following structure contains the variables necessary to perserve a context

typedef struct  {BOLE				FSmartbase_Initialized;
				 CHAR				ContextName[64];
				 NUM				ContextIndex;	/* see gContextMutex[] in sbglue for usage */
				 POINTER			SessionMgrContextThread; /* pointer to Session Managers apContext->mpThread */
				 POINTER			ContextBlocks[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
				 NUM				ContextBlockSize[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
				 NUM				JIT_codeBloatFactor;
				 POINTER			JIT_JumpTable[256];
				 AISWORD		    Tval_TRUE;
				 AISWORD			Tval_FALSE;
				 AISWORD			Tval_VOID;
				 BOLE				FSmartbase_SilentMode;
				 struct TSymbol*	FSmartbase_errorMsg;
				 struct TSymbol*	FSmartbase_errorSym;
				 struct TSymbol*	FSmartbase_sysErrorSym;
				 AISWORD			FSmartbase_sysError;
				 jmp_buf			FSmartbase_CatchEnv;
				 LpFUNWORD			FSmartbase_RegisterCProcedure;
				 LpFUNPOINTER		FSmartbase_ObjectPtr;
				 LpFUNWORDPTR		FSmartbase_GetSymbolValuePtr;
				 LpFUNWORD			FSmartbase_Eval;
				 LpFUNWORD			FSmartbase_Evals;
				 LpFUNWORD			FSmartbase_Evalv;
				 LpFUNWORD			FSmartbase_Ref;
				 LpFUNWORD			FSmartbase_Refv;
				 LpFUNWORD			FSmartbase_Set;
				 LpFUNWORD			FSmartbase_Setv;
				 LpFUNWORD			FSmartbase_CnvFromChar;
				 LpFUNWORD			FSmartbase_CnvFromBool;
				 LpFUNWORD			FSmartbase_Error;
				 LpFUNWORD			FSmartbase_Perror;
				 LpFUNWORD			FSmartbase_MakeCFunction;
				 LpFUNWORD			FSmartbase_CnvFromUInt;
				 LpFUNWORD			FSmartbase_CnvFromInt;
				 LpFUNWORD			FSmartbase_CnvFromPtr;
				 LpFUNWORD			FSmartbase_CnvFromObj;
				 LpFUNWORD			FSmartbase_CnvFromReal;
				 LpFUNWORD			FSmartbase_CnvToFrame;
				 LpFUNWORD			FSmartbase_CnvFromText;
				 LpFUNWORD			FSmartbase_CnvToSymbol;
				 LpFUNWORD			FSmartbase_CnvToQSymbol;
				 LpFUNWORD			FSmartbase_GetSymbolValue;
				 NUM				(*_Host_Display)(POINTER gCP, LpTHREAD gTP, char* string, NUM newline);
				 NUM				(*_Host_Escape)(POINTER gCP, LpTHREAD gTP);
				 NUM				(*_Host_UpdateState)(POINTER gCP, LpTHREAD gTP);
				 NUM				(*_Host_Openf)(POINTER gCP, LpTHREAD gTP,char* name, NUM mode, NUM type);
				 NUM				(*_Host_Readf)(POINTER gCP, LpTHREAD gTP, NUM fileID, NUM length, char* bufptr);
				 NUM				(*_Host_Writef)(POINTER gCP, LpTHREAD gTP, NUM fileID, NUM length, const char* bufptr);
				 REAL			    (*_Host_Seekf)(POINTER gCP, LpTHREAD gTP, NUM fileID, REAL adjustment, NUM opcode);
				 REAL				(*_Host_Resizef)(POINTER gCP, LpTHREAD gTP, NUM fileID, REAL newsize);
				 NUM				(*_Host_Closef)(POINTER gCP, LpTHREAD gTP, NUM fileID, NUM opcode);
				 LpATOUI			atoui;
				 LpATOI				atoi;
				 LpATOL				atol;
				 LpATOF				atof;
				 LpMODF				modf;
				 BOLE               FCompile_Initialized;
				 struct TSymbol*    FCompile_CharPointerSym;
				 struct TSymbol*    FCompile_FloatPointerSym;
				 struct TSymbol*    FCompile_IntegerSym;
				 struct TSymbol*    FCompile_IntPointerSym;
				 struct TSymbol*    FCompile_JumpPointerSym;
				 struct TSymbol*    FCompile_NumberSym;
				 struct TSymbol*    FCompile_NumPointerSym;
				 struct TSymbol*    FCompile_ShortPointerSym;
				 struct TSymbol*    FCompile_dbgSourceLinesSYM;
				 struct TSymbol*    FCompile_dbgSourceLinesInstrSYM;
				 struct TSymbol*    FCompile_dbgParseTreeSYM;
				 struct TSymbol*    FCompile_dbgListLinesSYM;
				 struct TSymbol*    FCompile_gotoLE;
				 struct TSymbol*    FCompile_gotoLT;
				 struct TSymbol*    FCompile_gotoEQ;
				 struct TSymbol*    FCompile_gotoNE;
				 struct TSymbol*    FCompile_gotoGT;
				 struct TSymbol*    FCompile_gotoGE;
				 struct TSymbol*    FCompile_gotoLEb;
				 struct TSymbol*    FCompile_gotoLTb;
				 struct TSymbol*    FCompile_gotoEQb;
				 struct TSymbol*    FCompile_gotoNEb;
				 struct TSymbol*    FCompile_gotoGTb;
				 struct TSymbol*    FCompile_gotoGEb;
				 struct TSymbol*    FCompile_gotoLEc;
				 struct TSymbol*    FCompile_gotoLTc;
				 struct TSymbol*    FCompile_gotoEQc;
				 struct TSymbol*    FCompile_gotoNEc;
				 struct TSymbol*    FCompile_gotoGTc;
				 struct TSymbol*    FCompile_gotoGEc;
				 struct TSymbol*    FCompile_gotoLEi;
				 struct TSymbol*    FCompile_gotoLTi;
				 struct TSymbol*    FCompile_gotoEQi;
				 struct TSymbol*    FCompile_gotoNEi;
				 struct TSymbol*    FCompile_gotoGTi;
				 struct TSymbol*    FCompile_gotoGEi;
				 struct TSymbol*    FCompile_gotoLEn;
				 struct TSymbol*    FCompile_gotoLTn;
				 struct TSymbol*    FCompile_gotoEQn;
				 struct TSymbol*    FCompile_gotoNEn;
				 struct TSymbol*    FCompile_gotoGTn;
				 struct TSymbol*    FCompile_gotoGEn;
				 struct TSymbol*    FCompile_return;
				 struct TSymbol*    FCompile_notSym;
				 struct TSymbol*    FCompile_andSym;
				 struct TSymbol*    FCompile_orSym;
				 struct TSymbol*    FCompile_ifSym;
				 struct TSymbol*    FCompile_ieqSym;
				 struct TSymbol*    FCompile_igtSym;
				 struct TSymbol*    FCompile_iltSym;
				 struct TSymbol*    FCompile_igeSym;
				 struct TSymbol*    FCompile_ileSym;
				 struct TSymbol*    FCompile_ineSym;
				 struct TSymbol*    FCompile_neqSym;
				 struct TSymbol*    FCompile_ngtSym;
				 struct TSymbol*    FCompile_nltSym;
				 struct TSymbol*    FCompile_ngeSym;
				 struct TSymbol*    FCompile_nleSym;
				 struct TSymbol*    FCompile_nneSym;
				 struct TSymbol*    FCompile_neSym;
				 struct TSymbol*    FCompile_eqSym;
				 struct TSymbol*    FCompile_gtSym;
				 struct TSymbol*    FCompile_ltSym;
				 struct TSymbol*    FCompile_geSym;
				 struct TSymbol*    FCompile_leSym;
				 struct TSymbol*    FCompile_refSym;
				 struct TSymbol*    FCompile_setSym;
				 struct TSymbol*    FCompile_addSym;
				 struct TSymbol*    FCompile_addiSym;
				 struct TSymbol*    FCompile_adduSym;
				 struct TSymbol*    FCompile_iaddSym;
				 struct TSymbol*    FCompile_uaddSym;
				 struct TSymbol*    FCompile_idivSym;
				 struct TSymbol*    FCompile_udivSym;
				 struct TSymbol*    FCompile_imodSym;
				 struct TSymbol*    FCompile_umodSym;
				 struct TSymbol*    FCompile_imulSym;
				 struct TSymbol*    FCompile_umulSym;
				 struct TSymbol*    FCompile_isubSym;
				 struct TSymbol*    FCompile_usubSym;
				 struct TSymbol*    FCompile_naddSym;
				 struct TSymbol*    FCompile_ndivSym;
				 struct TSymbol*    FCompile_nmodSym;
				 struct TSymbol*    FCompile_nmulSym;
				 struct TSymbol*    FCompile_nsubSym;
				 struct TSymbol*    FCompile_divSym;
				 struct TSymbol*    FCompile_diviSym;
				 struct TSymbol*    FCompile_divuSym;
				 struct TSymbol*    FCompile_divrSym;
				 struct TSymbol*    FCompile_divriSym;
				 struct TSymbol*    FCompile_divruSym;
				 struct TSymbol*    FCompile_mulSym;
				 struct TSymbol*    FCompile_muliSym;
				 struct TSymbol*    FCompile_muluSym;
				 struct TSymbol*    FCompile_shlSym;
				 struct TSymbol*    FCompile_shrSym;
				 struct TSymbol*    FCompile_subSym;
				 struct TSymbol*    FCompile_subiSym;
				 struct TSymbol*    FCompile_subuSym;
				 struct TSymbol*    FCompile_xorSym;
				 struct TSymbol*    FCompile_nandSym;
				 struct TSymbol*    FCompile_norSym;
				 struct TSymbol*    FCompile_nxorSym;
				 struct TSymbol*    FCompile_nandbSym;
				 struct TSymbol*    FCompile_norbSym;
				 struct TSymbol*    FCompile_nxorbSym;
				 struct TSymbol*    FCompile_add1Sym;
				 struct TSymbol*    FCompile_sub1Sym;
				 struct TSymbol*    FCompile_bandSym;
				 struct TSymbol*    FCompile_borSym;
				 struct TSymbol*    FCompile_selfSym;
				 struct TSymbol*    FCompile_argcountSym;
				 struct TSymbol*    FCompile_defineSym;
				 struct TSymbol*    FCompile_defunSym;
				 struct TSymbol*    FCompile_defmacroSym;
				 struct TSymbol*    FCompile_defchildSym;
				 struct TSymbol*    FCompile_defChildLambdaSym;
				 struct TSymbol*    FCompile_deforphanSym;
				 struct TSymbol*    FCompile_defriendSym;
				 struct TSymbol*    FCompile_defclassSym;
				 struct TSymbol*    FCompile_defcloneSym;
				 struct TSymbol*    FCompile_defstructSym;
				 struct TSymbol*    FCompile_defvmSym;
				 struct TSymbol*    FCompile_defmethodSym;
				 struct TSymbol*    FCompile_lambdaSym;
				 struct TSymbol*    FCompile_whileSym;
				 struct TSymbol*    FCompile_loopSym;
				 struct TSymbol*    FCompile_beginSym;
				 struct TSymbol*    FCompile_sendSym;
				 struct TSymbol*    FCompile_superSym;
				 struct TSymbol*    FCompile_argfetchSym;
				 struct TSymbol*    FCompile_onerrorSym;
				 struct TSymbol*    FCompile_letSym;
				 struct TSymbol*    FCompile_caseSym;
				 struct TSymbol*    FCompile_condSym;
				 struct TSymbol*    FCompile_setqSym;
				 struct TSymbol*    FCompile_setfSym;
				 struct TSymbol*    FCompile_setvSym;
				 struct TSymbol*    FCompile_addnSym;
				 struct TSymbol*    FCompile_divnSym;
				 struct TSymbol*    FCompile_mulnSym;
				 struct TSymbol*    FCompile_subnSym;
				 struct TSymbol*    FCompile_beqSym;
				 struct TSymbol*    FCompile_bgtSym;
				 struct TSymbol*    FCompile_bltSym;
				 struct TSymbol*    FCompile_bgeSym;
				 struct TSymbol*    FCompile_bleSym;
				 struct TSymbol*    FCompile_bneSym;
				 struct TSymbol*    FCompile_caddSym;
				 struct TSymbol*    FCompile_cdivSym;
				 struct TSymbol*    FCompile_cmulSym;
				 struct TSymbol*    FCompile_csubSym;
				 struct TSymbol*    FCompile_ceqSym;
				 struct TSymbol*    FCompile_cgtSym;
				 struct TSymbol*    FCompile_cltSym;
				 struct TSymbol*    FCompile_cgeSym;
				 struct TSymbol*    FCompile_cleSym;
				 struct TSymbol*    FCompile_cneSym;
				 struct TSymbol*    FCompile_plusPlusSym;
				 struct TSymbol*    FCompile_minusMinusSym;
				 struct TSymbol*    FCompile_plusEqualsSym;
				 struct TSymbol*    FCompile_refmacroSym;
				 struct TSymbol*    FCompile_minusEqualsSym;
				 struct TSymbol*    FCompile_timesEqualsSym;
				 struct TSymbol*    FCompile_divEqualsSym;
				 struct TSymbol*    FCompile_refTextSym;
				 struct TSymbol*    FCompile_refStringSym;
				 struct TSymbol*    FCompile_setStringSym;
				 struct TSymbol*    FCompile_refSymbolSym;
				 struct TSymbol*    FCompile_refVectorSym;
				 struct TSymbol*    FCompile_setVectorSym;
				 struct TSymbol*    FCompile_refStrValueSym;
				 struct TSymbol*    FCompile_setStrValueSym;
				 struct TSymbol*    FCompile_refStrKeySym;
				 struct TSymbol*    FCompile_setStrKeySym;
				 struct TSymbol*    FCompile_refDicValueSym;
				 struct TSymbol*    FCompile_setDicValueSym;
				 struct TSymbol*    FCompile_refDicKeySym;
				 struct TSymbol*    FCompile_setDicKeySym;
				 struct TSymbol*    FCompile_refDirValueSym;
				 struct TSymbol*    FCompile_setDirValueSym;
				 struct TSymbol*    FCompile_refDirKeySym;
				 struct TSymbol*    FCompile_setDirKeySym;
				 struct TSymbol*    FCompile_refBitVectorSym;
				 struct TSymbol*    FCompile_setBitVectorSym;
				 struct TSymbol*    FCompile_refBytVectorSym;
				 struct TSymbol*    FCompile_setBytVectorSym;
				 struct TSymbol*    FCompile_refCpxVectorSym;
				 struct TSymbol*    FCompile_setCpxVectorSym;
				 struct TSymbol*    FCompile_refPcdVectorSym;
				 struct TSymbol*    FCompile_setPcdVectorSym;
				 struct TSymbol*    FCompile_refObjVectorSym;
				 struct TSymbol*    FCompile_setObjVectorSym;
				 struct TSymbol*    FCompile_refIntVectorSym;
				 struct TSymbol*    FCompile_setIntVectorSym;
				 struct TSymbol*    FCompile_refNumVectorSym;
				 struct TSymbol*    FCompile_setNumVectorSym;
				 struct TSymbol*    FCompile_refFltVectorSym;
				 struct TSymbol*    FCompile_setFltVectorSym;
				 struct TSymbol*    FCompile_refMatrixSym;
				 struct TSymbol*    FCompile_setMatrixSym;
				 struct TSymbol*    FCompile_refNumMatrixSym;
				 struct TSymbol*    FCompile_setNumMatrixSym;
				 BOLE				FConio_Initialized;
				 BOLE				FControl_Initialized;
				 BOLE				FConvert_Initialized;
				 BOLE				FDatabas_Initialized;
				 AISWORD*			FDatabas_pCompress;
				 AISWORD*			FDatabas_pUncompress;
				 AISWORD*			FDatabas_pEncode;
				 AISWORD*			FDatabas_pDecode;
				 BOLE				FDatefnc_Initialized;
				 BOLE				FDebug_Initialized;
				 BOLE				FDebug_ShowRegVars;
				 BOLE				FDebug_ShowTempVars;
				 BOLE				FDebug_ShowArgVars;
				 BOLE				FDebug_ShowClassVars;
				 BOLE				FDebug_ShowConstVars;
				 BOLE				FDebug_ShowInterfaceVars;
				 BOLE				FDebug_ShowPersistentVars;
				 struct TSymbol*    FDebug_exit;
				 struct TSymbol*    FDebug_short;
				 struct TSymbol*    FDebug_bp;
				 struct TSymbol*    FDebug_bc;
				 struct TSymbol*    FDebug_bl;
				 struct TSymbol*    FDebug_src;
				 struct TSymbol*    FDebug_srcOnly;
				 struct TSymbol*    FDebug_srcCnt;
				 struct TSymbol*    FDebug_asm;
				 struct TSymbol*    FDebug_envs;
				 struct TSymbol*    FDebug_dbgVec;
				 struct TSymbol*    FDebug_dbgSym;
				 struct TSymbol*    FDebug_traceon;
				 struct TSymbol*    FDebug_traceoff;
				 struct TSymbol*    FDebug_on;
				 struct TSymbol*    FDebug_off;
				 struct TSymbol*    FDebug_jiton;
				 struct TSymbol*    FDebug_jitoff;
				 struct TSymbol*    FDebug_until;
				 struct TSymbol*    FDebug_go;
				 struct TSymbol*    FDebug_currentResult;
				 struct TObjVector* FDebug_Space;
				 struct TSymbol*    FDebug_getGlobalValue;
				 BOLE				FDefine_Initialized;
				 BOLE				FFinance_Initialized;
				 BOLE				FLisp_Initialized;
				 BOLE				FList_Initialized;
				 BOLE				FMacro_Initialized;
				 struct TSymbol*    FMacro_macro;
				 struct TSymbol*    FMacro_vm;
				 struct TSymbol*    FMacro_refSym;
				 struct TSymbol*    FMacro_makevecSym;
				 struct TSymbol*    FMacro_makeenvSym;
				 struct TSymbol*    FMacro_quoteSym;
				 struct TSymbol*    FMacro_boolean;
				 BOLE				FMake_Initialized;
				 BOLE				FMath1_Initialized;
				 BOLE				FMath2_Initialized;
				 NUM			    FMath2_mask[32];
				 BOLE				FMath3_Initialized;
				 BOLE				FMatrix1_Initialized;
				 BOLE				FXml_Initialized;
				 NUM				FMemory_BlockedMemoryBytes; /* see (inspect BlockedMemoryBytes:) */
                 BOLE				FMemory_Initialized;
				 BOLE               FMemory_SelfTest;
				 NUM                FMemory_UsedBlockCount;		/* see (inspect BlockCount:) */
				 NUM                FMemory_UsedMemoryBytes;	/* see (inspect UseMemoryBytes:) */
                 NUM                FMemory_ParentSize;
				 NUM				FMemory_myForcedGCCounter;
                 POINTER            FMemory_ParentPtr;
				 NUM                FMemory_MemoryAllocated;	/* see (inspect MemoryAllocated:) */
				 LpMHANDLEINFO		FMemory_PrevPtr;
				 NUM                FMemory_FrameMaxIndex;
				 NUM                FMemory_FrameSize[_MEMMAXFRAMES];
				 LpMHANDLEINFO		FMemory_FrameNextMHandle[_MEMMAXFRAMES];
				 NUM				FMemory_FreeBlockCount;		/* number of free blocks - see (inspect FreeBlockCount:) */
				 NUM				FMemory_FreeMemoryBytes;	/* number of free memory bytes - see (inspect FreeBlockCount:) */
				 NUM				FMemory_BlockedBlockCount;	/* number of blocked blocks (these map out gaps in the os supplied memory) - see (inspect BlockedBlockCount:)*/
				 NUM				FMemory_SystemCheckCount;	/* number of times systemCheck was performed - see (inspect SystemCheckCount:) */
				 NUM				FMemory_JoinCount;			/* number of times blocks were joined - see (inspect JoinCount:) */
				 NUM				FMemory_SplitCount;			/* number of times blocks were split - see (inspect SplitCount:) */
				 NUM				FMemory_NewCount;			/* number of times blocks were created from new memory - see (inspect NewCount:) */
				 NUM				FMemory_CopyCount;			/* number of times FMemory_copy called - see (inspect CopyCount:) */
				 NUM				FMemory_ReuseCount;			/* number of times blocks were reused in whole - see (inspect ReuseCount:) */
				 NUM				FMemory_FreeListHitCount;	/* number of times blocks on a free list were found first time. - see (inspect FreeListHitCount:) */
				 NUM				FMemory_LargerFrameHitCount;/* number of times blocks on a larger frame free list were found. - see (inspect LargerFrameHitCount:) */
				 NUM				FMemory_TimerFrequency;		/* cycles per second of time counters */
				 NUM				FMemory_NewTime;
				 NUM				FMemory_FreeTime;
				 NUM				FMemory_ResizeTime;
                 BOLE               FMySQL1_Initialized;
                 BOLE               FMySQL1_Enabled;
                 BOLE               FMySQL1_UseWordInt;
                 BOLE               FMySQL1_UseWordFlt;
                 BOLE               FMySQL1_UseWordDat;
				 BOLE               FMySQL1_UseDecimalMoney;
                 struct TIntVector* FMySQL1_IntVector;
                 struct TVector*    FMySQL1_InfoVector;
                 NUM				TObject_initMaxObjects;
				 BOLE				FOpt1_Initialized;
				 BOLE				FOpt2_Initialized;
				 BOLE				FPred_Initialized;
				 BOLE				FPred2_Initialized;
				 BOLE				FProperty_Initialized;
				 BOLE				FStatfnc_Initialized;
				 NUM				FStatfnc_count;
				 REAL				FStatfnc_kurtosis;
				 REAL				FStatfnc_minimum;
				 REAL				FStatfnc_maximum;
				 REAL				FStatfnc_xbar;
				 REAL				FStatfnc_stdev;
				 REAL				FStatfnc_skew;
				 AISWORD			FStatfnc_CallBackTval;
				 struct TNumVector* FStatfnc_NumVector;
				 BOLE				FString_Initialized;
				 BOLE				FText_Initialized;
				 BOLE				FText2_Initialized;
				 BOLE				FTextfnc_Initialized;
				 BOLE				FUtil1_Initialized;
				 BOLE				FUtil2_Initialized;
				 BOLE				FUtil3_Initialized;
				 BOLE				FVmappend_Initialized;
				 BOLE				FVmscript_Initialized;
				 AISWORD			FVmScript_ERROR_ILLEGAL_INSTRUCTION;
				 AISWORD			FVmScript_ERROR_ILLEGAL_VALUE;
				 AISWORD			FVmScript_ERROR_DIVIDE_BY_ZERO;
				 AISWORD			FVmScript_ERROR_OVERFLOW;
				 AISWORD			FVmScript_ERROR_MISSING_FUNCTION_NAME;
				 AISWORD			FVmScript_ERROR_NOT_AN_Lambda;
				 AISWORD			FVmScript_ERROR_MISSING_PCODES;
				 AISWORD			FVmScript_ERROR_VMADD_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMDIV_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMDIVR_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMMUL_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMSUB_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMADDI_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMADDU_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMSUBI_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMSUBU_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMDIVI_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMDIVU_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMDIVRI_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMDIVRU_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMMULI_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMMULU_BAD_VALUE;
				 AISWORD			FVmScript_ERROR_VMARGFETCH_MOD;
				 AISWORD			FVmScript_ERROR_VMARGFETCH_VALUE;
				 AISWORD			FVmScript_ERROR_VMONERROR_MOD;
				 AISWORD			FVmScript_ERROR_VMPUSH_MOD;
				 AISWORD			FVmScript_ERROR_VMREF_MOD;
				 AISWORD			FVmScript_ERROR_VMRETURN_MOD;
				 AISWORD			FVmScript_ERROR_VMSEND_MESSAGE;
				 AISWORD			FVmScript_ERROR_VMSET_MOD;
				 AISWORD			FVmScript_ERROR_VMTARGET_MOD;
				 BOLE				FXmlLoad_Initialized;
				 BOLE				FWorkspace_Initialized;
				 BOLE				TLambda_Initialized;
				 NUM				TLambda_MaxInputLen;
				 struct TSymbol*    TLambda_assign;
				 struct TSymbol*    TLambda_by;
				 struct TSymbol*    TLambda_close;
				 struct TSymbol*    TLambda_console;
				 struct TSymbol*    TLambda_data;
				 struct TSymbol*    TLambda_do;
				 struct TSymbol*    TLambda_EditWindow;
				 struct TSymbol*    TLambda_else;
				 struct TSymbol*    TLambda_end;
				 struct TSymbol*    TLambda_for;
				 struct TSymbol*    TLambda_from;
				 struct TSymbol*    TLambda_globals;
				 struct TSymbol*    TLambda_include;
				 struct TSymbol*    TLambda_nil;
				 struct TSymbol*    TLambda_otherwise;
				 struct TSymbol*    TLambda_pi;
				 struct TSymbol*    TLambda_quote;
				 struct TSymbol*    TLambda_save;
				 struct TSymbol*    TLambda_script;
				 struct TSymbol*    TLambda_select;
				 struct TSymbol*    TLambda_then;
				 struct TSymbol*    TLambda_to;
				 struct TSymbol*    TLambda_until;
				 struct TSymbol*    TLambda_title;
				 struct TSymbol*    TLambda_workspace;
				 struct TSymbol*    TLambda_dotcdr;
				 struct TSymbol*    TLambda_faces;
				 struct TSymbol*    TLambda_self;
				 struct TSymbol*    TLambda_vars;
				 struct TSymbol*    TLambda_args;
				 struct TSymbol*    TLambda_svars;
				 struct TSymbol*    TLambda_pvars;
				 struct TSymbol*    TLambda_cvars;
				 struct TSymbol*    TLambda_rvars;
				 struct TSymbol*    TLambda_label;
				 struct TSymbol*    TLambda_goto;
				 struct TSymbol*    TLambda_Sv;
				 struct TSymbol*    TLambda_Av;
				 struct TSymbol*    TLambda_Tv;
				 struct TSymbol*    TLambda_Pv;
				 struct TSymbol*    TLambda_Cv;
				 struct TSymbol*    TLambda_Rv;
				 struct TSymbol*    TLambda_Pc;
				 struct TSymbol*    TLambda_Sc;
				 struct TSymbol*    TLambda_Nc;
				 struct TSymbol*    TLambda_In;
				 struct TSymbol*    TLambda_Binding;
				 struct TSymbol*    TLambda_BreakList;
				 struct TSymbol*    TLambda_Vm;
				 struct TSymbol*    TLambda_LispVirtualMachine;
				 struct TSymbol*    TLambda_EvalWhenDoomed;
				 struct TSymbol*    TLambda_Doomed;
				 AISWORD			TLambda_TvalIn;
				 AISWORD			TLambda_TvalBinding;
				 BOLE				TBitVector_Initialized;
				 BOLE				TBitVector_Functions_Initialized;
				 CHAR				TBitVector_OrMasks[8];
				 CHAR				TBitVector_AndMasks[8];
				 BOLE				TByteVector_Initialized;
				 BOLE				TBrick_Initialized;
				 BOLE				TContinuation_Initialized;
				 BOLE				TCpx_Initialized;
				 BOLE				TCpxVector_Initialized;
				 BOLE				TDatabase_Initialized;
				 NUM				TDatabase_MinBlockSize;
				 BOLE				TDictionary_Initialized;
				 BOLE				TDirectory_Initialized;
				 BOLE				TError_Initialized;
				 struct	TError*     TError_NIL;
				 BOLE				TFltVector_Initialized;
				 BOLE				TLongVector_Initialized;
				 BOLE				TIntVector_Initialized;
				 BOLE				TShtVector_Initialized;
				 BOLE				TMatrix_Initialized;
				 BOLE				TNeuralNet_Initialized;
				 struct TSymbol*	TNeuralNet_NeuralNetSym;
				 struct TSymbol*    TNeuralNet_NeuralLayerSym;
				 struct TSymbol*    TNeuralNet_ContinuousSym;
				 struct TSymbol*    TNeuralNet_SigmoidalSym;
				 struct TSymbol*    TNeuralNet_BinarySym;
				 struct TSymbol*    TNeuralNet_BipolarSym;
				 BOLE				TNumMatrix_Initialized;
				 BOLE				TNumVector_Initialized;
				 NUM				TObject_MaxOutputLen;
				 BOLE				TObject_Initialized;
				 NUM				TObject_MaxObjectCount;
				 NUM				TObject_UsedObjectCount;
				 NUM				TObject_MaxUsedObjectCount;
				 NUM				TObject_FreeObjectIndex;
				 HMChar				TObject_ObjectFlag;
				 HMObj				TObject_MainObjectList;
				 HMObj				TObject_MainObjectHeaderArray;
				 SHORT				TObject_GarbageInhibit;
				 BOLE				TObject_GarbageON;
				 BOLE				TObject_GarbageCollectInProgress;
				 SHORT				TObject_MaxTypes;
				 HMName				TObject_TypeName;
				 HMChar				TObject_TypeFlag;
				 HMShort			TObject_TypeSize;
				 NUM				TObject_TypeNew;
				 NUM				TObject_TypeMark;
				 NUM				TObject_TypeGlobalMark;
				 NUM				TObject_TypeConvert;
				 NUM				TObject_TypeCompare;
				 NUM				TObject_TypeSetIV1;
				 NUM				TObject_TypeSetIV2;
				 NUM				TObject_TypeSetIV3;
				 NUM				TObject_TypeGetIV1;
				 NUM				TObject_TypeGetIV2;
				 NUM				TObject_TypeGetIV3;
				 NUM				TObject_TypeMap;
				 NUM				TObject_TypeMapc;
				 NUM				TObject_TypePrint;
				 NUM				TObject_TypeLoad;
				 NUM				TObject_TypeSave;
				 NUM				TObject_TypeDoom;
				 NUM				TObject_TypeCopy;
				 NUM				TObject_TypeComputeSize;
				 HMTval				TObject_TypeMethods;
				 HMTval				TObject_TypeFields;
				 HMType				TObject_TypeParent;
				 NUM				TObject_Ctype[256];
				 AISWORD			TObject_VOID;
				 AISWORD			TObject_OK;
				 AISWORD			TObject_TRUE;
				 AISWORD			TObject_FALSE;
				 AISWORD			TObject_HIGH;
				 AISWORD			TObject_EQUAL;
				 AISWORD			TObject_LOW;
				 AISWORD			TObject_NAN;
				 AISWORD			TObject_ERROR_OUT_OF_MEMORY;
				 AISWORD			TObject_ERROR_FRAME_ERROR;
				 AISWORD			TObject_ERROR_INVALID;
				 AISWORD			TObject_ERROR_DIVIDE_BY_ZERO;
				 AISWORD			TObject_ERROR_INVALID_ARGLIST;
				 AISWORD			TObject_ERROR_RUNTIME;
				 AISWORD			TObject_ERROR_SYNTAX;
				 AISWORD			TObject_ERROR_BADOPERATOR;
				 AISWORD			TObject_ERROR_BADSYMBOL;
				 AISWORD			TObject_ERROR_BADINDEX;
				 AISWORD			TObject_ERROR_BADTYPE;
				 AISWORD			TObject_ERROR_BADMSG;
				 AISWORD			TObject_ERROR_BADCONTINUATION;
				 AISWORD			TObject_ERROR_CONTINUATION;
				 AISWORD			TObject_ERROR_WINDOWACTIVE;
				 AISWORD			TObject_ERROR_BADCELL;
				 AISWORD			TObject_ERROR_SYMBOLMUSTFOLLOW;
				 AISWORD			TObject_ERROR_PAIRINDEXREQ;
				 AISWORD			TObject_ERROR_SYMBOLINDEXREQ;
				 AISWORD			TObject_ERROR_NUMINDEXREQ;
				 AISWORD			TObject_ERROR_BADIDXORKEY;
				 AISWORD			TObject_ERROR_BADIVAL;
				 AISWORD			TObject_ERROR_ACCESSDENIED;
				 BOLE               TObjVector_Initialized;
				 BOLE				TPair_Initialized;
				 BOLE				TPcodeVector_Initialized;
				 unsigned char		TPcodeVector_LegalModifiers[_VMMAXINSTRS][3];
				 BOLE				TString_Initialized;
				 struct TString*	TString_NIL;
				 BOLE				TStructure_Initialized;
				 struct TSymbol*    TStructure_key;
				 struct TSymbol*    TStructure_value;
				 BOLE				TSVector_Initialized;
				 BOLE				TSymbol_Initialized;
				 struct TSymbol*	TSymbol_NIL;
				 struct TObjVector*	TSymbol_SymbolTable;
				 NUM				TSymbol_SymbolTable_ActiveCount;
				 NUM				TSymbol_SymbolTable_MaxEmptyCount;
				 NUM				TSymbol_SymbolTable_MinEmptyCount;
				 NUM				TSymbol_SymbolTable_NewAllocationSize;
				 struct TSymbol*	TSymbol_Currentworkspace;
				 struct TSymbol*	TSymbol_Path;
				 struct TSymbol*	TSymbol_SaveTypes;
				 struct TSymbol*	TSymbol_Formula;
				 struct TSymbol*	TSymbol_Script;
				 struct TSymbol*	TSymbol_Value;
				 struct TSymbol*	TSymbol_Viewer;
				 struct TSymbol*	TSymbol_Key;
				 struct TSymbol*	TSymbol_hostSend;
				 struct TSymbol*	TSymbol_BreakManager;
				 struct TSymbol*	TSymbol_add;
				 struct TSymbol*	TSymbol_addi;
				 struct TSymbol*	TSymbol_addu;
				 struct TSymbol*	TSymbol_compare;
				 struct TSymbol*	TSymbol_compareLT;
				 struct TSymbol*	TSymbol_compareLE;
				 struct TSymbol*	TSymbol_compareEQ;
				 struct TSymbol*	TSymbol_compareNE;
				 struct TSymbol*	TSymbol_compareGE;
				 struct TSymbol*	TSymbol_compareGT;
				 struct TSymbol*	TSymbol_div;
				 struct TSymbol*	TSymbol_divi;
				 struct TSymbol*	TSymbol_divu;
				 struct TSymbol*	TSymbol_divr;
				 struct TSymbol*	TSymbol_divri;
				 struct TSymbol*	TSymbol_divru;
				 struct TSymbol*	TSymbol_mul;
				 struct TSymbol*	TSymbol_muli;
				 struct TSymbol*	TSymbol_mulu;
				 struct TSymbol*	TSymbol_new;
				 struct TSymbol*	TSymbol_ref;
				 struct TSymbol*	TSymbol_set;
				 struct TSymbol*	TSymbol_sub;
				 struct TSymbol*	TSymbol_subi;
				 struct TSymbol*	TSymbol_subu;
				 struct TSymbol*	TSymbol___send;
				 struct TSymbol*    TSymbol_eol;
				 NUM				TSymbol_MakeUniqueTime; /* time spend in the Make_Unique call - see (inspect MakeUnique:) */
				 BOLE				TVector_Initialized;
				 BOLE				TWorkspace_Initialized;
                 NUM                maxNumberOfThreads;
				 NUM				Custom01;	/* used with perfmon - see Fperfmon.h and (inspect Custom01:) */
				 NUM				Custom02;	/* ditto */
				 NUM				Custom03;	/* ditto */
				 NUM				Custom04;	/* ditto */
				 BOLE				logMode;
				 NUM				logFileID;
				 BOLE				logTime;
				 time_t				logStartTime;
				 BOLE				logNewLineFlag;
                 THREAD				ThreadBlocks[1];
				 /* The array of thread blocks follows here,   */
				 /* and after the array of thread blocks, the  */
				 /* main memory blocks follow. The array is    */
				 /* dimension one only for compilation. At     */
                 /* initialization time FMemory allocates the  */
                 /* space requested in the maxNumberOfThreads  */
                 /* variable.*/
                } XCONTEXT, *LpXCONTEXT;

#define XCONTEXTBLOCKLENGTH         (NUM)(sizeof(XCONTEXT))

/*  Declare the SmartLisp Lisp operations Stack and framing macros. */
/*  Note:   These variables and macros can be used to access the  */
/*          SmartLisp operations stack so direct inline C tagged  */
/*          variables in C function calls can be protected from */
/*          garbage collection.  */

typedef     AISWORD             (*LpVMEVALUATOR)	(LpXCONTEXT gCP, LpTHREAD gTP,struct TLambda* proc,NUM argc,LpWORD argP);
typedef     AISWORD             (*LpFUNC)			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,LpWORD argP);
typedef     AISWORD             (*LpFNEW)			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,LpWORD argP);

typedef     AISWORD             ( *LpFPRINT )		(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD self, LpNUM size, LpCHAR buf);
typedef     void			    ( *LpFMARK )		(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD srcTval);
typedef     AISWORD             ( *LpFGMARK )		(LpXCONTEXT gCP, LpTHREAD gTP);
typedef     AISWORD             ( *LpF2TVALS )		(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD v1, AISWORD v2);
typedef     AISWORD				( *LpF3TVALS )		(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD v1, AISWORD v2, AISWORD v3);
typedef     AISWORD				( *LpF4TVALS )		(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD v1, AISWORD v2, AISWORD v3, AISWORD v4);
typedef     AISWORD				( *LpF5TVALS )		(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD v1, AISWORD v2, AISWORD v3, AISWORD v4, AISWORD v5);
typedef     AISWORD				( *LpFCONVERT )		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, AISWORD oldValue);

typedef     AISWORD				(*LpREGCPROC)		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR funcSymbolName,const LpFUNC lpFunc);
typedef     POINTER				(*LpOBJECTPTR)		(LpXCONTEXT gCP,LpTHREAD gTP,const LpWORD anObject);
typedef     AISWORD*			(*LpGETSYMPTR)		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const BOLE perm);
typedef     AISWORD				(*LpEVAL)			(LpXCONTEXT gCP,LpTHREAD gTP,const AISWORD proc,const NUM argc, ... );

typedef struct  {
                LpFUNC      Lpfunc[1];
                } FMLpfunc, **HMLpfunc;
#define atHMLpfunc(h,n) ((*(HMLpfunc)(h))->Lpfunc[n])

typedef struct  {
                LpFNEW      Lpfnew[1];
                } FMLpfnew, **HMLpfnew;
#define atHMLpfnew(h,n) ((*(HMLpfnew)(h))->Lpfnew[n])

typedef struct  {
                LpFMARK     Lpfmark[1];
                } FMLpfmark, **HMLpfmark;
#define atHMLpfmark(h,n)    ((*(HMLpfmark)(h))->Lpfmark[n])

typedef struct  {
                LpFPRINT        Lpfprint[1];
                } FMLpfprint, **HMLpfprint;
#define atHMLpfprint(h,n)   ((*(HMLpfprint)(h))->Lpfprint[n])

typedef struct  {
                LpFCONVERT      Lpfconvert[1];
                } FMLpfconvert, **HMLpfconvert;
#define atHMLpfconvert(h,n) ((*(HMLpfconvert)(h))->Lpfconvert[n])

typedef struct  {
                LpF2TVALS       Lpf2tvals[1];
                } FMLpf2tvals, **HMLpf2tvals;
#define atHMLpf2tvals(h,n)  ((*(HMLpf2tvals)(h))->Lpf2tvals[n])

typedef struct  {
                LpF3TVALS       Lpf3tvals[1];
                } FMLpf3tvals, **HMLpf3tvals;
#define atHMLpf3tvals(h,n)  ((*(HMLpf3tvals)(h))->Lpf3tvals[n])

typedef struct  {
                LpF4TVALS       Lpf4tvals[1];
                } FMLpf4tvals, **HMLpf4tvals;
#define atHMLpf4tvals(h,n)  ((*(HMLpf4tvals)(h))->Lpf4tvals[n])

typedef struct  {
                LpF5TVALS       Lpf5tvals[1];
                } FMLpf5tvals, **HMLpf5tvals;
#define atHMLpf5tvals(h,n)  ((*(HMLpf5tvals)(h))->Lpf5tvals[n])

typedef struct  {
                LpFGMARK        Lpfgmark[1];
                } FMLpfgmark, **HMLpfgmark;
#define atHMLpfgmark(h,n)   ((*(HMLpfgmark)(h))->Lpfgmark[n])

typedef     AISWORD ( *LpFNLOAD )(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
typedef struct  {
                LpFNLOAD        Lpfnload[1];
                } FMLpfnload, **HMLpfnload;
#define atHMLpfnload(h,n)   ((*(HMLpfnload)(h))->Lpfnload[n])

// Define the prototype and access macros for the Save function vector
typedef HMemory ( *LpFNSAVE )(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD selfTval, HMemory aHMemory);
typedef struct  {
                LpFNSAVE        Lpfnsave[1];
                } FMLpfnsave, **HMLpfnsave;
#define atHMLpfnsave(h,n)   ((*(HMLpfnsave)(h))->Lpfnsave[n])


// Define the prototype and access macros for the computesize function vector
typedef void    ( *LpFCOMPUTESIZE )(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD selfTval, NUM* aSize);
typedef struct  {
                LpFCOMPUTESIZE  Lpfcomputesize[1];
                } FMLpfcomputesize, **HMLpfcomputesize;
#define atHMLpfcomputesize(h,n)   ((*(HMLpfcomputesize)(h))->Lpfcomputesize[n])

// Define the prototype and access macros for the copy function vector
typedef  struct TObject*  ( *LpFCOPY )(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD selfTval);
typedef struct  {
                LpFCOPY        Lpfcopy[1];
                } FMLpfcopy, **HMLpfcopy;
#define atHMLpfcopy(h,n)   ((*(HMLpfcopy)(h))->Lpfcopy[n])

// Define the prototype and access macros for the doom function vector
typedef  void   ( *LpFDOOM )(LpXCONTEXT gCP, LpTHREAD gTP,AISWORD selfTval);
typedef struct  {
                LpFDOOM        Lpfdoom[1];
                } FMLpfdoom, **HMLpfdoom;
#define atHMLpfdoom(h,n)   ((*(HMLpfdoom)(h))->Lpfdoom[n])

#define TopOfStack                  gTP->TvalStackIdx
#define StartRecursion\
					if ((++gTP->RecursionCount) >= gTP->MaxRecursions)\
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_RECURSION);\
					else\
					if (((TopOfStack - 20) >= gTP->MaxTvalStackSize) || (TopOfStack < 0))\
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_STACK)
#define EndRecursion ((gTP->RecursionCount > 0) ? --gTP->RecursionCount : (gTP->RecursionCount = 0))
#define ResetRecursion gTP->RecursionCount = 0
#define _TvalStackReset()\
    {for (gTP->TvalStackIdx = gTP->MaxTvalStackSize; gTP->TvalStackIdx > 0; gTP->TvalStack[--gTP->TvalStackIdx] = gCP->Tval_VOID);}
#define StartFrame\
    NUM __tvals__ = gTP->TvalStackIdx;NUM __tval__ = gTP->TvalStackIdx;		\
    NUM __objs__ =  gTP->ObjStackIdx; NUM __obj__ = gTP->ObjStackIdx;
#define EndFrame                                                            \
    {                                                                       \
    NUM cn;                                                                 \
    if(__tval__ > gTP->MaxTvalStackSize)									\
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_STACK);                     \
    for(cn=__tvals__;cn<__tval__;cn++)                                      \
        gTP->TvalStack[cn] = gCP->Tval_VOID;								\
    gTP->TvalStackIdx = __tval__;                                           \
    if(__obj__ > gTP->MaxObjStackSize)										\
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_FRAME_ERROR);				\
    for(cn=__objs__;cn<__obj__;cn++)                                        \
        *gTP->ObjStack[cn] = NIL;											\
    gTP->ObjStackIdx = __obj__;												\
	StartRecursion;															\
	ThrowOnEscape;															\
    }
#define DeclareWORD(var)				AISWORD* var = (AISWORD *)&gTP->TvalStack[__tval__++]
#define DeclareTVAL(var)				AISWORD* var = (AISWORD *)&gTP->TvalStack[__tval__++]
#define DeclareWORDArray(var,dim)		AISWORD* var = (AISWORD *)FSmartbase_InitWORDArray(gCP,gTP,&__tval__,dim)
#define DeclareTVALArray(var,dim)		AISWORD* var = (AISWORD *)FSmartbase_InitWORDArray(gCP,gTP,&__tval__,dim)
#define DeclareOBJ(type,var)            type* var = (type *)(gTP->ObjStack[__obj__++] = (OBJ *)((type*)&var))
#define DeclareOBJArray(type,var,dim)	type* var[dim] = {(type *)FSmartbase_InitOBJArray(gCP,gTP,&__obj__,(OBJ**)var,dim)}

#define FrameReset                  ThrowOnEscape;EndRecursion;gTP->TvalStackIdx = __tvals__;gTP->ObjStackIdx = __objs__
#define FrameExit(value)            {FrameReset; return(value);}
#define FrameReturn		            {FrameReset; return;}
#define LeaveOnError(value)			if (asTag(&value) == TYERROR) FrameReturn
#define ExitOnError(value)          if (asTag(&value) == TYERROR) FrameExit(value)
#define ReturnOnError(value)        if (asTag(&value) == TYERROR) return(value)


/*  Declare the direct C value to SmartLisp tagged value conversion macros. */
/*  Note:   These macros can be used to return SmartLisp tagged values for */
/*          direct inline C to SmartLisp function calls.  */
#define     TVOID           gCP->Tval_VOID
#define     TCHAR(c)        gCP->FSmartbase_CnvFromChar((POINTER)gCP,gTP,(char *)c)
#define     TBOOL(b)        gCP->FSmartbase_CnvFromBool((POINTER)gCP,gTP,(BOLE)b)
#define     TERROR(s)       gCP->FSmartbase_Error((POINTER)gCP,gTP,(char *)s)
#define		TPERROR(s)		gCP->FSmartbase_Perror((POINTER)gCP,gTP,(char *)s) /* make error permanent */
#define     TFUNCTION(f)    gCP->FSmartbase_MakeCFunction((POINTER)gCP,gTP,(LpFUNC)f)
#define     TPOINTER(p)     gCP->FSmartbase_CnvFromPtr((POINTER)gCP,gTP,(POINTER)p)
#define     TUINT(u)        gCP->FSmartbase_CnvFromUInt((POINTER)gCP,gTP,(UNUM)u)
#define     TINT(i)         gCP->FSmartbase_CnvFromInt((POINTER)gCP,gTP,(NUM)i)
#define     TOBJ(o)         gCP->FSmartbase_CnvFromObj((POINTER)gCP,gTP,(OBJ)o)
#define     TREAL(r)        gCP->FSmartbase_CnvFromReal((POINTER)gCP,gTP,(REAL)r)
#define     TFRAME(r)       gCP->FSmartbase_CnvToFrame((POINTER)gCP,gTP,(REAL)r)
#define     TSTRING(s)      gCP->FSmartbase_CnvFromText((POINTER)gCP,gTP,(char *)s)
#define     TSYMBOL(s)      gCP->FSmartbase_CnvToSymbol((POINTER)gCP,gTP,(char *)s)
#define     TQSYMBOL(s)     gCP->FSmartbase_CnvToQSymbol((POINTER)gCP,gTP,(char *)s)
#define     TGVALUE(s)      gCP->FSmartbase_GetSymbolValue((POINTER)gCP,gTP,(char *)s)

/* Define the test to determine whether the JIT is on. */
#define JITRUNNING (((gTP->DebugFlags & _FSmartbase_DEBUGON) == 0) && ((gTP->DebugFlags & _FSmartbase_TRACEON) == 0) && (gTP->DebugJitOn == TRUE) && (gTP->DebugJitAvailableForHost == TRUE))

/* Define the test to determine whether the automatic system self test is on . */
#define TESTON (gCP->FMemory_SelfTest == TRUE)

/*  **********************************************************  */
/*  Declare the built-in data types for the Smartbase engine.   */
/*  Note:   These are the fundamental types available for use   */
/*          in the Smartbase engine. All user defined types     */
/*          are constructed by combining these basic types.     */
/*  **********************************************************  */

								/* The following types describe Native types */
								/* whose value is contained entirely within  */
								/* immediate memory. These types are stored  */
								/* in a AISWORD(AISWORD) as immediate values. They */
								/* do NOT require HEAP space and do NOT      */
								/* require HEAP garbage collection.          */

#define TYVOID          0
#define TYTVAL          1
#define TYBOLE          2
#define TYCHAR          3
#define TYCFUNCTION     4
#define TYCOMPARE       5
#define TYDATE          6
#define TYELLIPSES      7
#define TYFRAME	        8
#define TYLEX           9
#define TYMONEY         10
#define TYNUM           11
#define TYUNUM          12
#define TYPCODE         13
#define TYPOINTER       14
#define TYREAL          15
#define TYREPOINSERT    16
#define TYSHORT         17
#define TYLONG			18
#define TYTYPE          19

								/* The following types are ONLY valid in the */
								/* DeclaredType field of the bindings in the */
								/* Av, Rv, Tv, Pv, & Cv elements of an Lambda */
								/* to be used only for strong typing by the  */
								/* DTRTL virtual machine compiler. They may  */
								/* NOT be found in the Tag field of a AISWORD.  */

#define TYCHARPOINTER	20
#define TYFLOAT         21
#define TYFLOATPOINTER	22
#define TYINTPOINTER	23
#define TYJUMPPOINTER	24
#define TYREALPOINTER	25
#define TYSHORTPOINTER	26
#define TYLONGPOINTER	27
#define TYWORDPOINTER	28

								/* The following types are the immediate     */
								/* repeating types with a non zero length    */
								/* whose value is an indexed reference into  */
								/* another object in AIS HEAP memory. They   */
								/* cannot be processed correctly by the      */
								/* vmregObjLength and the vmregObjPointer    */
								/* instructions */
#define TYBRICKFIELD	29
#define TYBRICKROW		30
#define TYSTRINGSUBSTR	31
#define TYMATRIXROW		32
#define TYNUMMATRIXROW  33

								/* The following type is the only native     */
								/* repeating type with a non zero length     */
								/* whose value is NOT a pointer into the     */
								/* AIS HEAP memory. Its type code must       */
								/* immediately preceed the HEAP object type  */
								/* codes so that the vmregObjLength and the  */
								/* vmregObjPointer instructions will be fast */
#define TYTEXT          34

								/* The following types describe Object types */
								/* whose value is a pointer into AIS HEAP    */
								/* memory. These types cannot be stored in   */
								/* a AISWORD(AISWORD) as can the Native types.     */
								/* All types, shown before this note, are    */
								/* Native types.                             */

#define TYLAMBDA		35
#define TYBITVECTOR     36
#define TYBYTEVECTOR    37
#define TYCMACRO        38
#define TYCONTINUATION  39
#define TYCPX           40
#define TYCPROCEDURE    41
#define TYCPXVECTOR     42
#define TYDICTIONARY    43
#define TYDIRECTORY     44
#define TYERROR         45
#define TYFLTVECTOR     46
#define TYGLOBALS       47
#define TYINTVECTOR     48
#define TYLABELSYMBOL   49
#define TYLONGVECTOR	50
#define TYMACRO         51
#define TYMATRIX		52
#define TYNUMMATRIX		53
#define TYNUMVECTOR     54
#define TYOBJ           55
#define TYOBJREPOSITORY 56
#define TYOBJVECTOR     57
#define TYPAIR          58
#define TYQUOTEDPAIR    59
#define TYQUOTEDSYMBOL  60
#define TYBRICK			61
#define TYSHORTVECTOR	62
#define TYSPECIALFORM   63
#define TYSTRING        64
#define TYSTRUCTURE		65
#define TYSYMBOL        66
#define TYVECTOR	    67
#define TYVMEVALUATOR   68
#define TYWORKSPACE     69
#define TYPCODEVECTOR   70
#define TYMAXVALIDTYPE  70


/*  The initialization structure containing Pointers to HOST supplied functions. */

struct  FSmartbase_HostCallBackFunctions
    {
	CHAR							ContextName[64];
    NUM     (*_Host_Display)        (LpXCONTEXT gCP, LpTHREAD gTP, char*  string,NUM  newline);
    NUM     (*_Host_Escape)         (LpXCONTEXT gCP, LpTHREAD gTP);
	NUM		(*_Host_UpdateState)	(LpXCONTEXT gCP, LpTHREAD gTP);
    NUM     (*_Host_OpenF)          (LpXCONTEXT gCP, LpTHREAD gTP, char* name, NUM mode, NUM type);
    NUM     (*_Host_ReadF)          (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, NUM length, char* bufptr);
    NUM     (*_Host_WriteF)         (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, NUM length, const char* bufptr);
    REAL    (*_Host_SeekF)          (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, REAL adjustment, NUM opcode);
    REAL    (*_Host_ResizeF)        (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, REAL newsize);
    NUM     (*_Host_CloseF)         (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, NUM opcode);
    NUM								_Host_memorySize;
    NUM								_Host_memoryObjHdrSize;
	NUM								_Host_MaxThreadCount;
    POINTER							_Host_memBlockPtr[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
	NUM								_Host_memBlockSize[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
    NUM								_Host_OperationsStackWords;
    NUM								_Host_GarbageStackWords;
	NUM								_Host_RequiredStackSpace;
	NUM								_MaxRecursions;
    BOLE                            _EmbeddedMySQLEnabled;
    };

typedef NUM  (*LpHOST_DISPLAY)      (POINTER gCP, LpTHREAD gTP, char*  string,NUM  newline);
typedef NUM  (*LpHOST_ESCAPE)       (POINTER gCP, LpTHREAD gTP);
typedef NUM  (*LpHOST_UPDATESTATE)	(POINTER gCP, LpTHREAD gTP);
typedef NUM  (*LpHOST_OPENF)        (POINTER gCP, LpTHREAD gTP, char*  name, NUM mode, NUM type);
typedef NUM  (*LpHOST_READF)        (POINTER gCP, LpTHREAD gTP, NUM  fileID,NUM length,char*  bufptr);
typedef NUM  (*LpHOST_WRITEF)       (POINTER gCP, LpTHREAD gTP, NUM  fileID,NUM length,const char*  bufptr);
typedef REAL (*LpHOST_SEEKF)        (POINTER gCP, LpTHREAD gTP, NUM  fileID,REAL adjustment,NUM  opcode);
typedef REAL (*LpHOST_RESIZEF)      (POINTER gCP, LpTHREAD gTP, NUM  fileID,REAL newsize);
typedef NUM  (*LpHOST_CLOSEF)       (POINTER gCP, LpTHREAD gTP, NUM  fileID,NUM opcode);

/*  API Object Disk Record Header declaration */

typedef struct  {
				CHAR	AisIdentifier[4];
				CHAR	VersionIdentifier[4];
                SHORT   recordVersion;
                CHAR    recordType;
                LONG    recordLength;
                REAL    SaveTimeStamp;
                } OBRECORDHEADER;

/*  API Function declarations */

PUBLIC  AISWORD*	FSmartbase_InitWORDArray	(LpXCONTEXT gCP,LpTHREAD gTP,NUM* stackIndexPtr,const NUM arraySize);
PUBLIC  OBJ**		FSmartbase_InitOBJArray		(LpXCONTEXT gCP,LpTHREAD gTP,NUM* stackIndexPtr,OBJ* variables[],const NUM arraySize);
PUBLIC  NUM			FSmartbase_CalculateStackSpace(NUM memorySize);
PUBLIC  LpXCONTEXT	FSmartbase_Init			(struct FSmartbase_HostCallBackFunctions Funcs);
PUBLIC  AISWORD		FSmartbase_Convert			(LpXCONTEXT gCP,LpTHREAD gTP,const TYPE targetType,const AISWORD oldValue);
PUBLIC  AISWORD		FSmartbase_CnvFromBool		(LpXCONTEXT gCP,LpTHREAD gTP,const BOLE inputBool);
PUBLIC  AISWORD		FSmartbase_CnvFromChar		(LpXCONTEXT gCP,LpTHREAD gTP,const CHAR inputChar);
PUBLIC  AISWORD		FSmartbase_CnvFromUInt		(LpXCONTEXT gCP,LpTHREAD gTP,const UNUM inputUInt);
PUBLIC  AISWORD		FSmartbase_CnvFromInt		(LpXCONTEXT gCP,LpTHREAD gTP,const NUM inputInt);
PUBLIC  AISWORD		FSmartbase_CnvFromPtr    	(LpXCONTEXT gCP,LpTHREAD gTP,const POINTER inputPtr);
PUBLIC  AISWORD		FSmartbase_CnvFromObj		(LpXCONTEXT gCP,LpTHREAD gTP,const OBJ obj);
PUBLIC  AISWORD		FSmartbase_CnvFromReal		(LpXCONTEXT gCP,LpTHREAD gTP,const REAL inputReal);
PUBLIC  AISWORD		FSmartbase_CnvToFrame		(LpXCONTEXT gCP,LpTHREAD gTP,const REAL inputReal);
PUBLIC  AISWORD		FSmartbase_CnvFromText		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputText);
PUBLIC  AISWORD		FSmartbase_CnvFromSubstring	(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputText,const NUM start,const NUM length);
PUBLIC  AISWORD		FSmartbase_CnvToSymbol		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputSymbol);
PUBLIC  AISWORD		FSmartbase_CnvToQSymbol		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputSymbol);
PUBLIC  AISWORD		FSmartbase_CnvToText		(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR buf, const NUM maxLen, const AISWORD source);
PUBLIC  NUM			FSmartbase_DebugOn			(LpXCONTEXT gCP,LpTHREAD gTP);
PUBLIC  BOLE		FSmartbase_DebugUntilExp	(LpXCONTEXT gCP,LpTHREAD gTP);
PUBLIC  AISWORD		FSmartbase_Eval				(LpXCONTEXT gCP,LpTHREAD gTP,const AISWORD proc,const NUM argc, ... );
PUBLIC  AISWORD		FSmartbase_Evals			(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const BOLE print);
PUBLIC  AISWORD		FSmartbase_Evalv			(LpXCONTEXT gCP,LpTHREAD gTP,const AISWORD proc, const NUM argc, const AISWORD argv[]);
PUBLIC  NUM			FSmartbase_GetObjectID		(LpXCONTEXT gCP,LpTHREAD gTP,const AISWORD objectTval);
PUBLIC  AISWORD		FSmartbase_GetSymbolValue	(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource);
PUBLIC  AISWORD*	FSmartbase_GetSymbolValuePtr(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const BOLE perm);
PUBLIC  AISWORD		FSmartbase_SetSymbolValue	(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const AISWORD newValue);
PUBLIC  void		FSmartbase_MarkAndSweep		(LpXCONTEXT gCP,LpTHREAD gTP);
PUBLIC  AISWORD		FSmartbase_recNumber		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const LpNUM iChar);
PUBLIC  AISWORD		FSmartbase_Ref				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... );
PUBLIC  AISWORD		FSmartbase_Refv				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc,const AISWORD argv[]);
PUBLIC  AISWORD		FSmartbase_RegisterCProcedure(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR funcSymbolName,const LpFUNC lpFunc);
PUBLIC  AISWORD		FSmartbase_RegisterVMEvaluator(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR funcSymbolName,const LpVMEVALUATOR lpFunc);
PUBLIC  AISWORD		FSmartbase_MakeCFunction	(LpXCONTEXT gCP,LpTHREAD gTP,const LpFUNC lpFunc);
PUBLIC  AISWORD		FSmartbase_SendMsg			(LpXCONTEXT gCP,LpTHREAD gTP,const AISWORD msg,const AISWORD target,const NUM argc, ... );
PUBLIC  AISWORD		FSmartbase_SendMsgv			(LpXCONTEXT gCP,LpTHREAD gTP,const AISWORD msg,const AISWORD target, const NUM argc, const AISWORD argv[]);
PUBLIC  AISWORD		FSmartbase_Set				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... );
PUBLIC  AISWORD		FSmartbase_Setv				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc,const AISWORD argv[]);
PUBLIC  AISWORD		FSmartbase_Throw			(LpXCONTEXT gCP,LpTHREAD gTP,const NUM ret);
PUBLIC  AISWORD		FSmartbase_Unimplemented	(void);
PUBLIC  AISWORD		FSmartbase_Writeln			(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... );
PUBLIC  HMemory		FSmartbase_NewMemory		(LpXCONTEXT gCP,LpTHREAD gTP,const NUM theSize);
PUBLIC  void		FSmartbase_FreeMemory		(LpXCONTEXT gCP,LpTHREAD gTP,HMemory theMemory);
PUBLIC  HMemory		FSmartbase_ResizeMemory		(LpXCONTEXT gCP,LpTHREAD gTP,HMemory theMemory,const NUM newSize);
PUBLIC  AISWORD		FSmartbase_Error			(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theMsg);
PUBLIC  AISWORD		FSmartbase_Perror			(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theMsg);
PUBLIC  POINTER		FSmartbase_ObjectPtr		(LpXCONTEXT gCP,LpTHREAD gTP,const LpWORD anObject);
PUBLIC  POINTER		FSmartbase_NilPtr			(void);
PUBLIC  HMemory		FSmartbase_NilHandle		(HMemory handle);
PUBLIC  NUM			FSmartbase_ObjectLen		(LpXCONTEXT gCP,LpTHREAD gTP,const LpWORD anObject);
PUBLIC  POINTER		FSmartbase_VectorPtr		(LpXCONTEXT gCP,LpTHREAD gTP,const AISWORD aVector);
PUBLIC  NUM			FSmartbase_VectorLen		(LpXCONTEXT gCP,LpTHREAD gTP,const AISWORD aVector);
PUBLIC  BOLE		FSmartbase_StringPtr		(LpXCONTEXT gCP,LpTHREAD gTP,const LpWORD aString,LpPOINTER strptr,LpNUM strlen);
PUBLIC  NUM			FSmartbase_StringLen		(LpXCONTEXT gCP,LpTHREAD gTP,const LpWORD aString);
PUBLIC  NUM			FSmartbase_ErrorTrace		(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch);
PUBLIC  AISWORD		FSmartbase_GlobalReferences	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,AISWORD argv[]);
PUBLIC  AISWORD		FSmartbase_LoadExtension	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,AISWORD argv[]);

/* engine state flag management functions */
PUBLIC  NUM			FSmartbase_InstructionTrace	(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch);
PUBLIC  NUM			FSmartbase_SystemCheck		(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch);
PUBLIC  NUM			FSmartbase_Jit				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch);
PUBLIC  NUM			FSmartbase_SetEngineFlags	(LpXCONTEXT gCP,LpTHREAD gTP,const NUM engineFlags);

PUBLIC TYPE			FSmartbase_NewType (LpXCONTEXT gCP,
										LpTHREAD gTP,
										TYPE   theType,
										LpCHAR aTypeName,
										CHAR flag,
										NUM aTypeSize,
										LpFNEW aNewFunction,
										LpFMARK aMarkFunction,
										LpFGMARK aGlobalMarkFunction,
										LpFCONVERT aCnvFunction,
										LpF2TVALS aCmpFunction,
										LpF3TVALS aSetIV1Function,
										LpF4TVALS aSetIV2Function,
										LpF5TVALS aSetIV3Function,
										LpF2TVALS aGetIV1Function,
										LpF3TVALS aGetIV2Function,
										LpF4TVALS aGetIV3Function,
										LpF2TVALS aMapFunction,
										LpF2TVALS aMapcFunction,
										LpFPRINT aPrintFunction,
										LpFNLOAD aLoadFunction,
										LpFNSAVE aSaveFunction,
										LpFCOMPUTESIZE aComputeSizeFunction,
										LpFCOPY	  aCopyFunction,
										LpFDOOM	  aDoomFunction);

PUBLIC  NUM FSmartbase_AlignMe			(LpXCONTEXT gCP,LpTHREAD gTP,NUM isize);
PUBLIC  void* FSmartbase_OperatorNew	(LpXCONTEXT gCP,LpTHREAD gTP,long objSize);
PUBLIC  AISWORD  FSmartbase_VectorDelete(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... );
PUBLIC  void  FSmartbase_MarkTval		(LpXCONTEXT gCP,LpTHREAD gTP,AISWORD aTval);
PUBLIC  void  FSmartbase_MarkTvalShallow(LpXCONTEXT gCP,LpTHREAD gTP,AISWORD aTval);

PUBLIC  LpNUM FSmartbase_FillWordArray(LpNUM var,LpNUM __tval__,NUM repeats);


/*  API Macro declarations */

#ifndef min
#define min(a,b)    (a < b ? a : b )
#endif
#ifndef max
#define max(a,b)    (a > b ? a : b )
#endif
#ifndef abs
#define abs(a)      (a < 0 ? -a : a )
#endif

#define _FSmartbase_Catch(sts,lbl)  {if ((sts = setjmp(gCP->FSmartbase_CatchEnv)) != 0) goto lbl;}
#define _FSmartbase_ECatch(xCP,sts,lbl)  {if ((sts = setjmp(xCP->FSmartbase_CatchEnv)) != 0) goto lbl;}
#define _FSmartbase_Throw(sts)      {longjmp(gCP->FSmartbase_CatchEnv,(sts));}

/*  File seek codes. */

#define _FSmartbase_fromStart       -1
#define _FSmartbase_fromCurPos       0
#define _FSmartbase_fromEOF      1

/*  File open codes. (To be added (ord) together). */

#define _FSmartbase_read        1
#define _FSmartbase_write       2
#define _FSmartbase_create      4
#define _FSmartbase_binary      8
#define _FSmartbase_text        16
#define _FSmartbase_perm        32

/*  Declare debug mode flags which are used to control execution during debugging */
// If you change these flags be sure to modify the corrosponding SBGLUE_ defines (/
// in asbglue.h
#define _FSmartbase_DEBUGON     0x01	/* 1st bit - ErrorTrace */
#define _FSmartbase_TRACEON     0x02	/* 2nd bit - InstructionTrace */
#define _FSmartbase_SYSCHECKON	0x04	/* 3rd bit - System Checking */
#define _FSmartbase_JITON		0x10	/* 4th bit - Just In Time Processing */
/* Note: Please reserve the 7th and 8th bit for use by the glue layer. */

#define _FSmartbase_TRACEASM    0x04	//Not used - TM, Remove?

/***********************************************************************************************/
/* Declaration of ALL object header definitions required for the creation of Smartbase objects */
/* Note1: All new object headers MUST be defined here.                                         */
/* Note2: (_FSmartbase_ObjectHeaderMaxSize) MUST reflect the largest possible object header	   */
/* Note3: On each new host computer these object layouts must be coordinated with a slot	   */
/*        size in the FMemory_FrameSize table in the Context so that no space is wasted.	   */
/***********************************************************************************************/

/* Note: Please keep this total size to an even multiple of eight bytes. */
/*       Current size of the maximum Object Header is sizeof(TDatabase) = 88 bytes on 32-bit systems. */
/*       Current size of the maximum Object Header is sizeof(TLambda) = 144 bytes on 64-bit systems. */
#ifdef _M32
#define _FSmartbase_ObjectHeaderMaxSize			sizeof(TDatabase)
#elif defined _M64
#define _FSmartbase_ObjectHeaderMaxSize			sizeof(TLambda)
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif

typedef struct TLambda
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
struct TStructure*		ClassVariables;			/* Sv: The Lambda's self variables structure (see OOP paradygm). */
struct TStructure*		ArgumentVariables;		/* Av: The Lambda's argument variables structure. */
struct TStructure*		TemporaryVariables;		/* Tv: The Lambda's temporary variables structure. */
struct TStructure*		PersistantVariables;	/* Pv: The Lambda's persistent variables structure (see AOP paradygm). */
struct TStructure*		ConstantVariables;		/* Cv: The Lambda's persistent  constant variables structure (see OOP paradygm). */
struct TStructure*		RegisterVariables;		/* Rv: The Lambda's register variable structure */
struct TPcodeVector*	PcodeVector;			/* Pv: The Lambda's pseudo codes vector. */
struct TObject*			DebuggerSource;			/* Sc: The Lambda's source code. */
struct TByteVector*		NativeCodeVector;		/* Nc: The Lambda's native code vector. */
struct TSymbol*			VirtualMachine;			/* Vm: The Lambda's virtual machine evaluator. */
struct TStructure*		Interfaces;				/* In: The Lambda's interfaces structure. */
NUM						EvalWhenDoomed;			/* EvalWhenDoomed: The Lambda's evaluate when doomed switch. */
NUM						InUse;					/* InUse: Count of the number of times the Lambda is actively invoked */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
} TLambda;

typedef struct TByteVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index of items in the array */
HMChar                  itsByteArray;           /* HMemory to variable length byte array */
AISWORD                 itsCdr;                 /* The Lisp tail(cdr) for the Array */
NUM                     itsMemoLength;			/* Memoized size of the text (used by appendWriteln function) */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TByteVector;
#define	_TByteVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TByteVector) + sizeof(NUM)))	// Must reserve fudge space

typedef struct TBitVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMChar					itsBitArray;            /* HMemory to variable length bit array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TBitVector;
#define	_TBitVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TBitVector) - sizeof(NUM)))

typedef struct TContinuation
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
AISWORD					itsResult;
BOLE					inScope;
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TContinuation;

typedef struct  TCpx
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
REAL					itsReal;				/* Real part of complex number */
REAL					itsImag;				/* Imaginary part of complex number */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TCpx;

typedef struct TCpxVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;		/* Allocated array size */
HMReal					itsCpxArray;			/* -> -> Array of pairs of doubles */
AISWORD					itsCdr;
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TCpxVector;
#define	_TCpxVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TCpxVector) - sizeof(NUM)))

typedef struct TDatabase
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
AISWORD					itsCodeKey;             /* Numeric key for encryption and compression, 0.0 for compression only. */
struct TString*			itsFileName;            /* File name of the Database archive disk file. */
struct TByteVector*		itsOdbID;               /* Database Vector obdID object (#void if file not open). */
struct TDirectory*		itsIndex;               /* Database Directory/Record index (#void if file not open). */
struct TVector*			itsBufferKeys;          /* Database buffer of previous keys accessed. */
struct TVector*			itsBufferValues;        /* Database buffer of previous items accessed. */
NUM						itsBaseFilePos;			/* Starting position in the database file. */
BOLE					itsTransactionOn;       /* Extended transaction in progress. */
BOLE					itsTreeOn;				/* Tree indexing strategy on. */
NUM						itsBufferCount;         /* Count of buffered items in ObjectRepository. */
NUM						itsBufferIndex;         /* Next buffered item to replace index. */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
} TDatabase;

typedef struct TDirectory
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMPBind					itsDirectoryArray;      /* The array of object/value Bondings */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Directory */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TDirectory;
#define	_TDirectory_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TDirectory) - sizeof(NUM)))

typedef struct TDictionary
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMBind					itsDictionaryArray;     /* The array of object/value Bondings */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Dictionary */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TDictionary;
#define	_TDictionary_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TDictionary) - sizeof(NUM)))

typedef struct TError
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of variable length data */
HMChar					itsCString;             /* HMemory to variable length data */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TError;
#define	_TError_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TError) + sizeof(NUM)))	// Must reserve fudge space

typedef struct  TFltVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMFloat					itsFloatArray;          /* HMemory to variable length float array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TFltVector ;
#define	_TFltVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TFltVector) - sizeof(NUM)))

typedef struct TIntVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMInt					itsIntArray;            /* HMemory to variable length object array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TIntVector;
#define	_TIntVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TIntVector) - sizeof(NUM)))

typedef struct TLongVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMLong					itsLongArray;			/* HMemory to variable length object array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TLongVector;
#define	_TLongVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TLongVector) - sizeof(NUM)))

typedef struct TMatrix
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;		/* Maximum index of items in the array		*/
HMTval					itsTvalMatrix;			/* HMemory to variable length item array	*/
AISWORD					itsCdr;					/* The Lisp tail(cdr) for the Matrix		*/
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
NUM						itsRank;				/* The rank of the array					*/
NUM						itsDimensions[_MAXDIMENSIONS];	/* The number of items in each dimension	*/
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TMatrix;
#define	_TMatrix_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TMatrix) - sizeof(NUM)))


typedef struct TNumMatrix
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;		/* Maximum index of items in the array		*/
HMReal					itsRealMatrix;			/* HMemory to variable length real array	*/
AISWORD					itsCdr;					/* The Lisp tail(cdr) for the Matrix		*/
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
NUM						itsRank;				/* The rank of the array					*/
NUM						itsDimensions[_MAXDIMENSIONS];	/* The number of items in each dimension	*/
CHAR					itsImmediateSpace[sizeof(NUM)];	/* Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TNumMatrix;
#define	_TNumMatrix_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TNumMatrix) - sizeof(NUM)))

typedef struct  TNumVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMReal					itsRealArray;           /* HMemory to variable length number array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TNumVector;
#define	_TNumVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TNumVector) - sizeof(NUM)))

typedef struct TObject
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
}TObject;

typedef struct TObjVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMObject				itsObjectArray;         /* HMemory to variable length object array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TObjVector;
#define	_TObjVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TObjVector) - sizeof(NUM)))

typedef struct TPair
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
AISWORD					itsCar;
AISWORD					itsCdr;
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TPair;

typedef struct TPcodeVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMInt					itsInstructionArray;    /* HMemory to variable length object array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
NUM						itsCurItemIndex;        /* Used in append to improve allocation scheme */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TPcodeVector;
#define	_TPcodeVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TPcodeVector) - sizeof(NUM)))

typedef struct TBrick
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index of items in the array (the byte count of the Brick data area) */
HMChar                  itsFieldArray;          /* HMemory to variable length byte array */
AISWORD                 itsCdr;                 /* The Lisp tail(cdr) for the Array */
AISWORD					itsFieldList;			/* Definition of fields (name, type, offset, repeats) in record byte array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
NUM                     itsFieldCount;			/* The number of fields per row (same as itsFieldList->itsMaxItemIndex) */
NUM                     itsRowCount;			/* The number of rows per record */
NUM                     itsRowByteCount;		/* Maximum byte count in a row (the byte count of each row in the Brick object) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TBrick;
#define	_TBrick_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TBrick) - sizeof(NUM)))

/* NOTES:
 * A reference to the Brick object is required.
 */
typedef struct TBrickRow
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Must be set to the number of Fields in the indexed Brick row */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
TBrick*                 itsBrickObj;            /* Brick object pointer */
NUM                     itsRowIndex;            /* Brick row index within the Brick object pointer (defined above)*/
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TBrickRow;
#define	_TBrickRow_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TBrickRow) - sizeof(NUM)))

/* NOTES:
 * A reference to the Brick object is required.
 */
typedef struct TBrickField
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Must be set to the number of repeats in the indexed Brick field */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
TBrick*                 itsBrickObj;            /* Brick object pointer */
NUM                     itsRowIndex;            /* Brick row index within the Brick object pointer (defined above)*/
NUM                     itsFieldIndex;          /* Brick field index within the Brick object pointer (defined above)*/
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TBrickField;
#define	_TBrickField_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TBrickField) - sizeof(NUM)))

typedef struct TShtVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMShort					itsShortArray;          /* HMemory to variable length object array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TShtVector;
#define	_TShtVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TShtVector) - sizeof(NUM)))

typedef struct TSymbol
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index of variable length data */
HMChar                  itsCString;             /* HMemory to variable length data */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
LpFUNC                  itsCProcedure;			/* Pointer to its built-in C function (if available) */
AISWORD                 itsGlobalValue;         /* The global value assigned to this symbol (if available) */
unsigned long           itsGlobalLock : 1;      /* The global locking switch for this symbol (1 if locked) */
unsigned long           itNeedsBars   : 1;      /* The global needs-bars switch for this symbol (1 if bars are needed in display) */
struct TLambda*         itsVMEmulator;          /* Present if this symbol is an Lambda Virtual Machine */
struct TSymbol*         itsUserTypeParent;      /* Present if a user type (see defstruct) */
struct TStructure*		itsUserTypeFields;      /* Present if a user type (see defstruct) */
struct TDictionary*		itsUserTypeMethods;     /* Present if a user type (see defstruct) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
} TSymbol;
#define	_TSymbol_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TSymbol) + sizeof(NUM)))	// Must reserve fudge space

typedef struct TString
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of variable length data */
HMChar					itsCString;             /* HMemory to variable length data */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TString;
#define	_TString_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TString) + sizeof(NUM)))	// Must reserve fudge space

typedef struct TStructure
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMBind					itsDictionaryArray;     /* The array of object/value bindings */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Structure */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
TSymbol*				itsMethods;       /* The Lisp user type (see defstruct) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TStructure;
#define	_TStructure_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TStructure) - sizeof(NUM)))

typedef struct TVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMTval					itsTvalArray;           /* HMemory to variable length item array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Vector */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
struct TObjVector*		itsAttributes;          /* The column attributes for the Vector (optional) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TVector;
#define	_TVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TVector) - sizeof(NUM)))

typedef struct TWorkspace
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMObject				itsObjectArray;         /* HMemory to variable length object array */
AISWORD					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TWorkspace;
#define	_TWorkspace_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TWorkspace) - sizeof(NUM)))


#define ExitWhenError(value)  if (value.Tag == TYERROR)\
                                 if (gTP->TvalStack[ErrorLambdaTval].Tag != TYVOID)\
									{\
									 value = FSmartbase_Evalv(gCP,gTP,gTP->TvalStack[ErrorLambdaTval],1,&value);\
							         FrameExit(value)\
									}\
								  else\
							         FrameExit(value)

#define ThrowOnEscape	if (++gTP->FVmscript_escapeCheck >= _FSMARTBASE_ESCAPECHECK) {gTP->FVmscript_escapeCheck = 0;if ((*gCP->_Host_Escape)((POINTER)gCP,gTP)) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);}

#endif


