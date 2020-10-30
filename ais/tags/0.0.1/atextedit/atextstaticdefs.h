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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/atextstaticdefs.h
													Text Edit Static Definitions
This language definition file initializes the common static variables used by each instance of the editor.

CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/10/2008	fchua	Removed association with Find, Replace, Go and Function List dialogs.
2.0004	 2/17/2007	tlw		Add quote characters to syntax highlighting.
1.0109	10/ 5/2006	tlw		dumpRow. Added in scpStates, scpTxtBg, scpTxtFg
1.0070	12/ 5/2005	tlw		Change globals to static.
1.0060	 5/ 3/2005	tlw		Add scDefaultFont to configurable parameters. Remove scDefMap.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
NOTES
This file should be included just once at file scope in atextedit.cpp.  It defines a structure and an array used exclusively
to define the syntax highlighting for each language supported by this editor.  To add a new language:
 1.	Create a new header file that defines the elements of the ALangDef structure.  Use aiscpp.h as a template.
 2.	Add a new include before definition of scpLangdefs below.
 3.	Add and entry to scpALangDefs list of pointers below.
 4.	Increase the defined constant ATED_MAXLANG by one.

Changing colors:
 1.	To modify a pen (foreground) color, just change the color in msPenColor list.
 2.	To modify a background color, just change the color in msBgColor list.

Two restrictions on the colors:
 1.	The string color is ignored in detecting color changes during display, so scMaskColorChange must mask out the shifted
	string color code (x0800).
 2. ParenMatcher ignores brackets if in-string or in-comment. It uses shiftedPenColor >= scPenComment to check current token
	state, so shifted pen color codes for both comments and strings must be at or above scPenComment.

Note: atextstaticdefs.h is unusual header in that it contains static data definitions (not just declarations).

SYNTAX HIGHLIGHTING
The language definitions are found in ais*.h, one for each supported language.  The header contains a set of 5 elements:
 1.	The first element contains the file extensions for the document. Separate extensions with commas. Extensions are case
	insensitive.
 2. The second element contains one or two characters that define the string delimiters (quote characters).
 3.	The third element contains one or two pairs of open/close comment delimiters. Separate delimiters with commas.  One char
	delimiters must be a semicolon or a newline; else, the second char must be /, *, !, or >.  Any new delimiters will require
	some change to updateForeground(). This optimization is done for performance. 
 4. The fourth element contains the regular expression identifying a function. Surround the portion you want displayed (such
	as the function name) with unescaped parentheses. The pattern is not case sensitive.  Use two backslashes to escape special
	characters.
 5.	The succeeding lines contain the keywords for this language.  Keywords are case sensitive.

Process.
 1. Each ais*.h file defines an ALangDef structure (see atextedit.h) containing 5 pointers to strings.
 2. Below, scpLangDefs (see atextedit.h) is set to an array of pointers to these structures.
 3. ATextEdit::initializeStaticDefs uses scpLangDefs to initialize the array of ALanguage structures scLanguages[].
 4. scLanguages is used by ATextRow::setForeground and elsewhere to perform syntax highlighting for the current language.

Character types. Characters are divided into several categories enumerated in AChType.
eAlpha - In-word character A-Z a-z underscore.
eSep - Punctuation characters other than the following types
eQuote1 - Quote character (usually double quote)
eQuote2 - Alternate quote character (usually apostrophe)
eBeg1 - Beginning comment delimiter (usually // or ;)
eEnd1 - Ending comment delimiter (usually \n)
eBeg2 - Alternate beginning comment delimiter (usually / *)
eEnd2 - Alternate ending comment delimiter (usually * /)

Index	Lang	Quote1	Quote2	Beg1	End1	Beg2	End2	Escape
0		Text
1		Html	 "				<!		->
2		Cpp		 "		  '		//		\n		/ *		* /		\
3		Java	 "		  '		//		\n		/ *		* /		\
4		Lisp	 "		  {		;		\n						\ (" only)

Note that Quote, comment delimiter, and Escape are single characters.  Beg and End are 1 or 2 character strings.
Empty entries contain a null string.

Comment Delimiter.  The comment prefix, Beg1, is used to comment out a block of selected code.

Escape Character.  In some languages, a quote character in a string can be escaped using a backslash.  For example,
"foo \" bar".  The escape character, if any, is noted by the character type Escape (usually a backslash).

For performance reasons, the code is somewhat dependent on the choice of delimiters shown in the above table.  See
ATextRow::setForeground for details on how the character type is set.

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#include "aglobaldefs.h"	// AGBL_

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
// Global static data definitions:

// Default Configuration Parameters:
const long		ATextEdit::scDefaultLeftOffset = 2;		// Offset 2[pixels] of  row from left edge of viewport(drawText)
const QString	ATextEdit::scDefaultFont ="Courier New";// Courier, Courier New, Fixedsys, Lucida Console, Terminal
const long		ATextEdit::scDefaultFontSize = 10;		// [points] 8-12,14,16,18,20 (72 pts/inch)
const long		ATextEdit::scDefaultMaxRowWidth = 1024;	// 128 - 4096
const long		ATextEdit::scDefaultMaxUndoDepth =1024;	// 0 - 2048
const long		ATextEdit::scDefaultTabWidth = 4;		// Tab stop every 4 chars (1-32)

// Defined Constants (not user settable):
const QString	ATextEdit::scApplication = "ATextEdit";	// Name of this application (ATextEdit)			
const long		ATextEdit::scBfrInc = 16;				// Minimum increment when resizing a buffer [ushort]
const char		ATextEdit::scCarriageReturn = '\252';	// Displayed carriage-return char (super a)
const char		ATextEdit::scCharDel = '|';				// Visible DEL (7f) char, vertical bar
const char		ATextEdit::scCharNewline = '\254';		// Visible newline char, right hook
const char		ATextEdit::scCharTab = '\327';			// Visible tab char, small x
const ushort	ATextEdit::scLatin1Space = 0x00a0;		// Latin1 space char
const long		ATextEdit::scMaxMouseMoves = 6;			// Mouse move events per update of display
const QString	ATextEdit::scOrganization = "InvestByLambda.com";// Name of our organization
const char*		ATextEdit::scpParenClose = " )>]}";		// Array of closing bracket chars
const char*		ATextEdit::scpParenOpen = " (<[{";		// Array of open bracket chars
const char*		ATextEdit::scpStates[] = {"Comment1", "Comment2", "Std", "Stg1", "Stg2", "InWord", "Same"};
const char*		ATextEdit::scpTxtBg[] = {"B","H","M"};	// Background identifiers: Base,Highlighted,Matched
const char*		ATextEdit::scpTxtPen[]={"T","H","K","C","S"};// Foreground identifiers: Text,High,Keyword,Comment,String
const char*		ATextEdit::scpUndoTypes[] = {"None","DelFwd","DelBack","InsFwd","InsBack","MoveFwd","MoveBack"};
const long		ATextEdit::scNTypes = 7;				// Number of undo types.
const char*		ATextEdit::scpUntitled = "Untitled";	// New (anonymous) file name
const char*		ATextEdit::scpVersion = AGBL_AISVERSION;// Current build label

// Language Info. Create list of ptrs to each language description.
#include "aiscpp.h"
#include "aishtml.h"
#include "aisjavascript.h"
#include "aislisp.h"
#include "aistext.h"
// NOTE: the order of LangDefs must correspond to entries in ALanguage
ALangDef* ATextEdit::scpLangDefs[] = {&scTextDef, &scHtmlDef, &scCppDef, &scJavascriptDef, &scLispDef, NULL};
#define ATED_MAXLANG  5

// Extensions. Key: extension, Value: Language index. Look up language index for each file extension
ALanguageMap  ATextEdit::scFileExts;
// Languages. Array of data structures extracted from language info
ALanguage ATextEdit::scLanguages[ATED_MAXLANG];

// Finite-state-machine. Next state given char type and current state
uchar ATextEdit::scNextState[][8] = {
//	 Alpha	   Sep       Quote1    Quote2    Beg1      End1      Beg2      End2	  CharType/CurState
	{eComment1,eComment1,eComment1,eComment1,eComment1,eStd,     eComment1,eComment1},  // Comment1
	{eComment2,eComment2,eComment2,eComment2,eComment2,eComment2,eComment2,eStd     },  // Comment2
	{eInWord,  eStd,     eString1, eString2, eComment1,eStd,     eComment2,eStd     },  // Standard
	{eString1, eString1, eStd,     eString1, eString1, eString1, eString1, eString1 },  // String1
	{eString2, eString2, eString2, eStd,     eString2, eString2, eString2, eString2 },  // String2
	{eInWord,  eStd,     eString1, eString2, eComment1,eStd,     eComment2,eStd     }   // Word
};

// Brackets. Index into ParenClose and ParenOpen arrays for all characters. Vales correspond to offsets in scParen arrays
char ATextEdit::scBracket[] = {
//		  0 1 2 3 4 5 6 7 8 9 a b c d e f  0 1 2 3 4 5 6 7 8 9 a b c d e f 
/*  0 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/* 20 */  0,0,0,0,0,0,0,0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,-2,0,// () <>
/* 40 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,3,0,-3,0,0,// []
/* 60 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,4,0,-4,0,0,// {}
/* 80 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/* a0 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/* c0 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/* e0 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

// InWord. For fast look-up of in-word characteristic for all characters.
uchar ATextEdit::scInWord[] = {
//		  0 1 2 3 4 5 6 7 8 9 a b c d e f  0 1 2 3 4 5 6 7 8 9 a b c d e f 
/*  0 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/* 20 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,	// 0-9
/* 40 */  0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,	// A-Z, _
/* 60 */  0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,	// a-z
/* 80 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/* a0 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/* c0 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/* e0 */  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

// InWord. For fast look-up of lower case characters.
uchar ATextEdit::scLower[] = {
0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,
0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,
0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f,
0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f,
0x40, 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k','l', 'm', 'n', 'o',
 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',0x5b,0x5c,0x5d,0x5e,0x5f,
0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,
0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7a,0x7b,0x7c,0x7d,0x7e,0x7f,
0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8a,0x8b,0x8c,0x8d,0x8e,0x8f,
0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9a,0x9b,0x9c,0x9d,0x9e,0x9f,
0xa0,0xa1,0xa2,0xa3,0xa4,0xa5,0xa6,0xa7,0xa8,0xa9,0xaa,0xab,0xac,0xad,0xae,0xaf,
0xb0,0xb1,0xb2,0xb3,0xb4,0xb5,0xb6,0xb7,0xb8,0xb9,0xba,0xbb,0xbc,0xbd,0xbe,0xbf,
0xc0,0xc1,0xc2,0xc3,0xc4,0xc5,0xc6,0xc7,0xc8,0xc9,0xca,0xcb,0xcc,0xcd,0xce,0xcf,
0xd0,0xd1,0xd2,0xd3,0xd4,0xd5,0xd6,0xd7,0xd8,0xd9,0xda,0xdb,0xdc,0xdd,0xde,0xdf,
0xe0,0xe1,0xe2,0xe3,0xe4,0xe5,0xe6,0xe7,0xe8,0xe9,0xea,0xeb,0xec,0xed,0xee,0xef,
0xf0,0xf1,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7,0xf8,0xf9,0xfa,0xfb,0xfc,0xfd,0xfe,0xff
};

// Dialogs:
ATabifyDialog*		ATextEdit::scpTabifyDialog = NULL;
ATabWidthDialog*	ATextEdit::scpTabWidthDialog = NULL;
ATestDialog*		ATextEdit::scpTestDialog = NULL;

// Colors:
// All odd entries will display white text, including Select.
const QString	scBgColorNames(
		// Std		Select	Matched
		 "#ffffff,#5070d0,#e8e8e8");
		// white   DkBlue  Gray
#define ATED_BGCOLORS  3

const QString	scPenColor(
		// Std	  Select  Keyword Comment String
		"#000000,#ffffff,#0000ff,#008000,#000000");
		//Black  White BrightBlue MedGreen Black
#define ATED_PENCOLORS  5

// See also initLanguageDefs()
QBrush			ATextEdit::scBgBrushes[ATED_BGCOLORS];
QColor			ATextEdit::scPenColors[ATED_PENCOLORS];
bool			ATextEdit::scInitDefs = false;

// Color codes shifted left:
const ushort	ATextEdit::scBgPlainText = 0;
const ushort	ATextEdit::scBgSelText  = 0x1000;
const ushort	ATextEdit::scBgMatchText= 0x2000;
const ushort	ATextEdit::scPenSelText = 0x100;
const ushort	ATextEdit::scPenKeyword = 0x200;
const ushort	ATextEdit::scPenComment = 0x300;
const ushort	ATextEdit::scPenString  = 0x400;

// Color code masks:
// Restrictions:
//	1. scBgSelText is never included in a char. Only added in paint routine iff scBgSelText set, so don't lose the old color.
//	2. PenComment and PenString are last. Paren matcher only counts parens if char is not in a comment or a string.
//	3. PenString must use upper foreground bits only. It is used for matching but is not used in display (fewer colors improves performance).
const ushort	ATextEdit::scMaskBg = 0xf000;		// Bits for background color indices
const ushort	ATextEdit::scMaskChar = 0x00ff;		// Bits for character
const ushort	ATextEdit::scMaskColor = 0xff00;	// Bits for background and foreground color indices
const ushort	ATextEdit::scMaskDisplay = 0xf300;	// Bits for colors used in display (string color not displayed)
const ushort	ATextEdit::scMaskPen = 0x0f00;		// Bits for foreground color indices

// end
