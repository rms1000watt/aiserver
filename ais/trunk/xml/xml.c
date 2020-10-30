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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/xml/xml.c
										XML

CHANGE HISTORY
Version	Date		Who		Change
1.0057	3/18/2005	tlw		Update documentation
							---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	----------------------------------------- IMPORTS ---------------------------------------------
#include <windows.h>
#include <time.h>
#include <stdlib.h>
#include <ctype.h>
#include "fsmtbase.h" // SmartBase engine declarations
#include "xml.h"

//	--------------------------------------- DEFINITIONS -------------------------------------------

//	----------------------------------- FUNCTION DEFINITIONS --------------------------------------
BOOL APIENTRY DllMain( HANDLE hModule, 
                      DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{

    return TRUE;
}

/*-----------------------------------------------------------------
registerFunctions
Arguments:
	none
Returns:
	result
------------------------------------------------------------------*/
PUBLIC LPCSTR __getLibName () {
	LPCSTR name = "xml Version 1.0";
	return (name);
	}

PUBLIC TVAL __registerLibFunctions(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) {
	StartFrame
	TVAL		ec;
	EndFrame
	ec = FXml_Init(gCP,gTP);
	FrameExit(ec);
	}

#if 0
FXml.c

This file contains some of the procedures required to support 
the Smartbase "xml" compiler function. This is an actual compiler 
not a parser replacement for the "lisp" function. The output of
this function is a complete XML data model.

PARENT:

AUTHORS:            Michael F. Korns

MODIFICATIONS:

#endif


/* Static parser variables for use by all parser functions. */

static TVAL			gLast;			/* The last symbol for appending elements to a Structure. */
static TVAL			gNewStructure;	/* The make a new Structure function. */
static TVAL			gAppend;        /* The append function */
static TVAL			gMoreSource;	/* The moreSource symbol */
static TVAL			gXml;		    /* The xml symbol */
static TVAL			gStartDocument;	/* The startDocument message symbol */
static TVAL			gEndDocument;	/* The endDocument message symbol */
static TVAL			gStartElement;	/* The startElement message symbol */
static TVAL			gEndElement;	/* The endElement message symbol */
static TVAL			gCharacters;	/* The characters message symbol */
static TVAL			gComments;		/* The comments message symbol */
static TVAL			gErrorMessage;	/* The errorMessage message symbol */
static TVAL			gIgWhitespace;	/* The ignorableWhitespace message symbol */
static TVAL			gProcessInstr;	/* The processingInstruction message symbol */
static TVAL			gDoctypeDef;	/* The doctypeDefinition message symbol */


/*--------------------------------------------------------------------------------------- */
#if 0

FXml_Init

Initialize the XML compiler portion of the Smartbase Function Library.

#endif

TVAL FXml_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ec);

EndFrame

if (gCP->FXml_Initialized) 
		FrameExit(gCP->TObject_OK);

gCP->FXml_Initialized = TRUE;

/* Register the Smartbase functions contained in this package */
Stack(ec) = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"xml",(LpFUNC)&FXml_Compiler);

ExitOnError(Stack(ec));
 
FrameExit(gCP->Tval_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FXml_Compiler

	(gCP,gTP,xml  [document] inputString [eventLambda/piStructure])

			-or-

	(xml  [document] inputLambda [eventLambda/piStructure])


The xml Function implements an XML compiler via the method of recursive 
descent. The xml function is a complete XML compiler and not a parser
replacement for the lisp or javaScript functions. The result of running
the xml function on an XML input string is a completed XML document model.

The XML document model is a recursive Structure with the data inside the 
XML document addressable by attribute as well as by numeric index. For 
example the following XML input string:


	<?xml version = '1.0' standalone='yes' encoding = 'hello' ?>
	<!-- This is a dynamic name and address example -->
	<Address FirstName = "yes" LastName = 'yes'> 
	   This is just general content for the Address element.
	   <FirstName>Michael</FirstName>
	   <LastName>Korns</LastName>
	   <Street>214 Shorebreaker</Street>
	   <City>Laguna Niguel</City>
	   <State>California</State>
	   This is more content for the Address element.
	</Address>


Returns the following XML document model:


	#{
	  __attlist: #{version: '1.0' standalone: 'yes' encoding: 'hello'}
	  Address: #{
				 __attlist: #{FirstName: "yes" LastName: 'yes'}
				 __content: "This is just general content for the Address element."
				 FirstName: "Michael"
				 LastName:  "Korns"
				 Street:    "214 Shorebreaker"
				 City:      "Laguna Niguel"
				 State:     "California"
				 __content: "This is more content for the Address element."
				}
	 }


Notes: Notice how the terminal nodes of the Structure are all singletons, 
       while the intermediate nodes of document Structure are recursive element
       Structures with attributes and values. Finally, notice how the various
	   parts of the document model can be referenced either by attribute or
       by element index number:

            document.Address.FirstName == "Michael"
                    
                              -or-

            document[4][2] == "Michael"


The XML document model also handles multiple elements with the same tag 
using the simple expedient that only the first (of non-unique) element can
be referenced by its tag, all others must be referenced by numeric index. 
For example the following XML input string:

	<?xml version="1.0"?>
	<poem>
	<line>Roses are red,</line>
	<line>Violets are blue.</line>
	<line>Sugar is sweet,</line>
	<line>and I love you.</line>
	</poem>


Returns the following XML document model:


	#{
	  __attlist: #{version: "1.0"}
	  poem: #{
			  line: "Roses are red,"
			  line: "Violets are blue."
			  line: "Sugar is sweet,"
			  line: "and I love you."
			 }
	 }


Notes: Notice how the terminal nodes of the Structure are all singletons, 
       while the intermediate nodes of document Structure are recursive element
       Structures with attributes and values. Finally, notice how the first
	   "line" element can be referenced either by name or by index and all
       other "line" elements can only be referenced by index number:

            document.poem.line == "Roses are red,"
                    
                              -or-

            document.poem[0] == "Roses are red,"
                    
                              -or-

            document.poem[2] == "Sugar is sweet,"


#endif

TVAL FXml_Compiler(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM				inputArgumentIndex = 0;
NUM				eventArgumentIndex = 1;
NUM				piArgumentIndex = 1;
XMLWork				x;
XMLWork*			work = &x;
StartFrame
DeclareTVAL(sLast);
DeclareTVAL(sNewStructure);
DeclareTVAL(sAppend);
DeclareTVAL(document);
DeclareTVAL(eventLambda);
DeclareTVAL(piStructure);
DeclareTVAL(contents);
DeclareTVAL(attributes);
DeclareTVAL(name);
DeclareTVAL(ename);
DeclareTVAL(value);
DeclareTVAL(last);
DeclareTVAL(xml);
DeclareTVAL(tmp);
DeclareTVAL(ec);
DeclareTVAL(moreSource);
DeclareTVAL(startDocument);
DeclareTVAL(endDocument);
DeclareTVAL(startElement);
DeclareTVAL(endElement);
DeclareTVAL(characters);
DeclareTVAL(comments);
DeclareTVAL(errorMessage);
DeclareTVAL(IgWhitespace);
DeclareTVAL(ProcessInstr);
DeclareTVAL(DoctypeDef);
EndFrame


/* Initialize the global symbols and protect them from garbage collection. */
/* Note: These symbols must be protected from garbage collection. If not, */
/*       they will be collected intermittantly and this will cause bugs */
/*       which will be intermittant and very difficult to track down. */

Stack(sLast) = gLast = TSYMBOL("last");
Stack(sNewStructure) = gNewStructure = TGVALUE("makeStructure");
Stack(sAppend) = gAppend = TGVALUE("append");
Stack(moreSource) = gMoreSource = TSYMBOL("moreSource");
Stack(xml) = gXml = TSYMBOL("xml");
Stack(startDocument) = gStartDocument = TSYMBOL("startDocument");
Stack(endDocument) = gEndDocument = TSYMBOL("endDocument");
Stack(startElement) = gStartElement = TSYMBOL("startElement");
Stack(endElement) = gEndElement = TSYMBOL("endElement");
Stack(characters) = gCharacters = TSYMBOL("characters");
Stack(comments) = gComments = TSYMBOL("comments");
Stack(errorMessage) = gErrorMessage = TSYMBOL("errorMessage");
Stack(IgWhitespace) = gIgWhitespace = TSYMBOL("ignorableWhitespace");
Stack(ProcessInstr) = gProcessInstr = TSYMBOL("processingInstruction");
Stack(DoctypeDef) = gDoctypeDef = TSYMBOL("doctypeDefinition");

/* Initialize the global flags and variables. */

work->inputString = gCP->Tval_VOID;
work->inputProvider = FALSE;
work->inputLambda = gCP->Tval_VOID;
work->eventHandler = FALSE;
work->eventLambda = gCP->Tval_VOID;
work->piHandler = FALSE;
work->piStructure = gCP->Tval_VOID;
work->htmlOn = FALSE;

/* Is the optional xml document argument present? */

if ((argc >= 1) && (argv[0].Tag == TYSTRUCTURE))
	{
	/* Save the XML document object which will be the result of this compilation. */
	inputArgumentIndex = 1;
	eventArgumentIndex = 2;
	piArgumentIndex = 2;
	Stack(document) = argv[0];
	work->Document = Stack(document);
	}
else
	{
	/* Create the empty XML document object which will be the result of this compilation. */
	Stack(document) = FSmartbase_Eval(gCP,gTP,gNewStructure,0);
	ExitOnError(Stack(document));
	work->Document = Stack(document);
	}

/* Identify the input string from the input argument. */

if ((argc > inputArgumentIndex) && (argv[inputArgumentIndex].Tag == TYLAMBDA))
	{
	/* The input argument is an input provider Lambda or host object. */
	work->inputLambda = argv[inputArgumentIndex];
	work->inputProvider = TRUE;
	/* Send the moreSource message symbol to the inputLambda. */
    /* Note: we do this only if we are in event handler mode. */
	Stack(tmp) = FSmartbase_SendMsg(gCP,gTP,gMoreSource,work->inputLambda,0);
	ExitOnError(Stack(tmp));
	work->inputString = Stack(tmp);
	}
else
	{
	/* The input argument is a simple string value. */
	work->inputString = argv[inputArgumentIndex];
	}

/* Isolate the input string pointer and validate the argument. */

if ((argc <= inputArgumentIndex) || (!FSmartbase_StringPtr(gCP,gTP,(LpTVAL)&work->inputString,(LpPOINTER)&work->IP,(LpNUM)&work->INLEN)))
	{
    Stack(ec) = TERROR("!xml: invalid argument list!");
    FrameExit(Stack(ec));
	}
else
/* Store the event Lambda or piStructure (if any is present). */

if (argc > eventArgumentIndex)
	{
	/* If the argument is a Structure, then we have a piStructure. */

	if (argv[piArgumentIndex].Tag == TYSTRUCTURE)
		{
		work->piHandler = TRUE;
		work->piStructure = Stack(piStructure) = argv[piArgumentIndex];
		}
	else
	/* If the argument is not a Structure, then we have an eventHandler. */
		{
		work->eventHandler = TRUE;
		work->eventLambda = Stack(eventLambda) = argv[eventArgumentIndex];
		}
	}

/* Create the XML document object which will be the result of this compilation. */
/* Note: If we are using an event handler, we do not create a document. */

if (work->eventHandler)
	{
	/* Send the startDocument message symbol to the eventLambda. */
	work->Document = gCP->Tval_VOID;
	Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gStartDocument,work->eventLambda,0);
	ExitOnError(Stack(ec));
	}

/* Initialize the input string pointer and the starting input pointer. */

work->past = 0;
work->start = work->IP;

/* Recognize the content of the XML document. */

Stack(contents) = FXml_Contents(gCP,gTP,work->Document,work,0);
ExitOnError(Stack(contents));

/* There should be no more content left. */

if ((*work->IP) != 0) FrameExit(FXml_Error(gCP,gTP,"Expected end of file",work));


/* Return the XML document object resulting from this compilation. */
/* Note: If we are using an event handler, we send the endDocument message. */

if (work->eventHandler)
	{
	/* Send the endDocument message symbol to the eventLambda. */
	Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gEndDocument,work->eventLambda,0);
	ExitOnError(Stack(ec));
	}
FrameExit(Stack(document));
}


/*--------------------------------------------------------------------------------------- */
#if 0

FXml_Contents

This Function recognizes the contents of XML elements. These 
can be either normal elements or process instruction elements 
For example:


	<?javaScript writeln("Hello World!"); ?>

						-or-

	<Address> <FirstName>Michael</FirstName> <LastName>Korns</LastName> </Address> 


If no content is recognized, an empty string literal is returned.
If singleton content is recognized, a string literal is returned;
otherwise, a Structure of the attributed contents is returned. 
The current input pointer is always left pointing to the first 
non whitespace character which is not element content.


#endif

TVAL FXml_Contents(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,XMLWork* work,NUM tagDepth)       
{
LpCHAR				start;
LpCHAR				namep;
CHAR				saveCH;
NUM					INLEN;
BOLE				whitespace = FALSE;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(attributes);
DeclareTVAL(__pi);
DeclareTVAL(result);
DeclareTVAL(contents);
DeclareTVAL(piLambda);
DeclareTVAL(name);
DeclareTVAL(ename);
DeclareTVAL(value);
DeclareTVAL(last);
DeclareTVAL(ec);
EndFrame

/* Set the default contents to a singleton empty string (if necessary). */

Stack(result) = theContents;
if (Stack(result).Tag == TYVOID)
	{
	Stack(result).Tag = TYTEXT;
	Stack(result).u.Text[0] = 0;
	whitespace = TRUE;
	}
start = work->IP;

/* Parse the input string looking for contents. */

Fetch:
switch (*work->IP)
	{
	case 0:
		if (work->inputProvider == TRUE)
			{
			/* Send the moreSource message symbol to the inputLambda. */
			/* Note: we do this only if we are in event handler mode. */
			Stack(tmp) = FSmartbase_SendMsg(gCP,gTP,gMoreSource,work->inputLambda,0);
			ExitOnError(Stack(tmp));
			if (Stack(tmp).Tag != TYVOID)
				{
				work->inputString = Stack(tmp);
				if (!FSmartbase_StringPtr(gCP,gTP,(LpTVAL)&work->inputString,(LpPOINTER)&work->IP,(LpNUM)&work->INLEN))
					{
					Stack(ec) = TERROR("!xml: invalid string returned by provider!");
					FrameExit(Stack(ec));
					}

				work->past = 0;
				work->start = work->IP;
				goto Fetch;
				}
			}

		goto Done;
		break;

	case '<':
		/********************************************/
 		/* Check for an end of element content tag. */
		/********************************************/
        /* Note: </Name> are end of element content tags. */

        if (*(work->IP+1) == '/')
			/* We have found an end of element content tag. */
			{
			/* Return to the calling recursion. */

            if (tagDepth > 0) goto Done;

			/* Try to recognize the element end tag name. */

			start = work->IP; /* Save the input pointer for backtracking. */
			work->IP += 2;	/* Promote input pointer past the </ symbols. */
			Stack(ename) = FXml_recName(gCP,gTP,work);
			if (Stack(ename).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Expected element end tag name",work));

			/* An HTML end name sets the HTML processing flag to true. */

			if (FXml_strcmpi(gCP,gTP,FSmartbase_ObjectPtr(gCP,gTP,&Stack(ename)),"html") == 0)
				{
				work->htmlOn = TRUE;
				}

			SP		/* Promote input pointer past whitespace */
			if (*work->IP != '>') FrameExit(FXml_Error(gCP,gTP,"Expected element end tag > symbol",work));
			++work->IP;	/* Promote input pointer past the > symbol. */
			goto Fetch;
			}

		/************************************************************/
		/* We are now recognizing imbedded content, so we convert	*/
		/* the parent contents into a Structure and turn off the	*/
		/* whitespace flag to indicate that we have important data. */
		/************************************************************/

        /* Note: <Name ..attlist..> ...contents... </Name> are imbedded element tags. */
        /*       <Name ..attlist../> are imbedded element tags without contents. */
        /*       <?Name ...contents... ?> are imbedded process instruction tags. */

		if (Stack(result).Tag != TYSTRUCTURE)
			{
			/* Save the current contents. */

			Stack(contents) = Stack(result);

			/* Make sure the contents are converted to a Structure. */

			Stack(result) = FSmartbase_Eval(gCP,gTP,gNewStructure,0);
			ExitOnError(Stack(result));

			/* Make sure that any non-whitespace contents are saved in the Structure. */
			/* Note: the whitespace flag will tell us whether we have seen important data. */

			if (whitespace == FALSE)
				{
				if (work->eventHandler)
					{
					/* Note: we do this only if we are in document mode. */
					Stack(tmp) = TSYMBOL("__content");
					Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(tmp),Stack(contents));
					ExitOnError(Stack(ec));
					}
				else
					{
					Stack(tmp) = TSYMBOL("__content");
					Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(tmp),Stack(contents));
					ExitOnError(Stack(ec));
					}
				}			
			}

		/*************************************/
		/* Check for a start of a comment.   */
        /* Note: <!--  --> are comment tags. */
		/*************************************/

        if ((*(work->IP+1) == '!') && (*(work->IP+2) == '-') && (*(work->IP+3) == '-'))
			/* We recognize and process the comment. */
			{
			Stack(result) = FXml_Comments(gCP,gTP,Stack(contents),Stack(result),work);     
			ExitOnError(Stack(result));
			goto Fetch;
			}

		/******************************************************************/
		/* We recognize marked sections here.                             */
		/* Note: <![CDATA[ ... ]]> is an example of a marked section.     */
		/*       Marked sections may occur anywhere in contents sections. */
		/******************************************************************/

		if ((*(work->IP+1) == '!') &&
			(*(work->IP+2) == '[') &&
			(*(work->IP+3) == 'C') &&
			(*(work->IP+4) == 'D') &&
			(*(work->IP+5) == 'A') &&
			(*(work->IP+6) == 'T') &&
			(*(work->IP+7) == 'A') &&
			(*(work->IP+8) == '['))
			{
			whitespace = FALSE;
			Stack(result) = FXml_CData(gCP,gTP,Stack(contents),Stack(result),work);
			ExitOnError(Stack(result));
			goto Fetch;
			}

		/*************************************************************************/
		/* Check for an imbedded process instruction tags.                       */
        /* Note: <?Name ...contents... ?> are imbedded process instruction tags. */
		/*************************************************************************/

        if (*(work->IP+1) == '?')
			/* We have found an imbedded process instruction. */
			{
			Stack(result) = FXml_ProcessInstruction(gCP,gTP,Stack(contents),Stack(result),work);
			ExitOnError(Stack(result));
			goto Fetch;
			}

		/*************************************************************************/
		/* Check for an imbedded document type definition tag.                   */
        /* Note: <!Name ...contents... > are document type definition tags.      */
		/*************************************************************************/

        if (*(work->IP+1) == '!')
			/* We have found an document type definition. */
			{
			Stack(result) = FXml_DoctypeHeader(gCP,gTP,Stack(contents),Stack(result),work,tagDepth);
			ExitOnError(Stack(result));
			goto Fetch;
			}

		/***************************************************************/
		/* Check for a SCRIPT tag which has meaning in HTML mode.      */
        /* Note: <script ...contents... <script> are HTML script tags. */
		/***************************************************************/

        if ((work->htmlOn == TRUE) &&
            (((*(work->IP+1)) == 's') || ((*(work->IP+1)) == 'S')) &&
            (((*(work->IP+2)) == 'c') || ((*(work->IP+2)) == 'C')) &&
            (((*(work->IP+3)) == 'r') || ((*(work->IP+3)) == 'R')) &&
            (((*(work->IP+4)) == 'i') || ((*(work->IP+4)) == 'I')) &&
            (((*(work->IP+5)) == 'p') || ((*(work->IP+5)) == 'P')) &&
            (((*(work->IP+6)) == 't') || ((*(work->IP+6)) == 'T')))
			/* We have found an HTML script tag. */
			{
			Stack(result) = FXml_ScriptInstruction(gCP,gTP,Stack(contents),Stack(result),work);
			ExitOnError(Stack(result));
			goto Fetch;
			}

		/*********************************************************************/
		/* We are now processing an element tag.                             */
        /* Note: <Name ..attlist..> ...contents... </Name> are element tags. */
        /*       <Name ..attlist../> are element tags without contents.      */
		/*********************************************************************/

		Stack(result) = FXml_Element(gCP,gTP,Stack(contents),Stack(result),work,tagDepth);
		ExitOnError(Stack(result));
		goto Fetch;
		break;

	default:
		/* Record the first occurance of character contents. */

		start = work->IP;
		
		while ((*work->IP != 0) && (*work->IP != '<'))
			{
			if (*work->IP++ > 32) whitespace = FALSE;
			}

		/* Make sure that any non-whitespace contents are saved in the Structure. */
		/* Note: the whitespace flag will tell us whether we have seen important data. */

		if (whitespace == FALSE)
			{
			/* Manage the characters event according to the mode we're in. */

			if (work->eventHandler)
				{
				/* Send the characters message symbol to the eventLambda. */
				/* Note: we do this only if we are in event handler mode. */
				/* Convert the recognized char data into a string. */
				if ((work->IP - start) > 0)
					{
					saveCH = *work->IP;
					*work->IP = 0;
					Stack(contents) = TSTRING(start);
					*work->IP = saveCH;
					Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gCharacters,work->eventLambda,1,Stack(contents));
					ExitOnError(Stack(ec));
					}
				}
			else
				{
				/* Note: we do this only if we are in document mode. */
				/* Convert the recognized char data into a string. */

				saveCH = *work->IP;
				*work->IP = 0;
				Stack(contents) = TSTRING(start);
				*work->IP = saveCH;

				/* Add the new content to the old content using the proper heuristic. */

				if (Stack(result).Tag == TYVOID)
					/* If the old content is empty, then just set the new content. */
					{
					Stack(result) = Stack(contents);
					}
				else

				if ((Stack(result).Tag == TYTEXT) && (Stack(result).u.Text[0] = 0))
					/* If the old content is a null string, then just set the new content. */
					{
					Stack(result) = Stack(contents);
					}
				else

				if (Stack(result).Tag != TYSTRUCTURE)
					/* If the old content is cdata, then append the new content. */
					{
					Stack(result) = FSmartbase_Eval(gCP,gTP,gAppend,2,Stack(result),Stack(contents));
					ExitOnError(Stack(result));
					}
				else

				if (Stack(result).Tag == TYSTRUCTURE)
					/* If the old content is a Structure, then insert the new content. */
					/* Note: If the last item inserted was character content, */
					/*       then append this content to it. */
					{
					INLEN = FSmartbase_ObjectLen(gCP,gTP,&Stack(result));
					if (INLEN > 0)
						{
						Stack(name) = FSmartbase_Ref(gCP,gTP,3,Stack(result),TINT(INLEN-1),TINT(0));
						Stack(value) = FSmartbase_Ref(gCP,gTP,3,Stack(result),TINT(INLEN-1),TINT(1));
						namep = FSmartbase_ObjectPtr(gCP,gTP,&Stack(name));
						if ((Stack(name).Tag == TYSYMBOL) && 
							(strcmp(namep,"__content") == 0) &&
							((Stack(value).Tag == TYSTRING) || (Stack(value).Tag == TYTEXT)))
							{
							Stack(contents) = FSmartbase_Eval(gCP,gTP,gAppend,2,Stack(value),Stack(contents));
							ExitOnError(Stack(contents));
							Stack(tmp) = TSYMBOL("__content");
							Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),TINT(INLEN-1),TINT(1),Stack(contents));
							ExitOnError(Stack(ec));
							goto Fetch;
							}
						}

					Stack(tmp) = TSYMBOL("__content");
					Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(tmp),Stack(contents));
					ExitOnError(Stack(ec));
					}
				}
			}
		else
			{
			/* Manage the ignorable whitespace event according to the mode we're in. */

			if (work->eventHandler)
				{
				/* Send the ignorableWhitespace message symbol to the eventLambda. */
				/* Note: we do this only if we are in event handler mode. */
				if ((work->IP - start) > 0)
					{
					saveCH = *work->IP;
					*work->IP = 0;
					Stack(contents) = TSTRING(start);
					*work->IP = saveCH;
					Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gIgWhitespace,work->eventLambda,1,Stack(contents));
					ExitOnError(Stack(ec));
					}
				}
			}				

		goto Fetch;
		break;
	}

/* Return the XML contents object. */

Done:
FrameExit(Stack(result));

/* Return an XML invalid contents error. */

FrameExit(FXml_Error(gCP,gTP,"Invalid element contents",work));
}


/*--------------------------------------------------------------------------------------- */
#if 0

FXml_Element

This Function recognizes HTML element tag sections.
For example:


	<Name> ...content...</Name>


#endif

TVAL FXml_Element(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work,NUM tagDepth)       
{
LpCHAR				start;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(name);
DeclareTVAL(ename);
DeclareTVAL(value);
DeclareTVAL(__pi);
DeclareTVAL(attributes);
DeclareTVAL(piLambda);
DeclareTVAL(result);
DeclareTVAL(contents);
DeclareTVAL(ec);
EndFrame

/* Set up the arguments in protected memory. */

Stack(contents) = theContents;
Stack(result) = theResult;

/*********************************************************************/
/* We are now processing an element tag.                             */
/* Note: <Name ..attlist..> ...contents... </Name> are element tags. */
/*       <Name ..attlist../> are element tags without contents.      */
/*********************************************************************/

++work->IP;	/* Promote the input pointer to the element tag name. */
Stack(contents) = gCP->Tval_VOID;

/* Try to recognize the element tag name. */

Stack(name) = FXml_recName(gCP,gTP,work);
if (Stack(name).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Expected element tag name",work));

/* An HTML tag name sets the HTML processing flag. */

if (FXml_strcmpi(gCP,gTP,FSmartbase_ObjectPtr(gCP,gTP,&Stack(name)),"html") == 0)
	{
	work->htmlOn = TRUE;
	}

SP		/* Promote input pointer past whitespace */
Stack(attributes) = gCP->Tval_VOID;
if ((*work->IP != '>') && (*work->IP != '/'))
	/* We need to recognize the element attributes. */
	{
	Stack(attributes) = FXml_AttributeList(gCP,gTP,work);
	ExitOnError(Stack(attributes));
	}

/* Manage the start element event according to the mode we're in. */

if (work->eventHandler)
	{
	/* Send the startElement message symbol to the eventLambda. */
	/* Note: we do this only if we are in event handler mode. */
	Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gStartElement,work->eventLambda,2,Stack(name),Stack(attributes));
	ExitOnError(Stack(ec));
	}
else
	{
	/* Assign any attributes to the element Structure under the key: __attlist */
	/* Note: the attributes will be assigned in the first position of the Structure. */
	/* Note: we do this only if we are in document mode. */

	if (Stack(attributes).Tag != TYVOID)
		{
		Stack(tmp) = TSYMBOL("__attlist");
		Stack(contents) = FSmartbase_Eval(gCP,gTP,gNewStructure,2,Stack(tmp),Stack(attributes));
		ExitOnError(Stack(contents));
		}
	}

/* Are there no contents for this imbedded element tag? */
/* Note: <Name ..attlist../> are imbedded element tags without contents. */

if ((*work->IP == '/') && (*(work->IP+1) == '>'))
	/* There are no contents for this element tag. */
	{
	work->IP += 2;	/* Promote input pointer past /> symbols. */

	/* Manage the start element event according to the mode we're in. */

	if (work->eventHandler)
		{
		/* Send the endElement message symbol to the eventLambda. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gEndElement,work->eventLambda,1,Stack(name));
		ExitOnError(Stack(ec));
		}
	else
		{
		/* Note: the imbedded element will be assigned in the last position of the Structure. */
		/* Note: we do this only if we are in document mode. */

		if (Stack(contents).Tag == TYVOID) Stack(contents) = gCP->Tval_TRUE;
		Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(name),Stack(contents));
		ExitOnError(Stack(ec));
		}

	FrameExit(Stack(result));
	}

/* We are now processing an imbedded element tag with contents. */
/* Note: <Name ..attlist..> ...contents... </Name> are imbedded element tags. */

if (*work->IP != '>') FXml_Error(gCP,gTP,"Expecting element tag > symbol",work);
++work->IP;	/* Promote input pointer past the > symbol. */
SP		/* Promote input pointer past whitespace */

Stack(contents) = FXml_Contents(gCP,gTP,Stack(contents),work,tagDepth+1);
ExitOnError(Stack(contents));

if (work->eventHandler == FALSE)
	{
	/* Note: the contents will be assigned in the last position of the Structure. */

	Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(name),Stack(contents));
	ExitOnError(Stack(ec));
	}


if ((*work->IP == '<') && (*(work->IP+1) == '/'))
	{
	/* Try to recognize the element end tag name. */

	start = work->IP; /* Save the input pointer for backtracking. */
	work->IP += 2;	/* Promote input pointer past the </ symbols. */
	Stack(ename) = FXml_recName(gCP,gTP,work);
	if (Stack(ename).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Expected element end tag name",work));

	/* An HTML end name never resets the HTML processing flag. */
    /* Note: This is because there are so many errors in web */
	/*       HTML pages that we can never be sure that we are */
	/*       not required to parse loosely once we have seen */
	/*       the HTML start tag. */

	if (FXml_strcmpi(gCP,gTP,FSmartbase_ObjectPtr(gCP,gTP,&Stack(name)),"html") == 0)
		{
		work->htmlOn = TRUE;
		}

	SP		/* Promote input pointer past whitespace */
	if (*work->IP != '>') FrameExit(FXml_Error(gCP,gTP,"Expected element end tag > symbol",work));
	++work->IP;	/* Promote input pointer past the > symbol. */

	/* Make sure this is the proper end tag for this element. */

	if (FXml_strcmpi(gCP,gTP,FSmartbase_ObjectPtr(gCP,gTP,&Stack(name)),FSmartbase_ObjectPtr(gCP,gTP,&Stack(ename))) != 0)
		{
		work->IP = start;
		}
	}

/* Send the end element event according to the mode we're in. */

if (work->eventHandler)
	{
	/* Send the endElement message symbol to the eventLambda. */
	/* Note: we do this only if we are in event handler mode. */
	Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gEndElement,work->eventLambda,1,Stack(name));
	ExitOnError(Stack(ec));
	}


/* Return the XML result object. */

FrameExit(Stack(result));

/* Return an XML invalid scripting instruction error. */

FrameExit(FXml_Error(gCP,gTP,"Expecting element tag",work));
}


/*--------------------------------------------------------------------------------------- */
#if 0

FXml_CData

This Function recognizes HTML CDATA sections.
For example:


	<![CDATA[ any data at all can be in here ]]>>


#endif

TVAL FXml_CData(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work)       
{
LpCHAR				header;
LpCHAR				start;
LpCHAR				namep;
CHAR				saveCH;
NUM					INLEN;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(name);
DeclareTVAL(ename);
DeclareTVAL(value);
DeclareTVAL(__pi);
DeclareTVAL(attributes);
DeclareTVAL(piLambda);
DeclareTVAL(result);
DeclareTVAL(contents);
DeclareTVAL(ec);
EndFrame

/* Set up the arguments in protected memory. */

Stack(contents) = theContents;
Stack(result) = theResult;

/* We recognize marked sections here.                             */
/* Note: <![CDATA[ ... ]]> is an example of a marked section.     */
/*       Marked sections may occur anywhere in contents sections. */

if ((*(work->IP+1) == '!') &&
	(*(work->IP+2) == '[') &&
	(*(work->IP+3) == 'C') &&
	(*(work->IP+4) == 'D') &&
	(*(work->IP+5) == 'A') &&
	(*(work->IP+6) == 'T') &&
	(*(work->IP+7) == 'A') &&
	(*(work->IP+8) == '['))
	{
	header = work->IP;
	work->IP += 9; /* Promote the input pointer past the mark header. */
	start = work->IP;
	while ((*work->IP != 0) && 
		   ((*work->IP != ']') || (*(work->IP+1) != ']') || (*(work->IP+2) != '>')))
		{
		*work->IP++;
		}


	/* Is the CDATA section not closed? */
	
	if (*work->IP == 0)
		{
		work->IP = header;
		FrameExit(FXml_Error(gCP,gTP,"CDATA section not closed",work));
		}

	/* Make sure that any marked section contents are saved in the Structure. */
	/* Note: the whitespace flag will tell us whether we have seen important data. */

	/* Convert the recognized marked section char data into a string. */

	saveCH = *work->IP;
	*work->IP = 0;
	Stack(contents) = TSTRING(start);
	*work->IP = saveCH;
	if (*work->IP != 0) work->IP += 3; /* Promote input pointer past the CDATA trailer. */

	/* Manage the character data according to the mode we're in. */

	if (work->eventHandler)
		{
		/* Send the processingInstruction message symbol to the eventLambda. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gCharacters,work->eventLambda,1,Stack(contents));
		ExitOnError(Stack(ec));
		}
	else
		{
		/* Add the new content to the old content using the proper heuristic. */
		/* Note: we do this only if we are in document mode. */
		if (Stack(result).Tag == TYVOID)
			/* If the old content is empty, then just set the new content. */
			{
			Stack(result) = Stack(contents);
			}
		else

		if ((Stack(result).Tag == TYTEXT) && (Stack(result).u.Text[0] = 0))
			/* If the old content is a null string, then just set the new content. */
			{
			Stack(result) = Stack(contents);
			}
		else

		if (Stack(result).Tag != TYSTRUCTURE)
			/* If the old content is cdata, then append the new content. */
			{
			Stack(result) = FSmartbase_Eval(gCP,gTP,gAppend,2,Stack(result),Stack(contents));
			ExitOnError(Stack(result));
			}
		else

		if (Stack(result).Tag == TYSTRUCTURE)
			/* If the old content is a Structure, then insert the new content. */
			/* Note: If the last item inserted was character content, */
			/*       then append this content to it. */
			{
			INLEN = FSmartbase_ObjectLen(gCP,gTP,&Stack(result));
			if (INLEN > 0)
				{
				Stack(name) = FSmartbase_Ref(gCP,gTP,3,Stack(result),TINT(INLEN-1),TINT(0));
				Stack(value) = FSmartbase_Ref(gCP,gTP,3,Stack(result),TINT(INLEN-1),TINT(1));
				namep = FSmartbase_ObjectPtr(gCP,gTP,&Stack(name));
				if ((Stack(name).Tag == TYSYMBOL) && 
					(strcmp(namep,"__content") == 0) &&
					((Stack(value).Tag == TYSTRING) || (Stack(value).Tag == TYTEXT)))
					{
					Stack(contents) = FSmartbase_Eval(gCP,gTP,gAppend,2,Stack(value),Stack(contents));
					ExitOnError(Stack(contents));
					Stack(tmp) = TSYMBOL("__content");
					Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),TINT(INLEN-1),TINT(1),Stack(contents));
					ExitOnError(Stack(ec));
					FrameExit(Stack(result));
					}
				}

			Stack(tmp) = TSYMBOL("__content");
			Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(tmp),Stack(contents));
			ExitOnError(Stack(ec));
			}
		}

	/* Return the XML result object. */

	FrameExit(Stack(result));
	}


/* Return an XML invalid scripting instruction error. */

FrameExit(FXml_Error(gCP,gTP,"Expecting CDATA section",work));
}


/*--------------------------------------------------------------------------------------- */
#if 0

FXml_ScriptInstruction

This Function recognizes HTML scripting instructions.
For example:


	<script language="javaScript"> writeln("Hello World");</script>


#endif

TVAL FXml_ScriptInstruction(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work)       
{
LpCHAR				start;
CHAR				saveCH;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(name);
DeclareTVAL(ename);
DeclareTVAL(value);
DeclareTVAL(__pi);
DeclareTVAL(attributes);
DeclareTVAL(piLambda);
DeclareTVAL(result);
DeclareTVAL(script);
DeclareTVAL(contents);
DeclareTVAL(ec);
EndFrame

/* Set up the arguments in protected memory. */

Stack(contents) = theContents;
Stack(result) = theResult;

/* Check for a SCRIPT tag which has meaning in HTML mode.      */
/* Note: <script ...contents... <script> are HTML script tags. */

if ((work->htmlOn == TRUE) &&
    (((*(work->IP+1)) == 's') || ((*(work->IP+1)) == 'S')) &&
    (((*(work->IP+2)) == 'c') || ((*(work->IP+2)) == 'C')) &&
    (((*(work->IP+3)) == 'r') || ((*(work->IP+3)) == 'R')) &&
    (((*(work->IP+4)) == 'i') || ((*(work->IP+4)) == 'I')) &&
    (((*(work->IP+5)) == 'p') || ((*(work->IP+5)) == 'P')) &&
    (((*(work->IP+6)) == 't') || ((*(work->IP+6)) == 'T')))
	/* We have found an HTML script tag. */
	{

	work->IP += 7;	/* Promote the input pointer to the script tag. */
	Stack(name) = TSTRING("script");
	SP		/* Promote input pointer past whitespace */
    Stack(contents) = gCP->Tval_VOID;

	/* We need to recognize the script attributes. */

	Stack(attributes) = FXml_AttributeList(gCP,gTP,work);
	ExitOnError(Stack(attributes));

	if (*work->IP != '>') FXml_Error(gCP,gTP,"Expecting element tag > symbol",work);
	++work->IP;	/* Promote input pointer past the > symbol. */

	/* Manage the start element event according to the mode we're in. */

	if (work->eventHandler)
		{
		/* Send the startElement message symbol to the eventLambda. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gStartElement,work->eventLambda,2,Stack(name),Stack(attributes));
		ExitOnError(Stack(ec));
		}
	else
		{
		/* Assign any attributes to the element Structure under the key: __attlist */
		/* Note: the attributes will be assigned in the first position of the Structure. */
		/* Note: we do this only if we are in document mode. */

		if (Stack(attributes).Tag != TYVOID)
			{
			Stack(tmp) = TSYMBOL("__attlist");
			Stack(contents) = FSmartbase_Eval(gCP,gTP,gNewStructure,2,Stack(tmp),Stack(attributes));
			ExitOnError(Stack(contents));
			}
		}
	/* Record the script contents. */

	start = work->IP;
	
	while ((*work->IP != 0) && ((*work->IP != '<') || (*(work->IP+1) != '/')))
		{
		*work->IP++;
		}

	/* Convert the recognized char data into a string. */

	saveCH = *work->IP;
	*work->IP = 0;
	Stack(script) = TSTRING(start);
	*work->IP = saveCH;

	/* Manage the character data according to the mode we're in. */

	if (work->eventHandler)
		{
		/* Send the characters message symbol to the eventLambda. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gCharacters,work->eventLambda,1,Stack(script));
		ExitOnError(Stack(ec));
		}
	else
		{
		/* Add the new script to its attributes using the proper heuristic. */
		/* Note: we do this only if we are in document mode. */
		if (Stack(contents).Tag == TYVOID)
			/* If the old content is empty, then just set the new content. */
			{
			Stack(contents) = Stack(script);
			}
		else

		if ((Stack(contents).Tag == TYTEXT) && (Stack(contents).u.Text[0] = 0))
			/* If the old content is a null string, then just set the new comment. */
			{
			Stack(contents) = Stack(script);
			}
		else

			{
			if (Stack(contents).Tag != TYSTRUCTURE)
				/* If the old content is cdata, then append the new content. */
				{
				Stack(tmp) = TSYMBOL("script");
				Stack(contents) = FSmartbase_Eval(gCP,gTP,gNewStructure,2,Stack(tmp),Stack(script));
				ExitOnError(Stack(contents));
				}

			Stack(tmp) = TSYMBOL("script");
			Stack(contents) = FSmartbase_Set(gCP,gTP,4,Stack(contents),gLast,Stack(tmp),Stack(script));
			ExitOnError(Stack(contents));
			}
		}

		/* Add the new script to the old content using the proper heuristic. */
		/* Note: we do this only if we are in document mode. */
		if (Stack(result).Tag == TYVOID)
			/* If the old content is empty, then just set the new content. */
			{
			Stack(result) = Stack(contents);
			}
		else

		if ((Stack(result).Tag == TYTEXT) && (Stack(result).u.Text[0] = 0))
			/* If the old content is a null string, then just set the new comment. */
			{
			Stack(result) = Stack(contents);
			}
		else

			{
			if (Stack(result).Tag != TYSTRUCTURE)
				/* If the old content is cdata, then append the new content. */
				{
				Stack(tmp) = TSYMBOL("__content");
				Stack(result) = FSmartbase_Eval(gCP,gTP,gNewStructure,2,Stack(tmp),Stack(result));
				ExitOnError(Stack(result));
				}

			Stack(tmp) = TSYMBOL("script");
			Stack(result) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(tmp),Stack(contents));
			ExitOnError(Stack(result));
			}


	/* Try to recognize the script end tag name. */

	if ((*work->IP == '<') && (*(work->IP+1) == '/'))
		{
		start = work->IP;
		work->IP += 2;	/* Promote input pointer past the </ symbols. */
		Stack(ename) = FXml_recName(gCP,gTP,work);
		if (Stack(ename).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Expecting end script tag",work));

		/* An HTML or XML name resets the HTML processing flag. */

		if (FXml_strcmpi(gCP,gTP,FSmartbase_ObjectPtr(gCP,gTP,&Stack(name)),"html") == 0)
			{
			work->htmlOn = FALSE;
			}

		SP		/* Promote input pointer past whitespace */
		if (*work->IP != '>') FrameExit(FXml_Error(gCP,gTP,"Expecting end script > symbol",work));
		++work->IP;	/* Promote input pointer past the > symbol. */

		/* Make sure this is the proper end tag for this element. */

		if (FXml_strcmpi(gCP,gTP,"script",FSmartbase_ObjectPtr(gCP,gTP,&Stack(ename))) != 0)
			{
			work->IP = start;
			}
		}

	/* Send the end element event according to the mode we're in. */

	if (work->eventHandler)
		{
		/* Send the endElement message symbol to the eventLambda. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gEndElement,work->eventLambda,1,Stack(name));
		ExitOnError(Stack(ec));
		}

	/* Return the XML result object. */

	FrameExit(Stack(result));
	}

/* Return an XML invalid scripting instruction error. */

FrameExit(FXml_Error(gCP,gTP,"Expecting scripting instruction",work));
}


/*--------------------------------------------------------------------------------------- */
#if 0

FXml_ProcessInstruction

This Function recognizes XML processing instructions.
For example:


	<?javaScript writeln("Hello World");?>


#endif

TVAL FXml_ProcessInstruction(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work)       
{
LpCHAR				header;
LpCHAR				start;
CHAR				saveCH;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(name);
DeclareTVAL(__pi);
DeclareTVAL(piLambda);
DeclareTVAL(result);
DeclareTVAL(contents);
DeclareTVAL(ec);
EndFrame

/* Set up the arguments in protected memory. */

Stack(contents) = theContents;
Stack(result) = theResult;

/* Check for an processing instruction tag. */
/* Note: <?Name ...contents... ?> is a process instruction tag. */

if ((*(work->IP) == '<') && (*(work->IP+1) == '?'))
	/* We have found a process instruction. */
	{
	header = work->IP;
	work->IP += 2;	/* Promote the input pointer to the PI tag name. */

	/* Try to recognize the PI tag name. */

	Stack(name) = FXml_recName(gCP,gTP,work);
	if (Stack(name).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Expecting processing instruction target name",work));

	/* Record the process instruction contents. */

	start = work->IP;
	
	while ((*work->IP != 0) && ((*work->IP != '?') || (*(work->IP+1) != '>')))
		{
		*work->IP++;
		}

	/* Is the processing instruction not closed? */
	
	if (*work->IP != '?')
		{
		work->IP = header;
		FrameExit(FXml_Error(gCP,gTP,"Processing instruction not closed",work));
		}

	/* Convert the recognized char data into a string. */

	saveCH = *work->IP;
	*work->IP = 0;
	Stack(contents) = TSTRING(start);
	*work->IP = saveCH;

	if (*work->IP != 0) work->IP += 2; /* Promote the input pointer past the PI trailer. */

	/* Manage the process instruction according to the mode we're in. */

	if (work->eventHandler)
		{
		/* Send the processingInstruction message symbol to the eventLambda. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gProcessInstr,work->eventLambda,2,Stack(name),Stack(contents));
		ExitOnError(Stack(ec));
		}
	else
		{
		/* Locate the __pi Structure (if not present, create one) */
		/* Note: the imbedded PI will be assigned in the last */
		/*       position of the __pi Structure. */
		/* Note: we do this only if we are in document mode. */
		Stack(__pi) = TSYMBOL("__pi");
		Stack(tmp) = FSmartbase_Ref(gCP,gTP,2,Stack(result),Stack(__pi));
		ExitOnError(Stack(tmp));

		if (Stack(tmp).Tag == TYVOID)
			{
			Stack(tmp) = FSmartbase_Eval(gCP,gTP,gNewStructure,0);
			ExitOnError(Stack(tmp));

			Stack(ec) = FSmartbase_Set(gCP,gTP,3,Stack(result),Stack(__pi),Stack(tmp));
			ExitOnError(Stack(ec));
			}


		Stack(__pi) = Stack(tmp);
		Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(__pi),gLast,Stack(name),Stack(contents));
		ExitOnError(Stack(ec));

		/* Are we processing any process instructions? */

		if (work->piHandler)
			{
			/* Are we processing this process instruction? */

			Stack(piLambda) = FSmartbase_Ref(gCP,gTP,2,work->piStructure,Stack(name));
			ExitOnError(Stack(piLambda));
			if (Stack(piLambda).Tag != TYVOID)
				{
				Stack(ec) = FSmartbase_Eval(gCP,gTP,Stack(piLambda),4,work->piStructure,work->Document,Stack(result),Stack(contents));
				ExitOnError(Stack(ec));
				}
			}
		}


	/* Return the XML result object. */

	FrameExit(Stack(result));
	}


/* Return an XML invalid processing instruction error. */

FrameExit(FXml_Error(gCP,gTP,"Expecting processing instruction",work));
}


/*--------------------------------------------------------------------------------------- */
#if 0

FXml_DoctypeHeader

This Function recognizes XML Doctype Headers.
For example:


	<!DOCTYPE HTML "version 2345">

		also

	<![if !supportEmptyParas]>


		also

	<![endif]>


#endif

TVAL FXml_DoctypeHeader(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work,NUM tagDepth)       
{
LpCHAR				header;
LpCHAR				start;
LpCHAR				endMarker;
CHAR				saveCH;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(name);
DeclareTVAL(__dtd);
DeclareTVAL(dtdContent);
DeclareTVAL(piLambda);
DeclareTVAL(result);
DeclareTVAL(contents);
DeclareTVAL(ec);
EndFrame

/* Set up the arguments in protected memory. */

Stack(contents) = theContents;
Stack(result) = theResult;

/* Check for an processing instruction tag. */
/* Note: <!Name ...contents... > is a doctype header tag. */

if ((*(work->IP) == '<') && (*(work->IP+1) == '!'))
	/* We have found a doctype header. */
	{
	header = work->IP;
	work->IP += 2;	/* Promote the input pointer to the doctype tag name. */

	/* Try to recognize the doctype tag name. */

	Stack(name) = FXml_recName(gCP,gTP,work);
	if (Stack(name).Tag == TYVOID) Stack(name) = TSTRING("dtd:unnamed");

	/* Record the doctype contents. */

	start = work->IP;
	
	while ((*work->IP != 0) && (*(work->IP) != '>') && (*(work->IP) != '['))
		{
		*work->IP++;
		}

	/* Convert the recognized char data into a string. */

	saveCH = *work->IP;
	*work->IP = 0;
	Stack(contents) = TSTRING(start);
	*work->IP = saveCH;

	/* Manage the doctype according to the mode we're in. */

	if (work->eventHandler)
		{
		/* Send the processingInstruction message symbol to the eventLambda. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gDoctypeDef,work->eventLambda,2,Stack(name),Stack(contents));
		ExitOnError(Stack(ec));
		}
	else
		{
		/* Locate the __dtd Structure (if not present, create one) */
		/* Note: the imbedded DTD will be assigned in the last */
		/*       position of the __dtd Structure. */
		/* Note: we do this only if we are in document mode. */
		Stack(__dtd) = TSYMBOL("__dtd");
		Stack(dtdContent) = FSmartbase_Ref(gCP,gTP,2,work->Document,Stack(__dtd));
		ExitOnError(Stack(dtdContent));

		if (Stack(dtdContent).Tag == TYVOID)
			{
			Stack(dtdContent) = FSmartbase_Eval(gCP,gTP,gNewStructure,0);
			ExitOnError(Stack(dtdContent));

			Stack(ec) = FSmartbase_Set(gCP,gTP,3,work->Document,Stack(__dtd),Stack(dtdContent));
			ExitOnError(Stack(ec));
			}


		Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(dtdContent),gLast,Stack(name),Stack(contents));
		ExitOnError(Stack(ec));
		}


	/* Is the document definition nested? */
	
	if (*work->IP == '[')
		{
		work->IP += 1;
		start = work->IP;
		
		while ((*work->IP != 0) && (*(work->IP) != ']'))
			{
			*work->IP++;
			}

		/* Convert the recognized char data into a nested document type definition. */

		saveCH = *work->IP;
		endMarker = work->IP;
		*work->IP = 0;
		work->IP = start;
		Stack(ec) = FXml_Contents(gCP,gTP,Stack(dtdContent),work,tagDepth+1);
		*endMarker = saveCH;
		ExitOnError(Stack(ec));		
		if (*work->IP != ']')
			{
			work->IP = header;
			FrameExit(FXml_Error(gCP,gTP,"Document definition not closed",work));
			}
		work->IP += 1;
		while ((*work->IP != 0) && (*(work->IP) != '>'))
			{
			*work->IP++;
			}
		}

	/* Is the document definition not closed? */
	
	if (*work->IP != '>')
		{
		work->IP = header;
		FrameExit(FXml_Error(gCP,gTP,"Document definition not closed",work));
		}

	if (*work->IP != 0) work->IP += 1; /* Promote the input pointer past the doctype trailer. */

	/* Return the XML result object. */

	FrameExit(Stack(result));
	}


/* Return an XML invalid processing instruction error. */

FrameExit(FXml_Error(gCP,gTP,"Expecting document type definition",work));
}


/*--------------------------------------------------------------------------------------- */
#if 0

FXml_Comments

This Function recognizes XML comments.
For example:


	<!-- this is a comment -->


#endif

TVAL FXml_Comments(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work)       
{
LpCHAR				start;
CHAR				saveCH;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
DeclareTVAL(contents);
DeclareTVAL(ec);
EndFrame

/* Set up the arguments in protected memory. */

Stack(contents) = theContents;
Stack(result) = theResult;

/* Check for a start of a comment.   */
/* Note: <!--  --> are comment tags. */

if ((*(work->IP+1) == '!') && (*(work->IP+2) == '-') && (*(work->IP+3) == '-'))
	{
	work->IP+=4;  /* Promote the input pointer past the comment header. */
	start = work->IP;

	/* We look for the end of the comment. */
	while ((*work->IP != 0) && 
		   ((*work->IP != '-') || (*(work->IP+1) != '-') || (*(work->IP+2) != '>')))
		{
		++work->IP;	/* Ignore comment characters. */
		}

	/* Is the comment not closed? */
	
	if (*work->IP != '-')
		{
		work->IP = start - 4;
		FrameExit(FXml_Error(gCP,gTP,"Comment not closed",work));
		}

	/* Convert the recognized comment into a string. */

	saveCH = *work->IP;
	*work->IP = 0;
	Stack(contents) = TSTRING(start);
	*work->IP = saveCH;

	if (*work->IP != 0) work->IP += 3;	/* Promote input pointer past the comment trailer. */

	/* Note: the imbedded comment will be assigned in the last position of the Structure. */

	if (work->eventHandler)
		{
		/* Send the comment message symbol to the eventLambda. */
		/* Note: we do this only if we are in event handler mode. */
		Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gComments,work->eventLambda,1,Stack(contents));
		ExitOnError(Stack(ec));
		}
	else

		{
		/* Add the new comment to the old content using the proper heuristic. */
		/* Note: we do this only if we are in document mode. */
		if (Stack(result).Tag == TYVOID)
			/* If the old content is empty, then just set the new comment. */
			{
			Stack(result) = Stack(contents);
			}
		else

		if ((Stack(result).Tag == TYTEXT) && (Stack(result).u.Text[0] = 0))
			/* If the old content is a null string, then just set the new comment. */
			{
			Stack(result) = Stack(contents);
			}
		else

			{
			if (Stack(result).Tag != TYSTRUCTURE)
				/* If the old content is cdata, then append the new content. */
				{
				Stack(tmp) = TSYMBOL("__content");
				Stack(result) = FSmartbase_Eval(gCP,gTP,gNewStructure,2,Stack(tmp),Stack(result));
				ExitOnError(Stack(result));
				}

			Stack(tmp) = TSYMBOL("__comment");
			Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(result),gLast,Stack(tmp),Stack(contents));
			ExitOnError(Stack(ec));
			}
		}

	/* Return the XML result object. */

	FrameExit(Stack(result));
	}

/* Return an XML invalid comment error. */

FrameExit(FXml_Error(gCP,gTP,"Expecting comment expression",work));
}

/*--------------------------------------------------------------------------------------- */
#if 0

FXml_AttributeList

This Function recognizes attribute lists inside the element tag of an XML
element. For example:

	<?xml version = '1.0' standalone='yes' encoding = 'hello' ?>

						-or-

	<Address FirstName = "yes" LastName = 'yes'> 


If an attribute list is recognized, a Structure of the attributes and
their values is returned; otherwise, void is returned. The current
input pointer is always left pointing to the first non whitespace
character which is not an attribute name.


#endif

TVAL FXml_AttributeList(LpXCONTEXT gCP,LpTHREAD gTP,XMLWork* work)       
{
StartFrame
DeclareTVAL(attributes);
DeclareTVAL(name);
DeclareTVAL(value);
DeclareTVAL(ec);
EndFrame

/* Parse the input string looking for the attributes. */

Fetch:
switch (*work->IP)
	{
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case '_':

		/* Try to recognize the attribute name. */

		Stack(name) = FXml_recName(gCP,gTP,work);
		if (Stack(name).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Invalid element attribute name",work));;
		SP		/* Promote input pointer past whitespace */

		/* Do we have an = symbol, indicating a attribute value follows? */
 
		if (*work->IP == '=') 
			{
			++work->IP;	/* Promote input pointer past = symbol */
			SP		/* Promote input pointer past whitespace */
			Stack(value) = FXml_recValue(gCP,gTP,work);
			if (Stack(value).Tag == TYVOID) FrameExit(FXml_Error(gCP,gTP,"Invalid element attribute value",work));;
			SP		/* Promote input pointer past whitespace */
			}
		else
			{
			if (work->htmlOn == TRUE)
				{
				Stack(value) = TBOOL(TRUE);
				}
			else
				{
				FrameExit(FXml_Error(gCP,gTP,"Expecting = symbol",work));;
				}
			}

		/* Append the attribute to the attribute list Structure. */

		if (Stack(attributes).Tag == TYVOID)
			{
			/* Create the attributes Structure (if necessary). */

			Stack(attributes) = FSmartbase_Eval(gCP,gTP,gNewStructure,0);
			ExitOnError(Stack(attributes));
			}

        /* Note: the attributes will be assigned in the last position of the Structure. */

		Stack(ec) = FSmartbase_Set(gCP,gTP,4,Stack(attributes),gLast,Stack(name),Stack(value));
		ExitOnError(Stack(ec));

		goto Fetch;
    
		break;

	case 1:
	case 2:
	case 3:
	case 4:
	case 5:
	case 6:
	case 7:
	case 8:
	case 9:
	case 11:
	case 12:
	case 14:
	case 15:
	case 16:
	case 17:
	case 18:
	case 19:
	case 20:
	case 21:
	case 22:
	case 23:
	case 24:
	case 25:
	case 26:
	case 27:
	case 28:
	case 29:
	case 30:
	case 31:
	case 32:
		SP		/* Promote input pointer past whitespace */
		goto Fetch;
		break;

	default:
		goto Done;
		break;

	}

/* Return the XML attribute list object (if any). */

Done:
FrameExit(Stack(attributes));

/* Return the XML attribute list object (if any). */

FrameExit(FXml_Error(gCP,gTP,"Invalid element attribute",work));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FXml_recName

Recognize a name in an XML source string. If a name is found, the work->IP pointer is
promoted and the name symbol is returned; otherwise, void is returned.

#endif

TVAL FXml_recName(LpXCONTEXT gCP,LpTHREAD gTP,XMLWork* work)
{
LpCHAR              start;
NUM                 len;
CHAR                stringBuf[1024];
StartFrame
DeclareTVAL(result);
EndFrame
 
/*  An XML name must begin with an underscore _ or an alpha character. */

if (((*work->IP >= 'a') && (*work->IP <= 'z')) || ((*work->IP >= 'A') && (*work->IP <= 'Z')) || (*work->IP == '_'))
	{
	goto StartToRecognize;
	}
else
	{
	goto Bad;
	}

StartToRecognize:
stringBuf[0] = *work->IP;
stringBuf[1] = 0;
len = 1;

/*  Recognize names up to 1024 characters long. */

start = work->IP + 1;
TryAnotherChar:
if (len >= 1020) goto Bad;
switch(*start)
    {
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
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
	case '_':
	case ':':
	case '-':

		/* Try to recognize the attribute name. */
		stringBuf[len++] = *start;
		stringBuf[len] = 0;
        ++start;	/* Promote input pointer past name symbol */
		goto TryAnotherChar;
		break;

	case 0:

		/* Try to get more source code. */
		goto Good;
    
		break;


	default:

		/* We have recognized a name. */
		goto Good;
    
		break;
    }

Good:

Stack(result) = TSYMBOL(stringBuf);
work->IP = start;
FrameExit(Stack(result));

/*  Can't recognize this as a name. */

Bad:

FrameExit(gCP->Tval_VOID);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FXml_recValue

Recognize a value in an XML source string. If a value is found, the work->IP pointer is
promoted and the value is returned; otherwise, void is returned.

#endif

TVAL FXml_recValue(LpXCONTEXT gCP,LpTHREAD gTP,XMLWork* work)
{
NUM                 len;
CHAR                stringBuf[1024];
StartFrame
DeclareTVAL(result);
EndFrame
 
/*  An XML symbol value must begin with a single quote character. */

if (*work->IP == '\'')
	{
	len = 0;
	stringBuf[len] = 0;
	while (*(++work->IP) != '\'')
		{
		if (len >= 1020) FrameExit(TERROR("!XML: symbol constant too long!"));
		stringBuf[len++] = *work->IP;
		stringBuf[len] = 0;
		}
    ++work->IP;		/* Promote input pointer past single quote symbol */
	Stack(result) = TSYMBOL(stringBuf);
	FrameExit(Stack(result));
	}
else
 
/*  An XML string value must begin with a double quote character. */

if (*work->IP == '\"')
	{
	len = 0;
	stringBuf[len] = 0;
	while (*(++work->IP) != '\"')
		{
		if (len >= 1020) FrameExit(TERROR("!XML: string constant too long!"));
		stringBuf[len++] = *work->IP;
		stringBuf[len] = 0;
		}
    ++work->IP;		/* Promote input pointer past double quote symbol */
	Stack(result) = TSTRING(stringBuf);
	FrameExit(Stack(result));
	}
else

/*  An XML numeric value must begin with a digit. */

if ((*work->IP >= '0') && (*work->IP <= '9'))
	{
	len = 0;
	stringBuf[len] = 0;
	while ((*work->IP >= '0') && (*work->IP <= '9'))
		{
		if (len >= 1020) FrameExit(TERROR("!XML: numeric constant too long!"));
		stringBuf[len++] = *work->IP;
		stringBuf[len] = 0;
		}
	Stack(result) = TSTRING(stringBuf);
	FrameExit(Stack(result));
	}
else

/*  An XML name must begin with an underscore _ or an alpha character. */

if (((*work->IP >= 'a') && (*work->IP <= 'z')) || ((*work->IP >= 'A') && (*work->IP <= 'Z')) || (*work->IP == '_'))
	{
	Stack(result) = FXml_recName(gCP,gTP,work);
	if (Stack(result).Tag != TYVOID) FrameExit(Stack(result));
	}
else

/*  Any other HTML attribute value ends with a whitespace character, or ! ? , >. */

if (work->htmlOn == TRUE)
	{
	len = 0;
	stringBuf[len++] = *work->IP;
	stringBuf[len] = 0;
	while ((*(++work->IP) > ' ') &&
	       (*(work->IP) != '!') &&
	       (*(work->IP) != '?') &&
	       (*(work->IP) != ',') &&
	       (*(work->IP) != '>'))
		{
		if (len >= 1020) FrameExit(TERROR("!XML: attribute constant too long!"));
		stringBuf[len++] = *work->IP;
		stringBuf[len] = 0;
		}
	Stack(result) = TSTRING(stringBuf);
	FrameExit(Stack(result));
	}

FrameExit(gCP->Tval_VOID);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FXml_Error

Return a parsing error in an XML source string. The error is returned
in a standard format.

#endif

TVAL FXml_Error(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR errorMsg,XMLWork* work)
{
#define	BUFLEN 1024
LpCHAR				INC;
NUM					i;				
NUM					j;				
NUM					line1;				
NUM					line2;
NUM					endline2;				
NUM					line3;				
NUM					line4;				
NUM					eof;				
NUM					tabs;				
NUM					nontabs;				
NUM					pos;				
NUM					lineno;
CHAR				ch;			
CHAR                errorBuf[(2*BUFLEN)];
CHAR				stringBuf[30];
StartFrame
DeclareTVAL(temp);
DeclareTVAL(result);
DeclareTVAL(msg);
DeclareTVAL(ec);
EndFrame
 
/*  Initialize the starting variables. */

INC = work->start;
pos = (NUM)(work->IP - work->start);
eof = (NUM)work->INLEN;
tabs = 0;
nontabs = 0;
lineno = 0;

/* Find the start of error line - line2. */

i = pos;
while ((i >= 0) && (INC[i] != 10) && (INC[i] != 13)) --i;
if (i > 0) 
	line2 = i + 1;
else
	line2 = 0;

/* find start of line1. */

while ((i >= 0) && ((INC[i] == 10) || (INC[i] == 13))) --i;
while ((i >= 0) && (INC[i] != 10) && (INC[i] != 13)) --i;
if (i > 0) 
	line1 = i + 1;
else
	line1 = 0;

/* Find start of line 3. */

i = pos; /* Reset to error position. */
while ((i < eof) && (INC[i] != 10) && (INC[i] != 13)) ++i;
endline2 = i;
while ((i < eof) && ((INC[i] == 10) || (INC[i] == 13))) ++i;
line3 = i;

/* Find start of line 4. */

while ((i < eof) && (INC[i] != 10) && (INC[i] != 13)) ++i;
while ((i < eof) && ((INC[i] == 10) || (INC[i] == 13))) ++i;
line4 = i;

/* Count number of tabs and non-tabs up to error in error line. */

j = line2;
while (j < pos)
	{
	if (INC[j] == 9)
		++tabs;
	else
		++nontabs;
    j++;
	}

/* Count number of lines up to the error line. */

j = 0;
while (j < pos)
	{
	if (INC[j] == 13) ++lineno;
    j++;
	}
++lineno;

/* Create error message. */

if ((line4 - line2) > BUFLEN)
	{
	/*  Create a short error message. */
	strncpy(stringBuf,&INC[pos],20);
	sprintf(errorBuf,"!%s%s%ld%s%s!",errorMsg," at [",pos,"] ",stringBuf);
	}
else
	{
	/*  Create a long error message. */
	ch = INC[endline2];
	INC[endline2] = 0;
	sprintf(errorBuf,
					"!%s%s%ld%s%ld,\n%s\n",
					errorMsg,
					" at line:",
					lineno,
					" char:",
					pos - line2 + 1,
					&INC[line1]);
	INC[endline2] = ch;
	j = (NUM)strlen(errorBuf);
	i = 0;
	while (i < tabs) errorBuf[j + i++] = 9;
	j += i;
	i = 0;
	while (i < nontabs) errorBuf[j + i++] = ' ';
	errorBuf[i + j] = 0;
	strcat(errorBuf,"^error\n");
	ch = INC[line4];
	INC[line4] = 0;
	strcat(errorBuf,&INC[line3]);
	INC[line4] = ch;
	strcat(errorBuf,"!");
	}

/* Send the errorMessage event (if necessary). */

if (work->eventHandler)
	{
	/* Send the errorMessage message symbol to the eventLambda. */
    /* Note: we do this only if we are in event handler mode. */
	Stack(msg) = TSTRING(errorBuf);
	Stack(ec) = FSmartbase_SendMsg(gCP,gTP,gErrorMessage,work->eventLambda,1,Stack(msg));
	ExitOnError(Stack(ec));
	}

/*  Return the error message. */

Stack(ec) = TERROR(errorBuf);
FrameExit(Stack(ec));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FXml_strcmpi

These cProcedures perform a case-insensitive comparison between the 
two strings {string1} and {string2}. If the comparison is true, then
true is returned; otherwise, false is returned. 

#endif

NUM FXml_strcmpi(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR left,LpCHAR right)      
{
register LpCHAR         sp1;
register LpCHAR         sp2;
register CHAR           c1;
register CHAR           c2;
NUM                     size;
 
/*  Note:   We must obtain a pointer to the left string. */

sp1 = left;
size = (NUM)strlen((char*)sp1);

/*  Note:   We must obtain a pointer to the right string. */

sp2 = right;

/*  Compare the two strings and return the result. */

while(TRUE)
    {
    c1 = *sp1++;
    c2 = *sp2++;
        
    if ((((c1 <= 'z') && (c1 >= 'a')) || ((c1 <= 'Z') && (c1 >= 'A'))) &&
        (((c2 <= 'z') && (c2 >= 'a')) || ((c2 <= 'Z') && (c2 >= 'A'))))
        {
        c1 |= 0x20; c2 |= 0x20;     
        }
        
    if (c1 < c2)
        {
        return(-1);
        }
    else
    if (c1 > c2)
        {
        return(1);
        }
    else
    if (c1 == 0)
        {
        return(0);
        }
    }

return(0);
}


