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

#ifndef _H_FXML
#define _H_FXML
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/xml/xml.h
										XML

This file contains the procedures required to support the Smartbase "xml" compiler function. This
is an actual compiler not a parser replacement for the "lisp" function. The output ofthis function
is a complete XML data model.

CHANGE HISTORY
Version	Date		Who		Change
1.0057	3/18/2005	tlw		Update documentation
							---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	-------------------------------------- DEFINITIONS --------------------------------------------
typedef struct XMLWork
{
NUM					past;			/* The XML past input character count */
LpCHAR				start;			/* The XML starting input pointer */
LpCHAR				INLEN;			/* The XML input string fragment length */
LpCHAR				IP;				/* The XML input pointer */
TVAL                inputString;	/* The XML current input string */
TVAL                Document;		/* The XML document model */
BOLE				inputProvider;	/* The input provider is present flag */
TVAL				inputLambda;		/* The input provider Lambda (if any) */
BOLE				eventHandler;	/* The event handler is present flag */
TVAL				eventLambda;		/* The event Lambda (if any) */
BOLE				piHandler;		/* The process instruction handler is present flag */
TVAL				piStructure;	/* The process instruction Structure (if any) */
BOLE				htmlOn;			/* The HTML is processing flag */
} XMLWork;


/* Function Library */

extern  TVAL    FXml_Init(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FXml_Compiler(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FXml_Contents(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,XMLWork* w,NUM tagDepth);
extern	TVAL	FXml_Element(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work,NUM tagDepth);      
extern	TVAL	FXml_CData(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work);      
extern	TVAL	FXml_ScriptInstruction(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work);      
extern	TVAL	FXml_ProcessInstruction(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work);      
extern	TVAL	FXml_DoctypeHeader(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work,NUM tagDepth);      
extern	TVAL	FXml_Comments(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theContents,TVAL theResult,XMLWork* work);    
extern  TVAL    FXml_AttributeList(LpXCONTEXT gCP,LpTHREAD gTP,XMLWork* w);
extern  TVAL    FXml_recName(LpXCONTEXT gCP,LpTHREAD gTP,XMLWork* w);
extern  TVAL    FXml_recValue(LpXCONTEXT gCP,LpTHREAD gTP,XMLWork* w);
extern  TVAL    FXml_Error(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR errorMsg,XMLWork* w);
extern  NUM		FXml_strcmpi(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR left,LpCHAR right);     

/* Recognition macros */

#define SP		while ((*work->IP <= 32) && (*work->IP > 0)) ++work->IP;

#endif	// _H_FXML
