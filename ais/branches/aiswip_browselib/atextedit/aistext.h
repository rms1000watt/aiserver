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

#ifndef AISTEXT_H
#define AISTEXT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/aistext.h
												Plain Text Syntax Definitions
This language definition file holds the default extensions and comment delimiters for plain text files.

CHANGE HISTORY
Version	Date		Who		Change
2.0004	 2/17/2007	tlw		Add place holder for quote character and escape char.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ------------------
NOTES
 1.	See atextstaticdefs.h for more info on language definitions

Text files do not include any comment lines or keywords.  Paragraph headers of the form:
	  Title. ...
are placed in the function list.  The first line after a blank line might be a better choice.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
ALangDef scTextDef = { "txt,log,ini", "", "", "", "^\\s+([^.\\s]+)\\.", ""};
#endif	// AISTEXT_H
