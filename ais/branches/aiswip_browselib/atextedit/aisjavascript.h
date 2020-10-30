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

#ifndef AISJAVASCRIPT_H
#define AISJAVASCRIPT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/aisjavascript.h
												Javascript Syntax Definitions
Aisjavascript.h holds the comment delimiters and keywords for the javascript language.

CHANGE HISTORY
Version	Date		Who		Change
2.0004	 2/17/2007	tlw		Add quote characters to syntax highlighting.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ------------------
NOTES
 1. See atextstaticdefs.h for more info on language definitions.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
ALangDef scJavascriptDef = { "js", "\"'", "\\", "//,\n,/*,*/", "^\\s*function\\s+([^(\\s]+)\\s*\\(",
"all,avg,bottom,check,checkoff,filter,omit,restore,sum,score,slice,sort,top"
"bool,char,class,cvar,define,display,else,extends,for,friend,function,if,int,"
"is,isVoid,math,matrix,mod,new,of,orphan,pvar,return,rulesLib,setnr,"
"string,symbol,text,today,var,while,writeln"};
#endif	// AISJAVASCRIPT_H
