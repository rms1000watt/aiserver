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

#ifndef AISCPP_H
#define AISCPP_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/aiscpp.h
														CPP Syntax Definitions
Aiscpp.h holds the comment delimiters and keywords for the C++ language.

CHANGE HISTORY
Version	Date		Who		Change
2.0004	 2/17/2007	tlw		Add quote characters to syntax highlighting.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ------------------
NOTES
 1. See atextstaticdefs.h for more info on language definitions.

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	---------------------------------------------------- DEFINITIONS ---------------------------------------------------------
ALangDef scCppDef = { "c,cpp,h", "\"", "\\", "//,\n,/*,*/", "^[\\*\\&\\w]+\\s+([^(\\s]+)\\s*\\([^)]+\\)",
"define,endif,ifdef,ifndef,include,"
"auto,break,case,char,const,continue,default,do,double,"
"else,enum,extern,false,FALSE,float,for,goto,if,int,long,register,return,short,signed,"
"sizeof,static,struct,switch,true,TRUE,typedef,undef,union,unsigned,void,volatile,while"
// C++ keywords
"asm,catch,class,delete,friend,inline,new,operator,private,protected,public,template,this,throw,"
"try,virtual"};
#endif	// AISCPP_H
