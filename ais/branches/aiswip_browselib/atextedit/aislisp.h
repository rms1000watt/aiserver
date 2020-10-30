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

#ifndef AISLISP_H
#define AISLISP_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/aislisp.h
												AIS Lisp Syntax Definitions
Aislisp.h holds the comment delimiters and keywords for the AIS lisp language.

CHANGE HISTORY
Version	Date		Who		Change
2.0004	 2/17/2007	tlw		Add quote characters and escape char.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------

NOTES
 1. See atextstaticdefs.h for more info on language definitions.
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
ALangDef scLispDef = { "sl", "\"{", "\\", ";,\n", "^\\s*\\(def\\w{2,6}\\s+([^(\\s]+)\\s*\\(",
"absolute,add1,addi,and,append,appendWriteln,apply,argCount,"
"argFetch,argument,"
"badd,balance,bdiv,begin,binaryInsert,binaryNot,binarySearch,bitwiseNot,"
"bitToIntegerVector,bitToNumberVector,bmod,bmul,boolean,by,"
"cadd,car,case,cdiv,cdr,char,character,clean,cmod,cmul,code,"
"compare,compareEQ,compareNE,compareGT,compareGE,compareLT,compareLE,"
"compile,complex,cond,conjugate,cons,copy,cos,count,cvars,"
"date,day,debug,defchild,define,defineStructure,defmacro,defmethod,"
"deforphan,defriend,defstruct,defun,delete,dic,dir,display,divi,divide,do,downcase,"
"else,end,error,errorTrap,eval,exp,expt,exportTab,"
"faces,false,fieldsOf,find,findBlock,for,freeBlock,from,"
"getTickCount,goto,gotoLT,gotoLE,gotoGT,gotoGE,gotoEQ,gotoNE,"
"hour,"
"iadd,idiv,if,imaginary,imod,importTab,imul,insert,inspect,integer,"
"isLambda,isAtom,isBitVector,isBoolean,isBound,isChar,isCharacter,"
"isCharAlphabetic,isCharAlphanumeric,isCharLowercase,isCharName,isCharNumeric,"
"isCharUppercase,isCharWhitespace,isComplex,"
"isDate,isDictionary,isDirectory,isEqual,isExact,isEven,isFloatVector,"
"isIdentical,isImmediate,isInexact,isInteger,isIntegerVector,isMember,"
"isMoney,isNegative,isNumber,isNumberVector,isNull,"
"isObject,isObjectVector,isOdd,isPair,isPcodeVector,isPositive,isString,"
"isStructure,isSymbol,isText,isub,isVector,isZero,isTransaction,isType,"
"jmp,julian,"
"lambda,last,left,length,let,list,loadRepository,lock,log,loop,"
"macro,makeQuotedList,makeQuotedSymbol,makeStructure,map,mapc,member,"
"memstat,methodsOf,"
"mid,minute,modulus,money,month,morph,muli,multiply,myself,"
"nadd,ncompare,ndiv,new,nmod,nmul,not,now,nsub,number,"
"objectToDictionary,objectToDirectory,objectToList,objectToMatrix,"
"objectToNumMatrix,objectToNumVector,objectToVector,onError,or,"
"pair,parse,proplist,proprecord,pvars,quote,"
"rank,real,ref,refAttributes,refValues,remove,remProp,rename,"
"replace,rept,resize,return,reverse,right,"
"saveImmediate,second,send,set,setAttributes,setCar,setCdr,setf,"
"setLastCdr,setq,sin,sizeof,sort,sqrt,stringFill,sub1,subi,subtract,super,"
"tan,text,then,time,to,today,trim,true,type,"
"uniqueInsert,until,upcase,vars,vectorDelete,void,"
"while,writeln,year"};
#endif	// AISLISP_H
