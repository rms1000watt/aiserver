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

#if 0
fmysql1.h

This include file contains external variable declarations, macro definitions and function prototypes
for the FMySQL1.c source file.

Th MySQL extension gives the smartbase engine the ability to connect to an embedded MySQL server
or to an external MySQL server and perform both MySQL and standard Structured Query Language (SQL)
queries.

This extension provides a single Lisp interface to access the various functionalities of MySQL.

PARENT:             None


AUTHORS:            Michael F. Korns
					Franklin Chua

#endif

#ifndef _H_FMYSQL
#define _H_FMYSQL

#include    "tvector.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tintvec.h"
#include    "tlambda.h"
#include    "tbrick.h"

#if __EXMYSQL


/*  MACRO DECLARATIONS */

#define MYSQL_BLOB_AIS_ASCII    "AIS_ASC:"   /* ascii representation header */
#define MYSQL_BLOB_AIS_BINARY   "AIS_BIN:"   /* binary representation header */
#define MYSQL_BLOB_HDR_LENGTH   8            /* length of the ascii/binary header */

#define MySQLExitOnError(value)             if (asTag(&value) == TYERROR) { if (mysqlResult) mysql_free_result(mysqlResult); FrameReset; return(value); }
#define MySQLCloseExitOnError(value)        if (asTag(&value) == TYERROR) { if (mysqlPtr) mysql_close(mysqlPtr); FrameReset; return(value); }

/*  FUNCTION DECLARATIONS */

extern  TVAL    FMySQL1_Init                (LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL    FMySQL1_End                 (LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL    FMySQL1_Sql                 (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlBit              (LpXCONTEXT gCP, LpTHREAD gTP, TVAL bitvector);
extern  TVAL    FMySQL1_SqlConnect          (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlCreate           (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlDate             (LpXCONTEXT gCP, LpTHREAD gTP, TVAL date);
extern  TVAL    FMySQL1_SqlDisconnect       (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlDisconnectAll    (LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL    FMySQL1_SqlEscape           (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlGetBrickSchema   (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlInfo             (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlInsert           (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlQuery            (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlTest             (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlUpdate           (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
extern  TVAL    FMySQL1_SqlUseWord          (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);

#endif

#endif
