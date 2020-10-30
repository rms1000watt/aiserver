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

#define _C_FMYSQL
#define _SMARTBASE
#if 0
fmysql1.c

This source file contains function definitions for the fmysql1.h header file.

Please refer to fmysql1.h for more information.

PARENT:             None


AUTHORS:            Michael F. Korns
                    Franklin Chua

#endif

#include "fmysql1.h"
#include "foptimize.h"
#include "tpair.h"
#include "tdiction.h"
#include "tdirect.h"
#include "futil1.h"
#include "futil2.h"
#include "terror.h"
#include "fmake.h"
#include "tobject.h"
#include "tobjvec.h"
#include "fvmcode.h"
#include "fvmscpt.h"
#include "fvmscpt2.h"
#include "fmacro.h"
#include "fsforms1.h"
#include "fsforms2.h"
#include "fsforms3.h"
#include "fconio.h"
#include "fopt1.h"
#include "fopt2.h"
#include "fdebug.h"
#include "flisp.h"
#include "fproc.h"
#include "tvector.h"
#include "tbrick.h"
#include "fdatefnc.h"
#include "tbytevec.h"
#include "tbitvec.h"
#include "ftextfnc.h"
#include "fconvert.h"
#include "tintvec.h"
#include "tstruct.h"

#if __EXMYSQL
/* Implementation Note: If MySQL is included (FSmtbase.h), we must also     */
/* add the following include path to the properties for this source file:   */
/* "C:\Program Files\MySQL\MySQL Server 5.1\include"                        */
#if _WIN
typedef unsigned int  SOCKET;
#endif
#include "mysql.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FMySQL1_Init

In order to initialize the MySQL facility, we register the cProcedures contained in this
file and also allocate and initialize any data structures used by the MySQL interface.

#endif

TVAL FMySQL1_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ret);
EndFrame

/*  We utilize the static FMySQL_Initialized as a flag which is set the first time that */
/*  this procedure is invoked to insure that initialization will occur only one time. */

if(gCP->FMySQL1_Initialized)
    {
    FrameExit(gCP->TObject_OK);
    }
else
    gCP->FMySQL1_Initialized = 1;

/* We will only call MySQL functions if MySQL is enabled */
if (gCP->FMySQL1_Enabled)
    {
    mysql_thread_init();
    /* mysql_thread_end() should be called before the context thread is terminated */

    /* We will keep a list of opened MySQL connections */
    gCP->FMySQL1_IntVector = TIntVector_New(gCP, gTP);
    gCP->FMySQL1_InfoVector = TVector_New(gCP, gTP);
    gCP->FMySQL1_UseWordInt = FALSE;
    gCP->FMySQL1_UseWordFlt = FALSE;
    gCP->FMySQL1_UseWordDat = FALSE;

    /* Make sure, we don't let the garbage collector deallocate our objects */
    FObject_Perm(gCP, gTP, (TObject*)gCP->FMySQL1_IntVector, TRUE);
    FObject_Perm(gCP, gTP, (TObject*)gCP->FMySQL1_InfoVector, TRUE);

    /* We will store the default embedded connection in index: 0 */
    *ret = TIntVector_AddNewValue(gCP, gTP, TOBJ(gCP->FMySQL1_IntVector), TINT(0));
    ExitOnError(*ret);

    *ret = TVector_AddNewValue(gCP, gTP, TOBJ(gCP->FMySQL1_InfoVector), gCP->TObject_VOID);
    ExitOnError(*ret);
    }

/* Register the new function for MySQL sql */
*ret = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"sql",(LpFUNC)&FMySQL1_Sql);
ExitOnError(*ret);

*ret = FSmartbase_RegisterCProcedure(gCP, gTP, (LpCHAR)"sqlend", (LpFUNC)&FMySQL1_End);
ExitOnError(*ret);

FrameExit(gCP->TObject_OK);
}

#if 0
FMySQL1_End

Closes all opened MySQL connections and release thread resources

#endif

TVAL FMySQL1_End(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ret);
EndFrame

/* we will only call MySQL functions if MySQL is enabled */
if (gCP->FMySQL1_Enabled)
    {
    /* disconnect MySQL connections */
    *ret = FMySQL1_SqlDisconnectAll(gCP, gTP);
    ExitOnError(*ret);

    /* release thread resources */
    mysql_thread_end();

    /* make sure we disable future calls to MySQL */
    gCP->FMySQL1_Enabled = FALSE;
    }

*ret = gCP->TObject_TRUE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
/*
FMySQL1_Sql

This is the procedure which serves as an entry point for the "sql" procedure defined in the
Lambda SQL Ref. Guide. It will input a set of arguments representing a MySQL command or an AIS
MySQL extension command.

Supported Commands:

1. (sql connect:)
- Connects to a MySQL server.
- To connect to the embedded server (if supported), the parameters should be left empty.
- To connect to an external server, the following parameters should be specified:
    * hostname (required) - the hostname or IP address of the server
    * database (optional) - current active database (if existing in MySQL server)
    * username (optional) - MySQL user account name
    * password (optional) - password (if defined in MySQL server)
- Returns a handle to the MySQL server connection.
- The return handle shall be pass to succeeding calls to (sql).

2. (sql disconnect: sqlHandle)
- Close an active MySQL connection.
- To close a MySQL connection, the connection handle is specified as the first parameter.

3. (sql disconnectall:)
- Close all active MySQL connections.

4. (sql sqlHandle query arguments) or (sql query arguments)
- Execute a MYSQL command or an SQL query on the MYSQL database or server.
- Passing a MySQL handle followed by a series of query arguments would execute the query on the specified MYSQL connection.
- The MySQL handle is an INTEGER value returned from a previous call to (sql connect:).
- The query arguments are one or more arguments which are appended into a Query String (much like writeln appends its arguments into a display String).
- The return of this command would depend on the MYSQL command or SQL statement issued:
    * SELECT, SHOW, DESCRIBE, EXPLAIN, CHECK TABLE - returns a result set (RECORD)
    * INSERT, UPDATE, DELETE - returns the number of affected rows (INTEGER)

5. (sql insert: sqlHandle table brick binarySW)
   (sql insert: sqlHandle table vectorOfBrickRefs binarySW)
- Inserts the contents of a record object to a SQL table
- Parameters:
    * sqlHandle         - handle returned by sql connect:
    * table             - name of the existing table
    * brick             - collection of rows that will be added to the table
    * vectorOfBrickRefs - collection of bricks that will be added to the table
    * binarySW          - true if we use binary encoding for objects else
                          we use ascii encoding for objects in Brick fields

6. (sql escape: sqlHandle string) or (sql escape: string)
- Escapes a string using the server's character set.
- Characters encoded include NUL, \n, \r, \, ', ", and Control-Z.

7. (sql info: sqlHandle)
- Displays the connection information of the given MySQL connection.

8. (sql create: sqlHandle brick [additionalDDLCmds])
- Creates a table from the Brick schema

9. (sql getbrickschema: sqlHandle table)
- Returns a Brick definition based from the table schema

10. (sql update: sqlHandle table brick [additionalDMLCmds])

11. (sql useWord: typeName: typeSw)
- Enables or disables the use of Word for columns which can contain NULL values.
- Parameters:
    * typeName - Integer, Float, or Date
    * typeSw - true or false

Examples:
(setq sqlHandle (sql connect:))    // for embedded server (default)
(setq sqlHandle (sql connect: "")) // for embedded server (new)
(setq sqlHandle (sql connect: -1)) // for embedded server (new)
(setq sqlHandle (sql connect: "localhost" #void "username" "password"))      // for external server w/o default database
(setq sqlHandle (sql connect: "localhost" "database" "username" "password")) // for external server w/ default database

(sql disconnect: sqlHandle)
(sql disconnectall:)

(setq tableName "myTable")
(sql sqlHandle "SELECT * FROM myTable")
(sql sqlHandle "CREATE TABLE " tableName)
(sql sqlHandle "CREATE DATABASE " databaseName)

Notes:
For passwords, "" is not the same as NULL or #void.

*/

TVAL FMySQL1_Sql(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

if (!gCP->FMySQL1_Enabled)
{
    *ret = TERROR("!sql: This feature has been disabled either manually or due to failed initialization!");
    goto Last;
}

/* ************************************************/
/* Manage the SQL connect command: (sql connect:) */
/* ************************************************/

if ((argc >= 1) &&
    (argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"connect") == 0))
    {
    *ret = FMySQL1_SqlConnect(gCP, gTP, argc, argv);
    } /* (sql connect:) */
else

/* ************************************************/
/* Manage the SQL create command: (sql create:) */
/* ************************************************/

if ((argc >= 1) &&
	(argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"create") == 0))
	{
	*ret = FMySQL1_SqlCreate(gCP, gTP, argc, argv);
	} /* (sql create:) */
else

/* ****************************************************************/
/* Manage the SQL disconnect command: (sql disconnect: sqlHandle) */
/* ****************************************************************/
if ((argc >= 1) &&
    (argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"disconnect") == 0))
    {
    *ret = FMySQL1_SqlDisconnect(gCP, gTP, argc, argv);
    } /* (sql disconnect:) */
else

/* ****************************************************************/
/* Manage the SQL disconnectall command: (sql disconnectall:) */
/* ****************************************************************/
if ((argc >= 1) &&
    (argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"disconnectall") == 0))
    {
    *ret = FMySQL1_SqlDisconnectAll(gCP, gTP);
    } /* (sql disconnectall:) */
else

/* ******************************************************************************/
/* Manage the SQL escape command: (sql escape: sqlHandle string) */
/* ******************************************************************************/
if ((argc >= 1) &&
    (argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"escape") == 0))
    {
    *ret = FMySQL1_SqlEscape(gCP,gTP,argc,argv);
    } /* (sql escape:) */
else

/* ******************************************************************************/
/* Manage the SQL getbrickschema command: (sql getbrickschema: sqlHandle table) */
/* ******************************************************************************/
if ((argc >= 1) &&
	(argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"getbrickschema") == 0))
	{
	*ret = FMySQL1_SqlGetBrickSchema(gCP,gTP,argc,argv);
	} /* (sql getbrickschema:) */
else

/* ******************************************************************************/
/* Manage the SQL info command: (sql info: sqlHandle) */
/* ******************************************************************************/
if ((argc >= 1) &&
    (argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"info") == 0))
    {
    *ret = FMySQL1_SqlInfo(gCP, gTP, argc, argv);
    } /* (sql info:) */
else

/* ******************************************************************************/
/* Manage the SQL insert command: (sql insert: sqlHandle table brick binarySW) */
/* ******************************************************************************/
if ((argc >= 1) &&
    (argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"insert") == 0))
    {
    *ret = FMySQL1_SqlInsert(gCP, gTP, argc, argv);
    } /* (sql insert:) */
else

/* ******************************************************************************/
/* Manage the SQL test command: (sql test: sqlHandle) */
/* ******************************************************************************/
if ((argc >= 1) &&
    (argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"test") == 0))
    {
    *ret = FMySQL1_SqlTest(gCP, gTP, argc, argv);
    } /* (sql test:) */
else

/* ******************************************************************************/
/* Manage the SQL useWord command: (sql update: ...) */
/* ******************************************************************************/
if ((argc >= 1) &&
	(argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"update") == 0))
	{
	*ret = FMySQL1_SqlUpdate(gCP, gTP, argc, argv);
	} /* (sql update:) */
else

/* ******************************************************************************/
/* Manage the SQL useWord command: (sql useWord: ...) */
/* ******************************************************************************/
if ((argc >= 1) &&
	(argv[0].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[0]),"useWord") == 0))
	{
	*ret = FMySQL1_SqlUseWord(gCP, gTP, argc, argv);
	} /* (sql useWord:) */
else

/* **********************************************************************/
/* Manage the SQL query command: (sql sqlHandle ...query arguments...)  */
/* **********************************************************************/
if ((argc >= 2) && (argv[0].Tag == TYNUM || argv[0].Tag == TYVOID))
    {
    *ret = FMySQL1_SqlQuery(gCP, gTP, argc, argv);
    } /* (sql handle query ...) */
else

/* **********************************************************************/
/* Manage the SQL query command: (sql ...query arguments...)  */
/* **********************************************************************/
if ((argc >= 1) && ((argv[0].Tag == TYSTRING) || (argv[0].Tag == TYTEXT)))
    {
    *ret = FMySQL1_SqlQuery(gCP, gTP, argc, argv);
    } /* (sql query ...) */
else

/* **********************/
/* Unknown SQL command  */
/* **********************/
    {
    *ret = TERROR("!sql: invalid sql command!");
    }

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlBit

This procedure converts a BitVector to an SQL compatible bit format.

Result: b'########'
#endif
TVAL FMySQL1_SqlBit(LpXCONTEXT gCP, LpTHREAD gTP, TVAL bitvector)
{
NUM         indexOf = 0;
NUM         length = 0;
LpCHAR      srcPtr = NULL;
LpCHAR      dstPtr = NULL;

StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,2);
EndFrame

/*  Check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

if (bitvector.Tag != TYBITVECTOR)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* b'...' */
length = bitvector.u.BitVector->itsMaxItemIndex;
prmv[0] = TINT(length + 3);
*ret = TByteVector_MakeNew(gCP,gTP,1,prmv);
ExitOnError(*ret);

srcPtr = BitArray(bitvector);
dstPtr = ByteArray(*ret);

dstPtr[0] = 'b';
dstPtr[1] = '\'';
dstPtr += 2;

for (indexOf = 0; indexOf < length; indexOf++)
    dstPtr[indexOf] = '0' + ((srcPtr[indexOf/8] & gCP->TBitVector_OrMasks[indexOf%8]) != 0);

dstPtr[length] = '\'';

FrameExit(*ret);
}

#if 0
FMySQL1_SqlConnect

This procedure creates a MySQL connection

Result: MySQL handle (pointer converted to integer)
#endif
TVAL FMySQL1_SqlConnect(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
LpCHAR          hostPtr = NULL;
LpCHAR          userNamePtr = NULL;
LpCHAR          passWordPtr = NULL;
LpCHAR          databasePtr = NULL;
LpCHAR          errorPtr = NULL;

BOLE            useDefaultEmbedded = FALSE;
MYSQL*          mysqlPtr = NULL;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(symbol);
DeclareOBJ(TStructure,info_entry);
DeclareTVAL(list);
DeclareTVAL(info);
EndFrame

/*  Check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* if host argument is "" or #void, use the _default embedded connection */
/* if host argument is -1, create a new embedded connection */
if (argc >= 2)
    {
    if (argv[1].Tag == TYVOID)
        {
        /* (sql connect: #void) */
        /* use existing embedded connection */
        useDefaultEmbedded = TRUE;
        }
    else
    if ((isNumIndex(&argv[1])) && (asNumIndex(&argv[1]) == -1))
        {
        /* (sql connect: -1) */
        /* create new embedded connection */
        }
    else
    if (((argv[1].Tag == TYSTRING) && ((hostPtr = CharArray(argv[1])) != 0)) ||
        ((argv[1].Tag == TYSYMBOL) && ((hostPtr = SymbolArray(argv[1])) != 0)) ||
        ((argv[1].Tag == TYTEXT) && ((hostPtr = asText(&argv[1])) != 0 )))
        {
        if (strcmp(hostPtr,"") == 0)
            {
            /* (sql connect: "") */
            /* use existing embedded connection */
            useDefaultEmbedded = TRUE;
            hostPtr = NULL;
            }
        }
    else
        {
        *ret = TERROR("!sql.connect: invalid host argument!");
        goto Last;
        }

    /* if database argument is #void, use NULL */
    if (argc >= 3)
        {
        /* Get the database argument */
        if (!((argv[2].Tag == TYVOID) ||
              ((argv[2].Tag == TYSTRING) && ((databasePtr = CharArray(argv[2])) != 0)) ||
              ((argv[2].Tag == TYSYMBOL) && ((databasePtr = SymbolArray(argv[2])) != 0)) ||
              ((argv[2].Tag == TYTEXT) && ((databasePtr = asText(&argv[2])) != 0))))
            {
            *ret = TERROR("!sql.connect: invalid database argument!");
            goto Last;
            }

        /* Get the username argument, if specified */
        if (argc >= 4)
            {
            if (!(((argv[3].Tag == TYSTRING) && ((userNamePtr = CharArray(argv[3])) != 0)) ||
                  ((argv[3].Tag == TYSYMBOL) && ((userNamePtr = SymbolArray(argv[3])) != 0)) ||
                  ((argv[3].Tag == TYTEXT) && ((userNamePtr = asText(&argv[3])) != 0))))
                {
                *ret = TERROR("!sql.connect: invalid username argument!");
                goto Last;
                }
            } /* argc >= 4 */

        /* Get the password argument, if specified */
        if (argc >= 5)
            {
            if (!(((argv[4].Tag == TYSTRING) && ((passWordPtr = CharArray(argv[4])) != 0)) ||
                  ((argv[4].Tag == TYSYMBOL) && ((passWordPtr = SymbolArray(argv[4])) != 0)) ||
                  ((argv[4].Tag == TYTEXT) && ((passWordPtr = asText(&argv[4])) != 0)) ||
                  ((argv[4].Tag == TYVOID))))
                {
                *ret = TERROR("!sql.connect: invalid password argument!");
                goto Last;
                }
            } /* argc >= 5 */

        } /* argc >= 3 */
    } /* argc >= 2 */
else
    {
    /* no additional parameters were specified */
    /* (sql connect:) */
    useDefaultEmbedded = TRUE;
    }

if (useDefaultEmbedded)
    {
    /* looks like we have a possible security issue */
    /* the global context variable can be changed by the user deliberately */
    /* there is no way for us to verify if the value is correct */
    *ret = TIntVector_GetIV1(gCP, gTP, TOBJ(gCP->FMySQL1_IntVector), TINT(0));
    ExitOnError(*ret);

    /* get default connection in index: 0 */
    mysqlPtr = (MYSQL*)asInt(ret);

    if (mysqlPtr == NULL)
        {
        /* default embedded connection not set */
        mysqlPtr = mysql_init(NULL);
        /* we will save the connection later */
        }
    else
        {
        /* default embedded connection already set */
        /* return connection handle immediately */
        *ret = TINT(mysqlPtr);
        FrameExit(*ret);
        }
    }
else
    {
    /* other embedded connections and remote connections */
    mysqlPtr = mysql_init(NULL);
    /* we will save the connection later */
    }

/* process new connections */
if (mysqlPtr == NULL)
    {
    FrameExit(TERROR("!sql.connect: insufficient memory!"));
    }

/* for the use of remote connection if hostname is present */
if (hostPtr != NULL)
    {
    mysql_options(mysqlPtr, MYSQL_OPT_USE_REMOTE_CONNECTION, 0);
    }

/* connect to MySQL server */
if (mysql_real_connect((MYSQL*)mysqlPtr, hostPtr, userNamePtr, passWordPtr, databasePtr, 0, NULL, 0) == NULL)
    {
    /* connection attempt failed */

    /* get the error */
    errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.connect: %s!", errorPtr);

    /* close MYSQL connection */
    mysql_close((MYSQL*)mysqlPtr);
    }
else
    {
    /* connection successful */
    *list = TOBJ(gCP->FMySQL1_IntVector);
    *info = TOBJ(gCP->FMySQL1_InfoVector);

    /* do final processing */
    if (useDefaultEmbedded)
        {
        /* save default embedded connection in index: 0 */
        *ret = TIntVector_SetIV1(gCP, gTP, *list, TINT(0), TINT(mysqlPtr));
        MySQLCloseExitOnError(*ret);

        info_entry = TStructure_New(gCP, gTP);
        *ret = TStructure_AddNewValue(gCP, gTP, TOBJ(info_entry), TSYMBOL("type"), TSTRING("Embedded"));
        MySQLCloseExitOnError(*ret);
        *ret = TStructure_AddNewValue(gCP, gTP, TOBJ(info_entry), TSYMBOL("host"), TSTRING(""));
        MySQLCloseExitOnError(*ret);

        *ret = TVector_SetIV1(gCP, gTP, *info, TINT(0), TOBJ(info_entry));
        MySQLCloseExitOnError(*ret);

        /* get the default connection */
        *symbol = TSYMBOL("_defaultSqlHandle");
        MySQLCloseExitOnError(*symbol);

        /* save to context global variable */
        *ret = TSymbol_SetGlobalValue(gCP, gTP, asSymbol(symbol), TINT(mysqlPtr));
        MySQLCloseExitOnError(*ret);
        }
    else
        {
        /* add the new connection to the list */
        *ret = TIntVector_AddNewValue(gCP, gTP, *list, TINT(mysqlPtr));
        MySQLCloseExitOnError(*ret);

        info_entry = TStructure_New(gCP, gTP);
        *ret = TStructure_AddNewValue(gCP, gTP, TOBJ(info_entry), TSYMBOL("type"), TSTRING("Out-of-Process"));
        MySQLCloseExitOnError(*ret);
        *ret = TStructure_AddNewValue(gCP, gTP, TOBJ(info_entry), TSYMBOL("host"), TSTRING(hostPtr));
        MySQLCloseExitOnError(*ret);
        *ret = TStructure_AddNewValue(gCP, gTP, TOBJ(info_entry), TSYMBOL("username"), TSTRING(userNamePtr));
        MySQLCloseExitOnError(*ret);

        *ret = TVector_AddNewValue(gCP, gTP, *info, TOBJ(info_entry));
        MySQLCloseExitOnError(*ret);
        }

    /* return our MySQL handle as an Integer */
    *ret = TINT(mysqlPtr);
    }

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlCreate

This procedure creates a table from a Brick definition.
If [additionalDDLCmds] is present, it will be appended to the
SQL create definition list.

CREATE TABLE table (create definition,... "PRIMARY KEY (id))"

The caller can introduce additional DLL statements such as
CONSTRAINT, KEY, and INDEX which will be part of the create definition list.
The create definition list MUST be terminated by a closing parenthesis.

After the closing parenthesis, the caller can include optional
table and partition options.

(sql create: sqlHandle table brick [additionalDDLCmds])

Result: TRUE if the table was created, FALSE if table already exists.

#endif
TVAL FMySQL1_SqlCreate(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
LpCHAR          errorPtr = NULL;
LpCHAR          queryPtr = NULL;
LpCHAR          tablePtr = NULL;
LpCHAR          ddlCmdPtr = NULL;
LpBIND          bindPtr = NULL;
BOLE            tblExists = FALSE;

MYSQL*          mysqlPtr = NULL;
MYSQL_RES*      mysqlResult = NULL;

UNUM            fieldIdx = 0;
UNUM            numFields = 0;

StartFrame
DeclareTVAL(query);
DeclareTVAL(table);
DeclareTVAL(ret);
DeclareTVAL(val);
DeclareTVAL(temp);
DeclareOBJ(TBrick,recordObj);
DeclareOBJ(TStructure,fieldListObj);
DeclareTVALArray(prmv,20);
EndFrame

/* check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. and type of arguments */
if (argc < 4)
	{
	*ret = gCP->TObject_ERROR_INVALID_ARGLIST;
	goto Last;
	}

/* get our MySQL handle */
if (argv[1].Tag == TYVOID)
	{
	/* (sql create: #void ...) */
UseDefault:
	/* use the default embedded connection */
	prmv[0] = TSYMBOL("connect");
	*ret = FMySQL1_SqlConnect(gCP, gTP, 1, prmv);
	ExitOnError(*ret);

	mysqlPtr = (MYSQL*)asInt(ret);
	}
else
if (argv[1].Tag == TYNUM)
	{
	mysqlPtr = (MYSQL*)asInt(&argv[1]);
	if (mysqlPtr == NULL)
		{
		/* (sql create: 0 ...) */
		goto UseDefault;
		}
	else
		{
		/* (sql create: non-zero ...) */
		/* check if connection is valid */
		prmv[0] = TSYMBOL("test");
		prmv[1] = argv[1];
		*ret = FMySQL1_SqlTest(gCP, gTP, 2, prmv);
		ExitOnError(*ret);

		if (asBool(ret) == FALSE)
			{
			*ret = TERROR("!sql.create: MySQL handle is not valid!");
			goto Last;
			}
		}
	}
else
	{
	*ret = TERROR("!sql.create: MySQL handle is not valid!");
	goto Last;
	}

/* get the table name */
if (!(((argv[2].Tag == TYSTRING) && ((tablePtr = CharArray(argv[2])) != 0)) ||
	  ((argv[2].Tag == TYSYMBOL) && ((tablePtr = SymbolArray(argv[2])) != 0)) ||
	  ((argv[2].Tag == TYTEXT) && ((tablePtr = asText(&argv[2])) != 0))))
	{
	*ret = TERROR("!sql.create: invalid table argument!");
	goto Last;
	}

/* check if table name is empty */
if (strlen(tablePtr) == 0)
	{
	*ret = TERROR("!sql.create: table name is blank!");
	goto Last;
	}

/* get the Brick object */
if (!((argv[3].Tag == TYBRICK) && ((recordObj = argv[3].u.Brick) != 0)))
	{
	*ret = TERROR("!sql.create: invalid brick argument!");
	goto Last;
	}

/* get the additional DLL commands */
if (argc >= 5)
	{
	if (!(((argv[4].Tag == TYSTRING) && ((ddlCmdPtr = CharArray(argv[4])) != 0)) ||
		  ((argv[4].Tag == TYSYMBOL) && ((ddlCmdPtr = SymbolArray(argv[4])) != 0)) ||
		  ((argv[4].Tag == TYTEXT) && ((ddlCmdPtr = asText(&argv[4])) != 0))))
		{
		*ret = TERROR("!sql.create: invalid DDL argument!");
		goto Last;
		}
	}

/* escape the "'" character in the table name, since we're using it in the query */
prmv[0] = TSTRING(tablePtr);
prmv[1] = TSTRING("'");
prmv[2] = TSTRING("''");
*table = FTextFnc_substitute(gCP,gTP,3,prmv);

/********************************************************/
/* Build SQL statement for checking if the table exists */
/********************************************************/

/* This way, we don't have to go through the trouble of building the long SQL statement */

prmv[0] = TSTRING("SHOW TABLES LIKE '");
prmv[1] = *table;
prmv[2] = TSTRING("'");
*query = FUtil2_Append(gCP,gTP,3,prmv);

if (query->Tag == TYTEXT)
	queryPtr = asText(query);
else
if (query->Tag == TYSTRING)
	queryPtr = CharArray(*query);
else
	{
	*ret = TERROR("!sql.create: failed in building sql query string!");
	goto Last;
	}

/* Execute our SQL SHOW TABLES statement */
if (mysql_query(mysqlPtr, queryPtr) != 0)
	{
	errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
	*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.create: %s!", errorPtr);
	goto Last;
	}

mysqlResult = mysql_store_result(mysqlPtr);

/* Check if the result set is not empty */
if (mysql_num_rows(mysqlResult) > 0)
	tblExists = TRUE;

mysql_free_result(mysqlResult);

if (tblExists)
	{
	*ret = gCP->TObject_FALSE;
	goto Last;
	}

mysqlResult = NULL;

if (!tblExists)
	{
    /**********************************************/
    /* Build SQL statement for creating the table */
    /**********************************************/

    /* SQL SYNTAX: CREATE TABLE [IF NOT EXISTS] table_name
     * (create_definition, ...)
     * [table_options]
     * [partition_options] */

	/* escape the "`" character in the table name, since we're using it in the query */
	prmv[0] = TSTRING(tablePtr);
	prmv[1] = TSTRING("`");
	prmv[2] = TSTRING("``");
	*table = FTextFnc_substitute(gCP,gTP,3,prmv);

    prmv[0] = TSTRING("CREATE TABLE `");
    prmv[1] = *table;
    prmv[2] = TSTRING("` (");

    *query = FUtil2_Append(gCP,gTP,3,prmv);

    fieldListObj = recordObj->itsFieldList.u.Structure;
    if (fieldListObj == NIL)
        {
        *ret = TERROR("!sql.create: record field list is null!");
        goto Last;
        }

    bindPtr = (LpBIND)*(fieldListObj->itsDictionaryArray);
    numFields = fieldListObj->itsMaxItemIndex;
    for (fieldIdx = 0; fieldIdx < numFields; fieldIdx++)
        {
        /* bindPtr->Key   = Field Name */
        /* bindPtr->Value = Field Type */

        /* We won't allow Bricks with repeating fields of non-character types */
        if ((bindPtr->Value.DeclaredType != TYCHAR) && bindPtr->Value.Modifier > 1)
            {
            *ret = TERROR("!sql.create: brick with repeating non-character fields is not supported!");
            goto Last;
            }

        /* SQL Field Name */
        asObject(val) = bindPtr->Key;
        asTag(val) = asObject(val)->itsObjectType;

        *temp = TSTRING("`");
        *query = FUtil2_QuickAppend(gCP,gTP,query,temp);

        /* escape the "`" character */
        /* prmv[0] = text */
        /* prmv[1] = old */
        /* prmv[2] = new */
        prmv[0] = *val;
        prmv[1] = TSTRING("`");
        prmv[2] = TSTRING("``");

        *temp = FTextFnc_substitute(gCP,gTP,3,prmv);
        *query = FUtil2_QuickAppend(gCP,gTP,query,temp);

        *temp = TSTRING("` ");
        *query = FUtil2_QuickAppend(gCP,gTP,query,temp);

        /* SQL Field Type */
        switch(bindPtr->Value.DeclaredType)
            {
            case TYCHAR:
                *temp = TSTRING("VARCHAR(");
                *val = TINT(bindPtr->Value.Modifier);
                *temp = FUtil2_QuickAppend(gCP,gTP,temp,val);
                *val = TSTRING(")");
                *temp = FUtil2_QuickAppend(gCP,gTP,temp,val);
                break;

            case TYBOLE:
                *temp = TSTRING("BOOL");
                break;

            case TYDATE:
                *temp = TSTRING("DATETIME");
                break;

            case TYNUM:
            case TYCHARPOINTER:
            case TYFLOATPOINTER:
            case TYREALPOINTER:
            case TYJUMPPOINTER:
            case TYINTPOINTER:
            case TYSHORTPOINTER:
            case TYLONGPOINTER:
            case TYWORDPOINTER:
#ifdef _M64
                *temp = TSTRING("BIGINT");
#else
                *temp = TSTRING("INTEGER");
#endif
                break;

            case TYFLOAT:
                *temp = TSTRING("FLOAT");
                break;

            case TYMONEY:
                *temp = TSTRING("DECIMAL(65,30)");
                break;

            case TYREAL:
                *temp = TSTRING("DOUBLE");
                break;

            case TYSHORT:
                *temp = TSTRING("SMALLINT");
                break;

            case TYLONG:
                *temp = TSTRING("INTEGER");
                break;

            case TYOBJ:
            case TYTVAL:
                *temp = TSTRING("LONGBLOB");
                break;
            }


        *query = FUtil2_QuickAppend(gCP,gTP,query,temp);

        /* SQL Field Separator */

        if (fieldIdx < (numFields - 1))
            {
            *temp = TSTRING(", ");
            *query = FUtil2_QuickAppend(gCP,gTP,query,temp);
            }

        ++bindPtr;
        }

    if (ddlCmdPtr != NULL)
    	*temp = TSTRING(ddlCmdPtr);
    else
    	*temp = TSTRING(")");

    *query = FUtil2_QuickAppend(gCP,gTP,query,temp);

    if (query->Tag == TYTEXT)
        queryPtr = asText(query);
    else
    if (query->Tag == TYSTRING)
        queryPtr = CharArray(*query);
    else
        {
        *ret = TERROR("!sql.create: failed in building sql query string!");
        goto Last;
        }

    /* Execute our SQL CREATE TABLE statement */
    if (mysql_query(mysqlPtr, queryPtr) != 0)
        {
        errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
        *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.create: %s!", errorPtr);
        goto Last;
        }

    *ret = gCP->TObject_TRUE;
	}
else
	{
	*ret = gCP->TObject_FALSE;
	}

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlDate

This procedure converts a Date to an SQL compatible date format.

Result: 'YYYY-MM-DD hh:mm:ss'
Note: The quotes are included to simplify the building of the query.
#endif
TVAL FMySQL1_SqlDate(LpXCONTEXT gCP, LpTHREAD gTP, TVAL date)
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(val);

DeclareTVAL(hour);
DeclareTVAL(min);
DeclareTVAL(sec);
DeclareTVALArray(prmv,15);
EndFrame

/*  Check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

if (date.Tag != TYDATE)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

prmv[0] = TSTRING("'");

*val = FDateFnc_year(gCP,gTP,1,&date);
prmv[1] = *val;
prmv[2] = TSTRING("-");

*val = FDateFnc_month(gCP,gTP,1,&date);
prmv[3] = *val;
prmv[4] = TSTRING("-");

*val = FDateFnc_day(gCP,gTP,1,&date);
prmv[5] = *val;

*hour = FDateFnc_hour(gCP,gTP,1,&date);
*min = FDateFnc_minute(gCP,gTP,1,&date);
*sec = FDateFnc_second(gCP,gTP,1,&date);

if (asInt(hour) == 0 && asInt(min) == 0 && asInt(sec) == 0)
    {
    prmv[6] = TSTRING("'");
    *ret = FUtil2_Append(gCP, gTP, 7, prmv);
    }
else
    {
    prmv[6] = TSTRING(" ");
    prmv[7] = *hour;
    prmv[8] = TSTRING(":");
    prmv[9] = *min;
    prmv[10] = TSTRING(":");
    prmv[11] = *sec;
    prmv[12] = TSTRING("'");
    *ret = FUtil2_Append(gCP, gTP, 13, prmv);
    }

FrameExit(*ret);
}

#if 0
FMySQL1_SqlDisconnect

This procedure disconnects a given MySQL connection

Result: true
#endif
TVAL FMySQL1_SqlDisconnect(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
NUM        mysqlHandle = 0;
NUM        maxIndex = 0;
NUM        indexOf = 0;
LpNUM      numPtr = NULL;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(symbol);
DeclareTVAL(list);
DeclareTVAL(info);
EndFrame

/* Check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. and type of arguments */
if (argc != 2 || argv[1].Tag != TYNUM)
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto Last;
    }

/* get our MySQL handle */
mysqlHandle = asInt(&argv[1]);

/* check if the connection handle is valid */
*ret = FMySQL1_SqlTest(gCP, gTP, argc, argv);
ExitOnError(*ret);

if (asBool(ret) == FALSE)
    {
    *ret = TERROR("!sql.disconnect: MySQL handle is not valid!");
    goto Last;
    }

/* close the connection */
mysql_close((MYSQL*)mysqlHandle);

/* check against our list of connections */
if ((gCP->FMySQL1_IntVector != NULL) && (gCP->FMySQL1_InfoVector != NULL))
    {
    *list = TOBJ(gCP->FMySQL1_IntVector);
    *info = TOBJ(gCP->FMySQL1_InfoVector);

    /* maxIndex is the no. of items in the vector */
    maxIndex = TIntVector_GetMaxIndex(gCP, gTP, *list);
    if (maxIndex > 0)
        {
        numPtr = IntArray(*list);

        /* check against default embedded connection */
        if (numPtr[0] == mysqlHandle)
            {
            *symbol = TSYMBOL("_defaultSqlHandle");
            ExitOnError(*symbol);

            /* reset default embedded connection to zero(0) */
            numPtr[0] = 0;

            TVector_SetIV1(gCP, gTP, *info, TINT(0), gCP->TObject_VOID);
            ExitOnError(*ret);

            /* make sure the global context variable is updated too */
            *ret = TSymbol_SetGlobalValue(gCP, gTP, asSymbol(symbol), TINT(0));
            ExitOnError(*ret);

            *ret = gCP->TObject_TRUE;
            goto Last;
            }

        for (indexOf = 1; indexOf < maxIndex; indexOf++)
            {
            /* check if we have a match */
            if (mysqlHandle == numPtr[indexOf])
                {
                /* remove connection from list */
                *ret = TIntVector_Delete(gCP, gTP, *list, TINT(indexOf));
                ExitOnError(*ret);

                /* remove connection info */
                *ret = TVector_Delete(gCP, gTP, *info, TINT(indexOf));
                ExitOnError(*ret);

                *ret = gCP->TObject_TRUE;
                goto Last;
                }
            }
        }
    }

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlDisconnectAll

This procedure disconnects all active MySQL connections (including the default embedded connection)

Result: true
#endif
TVAL FMySQL1_SqlDisconnectAll(LpXCONTEXT gCP, LpTHREAD gTP)
{
NUM        maxIndex = 0;
NUM        indexOf = 0;
MYSQL*     mysqlPtr = NULL;
LpNUM      numPtr = NULL;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(symbol);
DeclareTVAL(info);
DeclareTVAL(list);
EndFrame

/* make sure it's not NULL */
if ((gCP->FMySQL1_IntVector != NULL) && (gCP->FMySQL1_InfoVector != NULL))
    {
    *list = TOBJ(gCP->FMySQL1_IntVector);
    *info = TOBJ(gCP->FMySQL1_InfoVector);

    /* maxIndex is the no. of items in the vector */
    maxIndex = TIntVector_GetMaxIndex(gCP, gTP, *list);
    if (maxIndex > 0)
        {
        numPtr = IntArray(TOBJ(gCP->FMySQL1_IntVector));

        for (indexOf = 0; indexOf < maxIndex; indexOf++)
            {
            mysqlPtr = (MYSQL*)numPtr[indexOf];
            if (mysqlPtr != NULL)
                {
                /* Close active connection */
                mysql_close(mysqlPtr);
                numPtr[indexOf] = 0;
                }
            }
        }

    /* set no. of items to 1 */
    *ret = TIntVector_SetMaxIndex(gCP, gTP, *list, 1);
    ExitOnError(*ret);

    *ret = TVector_SetMaxIndex(gCP, gTP, *info, 1);
    ExitOnError(*ret);
    }

/* make sure the global context variable is updated too */
*symbol = TSYMBOL("_defaultSqlHandle");
ExitOnError(*symbol);
*ret = TSymbol_SetGlobalValue(gCP, gTP, asSymbol(symbol), TINT(0));
ExitOnError(*ret);

/* return TRUE */
*ret = gCP->TObject_TRUE;

FrameExit(*ret);
}

#if 0
FMySQL1_SqlEscape
#endif
TVAL FMySQL1_SqlEscape(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
LpCHAR          strPtr = NULL;
LpCHAR          escPtr = NULL;
UNUM            strLen = 0;
UNUM            escLen = 0;
UNUM            indexOf = 0;

MYSQL*          mysqlPtr = NULL;

StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,5);
EndFrame

/* check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. and type of arguments */
if (argc != 3)
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto Last;
    }

/* get our MySQL handle */
if (argv[1].Tag == TYVOID)
    {
    /* (sql escape: #void ...) */
UseDefault:
    /* use the default embedded connection */
    prmv[0] = TSYMBOL("connect");
    *ret = FMySQL1_SqlConnect(gCP, gTP, 1, prmv);
    ExitOnError(*ret);

    mysqlPtr = (MYSQL*)asInt(ret);
    }
else
if (argv[1].Tag == TYNUM)
    {
    mysqlPtr = (MYSQL*)asInt(&argv[1]);
    if (mysqlPtr == NULL)
        {
        /* (sql escape: 0 ...) */
        goto UseDefault;
        }
    else
        {
        /* (sql escape: non-zero ...) */
        /* check if connection is valid */
        prmv[0] = TSYMBOL("test");
        prmv[1] = argv[1];
        *ret = FMySQL1_SqlTest(gCP, gTP, 2, prmv);
        ExitOnError(*ret);

        if (asBool(ret) == FALSE)
            {
            *ret = TERROR("!sql.escape: MySQL handle is not valid!");
            goto Last;
            }
        }
    }
else
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto Last;
    }

/* Get the string to escape */
if (!(((argv[2].Tag == TYSTRING) && ((strPtr = CharArray(argv[2])) != 0)) ||
      ((argv[2].Tag == TYSYMBOL) && ((strPtr = SymbolArray(argv[2])) != 0)) ||
      ((argv[2].Tag == TYTEXT) && ((strPtr = asText(&argv[2])) != 0)) ||
      ((argv[2].Tag == TYBYTEVECTOR) && ((strPtr = ByteArray(argv[2])) != 0))
     ))
    {
    *ret = TERROR("!sql.escape: invalid string argument!");
    goto Last;
    }

/* Get the length of the source string */
if (argv[2].Tag == TYBYTEVECTOR)
    strLen = argv[2].u.ByteVector->itsMaxItemIndex;
else
    strLen = strlen(strPtr);

if (strLen > 0)
{
    /* Allocate (length * 2) + 1 */
    /* In the worst case, each character may need to be encoded as using two bytes, and you need room for the terminating null byte. */
    escPtr = (LpCHAR)malloc(strLen * 2 + 1);

    /* The length of the escape string does not include the null terminator */
    escLen = mysql_real_escape_string(mysqlPtr, escPtr, strPtr, strLen);

    /* For ByteVector, we should return another ByteVector */
    if (argv[2].Tag == TYBYTEVECTOR)
    {
        prmv[0] = TINT(escLen);
        *ret = TByteVector_MakeNew(gCP, gTP, 1, prmv);
        if (asTag(ret) == TYERROR)
        {
            free(escPtr);
            goto Last;
        }

        strPtr = ByteArray(*ret);

        /* Copy the resulting escaped string */
        for (indexOf = 0; indexOf < escLen; indexOf++)
            strPtr[indexOf] = escPtr[indexOf];
    }
    else
    {
        /* For Strings */
        *ret = TSTRING(escPtr);
    }

    free(escPtr);
}
else
{
    /* Length is Zero, just return the original string */
    *ret = argv[2];
}

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlGetBrickSchema
#endif
TVAL FMySQL1_SqlGetBrickSchema(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
LpCHAR        tablePtr = NULL;

StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);
EndFrame

/* check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. and type of arguments */
if (argc < 3)
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto Last;
    }

/* get the table */
if (!(((argv[2].Tag == TYSTRING) && ((tablePtr = CharArray(argv[2])) != 0)) ||
      ((argv[2].Tag == TYSYMBOL) && ((tablePtr = SymbolArray(argv[2])) != 0)) ||
      ((argv[2].Tag == TYTEXT) && ((tablePtr = asText(&argv[2])) != 0))))
    {
    *ret = TERROR("!sql.getbrickschema: invalid table argument!");
    goto Last;
    }

/* check if table name is empty */
if (strlen(tablePtr) == 0)
    {
    *ret = TERROR("!sql.getbrickschema: table name is blank!");
    goto Last;
    }

/* argv[0] - getbrickschema */
/* argv[1] - sqlHandle */
/* argv[2] - table */

prmv[0] = TSTRING("SELECT * FROM ");
prmv[1] = argv[2];
prmv[2] = TSTRING(" LIMIT 0");

prmv[1] = FUtil2_Append(gCP,gTP,3,&prmv[0]);
prmv[0] = argv[1];

/* Execute query: "SELECT * FROM table LIMIT 0" */
/* (sql sqlHandle query) */
*ret = FMySQL1_SqlQuery(gCP,gTP,2,&prmv[0]);

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlInfo

This procedure display information on a given MySQL connection

Result: Connection information string
#endif
TVAL FMySQL1_SqlInfo(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
NUM mysqlHandle = 0;
NUM maxIndex = 0;
NUM indexOf = 0;
NUM* numPtr = NULL;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(list);
DeclareTVAL(info);
EndFrame

/* Check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. and type of arguments */
if (argc != 2 || argv[1].Tag != TYNUM)
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto Last;
    }

/* get our MySQL handle */
mysqlHandle = asInt(&argv[1]);

/* check if the connection handle is valid */
*ret = FMySQL1_SqlTest(gCP, gTP, argc, argv);
ExitOnError(*ret);

if (asBool(ret) == FALSE)
    {
    *ret = gCP->TObject_VOID;
    goto Last;
    }

if ((gCP->FMySQL1_IntVector != NULL) && (gCP->FMySQL1_InfoVector != NULL))
    {
    *list = TOBJ(gCP->FMySQL1_IntVector);
    *info = TOBJ(gCP->FMySQL1_InfoVector);

    /* maxIndex is the no. of items in the vector */
    maxIndex = TIntVector_GetMaxIndex(gCP, gTP, *list);
    if (maxIndex > 0)
        {
        numPtr = IntArray(*list);

        for (indexOf = 0; indexOf < maxIndex; indexOf++)
            {
            /* check if we have a match */
            if (mysqlHandle == numPtr[indexOf])
                {
                /* get connection info */
                *ret = TVector_GetIV1(gCP, gTP, *info, TINT(indexOf));
                ExitOnError(*ret);

                goto Last;
                }
            }
        }
    }

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlInsert

This is the procedure which serves as an entry point for the "(sql insert: ...)"
command defined in the Lambda SQL Ref. Guide. It will insert an AIS Brick into
a MySQL table.

Command format:

  (sql insert: sqlHandle table brick binarySW)
- Inserts the contents of a brick object to a SQL table
- Parameters:
    * sqlHandle            - handle returned by sql connect:
    * table                - name of table
    * brick                - collection of rows that will be added to sql table
    * binarySW             - true if we use binary encoding for objects else
                             we use ascii encoding for objects in Brick fields

  (sql insert: sqlHandle table vectorOfBrickRefs binarySW)
- Inserts the contents of a vector of brick objects to a SQL table
- Parameters
    * sqlHandle            - handle returned by sql connect:
    * table                - name of the table
    * vectorOfBrickRefs    - collection of rows (stored as a vector of brick objects) to be added
    * binarySW             - true if we use binary encoding for objects else
                             we use asci encoding for objects in Brick fields

If the table does not exists, the function returns false.

#endif

TVAL FMySQL1_SqlInsert(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
LpCHAR          errorPtr = NULL;
LpCHAR          queryPtr = NULL;
LpCHAR          valuePtr = NULL;
LpCHAR          tablePtr = NULL;
LpCHAR          fieldPtr = NULL;
LpCHAR          rowPtr = NULL;
LpBIND          bindPtr = NULL;
BOLE            binaryMode = FALSE;
BOLE            tblExists = FALSE;

MYSQL*          mysqlPtr = NULL;
MYSQL_RES*      mysqlResult = NULL;
MYSQL_STMT*     mysqlStmt = NULL;
MYSQL_BIND*     mysqlBind = NULL;

UNUM            fieldIdx = 0;
UNUM            numFields = 0;
NUM             rowIdx = 0;
NUM             numRows = 0;
NUM             vectorRowIdx = 0;
NUM             vectorNumRows = 0;

TBrick*         brickObj = NULL;
TObjVector*     objVectorObj = NULL;
TVector*        vectorObj = NULL;

StartFrame
DeclareTVAL(query);
DeclareTVAL(insert);
DeclareTVAL(table);
DeclareTVAL(field);
DeclareTVAL(ret);
DeclareTVAL(val);
DeclareTVAL(temp2);
DeclareOBJ(TStructure,fieldListObj);
DeclareTVALArray(prmv,20);
EndFrame

/* check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. and type of arguments */
if (argc < 4)
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto Last;
    }

/* get our MySQL handle */
if (argv[1].Tag == TYVOID)
    {
    /* (sql insert: #void ...) */
UseDefault:
    /* use the default embedded connection */
    prmv[0] = TSYMBOL("connect");
    *ret = FMySQL1_SqlConnect(gCP, gTP, 1, prmv);
    ExitOnError(*ret);

    mysqlPtr = (MYSQL*)asInt(ret);
    }
else
if (argv[1].Tag == TYNUM)
    {
    mysqlPtr = (MYSQL*)asInt(&argv[1]);
    if (mysqlPtr == NULL)
        {
        /* (sql insert: 0 ...) */
        goto UseDefault;
        }
    else
        {
        /* (sql insert: non-zero ...) */
        /* check if connection is valid */
        prmv[0] = TSYMBOL("test");
        prmv[1] = argv[1];
        *ret = FMySQL1_SqlTest(gCP, gTP, 2, prmv);
        ExitOnError(*ret);

        if (asBool(ret) == FALSE)
            {
            *ret = TERROR("!sql.insert: MySQL handle is not valid!");
            goto Last;
            }
        }
    }
else
    {
    *ret = TERROR("!sql.insert: MySQL handle is not valid!");
    goto Last;
    }

/* get the table */
if (!(((argv[2].Tag == TYSTRING) && ((tablePtr = CharArray(argv[2])) != 0)) ||
      ((argv[2].Tag == TYSYMBOL) && ((tablePtr = SymbolArray(argv[2])) != 0)) ||
      ((argv[2].Tag == TYTEXT) && ((tablePtr = asText(&argv[2])) != 0))))
    {
    *ret = TERROR("!sql.insert: invalid table argument!");
    goto Last;
    }

/* check if table name is empty */
if (strlen(tablePtr) == 0)
    {
    *ret = TERROR("!sql.insert: table name is blank!");
    goto Last;
    }

/* get the Brick, Vector or ObjVector object */
if (!(((argv[3].Tag == TYBRICK) && ((brickObj = argv[3].u.Brick) != 0)) ||
      ((argv[3].Tag == TYOBJVECTOR) && ((objVectorObj = argv[3].u.ObjVector) != 0)) ||
	  ((argv[3].Tag == TYVECTOR) && ((vectorObj = argv[3].u.Vector) != 0))))
	{
	*ret = TERROR("!sql.insert: invalid argument, Brick, Vector, or ObjVector expected!");
	goto Last;
	}

/* get the use binary flag */
if (argc >= 5)
    {
    if (argv[4].Tag != TYBOLE)
        {
        *ret = TERROR("!sql.insert: boolean value expected!");
        goto Last;
        }
    binaryMode = asBool(&argv[4]);
    }

/* escape the "'" character in the table name, since we're using it in the query */
prmv[0] = TSTRING(tablePtr);
prmv[1] = TSTRING("'");
prmv[2] = TSTRING("''");
*table = FTextFnc_substitute(gCP,gTP,3,prmv);

/********************************************************/
/* Build SQL statement for checking if the table exists */
/********************************************************/

/* This way, we don't have to go through the trouble of building the long SQL statement */

prmv[0] = TSTRING("SHOW TABLES LIKE '");
prmv[1] = *table;
prmv[2] = TSTRING("'");
*query = FUtil2_Append(gCP,gTP,3,prmv);

if (query->Tag == TYTEXT)
    queryPtr = asText(query);
else
if (query->Tag == TYSTRING)
    queryPtr = CharArray(*query);
else
    {
    *ret = TERROR("!sql.insert: failed in building sql query string!");
    goto Last;
    }

/* Execute our SQL SHOW TABLES statement */
if (mysql_query(mysqlPtr, queryPtr) != 0)
    {
    errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.insert: %s!", errorPtr);
    goto Last;
    }

mysqlResult = mysql_store_result(mysqlPtr);

/* Check if the result set is not empty */
if (mysql_num_rows(mysqlResult) > 0)
    tblExists = TRUE;

mysql_free_result(mysqlResult);
mysqlResult = NULL;

if (!tblExists)
    {
    *ret = TERROR("!sql.insert: specified table does not exists!");
    goto Last;
    }

/****************************************************/
/* Build SQL statement for inserting into the table */
/****************************************************/

/* SQL SYNTAX: INSERT INTO table_name(field1, field2, ...) VALUES(value1, value2, ...) */

/* escape the "`" character in the table name, since we're using it in the query */
prmv[0] = TSTRING(tablePtr);
prmv[1] = TSTRING("`");
prmv[2] = TSTRING("``");
*table = FTextFnc_substitute(gCP,gTP,3,prmv);

prmv[0] = TSTRING("INSERT INTO `");
prmv[1] = *table;
prmv[2] = TSTRING("` (");

*insert = FUtil2_Append(gCP,gTP,3,prmv);
*temp2 = TSTRING("");

if (brickObj == NULL)
	{
	/* get the first Brick object in the Vector */
	if (vectorObj != NULL)
		brickObj = atHMTval(vectorObj->itsTvalArray,0).u.Brick;
	else
		brickObj = (TBrick*)atHMObject(objVectorObj->itsObjectArray,0);
	}

fieldListObj = brickObj->itsFieldList.u.Structure;
if (fieldListObj == NIL)
    {
    *ret = TERROR("!sql.insert: record field list is null!");
    goto Last;
    }

bindPtr = (LpBIND)*(fieldListObj->itsDictionaryArray);
numFields = fieldListObj->itsMaxItemIndex;

prmv[3] = TSTRING("`");
prmv[4] = TSTRING(", ");
prmv[5] = TSTRING("?");

for (fieldIdx = 0; fieldIdx < numFields; fieldIdx++)
    {
    /* SQL Field Name */
    asObject(field) = bindPtr->Key;
    asTag(field) = asObject(field)->itsObjectType;

    /* escape the "`" character */
    prmv[0] = *field;
    prmv[1] = TSTRING("`");
    prmv[2] = TSTRING("``");
    *field = FTextFnc_substitute(gCP,gTP,3,prmv);

    prmv[0] = *insert;
    prmv[1] = TSTRING("`");
    prmv[2] = *field;

    /* Add a "?" for each field */
    *temp2 = FUtil2_QuickAppend(gCP,gTP,temp2,&prmv[5]);

    /* SQL Field Separator */
    if (fieldIdx < (numFields - 1))
        {
        /* Add the "," separator */
        *insert = FUtil2_Append(gCP,gTP,5,prmv);
        *temp2 = FUtil2_QuickAppend(gCP,gTP,temp2,&prmv[4]);
        }
    else
        {
        *insert = FUtil2_Append(gCP,gTP,4,prmv);
        }

    ++bindPtr;
    }

/* INSERT INTO tbl VALUES(?,?,?,...,?) */
prmv[0] = *insert;                  /* INSERT INTO tbl(field1,field2,field3,...,field# */
prmv[1] = TSTRING(") VALUES(");     /* ) VALUES( */
prmv[2] = *temp2;                   /* ?,?,?,...,? */
prmv[3] = TSTRING(")");             /* ) */

*insert = FUtil2_Append(gCP,gTP,4,prmv);

if (insert->Tag == TYTEXT)
    queryPtr = asText(insert);
else
if (insert->Tag == TYSTRING)
    queryPtr = CharArray(*insert);
else
    {
    *ret = TERROR("!sql.insert: failed in building sql query string!");
    goto Last;
    }

mysqlStmt = mysql_stmt_init(mysqlPtr);

if (!mysqlStmt)
    {
    errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.insert: %s!", errorPtr);
    goto Last;
    }

if (mysql_stmt_prepare(mysqlStmt, queryPtr, strlen(queryPtr)))
    {
    errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.insert: %s!", errorPtr);
    goto Last;
    }

mysqlBind = (MYSQL_BIND*) malloc(sizeof(MYSQL_BIND) * numFields);
memset(mysqlBind, 0, sizeof(MYSQL_BIND) * numFields);

if (vectorObj != NULL)
	vectorNumRows = vectorObj->itsMaxItemIndex;
else
if (objVectorObj != NULL)
	vectorNumRows = objVectorObj->itsMaxItemIndex;
else
	goto ProcessRows;

for (vectorRowIdx = 0; vectorRowIdx < vectorNumRows; vectorRowIdx++)
	{
	/* Get the next Brick object in the Vector or ObjVector */
	if (vectorObj != NULL)
		brickObj = atHMTval(vectorObj->itsTvalArray,vectorRowIdx).u.Brick;
	else
		brickObj = (TBrick*)atHMObject(objVectorObj->itsObjectArray,vectorRowIdx);

	if (brickObj->itsObjectType != TYBRICK)
		{
	    *ret = TERROR("!sql.insert: Vector contains a non-Brick object!");
	    goto Last;
		}

ProcessRows:

	/* Get the no. of rows in the Brick */
	numRows = brickObj->itsRowCount;
	for (rowIdx = 0; rowIdx < numRows; rowIdx++)
		{
		rowPtr = asFieldArray(brickObj) + (rowIdx * brickObj->itsRowByteCount);

		bindPtr = (LpBIND)*fieldListObj->itsDictionaryArray;
		for (fieldIdx = 0; fieldIdx < numFields; fieldIdx++)
			{
			fieldPtr = rowPtr + bindPtr->Value.Offset;

			switch(bindPtr->Value.DeclaredType)
				{
				case TYBOLE:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_TINY;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(CHAR));

					((LpCHAR)mysqlBind[fieldIdx].buffer)[0] = ((LpCHAR)fieldPtr)[0];
					break;

				case TYCHAR:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_VARCHAR;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(bindPtr->Value.Modifier + 1);

					strncpy((char*)mysqlBind[fieldIdx].buffer, (char*)fieldPtr, bindPtr->Value.Modifier);
					((char*)mysqlBind[fieldIdx].buffer)[bindPtr->Value.Modifier] = 0;

					/* allocate length variable if not present */
					if (mysqlBind[fieldIdx].length == NULL)
						mysqlBind[fieldIdx].length = (unsigned long*)malloc(sizeof(unsigned long));

					*mysqlBind[fieldIdx].length = strlen(((char*)mysqlBind[fieldIdx].buffer));
					break;

				case TYDATE:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_DATETIME;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(MYSQL_TIME));

					asTag(val) = TYDATE;
					asReal(val) = ((LpREAL)fieldPtr)[0];

					*ret = FDateFnc_year(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->year = asInt(ret);

					*ret = FDateFnc_month(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->month = asInt(ret);

					*ret = FDateFnc_day(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->day = asInt(ret);

					*ret = FDateFnc_hour(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->hour = asInt(ret);

					*ret = FDateFnc_minute(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->minute = asInt(ret);

					*ret = FDateFnc_second(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->second = asInt(ret);

					break;

				case TYNUM:
				case TYCHARPOINTER:
				case TYFLOATPOINTER:
				case TYREALPOINTER:
				case TYJUMPPOINTER:
				case TYINTPOINTER:
				case TYSHORTPOINTER:
				case TYLONGPOINTER:
				case TYWORDPOINTER:
	#ifdef _M64
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_LONGLONG;
	#else
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_LONG;
	#endif
					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(NUM));

					((LpNUM)mysqlBind[fieldIdx].buffer)[0] = ((LpNUM)fieldPtr)[0];
					break;

				case TYFLOAT:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_FLOAT;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(FLOAT));

					((LpFLOAT)mysqlBind[fieldIdx].buffer)[0] = ((LpFLOAT)fieldPtr)[0];
					break;

				case TYMONEY:
				case TYREAL:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_DOUBLE;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(REAL));

					((LpREAL)mysqlBind[fieldIdx].buffer)[0] = ((LpREAL)fieldPtr)[0];
					break;

				case TYSHORT:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_SHORT;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(SHORT));

					((LpSHORT)mysqlBind[fieldIdx].buffer)[0] = ((LpSHORT)fieldPtr)[0];
					break;

				case TYLONG:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_LONG;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(NUM32));

					((LpNUM32)mysqlBind[fieldIdx].buffer)[0] = ((LpNUM32)fieldPtr)[0];
					break;

				case TYOBJ:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_BLOB;

					if (mysqlBind[fieldIdx].length == NULL)
						mysqlBind[fieldIdx].length = (unsigned long*)malloc(sizeof(unsigned long));

					if (binaryMode)
						{
						/* AIS_BIN: */
						asObject(val) = ((TObject**)fieldPtr)[0];
						asTag(val) = (asObject(val) == NULL) ? TYVOID : asObject(val)->itsObjectType;
						prmv[0] = *val;

						goto InsertBinary;
						}
					else
						{
						/* AIS_ASC: */
						asObject(val) = ((TObject**)fieldPtr)[0];
						asTag(val) = (asObject(val) == NULL) ? TYVOID : asObject(val)->itsObjectType;
						prmv[0] = *val;
						prmv[1] = gCP->TObject_TRUE;

						goto InsertAscii;
						}
					break;

				case TYTVAL:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_BLOB;

					if (mysqlBind[fieldIdx].length == NULL)
						mysqlBind[fieldIdx].length = (unsigned long*)malloc(sizeof(unsigned long));

					if (binaryMode)
						{
						/* Binary Representation */
						prmv[0] = ((LpTVAL)fieldPtr)[0];

						InsertBinary:

						/* convert to binary representation */
						*ret = FConio_saveObject(gCP,gTP,1,prmv);

						if (asTag(ret) == TYBYTEVECTOR)
							valuePtr = ByteArray(*ret);
						else
							{
							goto ErrorCleanup;
							}

						/* add length of header to the buffer length */
						*mysqlBind[fieldIdx].length = asByteVector(ret)->itsMaxItemIndex + MYSQL_BLOB_HDR_LENGTH;

						/* if the current buffer is enough, don't allocate */
						if (mysqlBind[fieldIdx].buffer_length < *mysqlBind[fieldIdx].length)
							{
							/* if there's an existing buffer, deallocate it first */
							if (mysqlBind[fieldIdx].buffer != NULL)
								free(mysqlBind[fieldIdx].buffer);

							/* allocate the storage for blob data */
							mysqlBind[fieldIdx].buffer = malloc(*mysqlBind[fieldIdx].length);
							mysqlBind[fieldIdx].buffer_length = *mysqlBind[fieldIdx].length;
							}

						/* prepend the binary header and the length field */
						sprintf((char*)mysqlBind[fieldIdx].buffer,"%s",MYSQL_BLOB_AIS_BINARY);

						/* append the binary data */
						memcpy((char*)mysqlBind[fieldIdx].buffer + MYSQL_BLOB_HDR_LENGTH,valuePtr,asByteVector(ret)->itsMaxItemIndex);
						}
					else
						{
						/* ASCII representation */
						prmv[0] = ((LpTVAL)fieldPtr)[0];
						prmv[1] = gCP->TObject_TRUE;

						InsertAscii:

						/* convert to ascii representation */
						*ret = FConvert_ToString(gCP,gTP,2,prmv);

						if (asTag(ret) == TYTEXT)
							valuePtr = asText(ret);
						else
						if (asTag(ret) == TYSTRING)
							valuePtr = CharArray(*ret);
						else
							{
							goto ErrorCleanup;
							}

						/* add length of header to the buffer length, and null terminator */
						*mysqlBind[fieldIdx].length = strlen(valuePtr) + MYSQL_BLOB_HDR_LENGTH + 1;

						/* if the current buffer is enough, don't allocate */
						if (mysqlBind[fieldIdx].buffer_length < *mysqlBind[fieldIdx].length)
							{
							/* if there's an existing buffer, deallocate it first */
							if (mysqlBind[fieldIdx].buffer != NULL)
								free(mysqlBind[fieldIdx].buffer);

							/* allocate the storage for blob data */
							mysqlBind[fieldIdx].buffer = malloc(*mysqlBind[fieldIdx].length);
							mysqlBind[fieldIdx].buffer_length = *mysqlBind[fieldIdx].length;
							}

						/* prepend the ascii header */
						sprintf((char*)mysqlBind[fieldIdx].buffer,"%s%s",MYSQL_BLOB_AIS_ASCII,valuePtr);
						}
					break;

				default:

					break;
				}
			++bindPtr;
			}

		/* bind parameters */
		if (mysql_stmt_bind_param(mysqlStmt,mysqlBind))
			{
			errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
			*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.insert: %s!", errorPtr);
			goto ErrorCleanup;
			}

		/* execute the prepared statement */
		if (mysql_stmt_execute(mysqlStmt))
			{
			errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
			*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.insert: %s!", errorPtr);
			goto ErrorCleanup;
			}

		} /* end of for rowIdx */

	} /* end of for vectorRowIdx */

*ret = gCP->TObject_OK;

ErrorCleanup:
/* free dynamically allocated resources */
for (fieldIdx = 0; fieldIdx < numFields; fieldIdx++)
    {
    free(mysqlBind[fieldIdx].buffer);
    mysqlBind[fieldIdx].buffer = NULL;

    free(mysqlBind[fieldIdx].length);
    mysqlBind[fieldIdx].length = NULL;
    }

free(mysqlBind);
mysql_stmt_close(mysqlStmt);

Last:
FrameExit(*ret);
}

TVAL FMySQL1_SqlQuery(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM             indexOf = 0;
NUM             lengthOf = 0;
LpCHAR          errorPtr = NULL;
LpCHAR          queryPtr = NULL;
LpCHAR          valuePtr = NULL;

LpCHAR          fromPtr = NULL;
LpCHAR          destPtr = NULL;

MYSQL*          mysqlPtr = NULL;
MYSQL_RES*      mysqlResult = NULL;
MYSQL_ROW       mysqlRow = NULL;
MYSQL_FIELD*    mysqlField = NULL;
MYSQL_TIME      mysqlTime;

UNUM            fieldIdx = 0;
UNUM            numFields = 0;
unsigned long*  mysqlLength = NULL;
NUM             rowIdx = 0;
NUM             numRows = 0;

StartFrame
DeclareTVAL(ec);
DeclareTVAL(query);
DeclareTVAL(ret);
DeclareTVAL(type);
DeclareTVAL(repeats);
DeclareTVAL(val);
DeclareTVAL(fieldVector);
DeclareTVALArray(prmv,20);
DeclareTVALArray(recprm,2);
EndFrame

/*  Check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/*  If no user escape was requested, then continue. */

/* Append the Query arguments into a String */
if (argc >= 20)
    {
    *ret = TERROR("!sql.query: too many arguments!");
    goto Last;
    }
else
if (argc == 2)
    {
    *query = argv[1];
    goto GetQueryPointer;
    }
else
if (argc == 1)
    {
    *query = argv[0];
    goto GetQueryPointer;
    }

/* Append the Query arguments into a String */

/* If the 1st argument is a String or Text */
if ((argv[0].Tag == TYSTRING) || (argv[0].Tag == TYTEXT))
    {
    indexOf = 0;
    }
else
    {
    prmv[0].u.Text[0] = 0;
    prmv[0].Tag = TYTEXT;
    indexOf = 1;
    }

while (indexOf < argc)
    {
    if (argv[indexOf].Tag == TYDATE)
        {
        prmv[indexOf] = FMySQL1_SqlDate(gCP, gTP, argv[indexOf]);
        }
    else
    if (argv[indexOf].Tag == TYBITVECTOR)
        {
        prmv[indexOf] = FMySQL1_SqlBit(gCP, gTP, argv[indexOf]);
        }
    else
    if (argv[indexOf].Tag == TYMONEY)
        {
        prmv[indexOf] = argv[indexOf];
        prmv[indexOf].Tag = TYREAL;
        }
    else
        {
        prmv[indexOf] = argv[indexOf];
        }
    indexOf++;
    }
*query = FUtil2_Append(gCP,gTP,argc,&prmv[0]);
ExitOnError(*query);

GetQueryPointer:
if (query->Tag == TYTEXT)
    {
    queryPtr = asText(query);
    }
else
if (query->Tag == TYSTRING)
    {
    queryPtr = CharArray(*query);
    }
else
if (query->Tag == TYSYMBOL)
    {
    queryPtr = SymbolArray(*query);
    }
else
    {
    *ret = TERROR("!sql.query: failed in building sql query string!");
    goto Last;
    }

/* Manage the SQL Query once the arguments have been appended into a String */

/* get our MySQL handle */
if ((argv[0].Tag == TYVOID) || (argv[0].Tag == TYSTRING) || (argv[0].Tag == TYTEXT))
    {
    /* (sql #void ...) */
    /* (sql 0 ...) */
    /* (sql ...) */
UseDefault:
    /* use the default embedded connection */
    prmv[0] = TSYMBOL("connect");
    *ret = FMySQL1_SqlConnect(gCP, gTP, 1, prmv);
    ExitOnError(*ret);

    mysqlPtr = (MYSQL*)asInt(ret);
    }
else
if (argv[0].Tag == TYNUM)
    {
    mysqlPtr = (MYSQL*)asInt(&argv[0]);

    if (mysqlPtr == NULL)
        {
        /* (sql 0 ...) */
        goto UseDefault;
        }
    else
        {
        /* (sql non-zero ...) */
        /* check if connection is valid */
        prmv[0] = TSYMBOL("test");
        prmv[1] = argv[0];
        *ret = FMySQL1_SqlTest(gCP, gTP, 2, prmv);
        ExitOnError(*ret);

        if (asBool(ret) == FALSE)
            {
            *ret = TERROR("!sql.query: MySQL handle is not valid!");
            goto Last;
            }
        }
    }
else
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto Last;
    }

    if (mysql_query(mysqlPtr, queryPtr) != 0)
        {
        errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
        *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.query: %s!", errorPtr);
        }
    else
        {
        /* execution of query was successful, construct our Brick structure */
        mysqlResult = mysql_store_result(mysqlPtr);
        if (mysqlResult != NULL)
            {
            /* there are rows */
            /* do something with the rows, create a Brick object and populate it */

            /* TVAL TBrick_MakeNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) */
            /* TVAL FMake_Vector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) */
            /* TVAL TVector_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue) */

            /* MYSQL_FIELD *mysql_fetch_fields(MYSQL_RES *result) */
            /* Use this function to determine the data type of each field */

            /* MYSQL_ROW mysql_fetch_row(MYSQL_RES *result) */
            /* Use this function to retrieve the row data from the result set */

             /* Get the number of rows */
            numRows = mysql_num_rows(mysqlResult);
            //if (numRows <= 0)
            //    {
            //    *ret = gCP->TObject_VOID;
            //    goto Last;
            //    }

           /* Get the number of fields */
            numFields = mysql_num_fields(mysqlResult);
            if (numFields <= 0)
                {
                *ret = gCP->TObject_VOID;
                goto Last;
                }

            /* Create our field vector with three entries for each MySQL field (Name, Type, Repeats) */
            /* Note: Repeats is always 1 for MySQL databases. */
            *fieldVector = FMake_Vector(gCP, gTP, 0, NULL);
            MySQLExitOnError(*fieldVector)

            /* Get the fields */
            mysqlField = mysql_fetch_fields(mysqlResult);

            /* Process each field information */
            for(fieldIdx = 0; fieldIdx < numFields; fieldIdx++)
                {
                /* Default repeats to 1 */
                repeats->Tag = TYNUM;repeats->u.Int = 1;
                /* Add the field name first */
                *ec = TVector_AddNewValue(gCP, gTP, *fieldVector, TSYMBOL(mysqlField[fieldIdx].name));
                MySQLExitOnError(*ec)

                /* Add the field type next */
                /* Choose and set the appropriate record data type */
                switch(mysqlField[fieldIdx].type)
                    {
                    /****************/
                    /* BINARY TYPES */
                    /****************/
                    case MYSQL_TYPE_BIT:
                        /* Type: Object: BitVector */
                        *type = TSYMBOL("Object");
                        break;

                    case MYSQL_TYPE_BLOB:
                    case MYSQL_TYPE_MEDIUM_BLOB:
                    case MYSQL_TYPE_LONG_BLOB:
                        /* Type: Object: ByteVector or Specific Object Type */
                        *type = TSYMBOL("Object");
                        break;


                    /**************/
                    /* REAL TYPES */
                    /**************/
                    case MYSQL_TYPE_DECIMAL:
                    case MYSQL_TYPE_NEWDECIMAL:
                        /* Type: Money */
                        /* Notes: Our engine doesn't support very large numbers yet so Money will be used for the mean time */
                    	if (IS_NOT_NULL(mysqlField[fieldIdx].flags) || !gCP->FMySQL1_UseWordFlt)
                    		*type = TSYMBOL("Money");
                    	else
                    		*type = TSYMBOL("Word");
                        break;

                    case MYSQL_TYPE_DOUBLE:
                        /* Type: Number */
                    	if (IS_NOT_NULL(mysqlField[fieldIdx].flags) || !gCP->FMySQL1_UseWordFlt)
                    		*type = TSYMBOL("Number");
                    	else
                    		*type = TSYMBOL("Word");
                        break;

                    case MYSQL_TYPE_FLOAT:
                        /* Type: Float */
                    	if (IS_NOT_NULL(mysqlField[fieldIdx].flags) || !gCP->FMySQL1_UseWordFlt)
                    		*type = TSYMBOL("Float");
                    	else
                    		*type = TSYMBOL("Word");
                        break;


                    /*****************/
                    /* INTEGER TYPES */
                    /*****************/
                    case MYSQL_TYPE_INT24:
                    case MYSQL_TYPE_LONG:
                        /* Type: Long */
                    	if (IS_NOT_NULL(mysqlField[fieldIdx].flags) || !gCP->FMySQL1_UseWordInt)
                    		*type = TSYMBOL("Long");
                    	else
                    		*type = TSYMBOL("Word");
                        break;

                    case MYSQL_TYPE_LONGLONG:
                        /* Type: Integer */
                    	if (IS_NOT_NULL(mysqlField[fieldIdx].flags) || !gCP->FMySQL1_UseWordInt)
                    		*type = TSYMBOL("Integer");
                    	else
                    		*type = TSYMBOL("Word");
                        break;
                        break;

                    case MYSQL_TYPE_TINY:
                    case MYSQL_TYPE_YEAR:
                    case MYSQL_TYPE_SHORT:
                        /* Type: Object: Short */
                    	if (IS_NOT_NULL(mysqlField[fieldIdx].flags) || !gCP->FMySQL1_UseWordInt)
                    		*type = TSYMBOL("Short");
                    	else
                    		*type = TSYMBOL("Word");
                        break;

                    /*******************/
                    /* DATE/TIME TYPES */
                    /*******************/
                    case MYSQL_TYPE_DATE:
                    case MYSQL_TYPE_DATETIME:
                    case MYSQL_TYPE_TIME:
                    case MYSQL_TYPE_TIMESTAMP:
                        /* Type: Object: Date */
                    	if (IS_NOT_NULL(mysqlField[fieldIdx].flags) || !gCP->FMySQL1_UseWordDat)
                    		*type = TSYMBOL("Date");
                    	else
                    		*type = TSYMBOL("Word");
                        break;


                    /**************************/
                    /* STRING OR BINARY TYPES */
                    /**************************/
                    case MYSQL_TYPE_STRING:
                    case MYSQL_TYPE_VAR_STRING:
                        /* CHAR - Type: Character or String Object */
                        /* BINARY - Type: ByteVector */

                        if (mysqlField[fieldIdx].flags & BINARY_FLAG)
                            {
                            *type = TSYMBOL("Object");
                            }
                        else
                            {
                            /* Store as a fixed length Character field if the maximum length for this variable length field is less than one object header. */
                            *type = TSYMBOL("Character");
                            repeats->u.Int = (mysqlField[fieldIdx].length);
                            /* We will not allow zero-length character fields */
                            if (repeats->u.Int == 0)
                            	repeats->u.Int = 1;
                            }
                        break;


                    /***********************/
                    /* ENUM, and SET TYPES */
                    /***********************/
                    case MYSQL_TYPE_ENUM:
                    case MYSQL_TYPE_SET:
                    	/* Type: Object: String */
                    	*type = TSYMBOL("Object");
                    	break;


                    /*************/
                    /* NULL TYPE */
                    /*************/
                    case MYSQL_TYPE_NULL:
                        /* Type: Word: #void */
                    	*type = TSYMBOL("Word");
                        break;


                    /*********************/
                    /* UNSUPPORTED TYPES */
                    /*********************/
                    default:
                        /* Issue an error if we do not support the MySQL field data type */
                        *type = TERROR("!sql: unsupported MySQL field data type encountered!");
                        break;
                    }
                MySQLExitOnError(*type)

                *ec = TVector_AddNewValue(gCP, gTP, *fieldVector, *type);
                MySQLExitOnError(*ec)

                /* Add the repeats */
                *ec = TVector_AddNewValue(gCP, gTP, *fieldVector, *repeats);
                MySQLExitOnError(*ec)
                } /* for(fieldIdx = 0; fieldIdx < numFields; fieldIdx++) */

            /* TBrick_MakeNew expects 2 arguments
             * [0] - Number of rows
             * [1] - Vector of field information
             */

            /* Construct the parameter for TBrick_MakeNew */
            if (numRows > 0)
				{
            	recprm[0] = TINT(numRows);
            	recprm[1] = *fieldVector;
                /* Create our Brick structure */
                *ret = TBrick_MakeNew(gCP, gTP, 2, &recprm[0]);
				}
            else
				{
            	recprm[0] = *fieldVector;
            	/* Create our Brick structure */
            	*ret = TBrick_MakeNew(gCP, gTP, 1, &recprm[0]);
				}

            MySQLExitOnError(*ret);

            /* Retrieve each row from our result set */
            for(rowIdx = 0; (rowIdx < numRows) && ((mysqlRow = mysql_fetch_row(mysqlResult)) != NULL); rowIdx++)
                {
                /* Get the lengths of the fields */
                /* This is used for binary data */
                mysqlLength = mysql_fetch_lengths(mysqlResult);

                /* Set each field value from our result row to the corresponding record entry */
                for(fieldIdx = 0; fieldIdx < numFields; fieldIdx++)
                    {
                    valuePtr = mysqlRow[fieldIdx];
                    if (valuePtr != NULL)
                        {
                        switch(mysqlField[fieldIdx].type)
                            {
                            /****************/
                            /* BINARY TYPES */
                            /****************/
                            case MYSQL_TYPE_BIT:
                                /* Type: Object: BitVector */
                                lengthOf = mysqlLength[fieldIdx];
                                prmv[0] = TINT(lengthOf*8);
                                *val = TBitVector_MakeNew(gCP, gTP, 1, &prmv[0]);
                                MySQLExitOnError(*val);
                                fromPtr = mysqlRow[fieldIdx];
                                destPtr = (LpCHAR)BitArray(*val);

                                for (indexOf = 0; indexOf < lengthOf; indexOf++)
                                    {
                                    destPtr[indexOf] = fromPtr[indexOf];
                                    }

                                /* This stores the BitVector Object in the field(Type=Object) of the Brick */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            case MYSQL_TYPE_BLOB:
                            case MYSQL_TYPE_MEDIUM_BLOB:
                            case MYSQL_TYPE_LONG_BLOB:
                                if (strncmp(valuePtr,MYSQL_BLOB_AIS_ASCII,MYSQL_BLOB_HDR_LENGTH) == 0)
                                    {
                                    /* ascii representation */
                                    valuePtr += MYSQL_BLOB_HDR_LENGTH;

                                    prmv[0] = TSTRING(valuePtr);

                                    /* parse string constant */
                                    *val = FConio_Parse(gCP,gTP,1,prmv);
                                    MySQLExitOnError(*val);

                                    /* add object to record */
                                    *ec =  TBrick_SetIV3(gCP,gTP,*ret,TINT(fieldIdx),TINT(0),TINT(rowIdx),*val);
                                    MySQLExitOnError(*ec);
                                    }
                                else
                                if (strncmp(valuePtr,MYSQL_BLOB_AIS_BINARY,MYSQL_BLOB_HDR_LENGTH) == 0)
                                    {
                                    /* binary representation */
                                    valuePtr += MYSQL_BLOB_HDR_LENGTH;

                                    /* get the actual length */
                                    lengthOf = mysqlLength[fieldIdx] - MYSQL_BLOB_HDR_LENGTH;
                                    prmv[0] = TINT(lengthOf);

                                    /* create a byte vector to contain the binary data */
                                    *val = TByteVector_MakeNew(gCP,gTP,1,prmv);
                                    MySQLExitOnError(*val);

                                    fromPtr = valuePtr;
                                    destPtr = (LpCHAR)ByteArray(*val);

                                    for (indexOf = 0; indexOf < lengthOf; indexOf++)
                                        destPtr[indexOf] = fromPtr[indexOf];

                                    /* convert binary data to it's appropriate object instance */
                                    prmv[0] = *val;
                                    *val = FConio_loadObject(gCP,gTP,1,prmv);
                                    MySQLExitOnError(*val);

                                    /* add object to record */
                                    *ec =  TBrick_SetIV3(gCP,gTP,*ret,TINT(fieldIdx),TINT(0),TINT(rowIdx),*val);
                                    MySQLExitOnError(*ec);
                                    }
                                else
                                    {
                                    /* default */
                                    lengthOf = mysqlLength[fieldIdx];
                                    prmv[0] = TINT(lengthOf);

                                    *val = TByteVector_MakeNew(gCP,gTP,1,prmv);
                                    MySQLExitOnError(*val);

                                    fromPtr = mysqlRow[fieldIdx];
                                    destPtr = (LpCHAR)ByteArray(*val);

                                    for (indexOf = 0; indexOf < lengthOf; indexOf++)
                                        destPtr[indexOf] = fromPtr[indexOf];

                                    *ec =  TBrick_SetIV3(gCP,gTP,*ret,TINT(fieldIdx),TINT(0),TINT(rowIdx),*val);
                                    MySQLExitOnError(*ec);
                                    }
                                break;

                            /**************/
                            /* REAL TYPES */
                            /**************/
                            case MYSQL_TYPE_DECIMAL:
                            case MYSQL_TYPE_NEWDECIMAL:
                                /* Type: Money */
                                asTag(val) = TYMONEY;
                                asReal(val) = gCP->atof(mysqlRow[fieldIdx]);

                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            case MYSQL_TYPE_FLOAT:
                            case MYSQL_TYPE_DOUBLE:
                                /* Type: Real */
                                asTag(val) = TYREAL;
                                asReal(val) = gCP->atof(mysqlRow[fieldIdx]);

                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            /*****************/
                            /* INTEGER TYPES */
                            /*****************/
                            case MYSQL_TYPE_TINY:
                                /* Type: Short */
                                *val = TINT(gCP->atol(mysqlRow[fieldIdx]));

                                /* This stores the value in the field of the Brick */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            case MYSQL_TYPE_SHORT:
                                /* Type: Short */
                                *val = TINT(gCP->atol(mysqlRow[fieldIdx]));

                                /* Short Upper Limit: 32767 */
                                if ((mysqlField[fieldIdx].flags & UNSIGNED_FLAG) && (asInt(val) > 32767))
                                {
                                    *ec = TERROR("!sql: field value exceeds data type (short) limit!");
                                    MySQLExitOnError(*ec);
                                }

                                /* This stores the value in the field of the Brick */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            case MYSQL_TYPE_YEAR:
                                /* Type: Short */
                            case MYSQL_TYPE_INT24:
                                /* Type: Long */
                                *val = TINT(gCP->atol(mysqlRow[fieldIdx]));

                                /* This stores the value in the field of the Brick */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            case MYSQL_TYPE_LONG:
                                /* Type: Long */
                                *val = TINT(gCP->atol(mysqlRow[fieldIdx]));

                                /* This stores the value in the field of the Brick */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            case MYSQL_TYPE_LONGLONG:
                                /* Type: Integer */
                                *val = TINT(gCP->atol(mysqlRow[fieldIdx]));

                                /* This stores the value in the field of the Brick */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            /* SQL DATETIME Format: YYYY-MM-DD HH:MM:SS */
                            /*******************/
                            /* DATE/TIME TYPES */
                            /*******************/
                            case MYSQL_TYPE_DATETIME:
                            case MYSQL_TYPE_TIMESTAMP:
                                sscanf(valuePtr, "%d-%d-%d %d:%d:%d", &mysqlTime.year, &mysqlTime.month, &mysqlTime.day,
                                    &mysqlTime.hour, &mysqlTime.minute, &mysqlTime.second);

                                prmv[0] = TINT(mysqlTime.year);
                                prmv[1] = TINT(mysqlTime.month);
                                prmv[2] = TINT(mysqlTime.day);
                                prmv[3] = TINT(mysqlTime.hour);
                                prmv[4] = TINT(mysqlTime.minute);
                                prmv[5] = TINT(mysqlTime.second);

                                *val = FDateFnc_date(gCP, gTP, 6, &prmv[0]);
                                MySQLExitOnError(*val);

                                /* This stores the ByteVector Object in the field(Type=Object) of the Brick */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            case MYSQL_TYPE_DATE:
                                sscanf(valuePtr, "%d-%d-%d", &mysqlTime.year, &mysqlTime.month, &mysqlTime.day);

                                prmv[0] = TINT(mysqlTime.year);
                                prmv[1] = TINT(mysqlTime.month);
                                prmv[2] = TINT(mysqlTime.day);

                                *val = FDateFnc_date(gCP, gTP, 3, &prmv[0]);
                                MySQLExitOnError(*val);

                                /* This stores the ByteVector Object in the field(Type=Object) of the Record */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret,
                                                            TINT(fieldIdx),    /* Field Index */
                                                            TINT(0),           /* Repeat Index */
                                                            TINT(rowIdx),      /* Row Index */
                                                            *val);
                                MySQLExitOnError(*ec);
                                break;

                            case MYSQL_TYPE_TIME:
                                sscanf(valuePtr, "%d:%d:%d", &mysqlTime.hour, &mysqlTime.minute, &mysqlTime.second);

                                prmv[0] = TINT(0); /* Year 0 */
                                prmv[1] = TINT(1); /* Jan */
                                prmv[2] = TINT(1); /* 1 */
                                prmv[3] = TINT(mysqlTime.hour);
                                prmv[4] = TINT(mysqlTime.minute);
                                prmv[5] = TINT(mysqlTime.second);

                                *val = FDateFnc_date(gCP, gTP, 6, &prmv[0]);
                                MySQLExitOnError(*val);

                                /* This stores the ByteVector Object in the field(Type=Object) of the Record */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;

                            /**************************/
                            /* STRING OR BINARY TYPES */
                            /**************************/
                            case MYSQL_TYPE_STRING:
                            case MYSQL_TYPE_VAR_STRING:
                                /* CHAR - Type: Character */
                                /* BINARY - Type: ByteVector */

                                if (mysqlField[fieldIdx].flags & BINARY_FLAG)
                                    {
                                    lengthOf = mysqlLength[fieldIdx];
                                    prmv[0] = TINT(lengthOf);
                                    *val = TByteVector_MakeNew(gCP, gTP, 1, &prmv[0]);
                                    MySQLExitOnError(*val);

                                    fromPtr = mysqlRow[fieldIdx];
                                    destPtr = (LpCHAR)ByteArray(*val);

                                    for (indexOf = 0; indexOf < lengthOf; indexOf++)
                                        {
                                        destPtr[indexOf] = fromPtr[indexOf];
                                        }

                                    /* This stores the ByteVector Object in the field(Type=Object) of the Record */
                                    *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                    MySQLExitOnError(*ec);
                                    }
                                else
                                    {
                                    *val = TSTRING(mysqlRow[fieldIdx]);

                                    /* stored as character */
                                    *ec = TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(-1), TINT(rowIdx), *val);

                                    MySQLExitOnError(*ec);
                                    }

                                break;


                            /*********************/
                            /* ENUM OR SET TYPES */
                            /*********************/
                            case MYSQL_TYPE_ENUM:
                            case MYSQL_TYPE_SET:
                                /* Type: Object: String */
                                *val = TSTRING(mysqlRow[fieldIdx]);
                                /* This stores the ByteVector Object in the field(Type=Object) of the Record */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;


                            /*************/
                            /* NULL TYPE */
                            /*************/
                            case MYSQL_TYPE_NULL:
                                /* Type: Object: #void */
                                *val = gCP->Tval_VOID;
                                /* This stores the #void Value in the field(Type=Object) of the Record */
                                *ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
                                MySQLExitOnError(*ec);
                                break;


                            /*********************/
                            /* UNSUPPORTED TYPES */
                            /*********************/
                            default:
                                /* Issue an error if we do not support the MySQL field data type */
                                *val = TERROR("!sql: unsupported MySQL field data type encountered!");
                                break;
                            }
                        MySQLExitOnError(*val);
                        }
                    else
                        {
                    	switch(mysqlField[fieldIdx].type)
							{
							/**************/
							/* REAL TYPES */
							/**************/
							case MYSQL_TYPE_DECIMAL:
							case MYSQL_TYPE_NEWDECIMAL:
								/* Type: Money */
							case MYSQL_TYPE_FLOAT:
							case MYSQL_TYPE_DOUBLE:
								/* Type: Real */
								if (gCP->FMySQL1_UseWordFlt != TRUE)
									{
									*val = gCP->TObject_NAN;
									*ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
									MySQLExitOnError(*ec);
									}
								break;

							/*******************/
							/* DATE/TIME TYPES */
							/*******************/
							case MYSQL_TYPE_DATETIME:
							case MYSQL_TYPE_TIMESTAMP:
							case MYSQL_TYPE_DATE:
							case MYSQL_TYPE_TIME:
								/* Type: Date/Time */
								if (gCP->FMySQL1_UseWordDat != TRUE)
									{
									*val = gCP->TObject_NAN;
									*ec =  TBrick_SetIV3(gCP, gTP, *ret, TINT(fieldIdx), TINT(0), TINT(rowIdx), *val);
									MySQLExitOnError(*ec);
									}
								break;

							/*********************/
							/* UNSUPPORTED TYPES */
							/*********************/
							default:
								break;
							} /* end of switch */
                        } /* end of if */

                    *val = gCP->TObject_VOID;
                    } /* end of for fields */
                } /* end of for rows */

            /* make sure we don't lose our resources */
            mysql_free_result(mysqlResult);
            mysqlResult = NULL;
            }
        else
            {
            /* mysql_store_result() returned nothing, check if it should have */
            if (mysql_field_count(mysqlPtr) == 0)
                {
                /* query was not SELECT (INSERT, UPDATE, DELETE, ...) */

                /* my_ulonglong mysql_affected_rows(MYSQL *mysql) */
                /* Use this function to determine the number of rows affected by the last query */

                /* return the number of rows affected by the last query */
                *ret = TINT(mysql_affected_rows(mysqlPtr));
                }
            else
                {
                /* query was SELECT */
                /* mysql_store_result() should have returned something */

                errorPtr = (LpCHAR)mysql_error(mysqlPtr);
                /* return the error message from the query */
                *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.query: %s!", errorPtr);
                }
            } /* (mysqlResult != NULL) */
        } /* (mysql_query(mysqlPtr, queryPtr) != 0) */

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlTest

This procedure returns TRUE if the connection is valid

Result: true
#endif
TVAL FMySQL1_SqlTest(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
NUM        mysqlHandle = 0;
NUM        maxIndex = 0;
NUM        indexOf = 0;
LpNUM      numPtr = NULL;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(list);
DeclareTVAL(info);
EndFrame

/* check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. and type of arguments */
if (argc != 2 || argv[1].Tag != TYNUM)
    {
    *ret = TERROR("!sql.test: invalid arguments!");
    goto Last;
    }

/* get our MySQL handle */
mysqlHandle = asInt(&argv[1]);

/* check against our list of connections */
if ((gCP->FMySQL1_IntVector != NULL) && (mysqlHandle != 0))
    {
    *list = TOBJ(gCP->FMySQL1_IntVector);
    *info = TOBJ(gCP->FMySQL1_InfoVector);

    /* maxIndex is the no. of items in the vector */
    maxIndex = TIntVector_GetMaxIndex(gCP, gTP, *list);

    if (maxIndex > 0)
        {
        numPtr = IntArray(*list);

        for (indexOf = 0; indexOf < maxIndex; indexOf++)
            {
            /* check if we have a match */
            if (mysqlHandle == numPtr[indexOf])
                {
                *ret = gCP->TObject_TRUE;
                goto Last;
                }
            }
        }
    }

/* no match, return FALSE */
*ret = gCP->TObject_FALSE;

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlUpdate

This is the procedure which serves as an entry point for the "(sql update: ...)"
command defined in the Lambda SQL Ref. Guide. It will update a table with
existing rows using an AIS Brick.

Command format:

  (sql update: sqlHandle table brick binarySW)
- Inserts the contents of a brick object to a SQL table
- Parameters:
    * sqlHandle            - handle returned by sql connect:
    * table                - name of table
    * brick                - collection of rows that will be added to sql table
    * binarySW             - true if we use binary encoding for objects else
                             we use ascii encoding for objects in Brick fields

  (sql update: sqlHandle table vectorOfBrickRefs binarySW)
- Inserts the contents of a vector of brick objects to a SQL table
- Parameters
    * sqlHandle            - handle returned by sql connect:
    * table                - name of the table
    * vectorOfBrickRefs    - collection of rows (stored as a vector of brick objects) to be added
    * binarySW             - true if we use binary encoding for objects else
                             we use asci encoding for objects in Brick fields

If the table does not exists, the function returns false.

#endif
TVAL FMySQL1_SqlUpdate(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
LpCHAR          errorPtr = NULL;
LpCHAR          queryPtr = NULL;
LpCHAR          valuePtr = NULL;
LpCHAR          tablePtr = NULL;
LpCHAR          fieldPtr = NULL;
LpCHAR          rowPtr = NULL;
LpBIND          bindPtr = NULL;
BOLE            binaryMode = FALSE;
BOLE            tblExists = FALSE;

MYSQL*          mysqlPtr = NULL;
MYSQL_RES*      mysqlResult = NULL;
MYSQL_ROW       mysqlRow = NULL;
MYSQL_STMT*     mysqlStmt = NULL;
MYSQL_BIND*     mysqlBind = NULL;

NUM             fieldIdx = 0;           /* current column index in the Brick */
NUM             numFields = 0;          /* number of columns in the Brick */

NUM             colIdx = 0;             /* current item index in keyIdxList or ColIdxList */
NUM             numCols = 0;            /* current number of items (keyIdxList or colIdxList) */

NUM             rowIdx = 0;             /* current row index in the Brick */
NUM             numRows = 0;            /* number of rows in the Brick */
NUM             vectorRowIdx = 0;       /* current row index in the Vector or ObjVector */
NUM             vectorNumRows = 0;      /* number of rows in the Vector or ObjVector */

NUM             keyIdx = 0;             /* current item index in the keyNameList */
NUM             numKeys = 0;            /* number of items in the keyNameList */

TBrick*         brickObj = NULL;
TObjVector*     objVectorObj = NULL;
TVector*        vectorObj = NULL;

StartFrame
DeclareTVAL(query);
DeclareTVAL(update);                    /* the UPDATE statement */
DeclareTVAL(where);                     /* the WHERE clause */
DeclareTVAL(field);                     /* the field name */
DeclareTVAL(ret);
DeclareTVAL(val);
DeclareOBJ(TStructure,fieldListObj);
DeclareTVAL(keyNameList);               /* list of primary key column names */
DeclareTVAL(keyIdxList);                /* list of indexes of primary key columns */
DeclareTVAL(colIdxList);                /* list of indexes of non-primary key columns */
DeclareTVALArray(prmv,5);
EndFrame

/* check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. and type of arguments */
if (argc < 4)
    {
    *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    goto Last;
    }

/* get our MySQL handle */
if (argv[1].Tag == TYVOID)
    {
    /* (sql update: #void ...) */
UseDefault:
    /* use the default embedded connection */
    prmv[0] = TSYMBOL("connect");
    *ret = FMySQL1_SqlConnect(gCP, gTP, 1, prmv);
    ExitOnError(*ret);

    mysqlPtr = (MYSQL*)asInt(ret);
    }
else
if (argv[1].Tag == TYNUM)
    {
    mysqlPtr = (MYSQL*)asInt(&argv[1]);
    if (mysqlPtr == NULL)
        {
        /* (sql update: 0 ...) */
        goto UseDefault;
        }
    else
        {
        /* (sql update: non-zero ...) */
        /* check if connection is valid */
        prmv[0] = TSYMBOL("test");
        prmv[1] = argv[1];
        *ret = FMySQL1_SqlTest(gCP, gTP, 2, prmv);
        ExitOnError(*ret);

        if (asBool(ret) == FALSE)
            {
            *ret = TERROR("!sql.update: MySQL handle is not valid!");
            goto Last;
            }
        }
    }
else
    {
    *ret = TERROR("!sql.update: MySQL handle is not valid!");
    goto Last;
    }

/* get the table */
if (!(((argv[2].Tag == TYSTRING) && ((tablePtr = CharArray(argv[2])) != 0)) ||
      ((argv[2].Tag == TYSYMBOL) && ((tablePtr = SymbolArray(argv[2])) != 0)) ||
      ((argv[2].Tag == TYTEXT) && ((tablePtr = asText(&argv[2])) != 0))))
    {
    *ret = TERROR("!sql.update: invalid table argument!");
    goto Last;
    }

/* check if table name is empty */
if (strlen(tablePtr) == 0)
    {
    *ret = TERROR("!sql.update: table name is blank!");
    goto Last;
    }

/* get the Brick, Vector or ObjVector object */
if (!(((argv[3].Tag == TYBRICK) && ((brickObj = argv[3].u.Brick) != 0)) ||
      ((argv[3].Tag == TYOBJVECTOR) && ((objVectorObj = argv[3].u.ObjVector) != 0)) ||
	  ((argv[3].Tag == TYVECTOR) && ((vectorObj = argv[3].u.Vector) != 0))))
	{
	*ret = TERROR("!sql.update: invalid argument, Brick, Vector, or ObjVector expected!");
	goto Last;
	}

/* get the use binary flag */
if (argc >= 5)
    {
    if (argv[4].Tag != TYBOLE)
        {
        *ret = TERROR("!sql.update: boolean value expected!");
        goto Last;
        }
    binaryMode = asBool(&argv[4]);
    }

/* escape the "'" character in the table name, since we're using it in the query */
prmv[0] = TSTRING(tablePtr);
prmv[1] = TSTRING("'");
prmv[2] = TSTRING("''");
prmv[3] = FTextFnc_substitute(gCP,gTP,3,prmv);

/********************************************************/
/* Build SQL statement for checking if the table exists */
/********************************************************/

/* This way, we don't have to go through the trouble of building the long SQL statement */

prmv[0] = TSTRING("SHOW TABLES LIKE '");
prmv[1] = prmv[3];
prmv[2] = TSTRING("'");
*query = FUtil2_Append(gCP,gTP,3,prmv);

if (query->Tag == TYTEXT)
    queryPtr = asText(query);
else
if (query->Tag == TYSTRING)
    queryPtr = CharArray(*query);
else
    {
    *ret = TERROR("!sql.update: failed in building sql query string!");
    goto Last;
    }

/* Execute our SQL SHOW TABLES statement */
if (mysql_query(mysqlPtr, queryPtr) != 0)
    {
    errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.update: %s!", errorPtr);
    goto Last;
    }

mysqlResult = mysql_store_result(mysqlPtr);

/* Check if the result set is not empty */
if (mysql_num_rows(mysqlResult) > 0)
    tblExists = TRUE;

mysql_free_result(mysqlResult);
mysqlResult = NULL;

if (!tblExists)
    {
    *ret = TERROR("!sql.update: specified table does not exists!");
    goto Last;
    }

/* get list of primary keys */
prmv[0] = TSTRING("DESCRIBE ");
prmv[1] = TSTRING(tablePtr);
*query = FUtil2_Append(gCP,gTP,2,prmv);

if (query->Tag == TYTEXT)
    queryPtr = asText(query);
else
if (query->Tag == TYSTRING)
    queryPtr = CharArray(*query);
else
    {
    *ret = TERROR("!sql.update: failed in building sql query string!");
    goto Last;
    }

/* Execute our SQL DESCRIBE statement */
if (mysql_query(mysqlPtr, queryPtr) != 0)
    {
    errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.update: %s!", errorPtr);
    goto Last;
    }

mysqlResult = mysql_store_result(mysqlPtr);

*keyNameList = FMake_Vector(gCP, gTP, 0, NULL);
ExitOnError(*keyNameList);

asObject(keyIdxList) = (TObject*) TIntVector_New(gCP, gTP);
asTag(keyIdxList) = asObject(keyIdxList)->itsObjectType;

asObject(colIdxList) = (TObject*) TIntVector_New(gCP, gTP);
asTag(colIdxList) = asObject(colIdxList)->itsObjectType;

/* Check if the result set is not empty */
if (mysql_num_rows(mysqlResult) > 0)
	{
	while ((mysqlRow = mysql_fetch_row(mysqlResult)) != NULL)
		{
		if (strcmp(mysqlRow[3],"PRI") == 0)
			{
			*ret = TVector_AddNewValue(gCP, gTP, *keyNameList, TSYMBOL(mysqlRow[0]));
			MySQLExitOnError(*ret);
			}
		}
	}
else
	{
	*ret = TERROR("!sql.update: unable to determine primary key/s!");
	MySQLExitOnError(*ret);
	}

mysql_free_result(mysqlResult);
mysqlResult = NULL;

if ((numKeys = TVector_GetMaxIndex(gCP, gTP, *keyNameList)) < 0)
	{
	*ret = TERROR("!sql.update: target table has no primary key/s defined!");
	goto Last;
	}

/****************************************************/
/* Build SQL statement for updating the table */
/****************************************************/

/* SQL SYNTAX: UPDATE table_name SET field1=value1, field2=value2, ... WHERE key1=value1 AND key2=value2 ... */

prmv[0] = TSTRING("UPDATE ");
prmv[1] = TSTRING(tablePtr);
prmv[2] = TSTRING(" SET ");

*update = FUtil2_Append(gCP,gTP,3,prmv);

/* Note: It is possible that the vector contains different Brick definitions */

if (brickObj == NULL)
	{
	/* get the first Brick object in the Vector */
	if (vectorObj != NULL)
		brickObj = atHMTval(vectorObj->itsTvalArray,0).u.Brick;
	else
		brickObj = (TBrick*)atHMObject(objVectorObj->itsObjectArray,0);
	}

fieldListObj = brickObj->itsFieldList.u.Structure;
if (fieldListObj == NIL)
    {
    *ret = TERROR("!sql.update: Brick field list is null!");
    goto Last;
    }

bindPtr = (LpBIND)*(fieldListObj->itsDictionaryArray);
numFields = fieldListObj->itsMaxItemIndex;

*where = TSTRING(" WHERE ");

/* we'll do a single pass on the field list and construct the UPDATE statement */

for (fieldIdx = 0; fieldIdx < numFields; fieldIdx++)
    {
    /* get column name */
    asObject(field) = bindPtr->Key;
    asTag(field) = asObject(field)->itsObjectType;

    /* check if column is a primary key */
    for (keyIdx = 0; keyIdx < numKeys; keyIdx++)
		{
    	if (strcmp(SymbolArray(*field), SymbolArray(atHMTval(keyNameList->u.Vector->itsTvalArray,keyIdx))) == 0)
    		goto ProcessKey;
		}

    /* column is not a primary key */
    goto ProcessColumn;

ProcessKey:

    /* escape the "`" character */
    //prmv[0] = *field;
    //prmv[1] = TSTRING("`");
    //prmv[2] = TSTRING("``");
    //*field = FTextFnc_substitute(gCP,gTP,3,prmv);

    /* append to WHERE clause */
    prmv[0] = *where;

    /* if we have more than 1 keys, use AND */
    if (TIntVector_GetMaxIndex(gCP, gTP, *keyIdxList) > 0)
		prmv[1] = TSTRING(" AND ");
    else
    	prmv[1] = TSTRING("");

    prmv[2] = *field;
    prmv[3] = TSTRING(" = ?");
    *where = FUtil2_Append(gCP,gTP,4,prmv);

    *ret = TIntVector_AddNewValue(gCP,gTP,*keyIdxList,TINT(fieldIdx));
    ExitOnError(*ret);

    goto ProcessNext;

ProcessColumn:

    /* append to UPDATE statement */
    prmv[0] = *update;

    /* if we have more than 1 columns, use , */
    if (TIntVector_GetMaxIndex(gCP, gTP, *colIdxList) > 0)
    	prmv[1] = TSTRING(",");
    else
    	prmv[1] = TSTRING("");

    prmv[2] = *field;
    prmv[3] = TSTRING(" = ?");
    *update = FUtil2_Append(gCP,gTP,4,prmv);

    *ret = TIntVector_AddNewValue(gCP,gTP,*colIdxList,TINT(fieldIdx));
    ExitOnError(*ret);

ProcessNext:
    /* move to next field in the Brick */
    ++bindPtr;
    }

prmv[0] = *update;                  /* UPDATE tbl SET field1=?,field2=?,...,field#=? */
prmv[1] = *where;                   /* WHERE key1=? AND key2=? */
*update = FUtil2_Append(gCP, gTP, 2, prmv);
ExitOnError(*update);

prmv[0] = *colIdxList;              /* list of non-primary key column indexes */
prmv[1] = *keyIdxList;              /* list of primary key column indexes */
*colIdxList = FUtil2_Append(gCP, gTP, 2, prmv);
ExitOnError(*colIdxList);
numCols = TIntVector_GetMaxIndex(gCP, gTP, *colIdxList);

if (update->Tag == TYTEXT)
    queryPtr = asText(update);
else
if (update->Tag == TYSTRING)
    queryPtr = CharArray(*update);
else
    {
    *ret = TERROR("!sql.update: failed in building sql query string!");
    goto Last;
    }

/* initialize prepare statement */
mysqlStmt = mysql_stmt_init(mysqlPtr);

if (!mysqlStmt)
    {
    errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.update: %s!", errorPtr);
    goto Last;
    }

/* compile prepare statement */
if (mysql_stmt_prepare(mysqlStmt, queryPtr, strlen(queryPtr)))
    {
    errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.update: %s!", errorPtr);
    goto Last;
    }

mysqlBind = (MYSQL_BIND*) malloc(sizeof(MYSQL_BIND) * numFields);
memset(mysqlBind, 0, sizeof(MYSQL_BIND) * numFields);

if (vectorObj != NULL)
	vectorNumRows = vectorObj->itsMaxItemIndex;
else
if (objVectorObj != NULL)
	vectorNumRows = objVectorObj->itsMaxItemIndex;
else
	goto ProcessRows;

for (vectorRowIdx = 0; vectorRowIdx < vectorNumRows; vectorRowIdx++)
	{
	/* Get the next Brick object in the Vector or ObjVector */
	if (vectorObj != NULL)
		brickObj = atHMTval(vectorObj->itsTvalArray,vectorRowIdx).u.Brick;
	else
		brickObj = (TBrick*)atHMObject(objVectorObj->itsObjectArray,vectorRowIdx);

	if (brickObj->itsObjectType != TYBRICK)
		{
	    *ret = TERROR("!sql.update: Vector contains a non-Brick object!");
	    goto Last;
		}

ProcessRows:

	/* Get the no. of rows in the Brick */
	numRows = brickObj->itsRowCount;
	for (rowIdx = 0; rowIdx < numRows; rowIdx++)
		{
		rowPtr = asFieldArray(brickObj) + (rowIdx * brickObj->itsRowByteCount);

		for (colIdx = 0; colIdx < numCols; colIdx++)
			{
			fieldIdx = atHMInt(colIdxList->u.IntVector->itsIntArray,colIdx);
			bindPtr = &atHMBind(fieldListObj->itsDictionaryArray, fieldIdx);
			fieldPtr = rowPtr + bindPtr->Value.Offset;
			fieldIdx = colIdx;

			switch(bindPtr->Value.DeclaredType)
				{
				case TYBOLE:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_TINY;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(CHAR));

					((LpCHAR)mysqlBind[fieldIdx].buffer)[0] = ((LpCHAR)fieldPtr)[0];
					break;

				case TYCHAR:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_VARCHAR;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(bindPtr->Value.Modifier + 1);

					strncpy((char*)mysqlBind[fieldIdx].buffer, (char*)fieldPtr, bindPtr->Value.Modifier);
					((char*)mysqlBind[fieldIdx].buffer)[bindPtr->Value.Modifier] = 0;

					/* allocate length variable if not present */
					if (mysqlBind[fieldIdx].length == NULL)
						mysqlBind[fieldIdx].length = (unsigned long*)malloc(sizeof(unsigned long));

					*mysqlBind[fieldIdx].length = strlen(((char*)mysqlBind[fieldIdx].buffer));
					break;

				case TYDATE:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_DATETIME;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(MYSQL_TIME));

					asTag(val) = TYDATE;
					asReal(val) = ((LpREAL)fieldPtr)[0];

					*ret = FDateFnc_year(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->year = asInt(ret);

					*ret = FDateFnc_month(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->month = asInt(ret);

					*ret = FDateFnc_day(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->day = asInt(ret);

					*ret = FDateFnc_hour(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->hour = asInt(ret);

					*ret = FDateFnc_minute(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->minute = asInt(ret);

					*ret = FDateFnc_second(gCP,gTP,1,val);
					((MYSQL_TIME*)mysqlBind[fieldIdx].buffer)->second = asInt(ret);

					break;

				case TYNUM:
				case TYCHARPOINTER:
				case TYFLOATPOINTER:
				case TYREALPOINTER:
				case TYJUMPPOINTER:
				case TYINTPOINTER:
				case TYSHORTPOINTER:
				case TYLONGPOINTER:
				case TYWORDPOINTER:
	#ifdef _M64
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_LONGLONG;
	#else
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_LONG;
	#endif
					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(NUM));

					((LpNUM)mysqlBind[fieldIdx].buffer)[0] = ((LpNUM)fieldPtr)[0];
					break;

				case TYFLOAT:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_FLOAT;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(FLOAT));

					((LpFLOAT)mysqlBind[fieldIdx].buffer)[0] = ((LpFLOAT)fieldPtr)[0];
					break;

				case TYMONEY:
				case TYREAL:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_DOUBLE;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(REAL));

					((LpREAL)mysqlBind[fieldIdx].buffer)[0] = ((LpREAL)fieldPtr)[0];
					break;

				case TYSHORT:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_SHORT;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(SHORT));

					((LpSHORT)mysqlBind[fieldIdx].buffer)[0] = ((LpSHORT)fieldPtr)[0];
					break;

				case TYLONG:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_LONG;

					if (mysqlBind[fieldIdx].buffer == NULL)
						mysqlBind[fieldIdx].buffer = malloc(sizeof(NUM32));

					((LpNUM32)mysqlBind[fieldIdx].buffer)[0] = ((LpNUM32)fieldPtr)[0];
					break;

				case TYOBJ:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_BLOB;

					if (mysqlBind[fieldIdx].length == NULL)
						mysqlBind[fieldIdx].length = (unsigned long*)malloc(sizeof(unsigned long));

					if (binaryMode)
						{
						/* AIS_BIN: */
						asObject(val) = ((TObject**)fieldPtr)[0];
						asTag(val) = (asObject(val) == NULL) ? TYVOID : asObject(val)->itsObjectType;
						prmv[0] = *val;

						goto InsertBinary;
						}
					else
						{
						/* AIS_ASC: */
						asObject(val) = ((TObject**)fieldPtr)[0];
						asTag(val) = (asObject(val) == NULL) ? TYVOID : asObject(val)->itsObjectType;
						prmv[0] = *val;
						prmv[1] = gCP->TObject_TRUE;

						goto InsertAscii;
						}
					break;

				case TYTVAL:
					mysqlBind[fieldIdx].buffer_type = MYSQL_TYPE_BLOB;

					if (mysqlBind[fieldIdx].length == NULL)
						mysqlBind[fieldIdx].length = (unsigned long*)malloc(sizeof(unsigned long));

					if (binaryMode)
						{
						/* Binary Representation */
						prmv[0] = ((LpTVAL)fieldPtr)[0];

						InsertBinary:

						/* convert to binary representation */
						*ret = FConio_saveObject(gCP,gTP,1,prmv);

						if (asTag(ret) == TYBYTEVECTOR)
							valuePtr = ByteArray(*ret);
						else
							{
							goto ErrorCleanup;
							}

						/* add length of header to the buffer length */
						*mysqlBind[fieldIdx].length = asByteVector(ret)->itsMaxItemIndex + MYSQL_BLOB_HDR_LENGTH;

						/* if the current buffer is enough, don't allocate */
						if (mysqlBind[fieldIdx].buffer_length < *mysqlBind[fieldIdx].length)
							{
							/* if there's an existing buffer, deallocate it first */
							if (mysqlBind[fieldIdx].buffer != NULL)
								free(mysqlBind[fieldIdx].buffer);

							/* allocate the storage for blob data */
							mysqlBind[fieldIdx].buffer = malloc(*mysqlBind[fieldIdx].length);
							mysqlBind[fieldIdx].buffer_length = *mysqlBind[fieldIdx].length;
							}

						/* prepend the binary header and the length field */
						sprintf((char*)mysqlBind[fieldIdx].buffer,"%s",MYSQL_BLOB_AIS_BINARY);

						/* append the binary data */
						memcpy((char*)mysqlBind[fieldIdx].buffer + MYSQL_BLOB_HDR_LENGTH,valuePtr,asByteVector(ret)->itsMaxItemIndex);
						}
					else
						{
						/* ASCII representation */
						prmv[0] = ((LpTVAL)fieldPtr)[0];
						prmv[1] = gCP->TObject_TRUE;

						InsertAscii:

						/* convert to ascii representation */
						*ret = FConvert_ToString(gCP,gTP,2,prmv);

						if (asTag(ret) == TYTEXT)
							valuePtr = asText(ret);
						else
						if (asTag(ret) == TYSTRING)
							valuePtr = CharArray(*ret);
						else
							{
							goto ErrorCleanup;
							}

						/* add length of header to the buffer length, and null terminator */
						*mysqlBind[fieldIdx].length = strlen(valuePtr) + MYSQL_BLOB_HDR_LENGTH + 1;

						/* if the current buffer is enough, don't allocate */
						if (mysqlBind[fieldIdx].buffer_length < *mysqlBind[fieldIdx].length)
							{
							/* if there's an existing buffer, deallocate it first */
							if (mysqlBind[fieldIdx].buffer != NULL)
								free(mysqlBind[fieldIdx].buffer);

							/* allocate the storage for blob data */
							mysqlBind[fieldIdx].buffer = malloc(*mysqlBind[fieldIdx].length);
							mysqlBind[fieldIdx].buffer_length = *mysqlBind[fieldIdx].length;
							}

						/* prepend the ascii header */
						sprintf((char*)mysqlBind[fieldIdx].buffer,"%s%s",MYSQL_BLOB_AIS_ASCII,valuePtr);
						}
					break;

				default:
					break;
				}
			}

		/* bind parameters */
		if (mysql_stmt_bind_param(mysqlStmt,mysqlBind))
			{
			errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
			*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.update: %s!", errorPtr);
			goto ErrorCleanup;
			}

		/* execute the prepared statement */
		if (mysql_stmt_execute(mysqlStmt))
			{
			errorPtr = (LpCHAR)mysql_error((MYSQL*)mysqlPtr);
			*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer, "!sql.update: %s!", errorPtr);
			goto ErrorCleanup;
			}

		} /* end of for rowIdx */

	} /* end of for vectorRowIdx */

*ret = gCP->TObject_OK;

ErrorCleanup:
/* free dynamically allocated resources */
for (fieldIdx = 0; fieldIdx < numFields; fieldIdx++)
    {
    free(mysqlBind[fieldIdx].buffer);
    mysqlBind[fieldIdx].buffer = NULL;

    free(mysqlBind[fieldIdx].length);
    mysqlBind[fieldIdx].length = NULL;
    }

free(mysqlBind);
mysql_stmt_close(mysqlStmt);

Last:
FrameExit(*ret);
}

#if 0
FMySQL1_SqlUseWord

This procedure sets the behavior for handling NULL-value fields
(sql useWord: Integer: true Float: true Date: true)
(sql useWord: All: true)
(sql useWord: All: false)

Result: true
#endif
TVAL FMySQL1_SqlUseWord(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
NUM        indexOf = 0;
BOLE       useWord = FALSE;

BOLE       useWordInt = gCP->FMySQL1_UseWordInt;
BOLE       useWordFlt = gCP->FMySQL1_UseWordFlt;
BOLE       useWordDat = gCP->FMySQL1_UseWordDat;

StartFrame
DeclareTVAL(ret);
EndFrame

/* check for a user escape request before continuing. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

/* check for correct no. of arguments */
if ((argc < 3) && ((argc % 2) == 0))
	{
	*ret = TERROR("!sql.useWord: invalid no. of arguments!");
	goto Last;
	}

for (indexOf = 1; indexOf < argc; indexOf+=2)
	{
	/* get boolean value */
	if (argv[indexOf+1].Tag == TYBOLE)
		{
		useWord = argv[indexOf+1].u.Bool;
		}
	else
		{
		*ret = TERROR("!sql.useWord: invalid argument, expected true or false!");
		goto Last;
		}

	/* determine which type */
	if ((argv[indexOf].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[indexOf]),"Integer") == 0))
		{
		useWordInt = useWord;
		}
	else
	if ((argv[indexOf].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[indexOf]),"Float") == 0))
		{
		useWordFlt = useWord;
		}
	else
	if ((argv[indexOf].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[indexOf]),"Date") == 0))
		{
		useWordDat = useWord;
		}
	else
	if ((argv[indexOf].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[indexOf]),"All") == 0))
		{
		useWordInt = useWordFlt = useWordDat = useWord;
		}
	else
		{
		*ret = TERROR("!sql.useWord: invalid argument, expected Integer:, Float:, or All:!");
		goto Last;
		}
	}

gCP->FMySQL1_UseWordInt = useWordInt;
gCP->FMySQL1_UseWordFlt = useWordFlt;
gCP->FMySQL1_UseWordDat = useWordDat;

/* return TRUE */
*ret = gCP->TObject_TRUE;

Last:
FrameExit(*ret);
}

#endif
