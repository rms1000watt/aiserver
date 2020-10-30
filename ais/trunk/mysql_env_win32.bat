@echo off
rem This file is used to prepare the MySQL development files required
rem in building the Analytic Information Server.
rem
rem %ProgramFiles% - maps to C:\Program Files in both 32-bit and 64-bit
rem %ProgramFiles(x86)% - maps to C:\Program Files (x86) in 64-bit

set PWD=%CD%
set INC_DIR=include
set LIB32_DIR=lib32
set LIB64_DIR=lib64
set DBG_DIR=debug
set REL_DIR=release
set LIB_DIR=Embedded\DLL
set SHARE_DIR=share
set ENG_DIR=english
set MINGW_DIR_1=C:\MinGW\bin
set MINGW_DIR_2=C:\Qt\2010.04\mingw\bin
set PATH=%PATH%;%MINGW_DIR_1%;%MINGW_DIR_2%
set MINGW_DLLTOOL=dlltool.exe
set MINGW_REIMP=reimp.exe
set MYSQL_DEF=libmysqld.def
set MYSQL_LIB_A=libmysqld.a
set MYSQL_DLL=libmysqld.dll
set MYSQL_LIB=libmysqld.lib

if "%MYSQLDIR%" == "" set MYSQLDIR=C:\MySQL
set PROGDIR=%ProgramFiles(x86)%
if "%ProgramFiles(x86)%" == "" set PROGDIR=%ProgramFiles%

set MYSQL_INSTALL_DIR=%PROGDIR%\MySQL\MySQL Server 5.1

if not exist %MYSQLDIR% md %MYSQLDIR%
if not exist %MYSQLDIR%\%INC_DIR% md %MYSQLDIR%\%INC_DIR%
if not exist %MYSQLDIR%\%LIB32_DIR% md %MYSQLDIR%\%LIB32_DIR%
if not exist %MYSQLDIR%\%LIB32_DIR%\%DBG_DIR% md %MYSQLDIR%\%LIB32_DIR%\%DBG_DIR%
if not exist %MYSQLDIR%\%LIB32_DIR%\%REL_DIR% md %MYSQLDIR%\%LIB32_DIR%\%REL_DIR%
if not exist %MYSQLDIR%\%SHARE_DIR% md %MYSQLDIR%\%SHARE_DIR%
if not exist %MYSQLDIR%\%SHARE_DIR%\%ENG_DIR% md %MYSQLDIR%\%SHARE_DIR%\%ENG_DIR%

echo Stage 1

rem Copy include files
copy "%MYSQL_INSTALL_DIR%\%INC_DIR%\*.*" "%MYSQLDIR%\%INC_DIR%\" /Y

rem Copy release and debug static libraries
copy "%MYSQL_INSTALL_DIR%\%LIB_DIR%\%DBG_DIR%\%MYSQL_DLL%" "%MYSQLDIR%\%LIB32_DIR%\%DBG_DIR%" /Y
copy "%MYSQL_INSTALL_DIR%\%LIB_DIR%\%DBG_DIR%\%MYSQL_LIB%" "%MYSQLDIR%\%LIB32_DIR%\%DBG_DIR%" /Y
copy "%MYSQL_INSTALL_DIR%\%LIB_DIR%\%REL_DIR%\%MYSQL_DLL%" "%MYSQLDIR%\%LIB32_DIR%\%REL_DIR%" /Y
copy "%MYSQL_INSTALL_DIR%\%LIB_DIR%\%REL_DIR%\%MYSQL_LIB%" "%MYSQLDIR%\%LIB32_DIR%\%REL_DIR%" /Y

rem Copy message file
copy "%MYSQL_INSTALL_DIR%\%SHARE_DIR%\%ENG_DIR%\*.*" "%MYSQLDIR%\%SHARE_DIR%\%ENG_DIR%" /Y

echo Stage 2
cd %MYSQLDIR%\%LIB32_DIR%\%DBG_DIR%
%MINGW_REIMP% -d "%MYSQL_LIB%"
%MINGW_DLLTOOL% -k -d "%MYSQL_DEF%" -l "%MYSQL_LIB_A%"

cd %MYSQLDIR%\%LIB32_DIR%\%REL_DIR%
%MINGW_REIMP% -d "%MYSQL_LIB%"
%MINGW_DLLTOOL% -k -d "%MYSQL_DEF%" -l "%MYSQL_LIB_A%"

echo Complete

cd %PWD%

:end
rem End