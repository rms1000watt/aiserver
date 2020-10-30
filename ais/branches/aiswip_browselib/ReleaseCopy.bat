@ECHO OFF
REM aisdev/ReleaseCopy.bat   Copy Release Files to Ais Folder
REM Run only by AIS administrator.  All others check out updates to ais from Starteam!
REM Change History:
REM Version	Date		Who	Change
REM  1.0118	12/15/2006	tlw	Libraries. Add egm, esm, gsm, svm, xml.
REM  1.0100	4/29/2006	tlw	Upgrade documentation. More complete copy.
REM  1.0065	7/11/2005	tlw	Sync up with ReleaseUpdate.bat
REM  1.0053	1/31/2005	tlw	Add libraries/farm
REM  1.0052	1/10/2005	tlw	Update files in test folders

REM               UPDATING RELEASE VERSION
REM Build a release version of webide, ais, aised, testaisclient to update the exe
REM files in ais.
REM Read the directions for updating ais in Release.txt.
REM Make sure that the QTDIR environment variable is set to the current QT
REM installation path
REM Set the environment variable AISDIR to the path to the ais directory, OR
REM uncomment and edit the SET AISDIR command below.
REM set AISDIR=C:\ais
REM Run this batch file from the aisdev directory.
REM SETUP To make a fresh copy of ais (examples shown in parentheses):
REM 1. Create an ais directory at the intended destination (C:\ais)
REM 2. Set the AISDIR environment variable to the path to AISDIR. (AISDIR=C:\ais).
REM 3. Add the following subdirectories in the ais directory.
REM	bin, include, libraries, test, testais, testaisclient, usr.

REM ais:
IF NOT EXIST %AISDIR% MD %AISDIR%
copy *.ini %AISDIR%\ /Y
copy astartup.sl %AISDIR%\ /Y
copy changehistory.txt %AISDIR%\ /Y
copy contextusersinfo.txt %AISDIR%\ /Y
copy parameters.txt %AISDIR%\ /Y
copy serverscfg.txt %AISDIR%\ /Y
copy docs\installation\readme.txt %AISDIR%\ /Y
copy docs\installation\*.dll %AISDIR%\ /Y
copy %QTDIR%\bin\QtCore4.dll %AISDIR%\ /Y
copy %QTDIR%\bin\QtGui4.dll %AISDIR%\ /Y
copy %QTDIR%\bin\QtNetwork4.dll %AISDIR%\ /Y
copy %QTDIR%\bin\QtXml4.dll %AISDIR%\ /Y

REM docs:
IF NOT EXIST %AISDIR%\docs MD %AISDIR%\docs
IF NOT EXIST %AISDIR%\docs\guides MD %AISDIR%\docs\guides
IF NOT EXIST %AISDIR%\docs\installation MD %AISDIR%\docs\installation
IF NOT EXIST %AISDIR%\docs\installation\starteam MD %AISDIR%\docs\installation\starteam
IF NOT EXIST %AISDIR%\docs\onlinedocs MD %AISDIR%\docs\onlinedocs
copy docs\guides\*.* %AISDIR%\docs\guides\ /Y
copy docs\installation\*.txt %AISDIR%\docs\installation\ /Y
copy docs\installation\releasenotes*.doc %AISDIR%\docs\installation\ /Y
copy docs\installation\starteam\*.* %AISDIR%\docs\installation\starteam\ /Y
copy docs\onlinedocs\*.* %AISDIR%\docs\onlinedocs\ /Y

REM include:
IF NOT EXIST %AISDIR%\include MD %AISDIR%\include
IF NOT EXIST %AISDIR%\include\images MD %AISDIR%\include\images
copy include\fsmtbase.h %AISDIR%\include\ /Y
copy include\images\*.bmp %AISDIR%\include\images\ /Y

REM libraries:
IF NOT EXIST %AISDIR%\libraries MD %AISDIR%\libraries
IF NOT EXIST %AISDIR%\libraries\alice MD %AISDIR%\libraries\alice
IF NOT EXIST %AISDIR%\libraries\browseLib MD %AISDIR%\libraries\browseLib
IF NOT EXIST %AISDIR%\libraries\datamineLambda MD %AISDIR%\libraries\datamineLambda
IF NOT EXIST %AISDIR%\libraries\egm MD %AISDIR%\libraries\egm
IF NOT EXIST %AISDIR%\libraries\esm MD %AISDIR%\libraries\esm
IF NOT EXIST %AISDIR%\libraries\farm MD %AISDIR%\libraries\farm
IF NOT EXIST %AISDIR%\libraries\gsm MD %AISDIR%\libraries\gsm
IF NOT EXIST %AISDIR%\libraries\farm\worker1 MD %AISDIR%\libraries\farm\worker1
IF NOT EXIST %AISDIR%\libraries\index MD %AISDIR%\libraries\index
IF NOT EXIST %AISDIR%\libraries\javascript MD %AISDIR%\libraries\javascript
IF NOT EXIST %AISDIR%\libraries\math MD %AISDIR%\libraries\math
IF NOT EXIST %AISDIR%\libraries\ParseLib MD %AISDIR%\libraries\ParseLib
IF NOT EXIST %AISDIR%\libraries\rulesLib MD %AISDIR%\libraries\rulesLib
IF NOT EXIST %AISDIR%\libraries\svm MD %AISDIR%\libraries\svm
IF NOT EXIST %AISDIR%\libraries\xml MD %AISDIR%\libraries\xml
copy libraries\*.sl %AISDIR%\libraries\ /Y
copy libraries\*.ini %AISDIR%\libraries\ /Y
copy libraries\alice\*.sl %AISDIR%\libraries\alice\ /Y
copy libraries\browseLib\browseLib.sl %AISDIR%\libraries\browseLib\ /Y
copy libraries\datamineLambda\datamineLambda.sl %AISDIR%\libraries\datamineLambda\ /Y
copy libraries\egm\*.sl %AISDIR%\libraries\egm\ /Y
copy libraries\esm\*.sl %AISDIR%\libraries\esm\ /Y
copy libraries\farm\*.ini %AISDIR%\libraries\farm\ /Y
copy libraries\farm\*.sl %AISDIR%\libraries\farm\ /Y
copy libraries\farm\*.txt %AISDIR%\libraries\farm\ /Y
copy libraries\farm\worker1\*.* %AISDIR%\libraries\farm\worker1\ /Y
copy libraries\gsm\*.sl %AISDIR%\libraries\gsm\ /Y
copy libraries\index\index.sl %AISDIR%\libraries\index\ /Y
copy libraries\javascript\javascript.sl %AISDIR%\libraries\javascript\ /Y
copy libraries\math\math.sl %AISDIR%\libraries\math\ /Y
copy libraries\ParseLib\ParseLib.sl %AISDIR%\libraries\ParseLib\ /Y
copy libraries\rulesLib\rulesLib.sl %AISDIR%\libraries\rulesLib\ /Y
copy libraries\svm\*.sl %AISDIR%\libraries\svm\ /Y
copy libraries\xml\*.* %AISDIR%\libraries\xml\ /Y

REM logs:
IF NOT EXIST %AISDIR%\logs MD %AISDIR%\logs
copy logs\empty.log %AISDIR%\logs\ /Y

REM test:
IF NOT EXIST %AISDIR%\test MD %AISDIR%\test
copy test\*.* %AISDIR%\test\ /Y

REM testais:
IF NOT EXIST %AISDIR%\testais MD %AISDIR%\testais
IF NOT EXIST %AISDIR%\testais\cabinets MD %AISDIR%\testais\cabinets
IF NOT EXIST %AISDIR%\testais\wwwroot MD %AISDIR%\testais\wwwroot
IF NOT EXIST %AISDIR%\testais\wwwroot\secure MD %AISDIR%\testais\wwwroot\secure
IF NOT EXIST %AISDIR%\testais\wwwroot\xml MD %AISDIR%\testais\wwwroot\xml
IF NOT EXIST %AISDIR%\testais\test2 MD %AISDIR%\testais\test2
IF NOT EXIST %AISDIR%\testais\test2\cabinets MD %AISDIR%\testais\test2\cabinets
IF NOT EXIST %AISDIR%\testais\test2\wwwroot MD %AISDIR%\testais\test2\wwwroot
IF NOT EXIST %AISDIR%\testais\test2\wwwroot\secure MD %AISDIR%\testais\test2\wwwroot\secure
IF NOT EXIST %AISDIR%\testais\test2\wwwroot\xml MD %AISDIR%\testais\test2\wwwroot\xml
copy testais\*.sl %AISDIR%\testais\ /Y
copy testais\*.ini %AISDIR%\testais\ /Y
copy testais\contextusersinfo.txt %AISDIR%\testais\ /Y
copy testais\cabinets\*.sl %AISDIR%\testais\cabinets\ /Y
copy testais\wwwroot\*.htm %AISDIR%\testais\wwwroot\ /Y
copy testais\wwwroot\*.txt %AISDIR%\testais\wwwroot\ /Y
copy testais\wwwroot\secure\*.htm %AISDIR%\testais\wwwroot\secure\ /Y
copy testais\wwwroot\xml\*.htm %AISDIR%\testais\wwwroot\xml\ /Y
copy testais\test2\astartup2.sl %AISDIR%\testais\test2\ /Y
copy testais\test2\context.ini %AISDIR%\testais\test2\ /Y
copy testais\test2\contextusers.ini %AISDIR%\testais\test2\ /Y
copy testais\test2\contextusersinfo.txt %AISDIR%\testais\test2\ /Y
copy testais\test2\cabinets\*.sl %AISDIR%\testais\test2\cabinets\ /Y
copy testais\test2\wwwroot\*.htm %AISDIR%\testais\test2\wwwroot\ /Y
copy testais\test2\wwwroot\*.txt %AISDIR%\testais\test2\wwwroot\ /Y
copy testais\test2\wwwroot\secure\*.htm %AISDIR%\testais\test2\wwwroot\secure\ /Y
copy testais\test2\wwwroot\xml\*.htm %AISDIR%\testais\test2\wwwroot\xml\ /Y

REM testaisclient:
IF NOT EXIST %AISDIR%\testaisclient MD %AISDIR%\testaisclient
copy testaisclient\*.tst %AISDIR%\testaisclient\ /Y
copy testaisclient\*.txt %AISDIR%\testaisclient\ /Y

REM usr
IF NOT EXIST %AISDIR%\usr MD %AISDIR%\usr
copy usr\*.txt %AISDIR%\usr\ /Y

REM wwwroot:
IF NOT EXIST %AISDIR%\wwwroot MD %AISDIR%\wwwroot
copy testais\wwwroot\default.htm %AISDIR%\wwwroot\ /Y
copy testais\wwwroot\LogonForm.htm %AISDIR%\wwwroot\ /Y
copy testais\wwwroot\nocookie.htm %AISDIR%\wwwroot\ /Y
copy testais\wwwroot\SimpleConsoleViewer.htm %AISDIR%\wwwroot\ /Y
copy testais\wwwroot\view.htm %AISDIR%\wwwroot\ /Y
copy testais\wwwroot\favicon.ico %AISDIR%\wwwroot\ /Y

REM xml:
IF NOT EXIST %AISDIR%\xml MD %AISDIR%\xml
copy xml\*.c %AISDIR%\xml\ /Y
copy xml\*.h %AISDIR%\xml\ /Y
copy xml\*.vcproj %AISDIR%\xml\ /Y
copy xml\*.sln %AISDIR%\xml\ /Y

REM end
