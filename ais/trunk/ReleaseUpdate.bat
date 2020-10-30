@ECHO OFF
REM aisdev/updateRelease.bat   Update Release Files to Ais Folder
REM Run only by AIS administrator.  All others check out updates to ais from Starteam!
REM Change History:
REM Version	Date	 Who	Change
REM  1.0100	4/27/06	 tlw	Upgrade documentation. More complete update.
REM  1.0057	3/10/05	 tlw	Replace modified files from aisdev into ais.

REM               UPDATING RELEASE VERSION
REM Build a release version of webide, ais, aised, testaisclient to update exe files.
REM Read the release instructions in NewRelease.txt
REM Make sure that the QTDIR environment variable is set to the current Qt installation path.
REM Set the environment variable AISDIR to the path to the ais directory, OR
REM uncomment and edit the SET AISDIR command below.
REM set AISDIR=C:\ais
REM Run this batch file from the aisdev directory.

replace ais.ini %AISDIR% /U
replace changes.txt %AISDIR% /U
replace aisinstall.ini %AISDIR% /U
replace context.ini %AISDIR% /U
replace contextusers.ini %AISDIR% /U
replace contextusersinfo.txt %AISDIR% /U
replace parameters.txt %AISDIR% /U
replace rideinstall.ini %AISDIR% /U
replace docs\installation\ReadMe.txt %AISDIR% /U

replace %QTDIR%\bin\QtCored4.dll %AISDIR% /U
replace %QTDIR%\bin\QtGuid4.dll %AISDIR% /U
replace %QTDIR%\bin\QtNetworkd4.dll %AISDIR% /U
replace %QTDIR%\bin\QtXmld4.dll %AISDIR% /U

REM docs:
replace docs\installation\*.txt %AISDIR%\docs\installation /U
replace docs\installation\ReleaseNotes0100.doc %AISDIR%\docs\installation /U
replace docs\installation\starteam\*.* %AISDIR%\docs\installation\starteam /U

REM include:
replace include\fsmtbase.h %AISDIR%\include /U
replace include\images\*.bmp %AISDIR%\include /U

REM libraries:
replace libraries\browseLib\browseLib.sl %AISDIR%\libraries\browseLib /U
replace libraries\datamineLambda\datamineLambda.sl %AISDIR%\libraries\datamineLambda /U
replace libraries\farm\*.ini %AISDIR%\libraries\farm /U
replace libraries\farm\*.sl %AISDIR%\libraries\farm /U
replace libraries\farm\*.txt %AISDIR%\libraries\farm /U
replace libraries\farm\worker1\*.* %AISDIR%\libraries\farm\worker1 /U
replace libraries\index\index.sl %AISDIR%\libraries\index /U
replace libraries\javascript\javascript.sl %AISDIR%\libraries\javascript /U
replace libraries\math\math.sl %AISDIR%\libraries\math /U
replace libraries\ParseLib\ParseLib.sl %AISDIR%\libraries\ParseLib /U
replace libraries\rulesLib\rulesLib.sl %AISDIR%\libraries\rulesLib /U

REM test:
replace test\*.* %AISDIR%\test /U

REM testais:
replace testais\astartup.sl %AISDIR%\testais /U
replace testais\astartupInit.sl %AISDIR%\testais /U
replace testais\*.ini %AISDIR%\testais /U
replace testais\contextusersinfo.txt %AISDIR%\testais /U
replace testais\cabinets\*.sl %AISDIR%\testais\cabinets /U
replace testais\wwwroot\*.htm %AISDIR%\testais\wwwroot /U
replace testais\wwwroot\*.txt %AISDIR%\testais\wwwroot /U
replace testais\wwwroot\secure\*.htm %AISDIR%\testais\wwwroot\secure /U
replace testais\wwwroot\xml\*.htm %AISDIR%\testais\wwwroot\xml /U
replace testais\test2\astartup2.sl %AISDIR%\testais\test2 /U
replace testais\test2\context.ini %AISDIR%\testais\test2 /U
replace testais\test2\contextusers.ini %AISDIR%\testais\test2 /U
replace testais\test2\contextusersinfo.txt %AISDIR%\testais\test2 /U
replace testais\test2\cabinets\*.sl %AISDIR%\testais\test2\cabinets /U
replace testais\test2\wwwroot\*.htm %AISDIR%\testais\test2\wwwroot /U
replace testais\test2\wwwroot\*.txt %AISDIR%\testais\test2\wwwroot /U
replace testais\test2\wwwroot\secure\*.htm %AISDIR%\testais\test2\wwwroot\secure /U
replace testais\test2\wwwroot\xml\*.htm %AISDIR%\testais\test2\wwwroot\xml /U

REM testaisclient:
replace testaisclient\*.tst %AISDIR%\testaisclient /U
replace testaisclient\*.txt %AISDIR%\testaisclient /U

REM usr
replace usr\*.txt %AISDIR%\usr /U

REM wwwroot
replace testais\wwwroot\default.htm %AISDIR%\wwwroot /U
replace testais\wwwroot\LogonForm.htm %AISDIR%\wwwroot /U
replace testais\wwwroot\nocookie.htm %AISDIR%\wwwroot /U
replace testais\wwwroot\SimpleConsoleViewer.htm %AISDIR%\wwwroot /U
replace testais\wwwroot\view.htm %AISDIR%\wwwroot /U
replace testais\wwwroot\favicon.ico %AISDIR%\wwwroot /U

REM xml:
replace xml\*.c %AISDIR%\xml /U
replace xml\*.h %AISDIR%\xml /U
replace xml\*.vcproj %AISDIR%\xml /U

REM end
