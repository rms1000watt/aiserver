@ECHO OFF
REM refguide/switch.bat   Switch Brief with Description in Function Ontology Documents
REM Run only by AIS administrator.  Proceed with caution!
REM Change History:
REM Version	Date	  Who	Change
REM  1.0006	8/31/2005 tlw	Reverse the Element names in Function_ docs


REM Notes:
REM 1. The following commands will reverse the role of brief and description every time it is run!
REM 2. sr.exe must be in the PATH environment variable.

REM Move to the directory containing the Function documents
CHDIR CoreContent

REM Rename Brief to NOT_VERBOSE, Description to Brief, NOT_VERBOSE to Description
sr "<(/?)Brief>" "<\1NOT_VERBOSE>" "Function_*.html" >> switch.log
sr "<(/?)Description>" "<\1Brief>" "Function_*.html" >> switch.log
sr "<(/?)NOT_VERBOSE>" "<\1Description>" "Function_*.html" >> switch.log


REM Return to starting folder
CHDIR ".."
REM end