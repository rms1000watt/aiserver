@ECHO OFF
REM refguide/convert.bat   Convert Ontology Documents in CoreContent to XML
REM Run only by AIS administrator.  Proceed with caution!
REM Change History:
REM Version	Date	  Who	Change
REM  1.0006	8/30/2005 tlw	Do not add CDATA section termination if already terminated. Fix capture of </Overview().
REM  1.0002	8/26/2005 tlw	Rename files

REM Notes:
REM 1. The following commands can safely be rerun on the same set of files
REM 2. sr.exe must be in the PATH environment variable.

REM Convert filename extension from .html to .xml
CHDIR CoreContent
RENAME *.html *.xml > convert.log

REM Strip HTML headers and footers
sr "<html>\s*<head>\s*</head>\s*<body>\s*" "" "*.xml" >> convert.log
sr "</body>\s*</html>\s*" "" "*.xml" >> convert.log

REM Clean out processing directives
sr "<\?([^?]|\?(?!>))+\?>\s*" "" "*.xml" >> convert.log
sr "<!DOCTYPE[^>]+>\s*" "" "*.xml" >> convert.log

REM Add back in new processing directives
sr "^" "<!DOCTYPE Datatype SYSTEM \"../DTD/Datatype.dtd\">\n" "Datatype_*.xml" >> convert.log
sr "^" "<!DOCTYPE Document SYSTEM \"../DTD/Document.dtd\">\n" "Document_*.xml" >> convert.log
sr "^" "<!DOCTYPE Essay SYSTEM \"../DTD/Essay.dtd\">\n" "Essay_*.xml" >> convert.log
sr "^" "<!DOCTYPE Example SYSTEM \"../DTD/Example.dtd\">\n" "Example_*.xml" >> convert.log
sr "^" "<!DOCTYPE Function SYSTEM \"../DTD/Function.dtd\">\n" "Function_*.xml" >> convert.log
sr "^" "<!DOCTYPE VMInstruction SYSTEM \"../DTD/VMInstruction.dtd\">\n" "VMInstruction_*.xml" >> convert.log
sr "^" "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>\n" "*.xml" >> convert.log

REM Strip out extra newlines and trailing spaces
sr ">([ \t]*\n){2,}" ">\n" "*.xml"
sr "\s+$" "\n" "*.xml"

REM Modify HTML tags to conform to XHTML standard
REM sr "<b>" "<B>" "*.xml" >> convert.log
REM sr "</b>" "</B>" "*.xml" >> convert.log
REM sr "<br>"  "<BR/>" "*.xml" >> convert.log
REM sr "<br/>" "<BR/>" "*.xml" >> convert.log
REM sr "<li>" "<LI>" "*.xml" >> convert.log
REM sr "</li>" "</LI>" "*.xml" >> convert.log
REM sr "<p[^>]*>" "<P>" "*.xml" >> convert.log
REM sr "</p>" "</P>" "*.xml" >> convert.log
REM sr "<ul>" "<UL>" "*.xml" >> convert.log
REM sr "</ul>" "</UL>" "*.xml" >> convert.log

REM Add CDATA section to content of the following elements
sr "<Description>(?!<!)" "<Description><![CDATA[" "*.xml" >> convert.log
sr "(([^\]]..)|(.[^\]].)|(..[^>]))</Description>" "\1]]></Description>" "*.xml" >> convert.log
sr "<Expression>(?!<!)" "<Expression><![CDATA[" "*.xml" >> convert.log
sr "(([^\]]..)|(.[^\]].)|(..[^>]))</Expression>" "\1]]></Expression>" "*.xml" >> convert.log
sr "<Hints>(?!<!)" "<Hints><![CDATA[" "*.xml" >> convert.log
sr "(([^\]]..)|(.[^\]].)|(..[^>]))</Hints>" "\1]]></Hints>" "*.xml" >> convert.log
sr "<Note>(?!<!)" "<Note><![CDATA[" "*.xml" >> convert.log
sr "(([^\]]..)|(.[^\]].)|(..[^>]))</Note>" "\1]]></Note>" "*.xml" >> convert.log
sr "<Overview(-\w+)?>(?!<!)" "<Overview\1><![CDATA[" "*.xml" >> convert.log
sr "(([^\]]..)|(.[^\]].)|(..[^>]))</Overview(-\w+)?>" "\1]]></Overview\5>" "*.xml" >> convert.log
sr "<Return>(?!<!)" "<Return><![CDATA[" "*.xml" >> convert.log
sr "(([^\]]..)|(.[^\]].)|(..[^>]))</Return>" "\1]]></Return>" "*.xml" >> convert.log
sr "<Returns>(?!<!)" "<Returns><![CDATA[" "*.xml" >> convert.log
sr "(([^\]]..)|(.[^\]].)|(..[^>]))</Returns>" "\1]]></Returns>" "*.xml" >> convert.log
sr "<Usage(-\w+)?>(?!<!)" "<Usage\1><![CDATA[" "*.xml" >> convert.log
sr "(([^\]]..)|(.[^\]].)|(..[^>]))</Usage(-\w+)?>" "\1]]></Usage\5>" "*.xml" >> convert.log


REM Return to starting folder
CHDIR ".."
REM end