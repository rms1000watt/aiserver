# ais/testaisclient/testxmlLambda.txt

# Tests of testLambda commands in the main test cabinet.

# CHANGE HISTORY
# Version	Date		Who		Change
# 1.0104	9/8/2006	tlw		Modify expected return for CloseConnection
# 1.0039	7/12/2004	tlw		Add all built-in _ais functions.

# NOTES
#	 1.	See testsuite.txt for more info
# -------------------------------------------------------------------------------------------------

# Empty probe
X0://$host$:$xmlport$/_ais
_ais|noop|connectid
# -------------------------------------------------------------------------------------------------

# ampAct
X0://$host$:$xmlport$/testLambda|ampAct|param1|Some Value
testLambda|ampAct|result|Input is Some Value
# -------------------------------------------------------------------------------------------------

# Long input message (8192+)
X0://$host$:$xmlport$/testLambda|ampAct|param1|{2}0123456789ABCDEF
testLambda|ampAct|result|Input is 0123456789ABCDEF0123456789ABCDEF
# -------------------------------------------------------------------------------------------------

# busyAct 
X0://$host$:$xmlport$/testLambda|busyAct|length|2
testLambda|busyAct
# -------------------------------------------------------------------------------------------------

# fcnAct
X0://$host$:$xmlport$/testLambda|fcnAct|param|string input
testLambda|fcnAct|error|!Standard function interface not implemented
# -------------------------------------------------------------------------------------------------

# fileAct
X0://$host$:$xmlport$/testLambda|fileAct|filename|default.txt
testLambda|fileAct|file|Contents of default.txt
# -------------------------------------------------------------------------------------------------

# noAccess
X0://$host$:$xmlport$/testLambda|noAccessAct
testLambda|noAccessAct|error|!Requested Lambda is not a public Lambda
# -------------------------------------------------------------------------------------------------

# sendToClient
X0://$host$:$xmlport$/testLambda|sendToClientAct
_ais|fcnsendtoclient|testLambda|pushed|msg|Async message from testLambda
testLambda|sendToClientAct|result|message has been sent back to client
# -------------------------------------------------------------------------------------------------

# showMsgAct 
X0://$host$:$xmlport$/testLambda|showMsgAct
testLambda|showMsgAct|testLambda|showMsgAct|sessionid|
# -------------------------------------------------------------------------------------------------

# sleepyAct
X0://$host$:$xmlport$/testLambda|sleepyAct|length|2
testLambda|sleepyAct
# -------------------------------------------------------------------------------------------------

# textAct 
X0://$host$:$xmlport$/testLambda|textAct|arg0|X%3Da%3C%3Eb|arg1|v%3D%22quoted%22g
testLambda|textAct|arg0|X=a<>b|arg1|v="quoted"g
# -------------------------------------------------------------------------------------------------

# urlAct
X0://$host$:$xmlport$/testLambda|urlAct|redirectTo|http://$host$:$xmlport$/
testLambda|urlAct|url|http://$host$:$xmlport$/
# -------------------------------------------------------------------------------------------------

# Noop
X0://$host$:$xmlport$/_ais|noop|arg0|value0|arg1|value1
_ais|noop|result|arg0=value0,arg1=value1,sessionid=
# -------------------------------------------------------------------------------------------------

# CloseSession
X0://$host$:$xmlport$/_ais|closesession|sessionid|$xmlsid$|mode|5
_ais|closesession|result|true
# -------------------------------------------------------------------------------------------------

# Logoff
X0://$host$:$xmlport$/_ais|logoff
_ais|logoff|result|
# -------------------------------------------------------------------------------------------------

# CloseConnection
X0://$host$:$xmlport$/_ais|closeconnection
TCP/IP Socket error
# -------------------------------------------------------------------------------------------------

# end
