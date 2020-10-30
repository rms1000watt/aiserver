# ais/testaisclient/testhttpLambda.txt

# Tests of Lambda commands in the testLambda cabinet from HTTP

# CHANGE HISTORY
# Version	Date		Who		Change
# 1.0104	9/8/2006	tlw		Add httpsid0 and httsid1 to distinguish sessions on H0 from P1.
# 1.0039	7/12/2004	tlw		Add all built-in tests to testxmlamp.txt

# NOTES
#	 1.	See testsuite.txt for more info on constructing tests
#	 2. See ahttpsvr.h for more info on HTTP requests and their responses.
# -------------------------------------------------------------------------------------------------

# ampAct
H0://$host$:$httpport$/amp.dll?testLambda=ampAct&param1=Some+Value
result|Input is Some Value
# -------------------------------------------------------------------------------------------------

# Long input message (8192+)
H0://$host$:$httpport$/amp.dll?testLambda=ampAct&param1={2}0123456789ABCDEF
result|Input is 0123456789ABCDEF0123456789ABCDEF
# -------------------------------------------------------------------------------------------------

# busyAct 
H0://$host$:$httpport$/amp.dll?testLambda=busyAct&length=2
result|busyAct finished.
# -------------------------------------------------------------------------------------------------

# fcnAct
H0://$host$:$httpport$/amp.dll?testLambda=fcnAct&param=string input
!Standard function interface not implemented yet
# -------------------------------------------------------------------------------------------------

# fileAct
H0://$host$:$httpport$/amp.dll?testLambda=fileAct&filename=wwwroot/afile.htm
<HTML><HEAD><TITLE> A File Page </TITLE></HEAD><BODY><H3>InvestByLambda </H3>
# -------------------------------------------------------------------------------------------------

# noAccessAct
H0://$host$:$httpport$/amp.dll?testLambda=noAccessAct
!Requested Lambda is not a public Lambda!
# -------------------------------------------------------------------------------------------------

# sendToClientAct
H0://$host$:$httpport$/amp.dll?testLambda=sendToClientAct
result|message has been sent back to client
# -------------------------------------------------------------------------------------------------

# showMsgAct
H0://$host$:$httpport$/amp.dll?testLambda=showMsgAct
testLambda|showMsgAct|sessionid|$httpsid0$|userid|$httpusrid$
# -------------------------------------------------------------------------------------------------

# sleepyAct
H0://$host$:$httpport$/amp.dll?testLambda=sleepyAct&length=2
result|sleepy slept 2 times for 500 msec.
# -------------------------------------------------------------------------------------------------

# textAct
H0://$host$:$httpport$/amp.dll?testLambda=textAct&arg0=X%3Da%3C%3Eb&arg1=v%3D%22quoted%22g
arg0|X=a<>b|arg1|v="quoted"g
# -------------------------------------------------------------------------------------------------

# urlAct
H0://$host$:$httpport$/amp.dll?testLambda=urlAct&redirectTo=http://$host$:$httpport$/redir.htm
<HTTP><HEAD><TITLE>Analytic Information Server
# -------------------------------------------------------------------------------------------------

# CloseSession
H0://$host$:$httpport$/amp.dll?_ais=closesession&sessionid=$httpsid0$&mode=5
true
# -------------------------------------------------------------------------------------------------









