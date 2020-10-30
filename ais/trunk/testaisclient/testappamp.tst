# ais/testaisclient/testappamp.txt

# Tests of App protocol built-in commands and protected file.

# CHANGE HISTORY
# Version	Date		Who		Change
# 1.0116	11/26/2006	tlw		Change Main to TestAis
# 1.0105	9/11/2006	tlw		Add setCurrentExtent and getExtentNames to accomodate AppClient.
# 1.0057	3/11/2005	tlw		Add built-in _ais tests.

# REQUESTS
# The submitted request is of the following form
#	%d|%d|!context|%s|%s...\01
# where first %d is the length of the request, %s..., not including the terminator (null or ^A).
# The header, %d|%d|!context|%s|, is automatically prepended to every request.  The request is
# everything past the last slash in:  A0://hostname/!%s/request in the test suite.
# If the !%s is not included in the test, the !context|%s| in the header is omitted.
# The request is of the form: targetLambda|speechAct|someArg|someArgValue...

# RESPONSE
# The raw response from the server is of the following form:
#	%d|%d|%s|%d|%d|%s|%s|%s
# The above fields are (in order):
#	Length	- total bytes transmitted not including the terminator (null or ^A) on end
#	RqId	- incrementing integer that serves as a unique identifier for request/response pairs.
#	ReqType	- request type which is ampmsg, eval, or the name of a built-in speech act
#	Status	- 0 if OK, >0 if an error
#	RetValue- returned integer value or 0 if no value returned by this request.
#	Display	- returned console output from a request submitted to the engine
#	Error	- error details if any.
#	Out		- returned result which may contain a sequence of DEL-delimited name-value pairs
# The Length and Xid are stripped off the response before it is compared with the expected response.
# Thus the fields in the expected response are:
#		ReqType|Status|RetValue|Display|Error|Out

# NOTES
#	 1.	See testsuite.txt for more info on constructing tests.

# -------------------------------------------------------------------------------------------------

# Logon
A0://$host$:$appport$/_ais|logon|user|guest|passwd|
&appusrid=userid\|(\d+)
logon|0|$appusrid$|||userid|$appusrid$
# -------------------------------------------------------------------------------------------------

# Probe
A0://$host$:$appport$/_ais
&appconnectid=connectid\|(\d+)
noop|0|0|||connectid|$appconnectid$
# -------------------------------------------------------------------------------------------------

# Noop
A0://$host$:$appport$/_ais|noop|arg0|value0|arg1|value1
noop|0|0|||arg0=value0,arg1=value1,sessionid=
# -------------------------------------------------------------------------------------------------

# GetExtentNames
A0://$host$:$appport$/_ais|getextentnames
getextentnames|0|0|||.Memory	
# -------------------------------------------------------------------------------------------------

# SetCurrentExtent
A0://$host$:$appport$/_ais|setcurrentextent|extent|TestAis
setcurrentextent|TestAis
# -------------------------------------------------------------------------------------------------

# OpenSession
A0://$host$:$appport$/_ais|opensession|context|TestAis
&appsid=0\|(\d+)
opensession|0|$appsid$|||$appsid$
# -------------------------------------------------------------------------------------------------

# Eval noop
A0://$host$:$appport$/_ais|eval|exp|(noop)
eval|0|0|||
# -------------------------------------------------------------------------------------------------

# Eval
A0://$host$:$appport$/_ais|eval|exp|(display {Hello from AIS})
eval|0|0|Hello from AIS||true
# -------------------------------------------------------------------------------------------------

# OpenContext -  to be determined
# A0://$host$:$appport$/_ais|opencontext|path|test2/astartup2.sl
# _ais|opencontext|sessionid|
# -------------------------------------------------------------------------------------------------

# CloseCabinet - to be determined
#A0://$host$:$appport$/_ais|closecabinet|cabname|TestAis
#_ais|closecabinet|result|true
# -------------------------------------------------------------------------------------------------

# OpenCabinet - to be determined
#A0://$host$:$appport$/_ais|opencabinet|cabinet|TestAis|path|cabinets/testais.db
#_ais|opencabinet|result|true
# -------------------------------------------------------------------------------------------------

# CloseConnection - see below
# CloseSession - see below
# CloseContext - to be determined
#A0://$host$:$appport$/_ais|closecontext|context|Test2|mode|0
#_ais|closecontext|error|Unknown or missing context name
# -------------------------------------------------------------------------------------------------

# CompileLambda
A0://$host$:$appport$/_ais|compilelambda|extent_Lambdas|TestAis	testFilter
compilelambda|0|0|||true
# -------------------------------------------------------------------------------------------------

# CompileCabinet
A0://$host$:$appport$/_ais|compilecabinet|extents|TestAis
compilecabinet|0|0|||true
# -------------------------------------------------------------------------------------------------

# OpenConsoleLog 
A0://$host$:$appport$/_ais|openconsolelog|clear|true|redirect|true|size|1000
openconsolelog|0|0|||true
# -------------------------------------------------------------------------------------------------

# Console Output
A0://$host$:$appport$/_ais|eval|exp|(display "Some console output.")
eval|0|0|||true
# -------------------------------------------------------------------------------------------------

# getConsoleLog
# Record Format ( V represents vertical tab \\013, Type is R (result) or D (display)):
#	VLength|Type|RqId|SessionId|Body
# Notes:
#	Record lengths can vary depending upon the length of RqId and sessionid.
#	See RESPONSE section above for fields preceding the record.
A0://$host$:$appport$/_ais|getconsolelog|clear|true
&retvalue=getconsolelog\|0\|(\d+)\|
&loghdr=(\d+\|D\|\d+\|\d+)
getconsolelog|0|$retvalue$|||$loghdr$|Some console output.
# -------------------------------------------------------------------------------------------------

# Console Output
A0://$host$:$appport$/_ais|eval|exp|(display " More console output.")
eval|0|0|||true
# -------------------------------------------------------------------------------------------------

# DisconnectSession - obsolete
# ConnectSession - obsolete
# -------------------------------------------------------------------------------------------------

# EnableConsoleLog - Suspend log
A0://$host$:$appport$/_ais|enableconsolelog|enable|suspend|sessionid|0
enableconsolelog|0|0|||true
# -------------------------------------------------------------------------------------------------

# getConsoleLog
A0://$host$:$appport$/_ais|getconsolelog|clear|true
getconsolelog|89|0||No console output buffer enabled|false
# -------------------------------------------------------------------------------------------------

# EnableConsoleLog - close the log
A0://$host$:$appport$/_ais|enableconsolelog|enable|close
enableconsolelog|0|0|||true
# -------------------------------------------------------------------------------------------------

# EraseNode
A0://$host$:$appport$/_ais|erasenode|nodes|testErase
erasenode|0|0|||true
# -------------------------------------------------------------------------------------------------

# SaveNode - Just temporary...
A0://$host$:$appport$/_ais|savenode|extent|TestAis|node|testErase|text|(defun testErase() (writeln "testing..."))
savenode|0|0|||true
# -------------------------------------------------------------------------------------------------

# ExportCabinet
A0://$host$:$appport$/_ais|exportcabinet|cabinet|TestAis|file|cabinets/gen/Remains.sl
exportcabinet|0|0|||true
# -------------------------------------------------------------------------------------------------

# ExportNode
A0://$host$:$appport$/_ais|exportnode|node|testErase|file|cabinets/gen/exportNode.sl|cabinet|TestAis
exportnode|0|0|||true
# -------------------------------------------------------------------------------------------------

# GetContextId
A0://$host$:$appport$/_ais|getcontextid|context|TestAis
&appcontextid=\|(\d+)$
getcontextid|0|$appcontextid$|||$appcontextid$
# -------------------------------------------------------------------------------------------------

# GetContextId
A0://$host$:$appport$/_ais|getcontextid|sessionid|$appsid$
getcontextid|0|$appcontextid$|||$appcontextid$
# -------------------------------------------------------------------------------------------------

# GetContextParams
A0://$host$:$appport$/_ais|getcontextparams|contextid|$appcontextid$
&nparams=0\|(\d+)
getcontextparams|0|$nparams$|||WorkDir|
# -------------------------------------------------------------------------------------------------

# GetContextParams
A0://$host$:$appport$/_ais|getcontextparams|context|TestAis
getcontextparams|0|$nparams$|||WorkDir|
# -------------------------------------------------------------------------------------------------

# GetCurrentContexts
A0://$host$:$appport$/_ais|getcurrentcontexts
getcurrentcontexts|0|0|||_SystemContext
# -------------------------------------------------------------------------------------------------

# GetDirInfo
A0://$host$:$appport$/_ais|getdirinfo|dir|cabinets
getdirinfo|0|0|||R	
# -------------------------------------------------------------------------------------------------

# GetExeSession
A0://$host$:$appport$/_ais|getexesession
&exesessionid=0\|(\d+)
getexesession|0|$exesessionid$|||$exesessionid$
# -------------------------------------------------------------------------------------------------

# GetExtentTypes
A0://$host$:$appport$/_ais|getextenttypes|extent|TestAis
getextenttypes|0|0|||.default.	edit
# -------------------------------------------------------------------------------------------------

# GetNextLevel
A0://$host$:$appport$/_ais|getnextlevel|node|testErase
getnextlevel|0|0|||0	99999
# -------------------------------------------------------------------------------------------------

# GetSessionUser
A0://$host$:$appport$/_ais|getsessionuser
getsessionuser|0|$appusrid$|||$appusrid$
# -------------------------------------------------------------------------------------------------

# GetSubscriptions
A0://$host$:$appport$/_ais|getsubscriptions|context|TestAis
&nsubscriptions=0\|(\d+)
getsubscriptions|0|$nsubscriptions$|||TestAis	1
# -------------------------------------------------------------------------------------------------

# GetWorkspaceStatistics
A0://$host$:$appport$/_ais|getworkspacestatistics
getworkspacestatistics|0|0|||Workspace Statistics\nMemory Block Count =
# -------------------------------------------------------------------------------------------------

# NewCabinet
A0://$host$:$appport$/_ais|newcabinet|cabinet|Remains|path|cabinets/gen/Remains.db
newcabinet|0|0|||true
# -------------------------------------------------------------------------------------------------

# GetExtentNames - Required in order for appclient to see the new cabinet.
A0://$host$:$appport$/_ais|getextentnames
getextentnames|0|0|||.Memory	
# -------------------------------------------------------------------------------------------------

# ImportCabinet
A0://$host$:$appport$/_ais|importcabinet|cabinet|Remains|path|cabinets/testais.sl
importcabinet|0|0|[
# -------------------------------------------------------------------------------------------------

# CloseCabinet
A0://$host$:$appport$/_ais|closecabinet|cabname|Remains
closecabinet|0|0|||true
# -------------------------------------------------------------------------------------------------

# IsContextBusy
A0://$host$:$appport$/_ais|iscontextbusy|context|TestAis
&sessionid=\|0\|(\d+)
iscontextbusy|0|$sessionid$|||$sessionid$
# -------------------------------------------------------------------------------------------------

# IsContextOpen
A0://$host$:$appport$/_ais|iscontextopen|context|TestAis
iscontextopen|0|$appcontextid$|||true
# -------------------------------------------------------------------------------------------------

# LogSysMsg
A0://$host$:$appport$/_ais|logsysmsg|level|2|msg|System message from APP Client
logsysmsg|0|0|||true
# -------------------------------------------------------------------------------------------------

# NewCabinet - see above
# Noop - see above
# OpenCabinet - see above
# OpenConsoleLog - see above
# OpenContext - see above
# OpenNode
A0://$host$:$appport$/_ais|opennode|extent|TestAis|nodename|veryBusy
opennode|0|0|||
# -------------------------------------------------------------------------------------------------

# OpenSession - see above

# RegisterContext - to be determined
##A0://$host$:$appport$/_ais|registercontext|path|Test2/astartup.sl
##registercontext|0|0|||true
# -------------------------------------------------------------------------------------------------

# RetFile
A0://$host$:$appport$/_ais|retfile|file|afile.xml
retfile|0|0|||<amp
# -------------------------------------------------------------------------------------------------

# RunScriptFile
A0://$host$:$appport$/_ais|runscriptfile|file|cabinets/remotescriptfile.sl
runscriptfile|0|0|Hello from RemoteScriptFile.sl||#<Lambda
# -------------------------------------------------------------------------------------------------

# SaveNode
A0://$host$:$appport$/_ais|savenode|extent|TestAis|node|testErase|text|(defun testErase() (writeln "testing..."))
savenode|0|0|||true
# -------------------------------------------------------------------------------------------------

# SetBreakpoint
#A0://$host$:$appport$/_ais|setbreakpoint|Lambda|????
#setbreakpoint|0|0|||true
# -------------------------------------------------------------------------------------------------

# SetEngineFlags
A0://$host$:$appport$/_ais|setengineflags|flags|0
setengineflags|0|0|||true
# -------------------------------------------------------------------------------------------------

# SetErrorTrace
A0://$host$:$appport$/_ais|seterrortrace|onoff|0
seterrortrace|0|0|||true
# -------------------------------------------------------------------------------------------------

# SetEscape - Pending
#A0://$host$:$appport$/_ais|setescape|sessionid|0
#setescape|0|0|||
# -------------------------------------------------------------------------------------------------

# SetJit
A0://$host$:$appport$/_ais|setjit|onoff|1
setjit|0|1|||true
# -------------------------------------------------------------------------------------------------

# SetInstructionTrace
A0://$host$:$appport$/_ais|setinstructiontrace|onoff|0
setinstructiontrace|0|0|||true
# -------------------------------------------------------------------------------------------------

# SetLogLvl
A0://$host$:$appport$/_ais|setloglvl|logtype|$logsysmsg$|level|$warnlvl$
setloglvl|0|0|||true
# -------------------------------------------------------------------------------------------------

# SetRules - add rule
A0://$host$:$appport$/_ais|setrules|rules|TestAis.in_process|remove|0
setrules|0|1|||1
# -------------------------------------------------------------------------------------------------

# SetRules - remove rule
A0://$host$:$appport$/_ais|setrules|rules|TestAis.in_process|remove|1
setrules|0|1|||1
# -------------------------------------------------------------------------------------------------

# SetSubscriptions
A0://$host$:$appport$/_ais|setsubscriptions|new|1|old|
setsubscriptions|0|1|||1
# -------------------------------------------------------------------------------------------------

# SetSubscriptions
A0://$host$:$appport$/_ais|setsubscriptions|new||old|1
setsubscriptions|0|0|||0
# -------------------------------------------------------------------------------------------------

# SetSysCheck
A0://$host$:$appport$/_ais|setsyscheck|onoff|0
setsyscheck|0|0|||true
# -------------------------------------------------------------------------------------------------

# ShowConsole
A0://$host$:$appport$/_ais|showconsole|exp|_path
showconsole|0|0|
# -------------------------------------------------------------------------------------------------

# CloseSession
A0://$host$:$appport$/_ais|closesession|sessionid|$appsid$|mode|5
closesession|0|0|||true
# -------------------------------------------------------------------------------------------------

# Logoff - PENDING
## A0://$host$:$appport$/_ais|logoff
## logoff|0|$appusrid$|||$appusrid$
# -------------------------------------------------------------------------------------------------

# CloseConnection  - to be determined
##A0://$host$:$appport$/_ais|closeconnection
## closeconnection|0|0|||true
# -------------------------------------------------------------------------------------------------

#stop
end




