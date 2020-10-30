# ais/testaisclient/testxmlamp.txt

# Tests of Amp built-in commands and protected files
# CHANGE HISTORY
# Version	Date		Who		Change
# 1.0116	11/26/2006	tlw		Change Main to TestAis
# 1.0039	7/12/2004	tlw		Add all built-in _ais tests.

# NOTES
#	 1.	See testsuite.txt for more info on constructing tests
#	 2. See axmlsvr.h for more info on XML requests and their responses.
# -------------------------------------------------------------------------------------------------

# Logon
X0://$host$:$xmlport$/_ais|logon|user|tmay|passwd|tim
&xmluserid=userid\|(\d+)
_ais|logon|userid|$xmluserid$
# -------------------------------------------------------------------------------------------------

# Probe
X0://$host$:$xmlport$/_ais|probe
&xmlconnectid=connectid\|(\d+)
_ais|noop|connectid|$xmlconnectid$
# -------------------------------------------------------------------------------------------------

# Noop
X0://$host$:$xmlport$/_ais|noop|arg0|value0|arg1|value1
_ais|noop|result|arg0=value0,arg1=value1,sessionid=
# -------------------------------------------------------------------------------------------------

# OpenSession
X0://$host$:$xmlport$/_ais|opensession|context|TestAis|userid|$xmluserid$
&xmlsid=result\|(\d+)
_ais|opensession|result|$xmlsid$
# -------------------------------------------------------------------------------------------------

# Eval noop
X0://$host$:$xmlport$/_ais|eval|exp|(noop)
_ais|eval|result|
# -------------------------------------------------------------------------------------------------

# Eval
X0://$host$:$xmlport$/_ais|eval|exp|(writeln {Hello from XML})
_ais|eval|result|true
# -------------------------------------------------------------------------------------------------

# OpenContext - PENDING????
# X0://$host$:$xmlport$/_ais|opencontext|path|test2/astartup2.sl
# _ais|opencontext|sessionid|
# -------------------------------------------------------------------------------------------------

# CloseCabinet
X0://$host$:$xmlport$/_ais|closecabinet|cabname|TestAis
_ais|closecabinet|result|true
# -------------------------------------------------------------------------------------------------

# OpenCabinet
X0://$host$:$xmlport$/_ais|opencabinet|cabinet|TestAis|path|cabinets/testais.db
_ais|opencabinet|result|true
# -------------------------------------------------------------------------------------------------

# CloseConnection - see below
# CloseSession - see below
# CloseContext
X0://$host$:$xmlport$/_ais|closecontext|context|Test2|mode|0
_ais|closecontext|error|Unknown or missing context name
# -------------------------------------------------------------------------------------------------

# CompileLambda
X0://$host$:$xmlport$/_ais|compilelambda|extent_Lambdas|TestAis	testFilter
_ais|compilelambda|result|true
# -------------------------------------------------------------------------------------------------

# CompileCabinet
X0://$host$:$xmlport$/_ais|compilecabinet|extents|TestAis
_ais|compilecabinet|result|true
# -------------------------------------------------------------------------------------------------

# OpenConsoleLog
X0://$host$:$xmlport$/_ais|openconsolelog|clear|true|redirect|true|sessionid|0|size|1000
_ais|openconsolelog|result|true
# -------------------------------------------------------------------------------------------------

# Console Output
X0://$host$:$xmlport$/_ais|eval|exp|(writeln {Console output})
_ais|eval|result|true
# -------------------------------------------------------------------------------------------------

# GetConsoleLog
X0://$host$:$xmlport$/_ais|getconsolelog|clear|true|sessionid|0|wait|true
_ais|getconsolelog|Display|Console output\n|Result|true
# -------------------------------------------------------------------------------------------------

# Console Output
X0://$host$:$xmlport$/_ais|eval|exp|(writeln {More console output.})
_ais|eval|result|true
# -------------------------------------------------------------------------------------------------

# DisconnectSession - obsolete
# X0://$host$:$xmlport$/_ais|disconnectsession
# _ais|disconnectsession|result|true
# -------------------------------------------------------------------------------------------------

# ConnectSession - obsolete
# X0://$host$:$xmlport$/_ais|connectsession|sessionid|$xmlsid$
# _ais|connectsession|result|true
# -------------------------------------------------------------------------------------------------

# EnableConsoleLog - Suspend log
X0://$host$:$xmlport$/_ais|enableconsolelog|enable|suspend|sessionid|0
_ais|enableconsolelog|result|true
# -------------------------------------------------------------------------------------------------

# GetConsoleLog
X0://$host$:$xmlport$/_ais|getconsolelog|clear|true|sessionid|0|wait|true
_ais|getconsolelog|error|No console output
# -------------------------------------------------------------------------------------------------

# EnableConsoleLog - close the log
X0://$host$:$xmlport$/_ais|enableconsolelog|enable|close|sessionid|0
_ais|enableconsolelog|result|true
# -------------------------------------------------------------------------------------------------

# EraseNode
X0://$host$:$xmlport$/_ais|erasenode|extent|TestAis|nodes|testErase
_ais|erasenode|result|true
# -------------------------------------------------------------------------------------------------

# SaveNode - Just temporary...
X0://$host$:$xmlport$/_ais|savenode|extent|TestAis|node|testErase|text|(defun testErase() (writeln {testing...}))
true
# -------------------------------------------------------------------------------------------------

# ExportCabinet
X0://$host$:$xmlport$/_ais|exportcabinet|cabinet|TestAis|file|cabinets/gen/Remains.sl
_ais|exportcabinet|result|true
# -------------------------------------------------------------------------------------------------

# ExportNode
X0://$host$:$xmlport$/_ais|exportnode|node|testErase|file|cabinets/gen/exportNode.sl|cabinet|TestAis
_ais|exportnode|result|true
# -------------------------------------------------------------------------------------------------

# GetContextId
X0://$host$:$xmlport$/_ais|getcontextid|context|TestAis
&xmlcontextid=result\|(\d+)
_ais|getcontextid|result|$xmlcontextid$
# -------------------------------------------------------------------------------------------------

# GetContextId
X0://$host$:$xmlport$/_ais|getcontextid|sessionid|$xmlsid$
_ais|getcontextid|result|$xmlcontextid$
# -------------------------------------------------------------------------------------------------

# GetContextParams
X0://$host$:$xmlport$/_ais|getcontextparams|contextid|$xmlcontextid$
_ais|getcontextparams|WorkDir|
# -------------------------------------------------------------------------------------------------

# GetContextParams
X0://$host$:$xmlport$/_ais|getcontextparams|context|TestAis
_ais|getcontextparams|WorkDir|
# -------------------------------------------------------------------------------------------------

# GetCurrentContexts
X0://$host$:$xmlport$/_ais|getcurrentcontexts
_ais|getcurrentcontexts|result|_SystemContext
# -------------------------------------------------------------------------------------------------

# GetDirInfo
X0://$host$:$xmlport$/_ais|getdirinfo|dir|cabinets
_ais|getdirinfo|result|R	
# -------------------------------------------------------------------------------------------------

# GetExeSession
X0://$host$:$xmlport$/_ais|getexesession|context|TestAis
&exesessionid=result\|(\d+)
_ais|getexesession|result|$exesessionid$
# -------------------------------------------------------------------------------------------------

# GetExtentNames
X0://$host$:$xmlport$/_ais|getextentnames
_ais|getextentnames|result|.Memory	TestAis
# -------------------------------------------------------------------------------------------------

# GetExtentTypes
X0://$host$:$xmlport$/_ais|getextenttypes|extent|TestAis
_ais|getextenttypes|result|.default.	edit
# -------------------------------------------------------------------------------------------------

# GetNextLevel
X0://$host$:$xmlport$/_ais|getnextlevel|extent|TestAis|node|testErase
_ais|getnextlevel|result|0	99999
# -------------------------------------------------------------------------------------------------

# GetSessionUser
X0://$host$:$xmlport$/_ais|getsessionuser
_ais|getsessionuser|result|$xmluserid$
# -------------------------------------------------------------------------------------------------

# GetSubscriptions
X0://$host$:$xmlport$/_ais|getsubscriptions|context|TestAis
_ais|getsubscriptions|result|TestAis	1
# -------------------------------------------------------------------------------------------------

# GetWorkspaceStatistics
X0://$host$:$xmlport$/_ais|getworkspacestatistics
_ais|getworkspacestatistics|result|Memory Block Count =
# -------------------------------------------------------------------------------------------------

# NewCabinet
X0://$host$:$xmlport$/_ais|newcabinet|cabinet|Remains|path|cabinets/gen/Remains.db
_ais|newcabinet|result|true
# -------------------------------------------------------------------------------------------------

# ImportCabinet
X0://$host$:$xmlport$/_ais|importcabinet|cabinet|Remains|file|cabinets/testais.sl
_ais|importcabinet|result|true
# -------------------------------------------------------------------------------------------------

# IsContextBusy
X0://$host$:$xmlport$/_ais|iscontextbusy|context|TestAis
_ais|iscontextbusy|result|
# -------------------------------------------------------------------------------------------------

# IsContextOpen
X0://$host$:$xmlport$/_ais|iscontextopen|context|TestAis
_ais|iscontextopen|result|true
# -------------------------------------------------------------------------------------------------

# LogSysMsg
X0://$host$:$xmlport$/_ais|logsysmsg|level|2|msg|System message from XML Client
_ais|logsysmsg|result|true
# -------------------------------------------------------------------------------------------------

# NewCabinet - see above
# Noop - see above
# OpenCabinet - see above
# OpenConsoleLog - see above
# OpenContext - see above
# OpenNode
X0://$host$:$xmlport$/_ais|opennode|extent|TestAis|nodename|veryBusy
_ais|opennode|result|
# -------------------------------------------------------------------------------------------------

# OpenSession
X0://$host$:$xmlport$/_ais|opensession|context|TestAis|userid|3
_ais|opensession|result|
# -------------------------------------------------------------------------------------------------

# RegisterContext
##X0://$host$:$xmlport$/_ais|registercontext|path|Test2/astartup.sl
##_ais|registercontext|result|true
# -------------------------------------------------------------------------------------------------

# RetFile
X0://$host$:$xmlport$/_ais|getfile|file|afile.xml
_ais|getfile|result|afile.xml
# -------------------------------------------------------------------------------------------------

# RunScriptFile
X0://$host$:$xmlport$/_ais|runscriptfile|file|cabinets/remotescriptfile.sl
_ais|runscriptfile|result|#<Lambda
# -------------------------------------------------------------------------------------------------

# SaveNode
X0://$host$:$xmlport$/_ais|savenode|extent|TestAis|node|testErase|text|(defun testErase() (writeln {testing...}))
_ais|savenode|result|true
# -------------------------------------------------------------------------------------------------

# SetBreakpoint
#X0://$host$:$xmlport$/_ais|setbreakpoint|Lambda|????
#_ais|setbreakpoint|result|true
# -------------------------------------------------------------------------------------------------

# SetEngineFlags
X0://$host$:$xmlport$/_ais|setengineflags|flags|0
_ais|setengineflags|result|true
# -------------------------------------------------------------------------------------------------

# SetErrorTrace
X0://$host$:$xmlport$/_ais|seterrortrace|onoff|0
_ais|seterrortrace|result|true
# -------------------------------------------------------------------------------------------------

# SetEscape
X0://$host$:$xmlport$/_ais|setescape|session|%d
_ais|setescap
# -------------------------------------------------------------------------------------------------

# SetJit
X0://$host$:$xmlport$/_ais|setjit|onoff|1
_ais|setjit|result|true
# -------------------------------------------------------------------------------------------------

# SetInstructionTrace
X0://$host$:$xmlport$/_ais|setinstructiontrace|onoff|0
_ais|setinstructiontrace|result|true
# -------------------------------------------------------------------------------------------------

# SetLogLvl
X0://$host$:$xmlport$/_ais|setloglvl|logtype|$logsysmsg$|level|$warnlvl$
_ais|setloglvl|result|true
# -------------------------------------------------------------------------------------------------

# SetRules - add rule
X0://$host$:$xmlport$/_ais|setrules|rules|TestAis.in_process|mode|0
_ais|setrules|result|1
# -------------------------------------------------------------------------------------------------

# SetRules - remove rule
X0://$host$:$xmlport$/_ais|setrules|rules|TestAis.in_process|mode|1
_ais|setrules|result|1
# -------------------------------------------------------------------------------------------------

# SetSubscriptions
X0://$host$:$xmlport$/_ais|setsubscriptions|new|1
_ais|setsubscriptions|result|1
# -------------------------------------------------------------------------------------------------

# SetSubscriptions
X0://$host$:$xmlport$/_ais|setsubscriptions|new||old|1
_ais|setsubscriptions|result|0
# -------------------------------------------------------------------------------------------------

# SetSysCheck
X0://$host$:$xmlport$/_ais|setsyscheck|onoff|0
_ais|setsyscheck|result|true
# -------------------------------------------------------------------------------------------------

# ShowConsole
X0://$host$:$xmlport$/_ais|showconsole|exp|(veryBusy 5)
_ais|showconsole|result|true
# -------------------------------------------------------------------------------------------------

#stop
Ignore the stuff below #stop
#end
