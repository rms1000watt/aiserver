package com.ecwise.ais;

public class AAppMessage {

    public static String AddUser = "_ais\177adduser\177username\177{1}\177password\177{2}\177securitylevel\177{3}\177enddate\177{4:MM/dd/yyyy}\177comment\177{5}";
    public static String CheckCabinet = "_ais\177checkcabinet\177cabname\177%s";
    public static String CloseCabinet = "_ais\177closecabinet\177cabname\177%s";
    public static String CloseConnection = "_ais\177closeconnection\177connectid\177%d\177closemode\177%s";
    public static String CloseContext = "_ais\177closecontext\177context\177%s\177closemode\177%s";
    public static String CloseSession = "_ais\177closesession\177sessionid\177%d\177closemode\177%s";
    public static String CompileAll = "_ais\177compileall";
    public static String CompileCabinet = "_ais\177compilecabinet\177extents\177";
    public static String CompileLambda = "_ais\177compilelambda\177extent_lambda\177";
    public static String ConnectionClosed = "_ais\177connectionclosed";
    public static String ConnectSession = "_ais\177connectsession\177sessionid\177%d";
    public static String Debug = "_ais\177debug\177exp\177{1}";
    public static String DeleteUser = "_ais\177deleteuser\177userid\177{1}";
    public static String EnableConsoleLog = "_ais\177enableconsolelog\177enable\177%d\177sessionid\177%d";
    public static String EraseNode = "_ais\177erasenode\177extent\177%s\177nodes\177";
    public static String Eval = "_ais\177eval\177exp\177%s";
    public static String Execute = "_ais\177execute\177exp\177%s";
    public static String ExportCabinet = "_ais\177exportcabinet\177cabinet\177%s\177%s\177%s\177setlocation\177%s";
    public static String ExportNode = "_ais\177exportnode\177node\177%s\177file\177%s\177cabinet\177%s";
    public static String GetConnectionStats = "_ais\177getconnectionstats";
    public static String GetConsoleLog = "_ais\177getconsolelog\177clear\177%d\177sessionid\177%d";
    public static String GetContextParams = "_ais\177getcontextparams\177context\177%s";
    public static String GetCurrentContexts = "_ais\177getcurrentcontexts";
    public static String GetDirInfo = "_ais\177getdirinfo\177dir\177%s";
    public static String GetExeSession = "_ais\177getexesession";
    public static String GetExtentNames = "_ais\177getextentnames";
    public static String GetExtentStatus = "_ais\177getextentstatus";
    public static String GetExtentTypes = "_ais\177getextenttypes\177extent\177%s";
    public static String GetLogonStats = "_ais\177getlogonstats";
    public static String GetMetaData = "_ais\177getmetadata\177extent\177%s";
    public static String GetNextLevel = "_ais\177getnextlevel\177extent\177%s\177nodePath\177%s\177options\177%s";
    public static String GetRequestStats = "_ais\177getrequeststats";
    public static String GetSessions = "_ais\177getsessions";
    public static String GetSessionsContext = "\177context\177%s";
    public static String GetSessionStats = "_ais\177getsessionstats";
    public static String GetSubscriptions = "_ais\177getsubscriptions";
    public static String GetSubscriptionsContext = "\177context\177%s";
    public static String GetUsers = "_ais\177getusers";
    public static String GetWorkspaceStatistics = "_ais\177getworkspacestatistics";
    public static String ImportCabinet = "_ais\177importcabinet\177cabinet\177%s\177%s\177%s\177setlocation\177%s";
    public static String IsContextOpen = "_ais\177iscontextopen\177context\177%s";
    public static String IsContextBusy = "_ais\177iscontextbusy\177context\177%s";
    public static String Logoff = "_ais\177logoff";
    public static String Logon = "_ais\177logon\177user\177%s\177passwd\177%s";
    public static String NewCabinet = "_ais\177newcabinet\177cabinet\177%s\177path\177%s\177location\177%s\177storage\177%s\177importsync\177%s\177exportsync\177%s\177autocompile\177%s\177searchsw\177%s\177dependencies\177%s";
    public static String OpenCabinet = "_ais\177opencabinet\177cabinet%s\177path\177%s\177location\177%s\177storage\177%s\177importsync\177%s\177exportsync\177%s\177autocompile\177%s\177searchsw\177%s\177dependencies\177%s";
    public static String OpenConnection = "_ais\177openconnection";
    public static String OpenConsoleLog = "_ais\177openconsolelog\177clear\177%s\177redirect\177%s\177sessionid\177%s\177size\177%d\177startatnewline\177%s";
    public static String OpenContext = "_ais\177opencontext";
    public static String OpenContextName = "\177context\177%s";
    public static String OpenContextPath = "\177path\177%s";
    public static String OpenNode = "_ais\177opennode\177extent\177%s\177node\177%s";
    public static String OpenSession = "_ais\177opensession\177context\177%s\177userid\177%d";
    public static String SaveNode = "_ais\177savenode\177extent\177%s\177node\177%s\177text\177%s";
    public static String SetBreakPoint = "_ais\177setbreakpoint\177agent\177{1}";
    public static String SetEngineFlags = "_ais\177setengineflags\177flags\177%d";
    public static String SetErrorTrace = "_ais\177seterrortrace\177onoff\177%d";
    public static String SetEscape = "_ais\177setescape\177sessionid\177%d";
    public static String SetInstructionTrace = "_ais\177setinstructiontrace\177onoff\177%d";
    public static String SetJit = "_ais\177setjit\177onoff\177%d";
    public static String SetLogLevel = "_ais\177setloglvl\177logtype\177%d\177level\177%d";
    public static String SetRules = "_ais\177setrules\177rules\177%s\177mode\177%d";
    public static String SetSubscriptions = "_ais\177setsubscriptions\177new\177%s\177old\177%s";
    public static String SetSysCheck = "_ais\177setsyscheck\177onoff\177%d";
    public static String ShowConsole = "_ais\177showconsole\177exp\177%s";
    public static String RegisterContext = "_ais\177registercontext\177path\177%s";
    public static String RegisterDirectory = "_ais\177registerdirectory\177databasedir\177%s\177sourcedir\177%s";
    public static String RunScriptFile = "_ais\177runscriptfile\177file\177%s";
    public static String RunScriptFilePrefix = "\177prefix\177%s";
    public static String UpdateMetaData = "_ais\177updatemetadata\177cabinet\177%s\177path\177%s\177location\177%s\177storage\177%s\177importsync\177%s\177exportsync\177%s\177autocompile\177%s\177searchsw\177%s\177dependencies\177%s";
    public static String UpdateUser = "_ais\177updateuser\177userid\177{1}\177username\177{2}\177password\177{3}\177securitylevel\177{4}\177enddate\177{5}\177comment\177{6}";

}
