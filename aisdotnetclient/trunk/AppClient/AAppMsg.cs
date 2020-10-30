using System;
using System.Collections.Generic;
using System.Text;

namespace AppClient
{
    public class AAppMsg
    {
        public static string AddUser = "_ais{0}adduser{0}username{0}{1}{0}password{0}{2}{0}securitylevel{0}{3}{0}enddate{0}{4:MM/dd/yyyy}{0}comment{0}{5}";
        public static string CloseCabinet = "_ais{0}closecabinet{0}cabname{0}{1}";
        public static string CloseConnection = "_ais{0}closeconnection{0}connectid{0}{1}{0}closemode{0}{2}";
        public static string CloseContext = "_ais{0}closecontext{0}context{0}{1}{0}closemode{0}{2}";
        public static string CloseSession = "_ais{0}closesession{0}sessionid{0}{1}{0}closemode{0}{2}";
        public static string CompileAgent = "_ais{0}compileagent{0}extent_agents{0}";
        public static string CompileCabinet = "_ais{0}compilecabinet{0}extents{0}";
        public static string ConnectionClosed = "_ais{0}connectionclosed";
        public static string ConnectSession = "_ais{0}connectsession{0}sessionid{0}{1}";
        public static string Debug = "_ais{0}debug{0}exp{0}{1}";
        public static string DeleteUser = "_ais{0}deleteuser{0}userid{0}{1}";
        public static string EnableConsoleLog = "_ais{0}enableconsolelog{0}enable{0}{1}{0}sessionid{0}{2}";
        public static string EraseNode = "_ais{0}erasenode{0}extent{0}{1}{0}nodes{0}";
        public static string Eval = "_ais{0}eval{0}exp{0}{1}";
        public static string Execute = "_ais{0}execute{0}exp{0}{1}";
        public static string ExportCabinet = "_ais{0}exportcabinet{0}cabinet{0}{1}{0}file{0}{2}";
        public static string ExportNode = "_ais{0}exportnode{0}node{0}{1}{0}file{0}{2}{0}cabinet{0}{3}";
        public static string GetConnectionStats = "_ais{0}getconnectionstats";
        public static string GetConsoleLog = "_ais{0}getconsolelog{0}clear{0}{1}{0}sessionid{0}{2}";
        public static string GetContextParams = "_ais{0}getcontextparams{0}context{0}{1}";
        public static string GetCurrentContexts = "_ais{0}getcurrentcontexts";
        public static string GetDirInfo = "_ais{0}getdirinfo{0}dir{0}{1}";
        public static string GetExeSession = "_ais{0}getexesession";
        public static string GetExtentNames = "_ais{0}getextentnames";
        public static string GetExtentTypes = "_ais{0}getextenttypes{0}extent{0}{1}";
        public static string GetLogonStats = "_ais{0}getlogonstats";
        public static string GetNextLevel = "_ais{0}getnextlevel{0}extent{0}{1}{0}nodePath{0}{2}{0}options{0}{3}";
        public static string GetRequestStats = "_ais{0}getrequeststats";
        public static string GetSessions = "_ais{0}getsessions";
        public static string GetSessionsContext = "{0}context{0}{1}";
        public static string GetSessionStats = "_ais{0}getsessionstats";
        public static string GetSubscriptions = "_ais{0}getsubscriptions";
        public static string GetSubscriptionsContext = "{0}context{0}{1}";
        public static string GetUsers = "_ais{0}getusers";
        public static string GetWorkspaceStatistics = "_ais{0}getworkspacestatistics";
        public static string ImportCabinet = "_ais{0}importcabinet{0}cabinet{0}{1}{0}file{0}{2}";
        public static string IsContextOpen = "_ais{0}iscontextopen{0}context{0}{1}";
        public static string Logoff = "_ais{0}logoff";
        public static string Logon = "_ais{0}logon{0}user{0}{1}{0}passwd{0}{2}";
        public static string NewCabinet = "_ais{0}newcabinet{0}{1}{0}path{0}{2}";
        public static string OpenCabinet = "_ais{0}opencabinet{0}cabinet{0}{1}{0}path{0}{2}";
        public static string OpenConnection = "_ais{0}openconnection";
        public static string OpenConsoleLog = "_ais{0}openconsolelog{0}clear{0}{1}{0}redirect{0}{2}{0}sessionid{0}{3}{0}size{0}{4}{0}startatnewline{0}{5}";
        public static string OpenContext = "_ais{0}opencontext";
        public static string OpenContextName = "{0}context{0}{1}";
        public static string OpenContextPath = "{0}path{0}{1}";
        public static string OpenNode = "_ais{0}opennode{0}extent{0}{1}{0}node{0}{2}";
        public static string OpenSession = "_ais{0}opensession{0}context{0}{1}{0}userid{0}{2}";
        public static string SaveNode = "_ais{0}savenode{0}extent{0}{1}{0}node{0}{2}{0}text{0}{3}";
        public static string SetBreakPoint = "_ais{0}setbreakpoint{0}agent{0}{1}";
        public static string SetEngineFlags = "_ais{0}setengineflags{0}flags{0}{1}";
        public static string SetErrorTrace = "_ais{0}seterrortrace{0}onoff{0}{1}";
        public static string SetEscape = "_ais{0}setescape{0}sessionid{0}{1}";
        public static string SetInstructionTrace = "_ais{0}setinstructiontrace{0}onoff{0}{1}";
        public static string SetJit = "_ais{0}setjit{0}onoff{0}{1}";
        public static string SetLogLevel = "_ais{0}setloglvl{0}logtype{0}{1}{0}level{0}{2}";
        public static string SetRules = "_ais{0}setrules{0}rules{0}{1}";
        public static string SetRulesMode = "{0}mode{0}{1}";
        public static string SetSubscriptions = "_ais{0}setsubscriptions{0}new{0}{1}{0}old{0}{2}";
        public static string SetSysCheck = "_ais{0}setsyscheck{0}onoff{0}{1}";
        public static string ShowConsole = "_ais{0}showconsole{0}exp{0}{1}";
        public static string RegisterContext = "_ais{0}registercontext{0}path{0}{1}";
        public static string RunScriptFile = "_ais{0}runscriptfile{0}file{0}{1}";
        public static string RunScriptFilePrefix = "{0}prefix{0}{1}";
        public static string UpdateUser = "_ais{0}updateuser{0}userid{0}{1}{0}username{0}{2}{0}password{0}{3}{0}securitylevel{0}{4}{0}enddate{0}{5}{0}comment{0}{6}";
    }
}
