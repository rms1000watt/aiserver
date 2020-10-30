/**********************************************************************************
    Copyright (C) 2009 Investment Science Corp.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/
namespace AppClient
{
    /// <summary>
    /// AIS Request Types.
    /// </summary>
    public enum ARequestType
    {   Unknown,
        AmpMsg,
        CloseCabinet,
        CompileAgent,
        CompileCabinet,
        Debug,
        EraseNode,
        Eval,
        Execute,
        ExportNode,
        ExportCabinet,
        GetDirInfo,
        GetExtentNames,
        GetExtentTypes,
        GetNextLevel,
        GetWorkspaceStatistics,
        ImportCabinet,
        Logoff,
        Logon,
        NewCabinet,
        OpenNode,
        OpenCabinet,
        RunScriptFile,
        SaveNode,
        SetBreakpoint,
        SetLogLvl,
        ShowConsole,
        CloseConnection,
        CloseContext,
        CloseSession,
        ConnectSession,
        Display,
        EnableConsoleLog,
        GetConsoleLog,
        GetContextId,
        GetContextParams,
        GetCurrentContexts,
        GetExeSession,
        GetSessions,
        GetSessionId,
        GetSessionUser,
        GetSubscriptions,
        IsContextBusy,
        IsContextOpen,
        Noop,
        OpenConnection,
        OpenConsoleLog,
        OpenContext,
        OpenSession,
        RegisterContext,
        SetEngineFlags,
        SetErrorTrace,
        SetEscape,
        SetInstructionTrace,
        SetJit,
        SetRules,
        SetSubscriptions,
        SetSysCheck,

        // Additional output function types. Never a request, only used in output dispatch.
        FcnDebug,
        FcnError,
        FcnFileOpen,
        FcnReadHtmlPage,
        FcnReturnResult,
        FcnRingBell,
        FcnEngineState,
        FcnSendToClient,

        // Additional log types. Never a request, only used in output dispatch.
        LogAll,
        LogAmp,
        LogConsole,
        LogReqHdr,
        LogSysMsg,
        LogUserAccess,
        LogStatus,

        // Outputs for various returned enctypes
        RetFile,
        RetQuery,
        RetText,
        RetUrl,

        // Local events posted in client. Never requested or returned from AIS.
        ProcCodeLines,
        ProcDebugLineEdit,
        ProcInfoLines,
        ProcSetDebuggerActive,
        ConnectionError,

        // User Management Request Types
        AddUser,
        UpdateUser,
        DeleteUser,
        GetUsers,

        // System Resouce Monitor Request Types
        GetConnectionStats,
        GetLogonStats,
        GetSessionStats,
        GetRequestStats,
        ReqTypeSz
    }

    /// <summary>
    /// AIS Session/Connection Close Modes.
    /// </summary>
    public enum ACloseMode
    {  Default,
        Disconnect,
        Open,
        Soft,
        Firm,
        Hard,
        End
    }

    /// <summary>
    /// AIS Error Levels.
    /// </summary>
    public enum AErrorLevel
    {    None,
        Info,
        Warn,
        Software,
        Critical,
        Fatal
    }
    /// <summary>
    /// AIS Error Codes.
    /// </summary>
    public enum AErrorCodes
    {   Empty,
        Generic,
        EvalFail,
        FileRead,
        FileWrite,
        OutMemory,
        FrameOvflw,
        DamagedMem,
        StackOvflw,
        UsrEsc,
        Pcode,
        DamagedObject,
        RecursionLimit,
        FrameRelease,
        StackRelease,
        RecursionRel,
        UsrQuit,
        FileVersion,
        EngineBusy,
        Repository,
        Unexpected,
        NotImplemented,
        System,
        Access,
        CheckIn,
        Filter,
        Score,
        SendToClient,
        UnkContextName,
        ContextIsOpen,
        ContextOpen,
        InactiveSession,
        SessionId,
        ReqPending,
        SessionClosed,
        Security,
        SessionOpen,
        InactiveContext,
        LoggedOn,
        UnkUsrName,
        BadUsrId,
        UsrMismatch,
        AcctDisabled,
        BadPasswd,
        AcctExpired,
        NoLogon,
        ParseAmp,
        FewArgs,
        UnkAct,
        Disconnected,
        EmptyArg,
        NoExtent,
        NoContext,
        NoPair,
        OutOfRange,
        UnkConnectId,
        UnkMethod,
        NoCookie,
        NoArg,
        BfrOvflw,
        BadHttp,
        BadXml,
        BadHttpReq,
        CookieName,
        Timeout,
        IpMismatch,
        ConnectId,
        LostConnection,
        NoServer,
        NoClient,
        ContextId,
        LostCookie,
        BadAmp,
        BadRequest,
        Connected,
        UnkHost,
        NotRegistered,
        BadCabinet,
        BadNode,
        BadAgent,
        NoInput,
        TcpIpError,
        AtBeg,
        AtEnd,
        CabinetOpen,
        BadFilename,
        InProcess,
        ServerOpen,
        BadContext,
        NoConsoleLog,
        BfrTruncate,
        BfrSeek,
        BadArg,
        PortInUse,
        CookieIp,
        StaleCookie,
        CookieMismatch,
        SystemContext,
        UnkProtocol,
        Unterminated,
        UsrNameExists,
        MaxUsrReached,
        BadSecLvl,
        PwdOpenFail,
        PwdWriteFail,
        ErrCodesSz
    }
}