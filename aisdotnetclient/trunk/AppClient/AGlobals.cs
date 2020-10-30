using System;
using System.Collections.Generic;
using System.Text;

namespace AppClient
{
    /// <summary>
    /// Container class for global variables and settings.
    /// </summary>
    public class AGlobals
    {
        private static AGlobals _Singleton = null;

        /// <summary>
        /// Creates, if not existing, and returns the singleton instance of AGlobals.
        /// </summary>
        /// <returns>The singleton instance.</returns>
        public static AGlobals GetSingleton()
        {
            if (_Singleton == null)
                _Singleton = new AGlobals();

            return _Singleton;
        }

        /// <summary>
        /// Default constructor.
        /// </summary>
        public AGlobals()
        {
            InitCloseModes();
            InitErrorLevels();
            InitErrorMessages();
            InitRequestTypes();
        }

        /// <summary>
        /// Initialize close mode name mappings.
        /// </summary>
        private void InitCloseModes()
        {
            ACloseMode[] aAllCloseModes = (ACloseMode[])Enum.GetValues(typeof(ACloseMode));
            int aSz = aAllCloseModes.Length;
            string aKey = string.Empty;

            _CloseModeNames = new List<string>(aSz);
            _CloseModes = new Dictionary<string, ACloseMode>(aSz);

            for (int i = 0; i < aSz; i++)
            {
                aKey = Enum.GetName(typeof(ACloseMode), aAllCloseModes[i]).ToLower();
                _CloseModeNames.Insert(i, aKey);
                _CloseModes.Add(aKey, aAllCloseModes[i]);
            }
        }

        /// <summary>
        /// Initialize error level name mapppings.
        /// </summary>
        private void InitErrorLevels()
        {
            AErrorLevel[] aAllErrorLevels = (AErrorLevel[])Enum.GetValues(typeof(AErrorLevel));
            int aSz = aAllErrorLevels.Length;
            string aKey = string.Empty;

            _ErrorLevels = new List<string>(aSz);

            for (int i = 0; i < aSz; i++)
            {
                aKey = Enum.GetName(typeof(AErrorLevel), aAllErrorLevels[i]);
                _ErrorLevels.Insert(i, aKey);
            }
        }

        /// <summary>
        /// Initialize error message mappings.
        /// </summary>
        private void InitErrorMessages()
        {
            _ErrorMessages = new Dictionary<int, string>();
            
            _ErrorMessages[(int)AErrorCodes.Empty] = "";
            _ErrorMessages[(int)AErrorCodes.Generic] = "Unspecified error";
            _ErrorMessages[(int)AErrorCodes.EvalFail] = "!evalFailure!";
            _ErrorMessages[(int)AErrorCodes.FileRead] = "!errReadIO!";
            _ErrorMessages[(int)AErrorCodes.FileWrite] = "!errWriteIO!";
            _ErrorMessages[(int)AErrorCodes.OutMemory] = "!outOfMemory!";
            _ErrorMessages[(int)AErrorCodes.FrameOvflw] = "!gcFrameOverflw!";
            _ErrorMessages[(int)AErrorCodes.DamagedMem] = "!damageMemoryHD!";
            _ErrorMessages[(int)AErrorCodes.StackOvflw] = "!stackOverflw!";
            _ErrorMessages[(int)AErrorCodes.UsrEsc] = "!userEscape!";
            _ErrorMessages[(int)AErrorCodes.Pcode] = "!invalidPcode!";
            _ErrorMessages[(int)AErrorCodes.DamagedObject] = "!damagedObject!";
            _ErrorMessages[(int)AErrorCodes.RecursionLimit] = "!recursionLimit!";
            _ErrorMessages[(int)AErrorCodes.FrameRelease] = "!gcFrameRelease!";
            _ErrorMessages[(int)AErrorCodes.StackRelease] = "!stackRelease!";
            _ErrorMessages[(int)AErrorCodes.RecursionRel] = "!recursionRelease!";
            _ErrorMessages[(int)AErrorCodes.UsrQuit] = "!userQuit!";
            _ErrorMessages[(int)AErrorCodes.FileVersion] = "!diskFileVersion!";
            _ErrorMessages[(int)AErrorCodes.EngineBusy] = "!engineBusy!";
            _ErrorMessages[(int)AErrorCodes.Repository] = "!repositoryGC!";
            _ErrorMessages[(int)AErrorCodes.Unexpected] = "!unexpectedError!";
            _ErrorMessages[(int)AErrorCodes.NotImplemented] = "!notImplemented!";
            _ErrorMessages[(int)AErrorCodes.System] = "!systemError!";
            _ErrorMessages[(int)AErrorCodes.Access] = "!access denied!";
            _ErrorMessages[(int)AErrorCodes.CheckIn] = "!bad checkin argument!";
            _ErrorMessages[(int)AErrorCodes.Filter] = "!bad filter argument!";
            _ErrorMessages[(int)AErrorCodes.Score] = "!bad score argument!";
            _ErrorMessages[(int)AErrorCodes.SendToClient] = "!sendToClientFailure!";
            _ErrorMessages[(int)AErrorCodes.UnkContextName] = "Unknown or missing context name";
            _ErrorMessages[(int)AErrorCodes.ContextIsOpen] = "Context already open";
            _ErrorMessages[(int)AErrorCodes.ContextOpen] = "Open context fails";
            _ErrorMessages[(int)AErrorCodes.InactiveSession] = "Session not active";
            _ErrorMessages[(int)AErrorCodes.SessionId] = "Invalid session ID";
            _ErrorMessages[(int)AErrorCodes.ReqPending] = "Pending request has not completed";
            _ErrorMessages[(int)AErrorCodes.SessionClosed] = "Session is closed. Request denied.";
            _ErrorMessages[(int)AErrorCodes.Security] = "Access denined";
            _ErrorMessages[(int)AErrorCodes.SessionOpen] = "Session is already open";
            _ErrorMessages[(int)AErrorCodes.InactiveContext] = "Context not active";
            _ErrorMessages[(int)AErrorCodes.LoggedOn] = "User already logged on";
            _ErrorMessages[(int)AErrorCodes.UnkUsrName] = "Unknown or missing user name";
            _ErrorMessages[(int)AErrorCodes.BadUsrId] = "Invalid user ID";
            _ErrorMessages[(int)AErrorCodes.UsrMismatch] = "Usr Name mismatch";
            _ErrorMessages[(int)AErrorCodes.AcctDisabled] = "Account disabled";
            _ErrorMessages[(int)AErrorCodes.BadPasswd] = "Invalid user name or password";
            _ErrorMessages[(int)AErrorCodes.AcctExpired] = "Account has expired";
            _ErrorMessages[(int)AErrorCodes.NoLogon] = "No one logged on";
            _ErrorMessages[(int)AErrorCodes.ParseAmp] = "Unable to parse AMP input";
            _ErrorMessages[(int)AErrorCodes.FewArgs] = "Too few input args";
            _ErrorMessages[(int)AErrorCodes.UnkAct] = "Unsupported request";
            _ErrorMessages[(int)AErrorCodes.Disconnected] = "Not currently connected";
            _ErrorMessages[(int)AErrorCodes.EmptyArg] = "Empty input arg";
            _ErrorMessages[(int)AErrorCodes.NoExtent] = "No extent selected";
            _ErrorMessages[(int)AErrorCodes.NoContext] = "No context established for this connection";
            _ErrorMessages[(int)AErrorCodes.NoPair] = "Expected a name-value pair following speech act";
            _ErrorMessages[(int)AErrorCodes.OutOfRange] = "Error code out of range";
            _ErrorMessages[(int)AErrorCodes.UnkConnectId] = "Unknown connection";
            _ErrorMessages[(int)AErrorCodes.UnkMethod] = "Unknown HTTP method";
            _ErrorMessages[(int)AErrorCodes.NoCookie] = "No cookie returned by client";
            _ErrorMessages[(int)AErrorCodes.NoArg] = "Missing required argument";
            _ErrorMessages[(int)AErrorCodes.BfrOvflw] = "Input buffer overflow";
            _ErrorMessages[(int)AErrorCodes.BadHttp] = "Garbled HTTP request";
            _ErrorMessages[(int)AErrorCodes.BadXml] = "Garbled XML request";
            _ErrorMessages[(int)AErrorCodes.BadHttpReq] = "Incomplete HTTP request";
            _ErrorMessages[(int)AErrorCodes.CookieName] = "Unknown cookie name (expected abaseid)";
            _ErrorMessages[(int)AErrorCodes.Timeout] = "Wait period expired, operation aborted";
            _ErrorMessages[(int)AErrorCodes.IpMismatch] = "Client IP address does not match original";
            _ErrorMessages[(int)AErrorCodes.ConnectId] = "Invalid connection ID";
            _ErrorMessages[(int)AErrorCodes.LostConnection] = "Connection closed. Unable to return response";
            _ErrorMessages[(int)AErrorCodes.NoServer] = "No protocol server for this connection";
            _ErrorMessages[(int)AErrorCodes.NoClient] = "Unable to locate module that initiated this request";
            _ErrorMessages[(int)AErrorCodes.ContextId] = "Invalid context ID";
            _ErrorMessages[(int)AErrorCodes.LostCookie] = "Once valid cookie missing or invalid";
            _ErrorMessages[(int)AErrorCodes.BadAmp] = "Ill-formed AMP msg";
            _ErrorMessages[(int)AErrorCodes.BadRequest] = "Unexpected request returned";
            _ErrorMessages[(int)AErrorCodes.Connected] = "Already connected";
            _ErrorMessages[(int)AErrorCodes.UnkHost] = "Unknown DNS name, IP address or port";
            _ErrorMessages[(int)AErrorCodes.NotRegistered] = "Context is not registered";
            _ErrorMessages[(int)AErrorCodes.BadCabinet] = "Invalid or missing cabinet specification";
            _ErrorMessages[(int)AErrorCodes.BadNode] = "Invalid or missing node specification";
            _ErrorMessages[(int)AErrorCodes.BadAgent] = "Invalid or missing agent name";
            _ErrorMessages[(int)AErrorCodes.NoInput] = "Missing or invalid input";
            _ErrorMessages[(int)AErrorCodes.TcpIpError] = "TCP/IP Socket error";
            _ErrorMessages[(int)AErrorCodes.AtBeg] = "Already at beginning";
            _ErrorMessages[(int)AErrorCodes.AtEnd] = "Already at end";
            _ErrorMessages[(int)AErrorCodes.CabinetOpen] = "Cabinet is already open";
            _ErrorMessages[(int)AErrorCodes.BadFilename] = "Invalid or missing file specification";
            _ErrorMessages[(int)AErrorCodes.InProcess] = "Operation not supported on in-process client";
            _ErrorMessages[(int)AErrorCodes.ServerOpen] = "Server connection already established";
            _ErrorMessages[(int)AErrorCodes.BadContext] = "Request not supported on this context";
            _ErrorMessages[(int)AErrorCodes.NoConsoleLog] = "No console output buffer enabled";
            _ErrorMessages[(int)AErrorCodes.BfrTruncate] = "Truncate buffer fails";
            _ErrorMessages[(int)AErrorCodes.BfrSeek] = "Move position in buffer fails";
            _ErrorMessages[(int)AErrorCodes.BadArg] = "Argument value is out-of-range";
            _ErrorMessages[(int)AErrorCodes.PortInUse] = "Port is in-use by another protocol";
            _ErrorMessages[(int)AErrorCodes.CookieIp] = "Client's IP address and cookie's IP do not match";
            _ErrorMessages[(int)AErrorCodes.StaleCookie] = "Cookie key is outdated or corrupted";
            _ErrorMessages[(int)AErrorCodes.CookieMismatch] = "Cookie key does not match key for this socket";
            _ErrorMessages[(int)AErrorCodes.SystemContext] = "Unsupported operation on _SystemContext";
            _ErrorMessages[(int)AErrorCodes.UnkProtocol] = "Unexpected protocol on this port";
            _ErrorMessages[(int)AErrorCodes.Unterminated] = "Unterminated message";
            _ErrorMessages[(int)AErrorCodes.UsrNameExists] = "Username already exists";
            _ErrorMessages[(int)AErrorCodes.MaxUsrReached] = "Maximum no. of users reached";
            _ErrorMessages[(int)AErrorCodes.BadSecLvl] = "Invalid security level";
            _ErrorMessages[(int)AErrorCodes.PwdOpenFail] = "Failed in opening of password file";
            _ErrorMessages[(int)AErrorCodes.PwdWriteFail] = "Failed in writing to password file";
        }

        /// <summary>
        /// Initialize request types mapping.
        /// </summary>
        private void InitRequestTypes()
        {
            ARequestType[] aAllReqTypes = (ARequestType[])Enum.GetValues(typeof(ARequestType));
            int aSz = aAllReqTypes.Length;
            string aKey = string.Empty;

            _RequestNames = new List<string>(aSz);
            _RequestTypes = new Dictionary<string, ARequestType>();

            for (int i = 0; i < aSz; i++)
            {
                aKey = Enum.GetName(typeof(ARequestType), aAllReqTypes[i]).ToLower();
                _RequestNames.Insert(i, aKey);
                _RequestTypes.Add(aKey, aAllReqTypes[i]);
            }
        }

        /// <summary>
        /// String-Value Close Mode dictionary.
        /// </summary>
        public Dictionary<string, ACloseMode> CloseModes
        {
            get { return _CloseModes; }
        }

        /// <summary>
        /// Value-String Close Mode list.
        /// </summary>
        public List<string> CloseModeNames
        {
            get { return _CloseModeNames; }
        }

        /// <summary>
        /// Value-String Error Message list.
        /// </summary>
        public Dictionary<int, string> ErrorMessages
        {
            get { return _ErrorMessages; }
        }

        /// <summary>
        /// Value-String Error Level list.
        /// </summary>
        public List<string> ErrorLevels
        {
            get { return _ErrorLevels; }
        }

        /// <summary>
        /// String-Value Request Type dictionary.
        /// </summary>
        public Dictionary<string, ARequestType> RequestTypes
        {
            get { return _RequestTypes; }
        }

        /// <summary>
        /// Value-String Request Type list.
        /// </summary>
        public List<string> RequestNames
        {
            get { return _RequestNames; }
        }

        private Dictionary<string, ACloseMode> _CloseModes;
        private List<string> _CloseModeNames;

        private Dictionary<int, string> _ErrorMessages;
        private List<string> _ErrorLevels;

        private Dictionary<string, ARequestType> _RequestTypes;
        private List<string> _RequestNames;
    }
}
