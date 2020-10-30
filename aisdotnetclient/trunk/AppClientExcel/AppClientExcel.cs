using System.Collections.Generic;
using System.Runtime.InteropServices;
using Excel = Microsoft.Office.Interop.Excel;
using System;
using System.Text;
using System.Threading;
using AppClient;

namespace AppClientExcel
{
    /// <summary>
    /// Return output delegate declaration.
    /// </summary>
    /// <param name="iReqId">Request id.</param>
    /// <param name="iReqType">Request type.</param>
    /// <param name="iStatus">Request status.</param>
    /// <param name="iRetVal">Return value.</param>
    /// <param name="iOut">Request output.</param>
    /// <param name="iDisp">Optional display message.</param>
    /// <param name="iErr">Optional error message.</param>
    /// <param name="iClientData">Optional client data.</param>
    public delegate void ReturnOutputDelegate(int iReqId, int iReqType, int iStatus, int iRetVal, string iOut, string iDisp, string iErr, string iClientData);

    /// <summary>
    /// This interface exposes the methods and properties available to the COM client.
    /// </summary>
    [System.Runtime.InteropServices.ComVisibleAttribute(true)]
    [System.Runtime.InteropServices.InterfaceType(ComInterfaceType.InterfaceIsIDispatch)]
    public interface AppClientOperations
    {
        /// <summary>
        /// Open a connection to AIS.
        /// </summary>
        /// <param name="iHost">Hostname or IP address.</param>
        /// <param name="iPort">Port number.</param>
        /// <returns>Request id.</returns>
        int OpenConnection(string iHost, int iPort);

        /// <summary>
        /// Close the current connection to AIS.
        /// </summary>
        /// <param name="iCloseMode">Close mode.</param>
        /// <returns>Request id.</returns>
        int CloseConnection(int iCloseMode);

        /// <summary>
        /// Logon to AIS.
        /// </summary>
        /// <param name="iUsername">Username.</param>
        /// <param name="iPassword">Password.</param>
        /// <returns>Request id.</returns>
        int Logon(string iUsername, string iPassword);

        /// <summary>
        /// Logoff from AIS.
        /// </summary>
        /// <returns>Request id.</returns>
        int Logoff();

        /// <summary>
        /// Open a new session in AIS.
        /// </summary>
        /// <param name="iContextName">Target context.</param>
        /// <returns>Request id.</returns>
        int OpenSession(string iContextName);

        /// <summary>
        /// Close an AIS session.
        /// </summary>
        /// <param name="iSessionId">Session id.</param>
        /// <param name="iCloseMode">Close mode.</param>
        /// <returns>Request id.</returns>
        int CloseSession(int iSessionId, int iCloseMode);

        /// <summary>
        /// Submit a command to AIS.
        /// </summary>
        /// <param name="iCommand">AIS command or query.</param>
        /// <returns>Request id.</returns>
        int Submit(string iCommand);

        /// <summary>
        /// Retrieves the available contexts in AIS.
        /// </summary>
        /// <returns>Request id.</returns>
        int GetCurrentContexts();

        /// <summary>
        /// Retrieves the request name from the request type value.
        /// </summary>
        /// <param name="iReqType">Request type.</param>
        /// <returns>Request name.</returns>
        string GetRequestName(int iReqType);

        /// <summary>
        /// Retrieves the error message from the error code.
        /// </summary>
        /// <param name="iErrCode">Error code.</param>
        /// <returns>Error message.</returns>
        string GetErrorMessage(int iErrCode);

        /// <summary>
        /// Returns true if connected to AIS.
        /// </summary>
        /// <returns>true if connected, false otherwise.</returns>
        bool IsConnected();

        /// <summary>
        /// Get the current user id.
        /// </summary>
        /// <returns>Request id.</returns>
        int GetUserId();

        /// <summary>
        /// Gets the directory information.
        /// </summary>
        /// <param name="iPath">Target path.</param>
        /// <returns>Request id.</returns>
        int GetDirInfo(string iPath);

        /// <summary>
        /// Retrieves the list of active sessions in a given context.
        /// </summary>
        /// <param name="iContextName">Context name.</param>
        /// <returns>Request id.</returns>
        int GetSessions(string iContextName);

        /// <summary>
        /// Retrieves the workspace statistics.
        /// </summary>
        /// <returns>Request id.</returns>
        int GetWorkspaceStatistics();

        /// <summary>
        /// Retrieves the active/loaded extent names.
        /// </summary>
        /// <returns>Request id.</returns>
        int GetExtentNames();

        /// <summary>
        /// Sets the current extent.
        /// </summary>
        /// <param name="iExtentName">Extent name.</param>
        /// <returns>true if the extent name is valid.</returns>
        /// <remarks>GetExtentNames should be called before using this function.</remarks>
        bool SetCurrentExtent(string iExtentName);

        /// <summary>
        /// Gets the current extent.
        /// </summary>
        /// <returns>Current extent name.</returns>
        string GetCurrentExtent();

        /// <summary>
        /// Enables application trace.
        /// </summary>
        /// <param name="iFilePath">Trace file path.</param>
        /// <param name="iAppend">Append flag.</param>
        /// <returns>0 if successful.</returns>
        int SetTraceFile(string iFilePath, bool iAppend);

        /// <summary>
        /// Custom lock mechanism for VBA.
        /// </summary>
        int Lock();

        /// <summary>
        /// Custom unlock mechanism for VBA.
        /// </summary>
        int Unlock();
    }

    /// <summary>
    /// This interface exposes the events available to the COM client.
    /// </summary>
    [System.Runtime.InteropServices.ComVisibleAttribute(true)]
    [System.Runtime.InteropServices.InterfaceType(ComInterfaceType.InterfaceIsIDispatch)]
    public interface AppClientEvents
    {
        /// <summary>
        /// Event handler interface.
        /// </summary>
        /// <param name="iReqId">Request id.</param>
        /// <param name="iReqType">Request type.</param>
        /// <param name="iStatus">Request status.</param>
        /// <param name="iRetVal">Return value.</param>
        /// <param name="iOut">Request output.</param>
        /// <param name="iDisp">Optional display message.</param>
        /// <param name="iErr">Optional error message.</param>
        /// <param name="iClientData">Optional client data.</param>
        void ReturnOutput(int iReqId, int iReqType, int iStatus, int iRetVal, string iOut, string iDisp, string iErr, string iClientData);
    }

    /// <summary>
    /// This class implements the services offered to the COM client.
    /// </summary>
    [System.Runtime.InteropServices.ComVisibleAttribute(true)]
    [System.Runtime.InteropServices.ClassInterface(ClassInterfaceType.None)]
    [System.Runtime.InteropServices.ComSourceInterfaces(typeof(AppClientEvents))]
    public class AppClientExcel: AppClientOperations
    {
        /// <summary>
        /// Event object. MUST HAVE THE SAME NAME AS THE EVENT NAME in AppClientEvents.
        /// </summary>
        public event ReturnOutputDelegate ReturnOutput;

        private AAppClient _AppClient = null;
        private AAsyncEventArgs _DefaultReceiver = null;
        private string _TraceFile = null;
        private bool _TraceAppend = true;
        private Mutex _OpMutex = null;

        /// <summary>
        /// Constructor.
        /// </summary>
        public AppClientExcel()
        {
            _OpMutex = new Mutex();
        }

        /// <summary>
        /// Default response handler. Issues an event notification.
        /// </summary>
        /// <param name="iSource"></param>
        /// <param name="iEvent"></param>
        private void ResponseHandler(object iSource, AAsyncEventArgs iEvent)
        {
            ReturnOutput(iEvent.RequestId, (int)iEvent.RequestType, iEvent.Status, iEvent.ReturnValue,
                iEvent.Out, iEvent.Display, iEvent.Error, iEvent.ClientData);
        }

        /// <summary>
        /// Wraps the AppClient.OpenConnection method.
        /// </summary>
        /// <param name="iHost">Hostname or IP address.</param>
        /// <param name="iPort">Port number.</param>
        /// <returns>Request Id.</returns>
        public int OpenConnection(string iHost, int iPort)
        {
            if (_AppClient == null)
            {
                _DefaultReceiver = new AAsyncEventArgs();
                _DefaultReceiver.Completed+=new System.EventHandler<AAsyncEventArgs>(ResponseHandler);

                _AppClient = new AAppClient(iHost, (ushort)iPort, _DefaultReceiver, "Excel AppClient");

                if (_TraceFile != null)
                {
                    _AppClient.SetTraceFile(_TraceFile, _TraceAppend);
                }
            }
            return _AppClient.OpenConnection(_DefaultReceiver);
        }

        /// <summary>
        /// Wraps the AppClient.CloseConnection method.
        /// </summary>
        /// <param name="iCloseMode">Close mode.</param>
        /// <returns>Request Id.</returns>
        /// <remarks>
        /// 0 = Default
        /// 1 = Disconnect
        /// 3 = Soft
        /// 4 = Firm
        /// 5 = Hard
        /// </remarks>
        public int CloseConnection(int iCloseMode)
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.CloseConnection(_DefaultReceiver, 0, (ACloseMode)iCloseMode);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.Logon method.
        /// </summary>
        /// <param name="iUsername">Username.</param>
        /// <param name="iPassword">Password.</param>
        /// <returns>Request Id.</returns>
        public int Logon(string iUsername, string iPassword)
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.Logon(_DefaultReceiver, iUsername, iPassword);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.Logoff method.
        /// </summary>
        /// <returns>Request Id.</returns>
        public int Logoff()
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.Logoff(_DefaultReceiver);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.OpenSession method.
        /// </summary>
        /// <param name="iContextName">Target context name.</param>
        /// <returns>Request Id.</returns>
        public int OpenSession(string iContextName)
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.OpenSession(_DefaultReceiver, iContextName);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.CloseSession method.
        /// </summary>
        /// <param name="iSessionId">Session Id.</param>
        /// <param name="iCloseMode">Close mode.</param>
        /// <returns>Request Id.</returns>
        /// <remarks>
        /// 0 = Default
        /// 1 = Disconnect
        /// 3 = Soft
        /// 4 = Firm
        /// 5 = Hard
        /// </remarks>
        public int CloseSession(int iSessionId, int iCloseMode)
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.CloseSession(_DefaultReceiver, iSessionId, iCloseMode);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.Submit method.
        /// </summary>
        /// <param name="iCommand">Command string.</param>
        /// <returns>Request Id.</returns>
        public int Submit(string iCommand)
        {
            string aAmpMsg = string.Empty;
            int aRet = -1;

            if (_AppClient != null)
            {
                if (iCommand.StartsWith("_ais"))
                {
                    // command is an ais formatted query
                    aAmpMsg = iCommand.Replace('|', '\x7F');
                }
                else
                {
                    // prepend _ais|eval|exp|
                    aAmpMsg = string.Format("_ais{0}eval{0}exp{0}{1}", "\x7F", iCommand);
                }

                aRet = _AppClient.Submit(_DefaultReceiver, ref aAmpMsg);
            }

            return aRet;            
        }

        /// <summary>
        /// Wraps the AppClient.GetCurrentContexts
        /// </summary>
        /// <returns>Request Id.</returns>
        public int GetCurrentContexts()
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.GetCurrentContexts(_DefaultReceiver);
            }
            return aRet;
        }

        /// <summary>
        /// Helper function to determine the request type name from a request type value.
        /// </summary>
        /// <param name="iReqType">Request type code.</param>
        /// <returns>Request name.</returns>
        public string GetRequestName(int iReqType)
        {
            List<string> aReqList = AGlobals.GetSingleton().RequestNames;
            if (iReqType < 0 || iReqType >= aReqList.Count)
            {
                iReqType = 0;
            }
            return aReqList[iReqType];
        }

        /// <summary>
        /// Helper function to determinte the error message from an error code.
        /// </summary>
        /// <param name="iErrCode">Error code.</param>
        /// <returns>Error message.</returns>
        public string GetErrorMessage(int iErrCode)
        {
            Dictionary<int, string> aErrMap = AGlobals.GetSingleton().ErrorMessages;
            if (!aErrMap.ContainsKey(iErrCode))
            {
                iErrCode = 0;
            }
            return aErrMap[iErrCode];
        }

        /// <summary>
        /// Wraps the AppClient.IsConnected method.
        /// </summary>
        /// <returns>true if client is connected.</returns>
        public bool IsConnected()
        {
            bool aRet = false;
            if (_AppClient != null)
            {
                aRet = _AppClient.IsConnected;
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.UserId property.
        /// </summary>
        /// <returns>User Id if logged on.</returns>
        public int GetUserId()
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.UserId;
            }
            return aRet;            
        }

        /// <summary>
        /// Wraps the AppClient.GetDirInfo method.
        /// </summary>
        /// <param name="iPath">Target path.</param>
        /// <returns>Request Id.</returns>
        public int GetDirInfo(string iPath)
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.GetDirInfo(_DefaultReceiver, iPath);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.GetSessions method.
        /// </summary>
        /// <param name="iContextName">Target context name.</param>
        /// <returns>Request Id.</returns>
        public int GetSessions(string iContextName)
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.GetSessions(_DefaultReceiver, iContextName);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.GetWorkspaceStatistics method.
        /// </summary>
        /// <returns>Request Id.</returns>
        public int GetWorkspaceStatistics()
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.GetWorkspaceStatistics(_DefaultReceiver);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.GetExtentNames method.
        /// </summary>
        /// <returns>Request Id.</returns>
        public int GetExtentNames()
        {
            int aRet = -1;
            if (_AppClient != null)
            {
                aRet = _AppClient.GetExtentNames(_DefaultReceiver);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.SetCurrentExtent method.
        /// </summary>
        /// <param name="iExtentName">Extent name.</param>
        /// <returns>true if the current extent was set.</returns>
        public bool SetCurrentExtent(string iExtentName)
        {
            bool aRet = false;
            if (_AppClient != null)
            {
                aRet = _AppClient.SetCurrentExtent(iExtentName);
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.CurrentExtent property.
        /// </summary>
        /// <returns>Current extent name.</returns>
        public string GetCurrentExtent()
        {
            string aRet = string.Empty;
            if (_AppClient != null)
            {
                aRet = _AppClient.CurrentExtentName;
            }
            return aRet;
        }

        /// <summary>
        /// Wraps the AppClient.SetTraceFile method.
        /// </summary>
        /// <param name="iFilePath">Trace file path.</param>
        /// <param name="iAppend">Append flag.</param>
        public int SetTraceFile(string iFilePath, bool iAppend)
        {
            _TraceFile = iFilePath;
            _TraceAppend = iAppend;

            if (_AppClient != null)
            {
                _AppClient.SetTraceFile(_TraceFile, _TraceAppend);
            }

            return 0;
        }

        /// <summary>
        /// Lock the mutex variable.
        /// </summary>
        public int Lock()
        {
            int aRet = 0;
            try
            {
                _OpMutex.WaitOne();
            }
            catch (Exception)
            {
                aRet = -1;
            }

            return aRet;
        }
        
        /// <summary>
        /// Unlock the mutex variable.
        /// </summary>
        public int Unlock()
        {
            int aRet = 0;
            try
            {
                _OpMutex.ReleaseMutex();
            }
            catch (Exception)
            {
                aRet = -1;
            }

            return aRet;
        }
    }
}
