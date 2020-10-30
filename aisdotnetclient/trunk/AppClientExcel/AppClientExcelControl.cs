using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using AppClient;

namespace AppClientExcel
{
    public partial class AppClientExcelControl : UserControl
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        public AppClientExcelControl()
        {
            InitializeComponent();
            InitObjects();
        }

        /// <summary>
        /// Close the AppClient connection.
        /// </summary>
        public void Shutdown()
        {
            if (_AppClient.IsConnected)
            {
                _AppClient.CloseConnection(null, 0, ACloseMode.Disconnect);
            }
        }

        /// <summary>
        /// Initialize add-in objects.
        /// </summary>
        private void InitObjects()
        {
            _DefaultReceiver = new AAsyncEventArgs();
            _DefaultReceiver.Completed += new EventHandler<AAsyncEventArgs>(DefaultResponseHandler);
            _AppClient = new AAppClient("", 0, _DefaultReceiver, "Excel");

            _SecondaryReceiver = new AAsyncEventArgs();
            _SecondaryReceiver.Completed += new EventHandler<AAsyncEventArgs>(SecondaryResponseHandler);

            // default close mode is soft
            uCloseModeCombo.SelectedIndex = 2;
            uPrefixText.Text = "(debug  traceoff: erroroff: checkoff: jiton:)";

            SetConnectionState(false);
            StatusInfo("Ready");

            _ConsoleContextMenu = new ContextMenu();
            _ConsoleContextMenu.MenuItems.Add("Run Selection", uRunSelection_Click);
            uConsoleText.ContextMenu = _ConsoleContextMenu;

            uLogonButton.Click += new EventHandler(uLogonButton_Click);
            uLogoffButton.Click += new EventHandler(uLogoffButton_Click);
            uRefreshContextsButton.Click += new EventHandler(uRefreshContextsButton_Click);
            uAvailContextsList.Click += new EventHandler(uAvailContextsList_Click);
            uRefreshSessionsButton.Click += new EventHandler(uRefreshSessionsButton_Click);
            uNewSessionButton.Click += new EventHandler(uNewSessionButton_Click);
            uConnectSessionButton.Click += new EventHandler(uConnectSessionButton_Click);
            uEnterButton.Click += new EventHandler(uEnterButton_Click);
            uCommandCombo.KeyDown += new KeyEventHandler(uCommandCombo_KeyDown);
            uRefreshCabinetsButton.Click += new EventHandler(uRefreshCabinetsButton_Click);
            uCabinetsList.DoubleClick += new EventHandler(uCabinetsList_DoubleClick);

            uActiveSessionList.ColumnClick += new ColumnClickEventHandler(uActiveSessionList_ColumnClick);
            uNodesList.ColumnClick += new ColumnClickEventHandler(uNodesList_ColumnClick);

            _ActiveSessionListSorter = new ListViewColumnSorter();
            _ActiveNodesListSorter = new ListViewColumnSorter();

            uActiveSessionList.ListViewItemSorter = _ActiveSessionListSorter;
            uNodesList.ListViewItemSorter = _ActiveNodesListSorter;
        }

        /// <summary>
        /// Active nodes list column header click event handler
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uNodesList_ColumnClick(object sender, ColumnClickEventArgs e)
        {
            if (_ActiveNodesListSorter.SortColumn == e.Column)
            {
                if (_ActiveNodesListSorter.Order == SortOrder.Ascending)
                {
                    _ActiveNodesListSorter.Order = SortOrder.Descending;
                }
                else
                {
                    _ActiveNodesListSorter.Order = SortOrder.Ascending;
                }
            }
            else
            {
                _ActiveNodesListSorter.SortColumn = e.Column;
                _ActiveNodesListSorter.Order = SortOrder.Ascending;
            }

            uNodesList.Sort();            
        }

        /// <summary>
        /// Active session list column header click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uActiveSessionList_ColumnClick(object sender, ColumnClickEventArgs e)
        {
            if (_ActiveSessionListSorter.SortColumn == e.Column)
            {
                if (_ActiveSessionListSorter.Order == SortOrder.Ascending)
                {
                    _ActiveSessionListSorter.Order = SortOrder.Descending;
                }
                else
                {
                    _ActiveSessionListSorter.Order = SortOrder.Ascending;
                }
            }
            else
            {
                _ActiveSessionListSorter.SortColumn = e.Column;
                _ActiveSessionListSorter.Order = SortOrder.Ascending;
            }

            uActiveSessionList.Sort();
        }

        /// <summary>
        /// Cabinet list double click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uCabinetsList_DoubleClick(object sender, EventArgs e)
        {
            if (uCabinetsList.SelectedItems.Count > 0)
            {
                uCurrentCabinetText.Text = uCabinetsList.SelectedItems[0].Text;
                _AppClient.SetCurrentExtent(uCurrentCabinetText.Text);
                _GetNextLevel = true;
                _AppClient.GetExtentTypes(_SecondaryReceiver, true);
            }
            else
            {
                uCurrentCabinetText.Text = "";
            }
        }

        /// <summary>
        /// Cabinet refresh button click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uRefreshCabinetsButton_Click(object sender, EventArgs e)
        {
            DoRefreshCabinets();
        }

        /// <summary>
        /// Run the selected text.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uRunSelection_Click(object sender, EventArgs e)
        {
            DoRunSelection();
        }

        /// <summary>
        /// Command text input key down event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uCommandCombo_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                e.Handled = true;
                DoSubmit();
            }
        }

        /// <summary>
        /// Enter button click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uEnterButton_Click(object sender, EventArgs e)
        {
            DoSubmit();
        }

        /// <summary>
        /// Connect session button click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uConnectSessionButton_Click(object sender, EventArgs e)
        {
            DoConnectSession();
        }

        /// <summary>
        /// New session button click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uNewSessionButton_Click(object sender, EventArgs e)
        {
            DoNewSession();
        }

        /// <summary>
        /// Context list item click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uAvailContextsList_Click(object sender, EventArgs e)
        {
            if (uAvailContextsList.SelectedItems.Count > 0)
            {
                uCurrentContextText.Text = uAvailContextsList.SelectedItems[0].Text;

                DoRefreshSessions();
            }
        }

        /// <summary>
        /// Perform logon process.
        /// </summary>
        private void DoLogon()
        {
            // check if all required values are present
            string aHost = uHostText.Text.Trim();
            string aPort = uPortText.Text.Trim();
            string aUname = uUsernameText.Text.Trim();
            string aPword = uPasswordText.Text;
            ushort aPortNum = 0;

            bool aError = false;
    
            // back to default color
            uHostText.BackColor = Color.White;
            uPortText.BackColor = Color.White;
            uUsernameText.BackColor = Color.White;

            // check if host is empty
            if (aHost.Length == 0)
            {
                uHostText.BackColor = Color.Yellow;
                aError = true;
            }

            // check if port is empty
            if (aPort.Length == 0)
            {
                uPortText.BackColor = Color.Yellow;
                aError = true;
            }
            else
            {
                // check if port number is valid
                if (!ushort.TryParse(aPort, out aPortNum))
                {
                    uPortText.BackColor = Color.Yellow;
                    aError = true;
                }
            }

            // check if username is empty
            if (aUname.Length == 0)
            {
                uUsernameText.BackColor = Color.Yellow;
                aError = true;
            }

            // if error is encountered
            if (aError)
            {
                StatusError("Invalid connection/logon parameters.");
            }
            else
            {
                _Username = aUname;
                _Password = aPword;
                _Host = aHost;
                _Port = aPortNum;

                // initiate connection
                InitConnection();
            }
        }

        /// <summary>
        /// Perform logoff/disconnect process.
        /// </summary>
        private void DoLogoff()
        {
            if (_AppClient.IsConnected)
            {
                Dictionary<string, ACloseMode> aCloseModes = AGlobals.GetSingleton().CloseModes;
                ACloseMode aCloseMode = aCloseModes[uCloseModeCombo.Text.ToLower()];
                _AppClient.CloseConnection(_DefaultReceiver, 0, aCloseMode);
            }
        }

        /// <summary>
        /// Retrieve active contexts.
        /// </summary>
        private void DoRefreshContexts()
        {
            SetAvailableContexts(null);
            _AppClient.GetCurrentContexts(_SecondaryReceiver);
        }

        /// <summary>
        /// Retrieve active sessions.
        /// </summary>
        private void DoRefreshSessions()
        {
            _AppClient.GetSessions(_SecondaryReceiver, uCurrentContextText.Text);
        }

        /// <summary>
        /// Create new session.
        /// </summary>
        private void DoNewSession()
        {
            _AppClient.OpenSession(_SecondaryReceiver, uCurrentContextText.Text);
        }

        /// <summary>
        /// Connect to a disconnected session.
        /// </summary>
        private void DoConnectSession()
        {
            if (uActiveSessionList.SelectedItems.Count > 0)
            {
                // get the selected session
                int aSessionId = int.Parse(uActiveSessionList.SelectedItems[0].Text);
                string aSessionOwner = uActiveSessionList.SelectedItems[0].SubItems[2].Text;

                if (aSessionOwner != "system")
                {
                    _AppClient.ConnectSession(_SecondaryReceiver, aSessionId);
                }
                else
                {
                    StatusError("Connecting to a system session not allowed.");
                }
            }
            else
            {
                StatusError("No session selected.");
            }
        }

        /// <summary>
        /// Submit command to AIS.
        /// </summary>
        private void DoSubmit()
        {
            string aCommand = uCommandCombo.Text.Trim();

            if (aCommand.StartsWith("_ais"))
            {
                aCommand.Replace('|', '\x7F');
            }
            else
            {
                string aPrefix = uPrefixText.Text.Trim();
                aCommand = string.Format(AAppMsg.Eval, ASocket.MSG_DELIM, aPrefix + aCommand);
            }

            if (aCommand.Length > 0)
            {
                SetEnable(false);
                _AppClient.Submit(_DefaultReceiver, ref aCommand);
            }
        }

        /// <summary>
        /// Submit selected text to AIS.
        /// </summary>
        private void DoRunSelection()
        {
            string aCommand = uConsoleText.SelectedText.Trim();
            string aPrefix = uPrefixText.Text.Trim();

            aCommand = string.Format(AAppMsg.Eval, ASocket.MSG_DELIM, aPrefix + aCommand);

            if (aCommand.Length > 0)
            {
                SetEnable(false);
                _AppClient.Submit(_DefaultReceiver, ref aCommand);
            }
        }

        /// <summary>
        /// Retrieve active cabinets.
        /// </summary>
        private void DoRefreshCabinets()
        {
            _AppClient.GetExtentNames(_SecondaryReceiver);
        }

        /// <summary>
        /// Set LogStatus.
        /// </summary>
        private void DoSetLogLevel()
        {
            _AppClient.SetLogLevel(_SecondaryReceiver, ARequestType.LogStatus, AErrorLevel.Info);
        }

        /// <summary>
        /// Retrieve next level.
        /// </summary>
        private void DoGetNextLevel()
        {
            if (_GetNextLevel)
            {
                _AppClient.GetNextLevel(_SecondaryReceiver);
            }
        }

        /// <summary>
        /// Initiate logon to server.
        /// </summary>
        private void InitLogon()
        {
            if (_AppClient.IsConnected)
            {
                _AppClient.Logon(_DefaultReceiver, _Username, _Password);
            }
        }

        /// <summary>
        /// Initiate connection to server.
        /// </summary>
        private void InitConnection()
        {
            // check if host and port was changed
            if (_AppClient.Host == _Host && _AppClient.Port == _Port)
            {
                // check if already connected
                if (_AppClient.IsConnected)
                {
                    // check if not yet logged on
                    if (!_LoggedOn)
                    {
                        InitLogon();
                    }
                }
                else
                {
                    // open connection first
                    _AppClient.OpenConnection(_DefaultReceiver);
                }
            }
            else
            {
                // host or port was changed
                if (_AppClient.IsConnected)
                {
                    // close previous connection, before reinitiaing connection
                    _AppClient.CloseConnection(_DefaultReceiver, 0, ACloseMode.Soft);
                    _Reconnect = true;
                }
                else
                {
                    // establish connection to server                    
                    _AppClient.Host = _Host;
                    _AppClient.Port = _Port;
                    _AppClient.OpenConnection(_DefaultReceiver);
                }
            }
        }

        /// <summary>
        /// Logoff button click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uLogoffButton_Click(object sender, EventArgs e)
        {
            DoLogoff();
        }

        /// <summary>
        /// Logon button click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uLogonButton_Click(object sender, EventArgs e)
        {
            DoLogon();
        }

        /// <summary>
        /// Refresh Contexts button click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uRefreshContextsButton_Click(object sender, EventArgs e)
        {
            DoRefreshContexts();   
        }

        /// <summary>
        /// Refresh session button click event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void uRefreshSessionsButton_Click(object sender, EventArgs e)
        {
            DoRefreshSessions();
        }

        /// <summary>
        /// Display error message.
        /// </summary>
        /// <param name="iErrMsg"></param>
        private void StatusError(string iErrMsg)
        {
            if (uStatusBar.InvokeRequired)
            {
                uStatusBar.Invoke(new SetStatusDelegate(StatusError), iErrMsg);
            }
            else
            {
                uStatusMessage.ForeColor = Color.Red;
                uStatusMessage.Text = "Error: " + iErrMsg;
            }
        }

        /// <summary>
        /// Display information message.
        /// </summary>
        /// <param name="iInfMsg"></param>
        private void StatusInfo(string iInfMsg)
        {
            if (uStatusBar.InvokeRequired)
            {
                uStatusBar.Invoke(new SetStatusDelegate(StatusInfo), iInfMsg);
            }
            else
            {
                uStatusMessage.ForeColor = DefaultForeColor;
                uStatusMessage.Text = "Info: " + iInfMsg;
            }
        }

        /// <summary>
        /// Append to log.
        /// </summary>
        /// <param name="iLogMsg"></param>
        /// <param name="iNewLine"></param>
        private void AppendToLog(string iLogMsg, bool iNewLine)
        {
            if (uLogText.InvokeRequired)
            {
                uLogText.Invoke(new AppendDelegate(AppendToLog), iLogMsg, iNewLine);
            }
            else
            {
                uLogText.AppendText(iLogMsg);
                if (iNewLine)
                {
                    uLogText.AppendText("\n");
                }
            }
        }

        /// <summary>
        /// Append to log.
        /// </summary>
        /// <param name="iLogMsg"></param>
        private void AppendToLog(string iLogMsg)
        {
            AppendToLog(iLogMsg, true);
        }

        /// <summary>
        /// Append to console.
        /// </summary>
        /// <param name="iText"></param>
        /// <param name="iNewLine"></param>
        private void AppendToConsole(string iText, bool iNewLine)
        {
            if (uConsoleText.InvokeRequired)
            {
                uConsoleText.Invoke(new AppendDelegate(AppendToConsole), iText, iNewLine);
            }
            else
            {
                string[] aLines = iText.Split("\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                foreach (string aLine in aLines)
                {
                    uConsoleText.AppendText(aLine);
                    uConsoleText.AppendText("\n");
                }
            }            
        }

        /// <summary>
        /// Append to console.
        /// </summary>
        /// <param name="iText"></param>
        private void AppendToConsole(string iText)
        {
            AppendToConsole(iText, true);
        }

        /// <summary>
        /// Populates the available contexts list.
        /// </summary>
        /// <param name="iOut"></param>
        private void SetAvailableContexts(string iOut)
        {
            if (uAvailContextsList.InvokeRequired)
            {
                uAvailContextsList.Invoke(new SetDataDelegate(SetAvailableContexts), iOut);
            }
            else
            {
                uAvailContextsList.Items.Clear();

                if (iOut != null)
                {
                    string[] aRecords = iOut.Split("\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    foreach (string aRecord in aRecords)
                    {
                        string[] aFields = aRecord.Split("\t".ToCharArray());
                        if (aFields[0].Length > 0 && aFields[0] != "_SystemContext")
                        {
                            uAvailContextsList.Items.Add(aFields[0]);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Populates the cabinets list.
        /// </summary>
        /// <param name="iOut"></param>
        private void SetActiveExtents(string iOut)
        {
            if (uCabinetsList.InvokeRequired)
            {
                uCabinetsList.Invoke(new SetDataDelegate(SetActiveExtents), iOut);
            }
            else
            {
                uCabinetsList.Items.Clear();

                if (iOut != null)
                {
                    string[] aRecords = iOut.Split("\t".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    foreach (string aRecord in aRecords)
                    {
                        uCabinetsList.Items.Add(aRecord);
                    }
                }
            }
        }

        /// <summary>
        /// Populates the active sessions list.
        /// </summary>
        /// <param name="iOut"></param>
        private void SetActiveSessions(string iOut)
        {
            if (uActiveSessionList.InvokeRequired)
            {
                uActiveSessionList.Invoke(new SetDataDelegate(SetActiveSessions), iOut);
            }
            else
            {
                uActiveSessionList.Items.Clear();

                if (iOut != null)
                {
                    string[] aRecords = iOut.Split("\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    foreach (string aRecord in aRecords)
                    {
                        string[] aFields = aRecord.Split("\t".ToCharArray());

                        ListViewItem aItem = new ListViewItem();
                        aItem.Text = aFields[1]; // Session Id
                        aItem.SubItems.Add(aFields[2].ToLower() == "admin" ? "Yes" : "No"); // Admin Session
                        aItem.SubItems.Add(aFields[3]); // Owner
                        aItem.SubItems.Add(aFields[4]); // Mode
                        aItem.SubItems.Add(aFields[5]); // Protocol

                        uActiveSessionList.Items.Add(aItem).SubItems[0].Tag = "number";
                    }
                }
            }
        }

        /// <summary>
        /// Populate the node list.
        /// </summary>
        /// <param name="iOut"></param>
        private void SetActiveNodes(string iOut)
        {
            if (uNodesList.InvokeRequired)
            {
                uNodesList.Invoke(new SetDataDelegate(SetActiveNodes), iOut);
            }
            else
            {
                uNodePathCombo.Items.Clear();
                string[] aNodePaths = _AppClient.GetNodePathTree();
                foreach (string aNodePath in aNodePaths)
                {
                    uNodePathCombo.Items.Add(aNodePath);
                }

                uNodesList.Items.Clear();
                int aNumNodes = _AppClient.GetNumNodes();
                ANodeInfo aNodeInfo = null;

                for (int i = 0; i < aNumNodes; i++)
                {
                    aNodeInfo = _AppClient.GetNode(i);
                    if (aNodeInfo != null)
                    {
                        ListViewItem aItem = new ListViewItem();
                        aItem.Text = aNodeInfo.Value;
                        aItem.SubItems.Add(aNodeInfo.Type);
                        aItem.SubItems.Add(aNodeInfo.Size).Tag = "number";
                        aItem.SubItems.Add(aNodeInfo.Date);
                        aItem.SubItems.Add(aNodeInfo.Time);
                        aItem.SubItems.Add(aNodeInfo.Version);
                        aItem.SubItems.Add(i.ToString()).Tag = "number";
                        uNodesList.Items.Add(aItem);
                    }
                }
            }
        }

        /// <summary>
        /// Sets the current session.
        /// </summary>
        /// <param name="iOut"></param>
        private void SetCurrentSession(string iOut)
        {
            if (uCurrentSessionText.InvokeRequired)
            {
                uCurrentSessionText.Invoke(new SetDataDelegate(SetCurrentSession), iOut);
            }
            else
            {
                uCurrentSessionText.Text = iOut;
            }
        }

        /// <summary>
        /// Set the connection state.
        /// </summary>
        /// <param name="iState"></param>
        private void SetConnectionState(bool iState)
        {
            if (uStatusBar.InvokeRequired)
            {
                uStatusBar.Invoke(new SetStateDelegate(SetConnectionState), iState);
            }
            else
            {
                if (iState == true)
                {
                    uConnectState.ForeColor = Color.Green;
                    uConnectState.Text = "Connected";
                }
                else
                {
                    uConnectState.ForeColor = Color.Red;
                    uConnectState.Text = "Disconnected";
                }
            }
        }

        /// <summary>
        /// Enable/disable sending of command.
        /// </summary>
        /// <param name="iEnable"></param>
        private void SetEnable(bool iEnable)
        {
            if (uCommandCombo.InvokeRequired)
            {
                uCommandCombo.Invoke(new SetStateDelegate(SetEnable), iEnable);
            }
            else
            {
                uCommandCombo.Enabled = iEnable;
                uEnterButton.Enabled = iEnable;
            }
        }

        /// <summary>
        /// Sets the engine state display.
        /// </summary>
        /// <param name="iEnable"></param>
        private void SetEngineIdle(bool iEnable)
        {
            if (uStatusBar.InvokeRequired)
            {
                uStatusBar.Invoke(new SetStateDelegate(SetEngineIdle), iEnable);
            }
            else
            {
                if (iEnable)
                {
                    uEngineState.ForeColor = Color.Green;
                    uEngineState.Text = "Engine: Idle";
                }
                else
                {
                    uEngineState.ForeColor = Color.Red;
                    uEngineState.Text = "Engine: Busy";
                }
            }
        }

        /// <summary>
        /// Default response handler.
        /// </summary>
        /// <param name="iSrc"></param>
        /// <param name="iArgs"></param>
        private void DefaultResponseHandler(object iSrc, AAsyncEventArgs iArgs)
        {
            if (iArgs.Status == 0)
            {
                StatusInfo("Ready");

                switch (iArgs.RequestType)
                {
                    case ARequestType.OpenConnection:
                        InitLogon();
                        break;
                    case ARequestType.CloseConnection:
                        SetConnectionState(false);
                        if (_Reconnect == true)
                        {
                            InitConnection();
                            _Reconnect = false;
                        }
                        _LoggedOn = false;
                        break;
                    case ARequestType.Logon:
                        SetConnectionState(true);
                        _LoggedOn = true;
                        break;
                    case ARequestType.Display:
                    case ARequestType.Eval:
                        if (iArgs.Display.Length > 0)
                        {
                            AppendToConsole(iArgs.Display);
                        }
                        if (iArgs.Out.Length > 0)
                        {
                            AppendToConsole(iArgs.Out);
                        }
                        break;
                    case ARequestType.LogStatus:
                        if (iArgs.Out.StartsWith("_icon"))
                        {
                            bool iEnable = iArgs.Out.EndsWith("off");
                            SetEngineIdle(iEnable);
                        }
                        else
                        {
                            StatusInfo(iArgs.Out);
                        }
                        break;
                    default:
                        AppendToLog("Request Id: " + iArgs.RequestId);
                        AppendToLog("Request Type: " + iArgs.RequestType.ToString());
                        AppendToLog("Status: " + iArgs.Status);
                        AppendToLog("Return: " + iArgs.ReturnValue);
                        AppendToLog("Error: " + iArgs.Error);
                        AppendToLog("Display: " + iArgs.Display);
                        AppendToLog("Out: " + iArgs.Out);
                        AppendToLog("-----");
                        break;
                }
            }
            else
            {
                if (iArgs.Error.Length == 0)
                {
                    if (AGlobals.GetSingleton().ErrorMessages.ContainsKey(iArgs.Status))
                    {
                        iArgs.Error = AGlobals.GetSingleton().ErrorMessages[iArgs.Status];
                    }
                    else
                    {
                        iArgs.Error = AGlobals.GetSingleton().ErrorMessages[(int)AErrorCodes.Generic];
                    }
                }
                StatusError(iArgs.Error);
                SetEnable(true);
            }

            if (iArgs.RequestType == ARequestType.Eval)
            {
                SetEnable(true);
            }
        }

        /// <summary>
        /// Secondary response handler.
        /// </summary>
        /// <param name="iSrc"></param>
        /// <param name="iArgs"></param>
        private void SecondaryResponseHandler(object iSrc, AAsyncEventArgs iArgs)
        {
            if (iArgs.Status == 0)
            {
                StatusInfo("Ready");

                switch (iArgs.RequestType)
                {
                    case ARequestType.GetCurrentContexts:
                        SetAvailableContexts(iArgs.Out);
                        break;
                    case ARequestType.GetSessions:
                        SetActiveSessions(iArgs.Out);
                        break;
                    case ARequestType.GetExtentNames:
                        SetActiveExtents(iArgs.Out);
                        break;
                    case ARequestType.GetExtentTypes:
                        DoGetNextLevel();
                        break;
                    case ARequestType.GetNextLevel:
                        SetActiveNodes(null);
                        break;
                    case ARequestType.OpenSession:
                        SetCurrentSession(iArgs.ReturnValue.ToString());
                        DoRefreshSessions();
                        DoSetLogLevel();
                        break;
                    case ARequestType.SetLogLvl:
                        _AppClient.SetExtentTypeOptions(".Memory", "#(1 1 1)");
                        break;
                    case ARequestType.ConnectSession:
                        SetCurrentSession(iArgs.ReturnValue.ToString());
                        DoRefreshSessions();
                        break;
                    default:
                        AppendToLog("Request Id: " + iArgs.RequestId);
                        AppendToLog("Request Type: " + iArgs.RequestType.ToString());
                        AppendToLog("Status: " + iArgs.Status);
                        AppendToLog("Return: " + iArgs.ReturnValue);
                        AppendToLog("Error: " + iArgs.Error);
                        AppendToLog("Display: " + iArgs.Display);
                        AppendToLog("Out: " + iArgs.Out);
                        AppendToLog("-----");
                        break;
                }
            }
            else
            {
                if (iArgs.Error.Length == 0)
                {
                    if (AGlobals.GetSingleton().ErrorMessages.ContainsKey(iArgs.Status))
                    {
                        iArgs.Error = AGlobals.GetSingleton().ErrorMessages[iArgs.Status];
                    }
                    else
                    {
                        iArgs.Error = AGlobals.GetSingleton().ErrorMessages[(int)AErrorCodes.Generic];
                    }
                }
                StatusError(iArgs.Error);
            }
        }

        #region Delegates

        private delegate void AppendDelegate(string iMsg, bool iNew);
        private delegate void SetStatusDelegate(string iMsg);
        private delegate void SetStateDelegate(bool iState);
        private delegate void SetDataDelegate(string iOut);

        #endregion

        #region Members

        private AAppClient _AppClient;
        private AAsyncEventArgs _DefaultReceiver;
        private AAsyncEventArgs _SecondaryReceiver;

        private bool _Reconnect = false;
        private string _Username = string.Empty;
        private string _Password = string.Empty;
        private string _Host = string.Empty;
        private ushort _Port = 0;
        private bool _LoggedOn = false;
        private bool _GetNextLevel = false;

        private ContextMenu _ConsoleContextMenu = null;

        private ListViewColumnSorter _ActiveSessionListSorter = null;
        private ListViewColumnSorter _ActiveNodesListSorter = null;

        #endregion
    }
}
