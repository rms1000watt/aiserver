using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using AppClient;

namespace AppTestClient
{
    public partial class AppTestClientForm : Form
    {
        #region Constructor
        public AppTestClientForm()
        {
            InitializeComponent();

            InitObjects();
        }
        #endregion

        #region Member variables
        private AAppClient _AppClient;
        private AReturnReceiver _DefaultReceiver;
        private int _UserId;
        private int _SessionId;
        #endregion

        #region Delegates
        // required for the cross-thread component update
        private delegate void AppendToTextBox(string iText, bool iNewLine);
        #endregion

        private void AppendToConsole(string iText, bool iNewLine)
        {
            if (uConsoleTextBox.InvokeRequired)
            {
                uConsoleTextBox.Invoke(new AppendToTextBox(this.AppendToConsole), iText, iNewLine);
            }
            else
            {
                uConsoleTextBox.AppendText(iText);
                if (iNewLine)
                {
                    uConsoleTextBox.AppendText("\n");
                }
            }
        }

        private void AppendToOutput(string iText, bool iNewLine)
        {
            if (uOutputTextBox.InvokeRequired)
            {
                uOutputTextBox.BeginInvoke(new AppendToTextBox(this.AppendToOutput), iText, iNewLine);
            }
            else
            {
                uOutputTextBox.AppendText(iText);
                if (iNewLine)
                {
                    uOutputTextBox.AppendText("\n");
                }
            }
        }

        private void closeSessionToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestCloseForm aInput = new AppTestCloseForm();
                aInput.Text = "Close Session";
                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    _AppClient.CloseSession(_DefaultReceiver, _SessionId, (int)Enum.Parse(typeof(ACloseMode), aInput.CloseMode));
                }
            }
            else
            {
                uStatusError.Text = "Close session failed: Session not opened.";
            }
        }

        private void connectSessionToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestInputForm aInput = new AppTestInputForm();
                aInput.Text = "Connect Session";
                aInput.Prompt = "Session Id:";
                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        _AppClient.ConnectSession(_DefaultReceiver, int.Parse(aInput.Value));
                    }
                    catch (Exception)
                    {
                        uStatusError.Text = "Connect session failed: Invalid Session Id.";
                    }
                }
            }
            else
            {
                uStatusError.Text = "Connect session failed: Not connected.";
            }
        }

        private void connectToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null && _AppClient.IsConnected)
            {
                uStatusError.Text = "Connect failed: Already connected.";
            }
            else
            {
                AppTestConnectForm aConnForm = new AppTestConnectForm();
                if (aConnForm.ShowDialog() == DialogResult.OK)
                {
                    InitAppClient(aConnForm.Host, aConnForm.Port);
                }
            }
        }

        private void disconnectToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestCloseForm aInput = new AppTestCloseForm();
                aInput.Text = "Close Connection";
                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    _AppClient.CloseConnection(_DefaultReceiver, 0, (ACloseMode)Enum.Parse(typeof(ACloseMode), aInput.CloseMode));
                }
            }
            else
            {
                uStatusError.Text = "Disconnect failed: Not connected.";
            }
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void getContextsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetCurrentContexts(_DefaultReceiver);
            }
            else
            {
                uStatusError.Text = "Get current contexts failed: Not connected.";
            }
        }

        private void getSessionsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetSessions(_DefaultReceiver, _AppClient.ContextName);
            }
            else
            {
                uStatusError.Text = "Get sessions failed: Not connected.";
            }
        }

        private void InitAppClient(string iHost, ushort iPort)
        {
            if (_AppClient == null)
            {
                _AppClient = new AAppClient(iHost, iPort, _DefaultReceiver, "Test");
            }
            else
            {
                _AppClient.Host = iHost;
                _AppClient.Port = iPort;
            }

            _AppClient.OpenConnection(_DefaultReceiver);
        }

        private void InitObjects()
        {
            // initialize response handler
            _DefaultReceiver = new AReturnReceiver();
            _DefaultReceiver.ReturnOutputEvent += new EventHandler<AAsyncEventArgs>(ResponseHandler);

            _UserId = 0;

            uCommandTextBox.KeyDown += new KeyEventHandler(uCommandTextBox_KeyDown);
            uPrefixTextBox.Text = "(debug traceoff: erroroff: checkoff: jiton:)";
        }

        private void logoffToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.Logoff(_DefaultReceiver);
            }
            else
            {
                uStatusError.Text = "Logoff failed: Not connected/logged on.";
            }
        }

        private void logonToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestLogonForm aLogonForm = new AppTestLogonForm();
                if (aLogonForm.ShowDialog() == DialogResult.OK)
                {
                    _AppClient.Logon(_DefaultReceiver, aLogonForm.Username, aLogonForm.Password);
                }
            }
            else
            {
                uStatusError.Text = "Logon failed: Not connected.";
            }
        }

        private void openSessionToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestInputForm aInput = new AppTestInputForm();
                aInput.Text = "Open Session";
                aInput.Prompt = "Context Name:";
                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    _AppClient.OpenSession(_DefaultReceiver, aInput.Value);
                }
            }
            else
            {
                uStatusError.Text = "Open session failed: Not connected.";
            }
        }

        private void ResponseHandler(object iSource, AAsyncEventArgs iArgs)
        {
            AppendToConsole(string.Format("Request Id: {0}", iArgs.RequestId), true);
            AppendToConsole(string.Format("Request Type: {0}", iArgs.RequestType.ToString()), true);
            AppendToConsole(string.Format("Status: {0}", iArgs.Status), true);
            AppendToConsole(string.Format("Return Value: {0}", iArgs.ReturnValue), true);
            AppendToConsole(string.Format("Display: {0}", iArgs.Display), true);
            AppendToConsole(string.Format("Error: {0}", iArgs.Error), true);
            AppendToConsole(string.Format("Out: {0}", iArgs.Out), true);

            uStatusError.Text = "Err: " + iArgs.Error;

            if (iArgs.Status == 0)
            {
                switch (iArgs.RequestType)
                {
                    case ARequestType.OpenConnection:
                        uStatusConnection.Text = "Connected.";
                        break;

                    case ARequestType.CloseConnection:
                        uStatusConnection.Text = "Disconnected.";
                        _UserId = 0;
                        break;

                    case ARequestType.Logon:
                        uStatusConnection.Text = "Logged On.";
                        _UserId = iArgs.ReturnValue;
                        break;

                    case ARequestType.Logoff:
                        uStatusConnection.Text = "Logged Off.";
                        _UserId = 0;
                        break;

                    case ARequestType.GetCurrentContexts:
                        AppendToOutput("Get Current Contexts", true);
                        string[] aContexts = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aContext in aContexts)
                        {
                            string[] aEntry = aContext.Split("\t".ToCharArray());
                            AppendToOutput(aEntry[0], false);
                            if (aEntry.Length > 1)
                            {
                                AppendToOutput(": " + aEntry[1], true);
                            }
                        }
                        AppendToOutput("", true);
                        break;

                    case ARequestType.GetSessions:
                        AppendToOutput("Get Sessions", true);
                        string[] aSessions = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aSession in aSessions)
                        {
                            AppendToOutput(aSession, true);
                        }

                        AppendToOutput("", true);
                        break;

                    case ARequestType.Eval:
                    case ARequestType.Display:
                        if (iArgs.Display.Length > 0)
                        {
                            string[] aDisplayLines = iArgs.Display.Split("\n".ToCharArray());
                            foreach (string aLine in aDisplayLines)
                            {
                                if (aLine.Length > 0)
                                {
                                    AppendToOutput(aLine, true);
                                }
                            }
                        }
                        if (iArgs.Out.Length > 0)
                        {
                            AppendToOutput(iArgs.Out, true);
                        }
                        break;

                    case ARequestType.OpenSession:
                        AppendToOutput("Open Session", true);
                        AppendToOutput(iArgs.Out, true);
                        _SessionId = iArgs.ReturnValue;
                        break;

                    case ARequestType.CloseSession:
                        AppendToOutput("Close Session", true);
                        AppendToOutput(iArgs.Out, true);
                        _SessionId = 0;
                        break;

                    case ARequestType.GetUsers:
                        AppendToOutput("Get Users", true);
                        string[] aUsers = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aUser in aUsers)
                        {
                            AppendToOutput(aUser, true);
                        }
                        break;

                    case ARequestType.GetExtentNames:
                        AppendToOutput("Get Extent Names", true);
                        string[] aExtentNames = iArgs.Out.Split("\t".ToCharArray());
                        foreach (string aExtentName in aExtentNames)
                        {
                            AppendToOutput(aExtentName, true);
                        }
                        break;

                    case ARequestType.GetExtentTypes:
                        AppendToOutput("Get Extent Types", true);
                        string[] aExtentTypes = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aExtentType in aExtentTypes)
                        {
                            AppendToOutput(aExtentType, true);
                        }
                        break;

                    case ARequestType.GetNextLevel:
                        AppendToOutput("Get Next Level", true);
                        string[] aNodes = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aNode in aNodes)
                        {
                            AppendToOutput(aNode, true);
                        }
                        break;

                    case ARequestType.GetDirInfo:
                        AppendToOutput("Get Dir Info", true);
                        string[] aDirItems = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aItem in aDirItems)
                        {
                            AppendToOutput(aItem, true);
                        }
                        break;

                    case ARequestType.GetRequestStats:
                        AppendToOutput("Get Request Stats", true);
                        string[] aReqItems = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aItem in aReqItems)
                        {
                            AppendToOutput(aItem, true);
                        }
                        break;

                    case ARequestType.GetSessionStats:
                        AppendToOutput("Get Session Stats", true);
                        string[] aSessItems = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aItem in aSessItems)
                        {
                            AppendToOutput(aItem, true);
                        }
                        break;
                        
                    case ARequestType.GetWorkspaceStatistics:
                        AppendToOutput("Get Workspace Stats", true);
                        string[] aWorkItems = iArgs.Out.Split("\n".ToCharArray());
                        foreach (string aItem in aWorkItems)
                        {
                            AppendToOutput(aItem, true);
                        }
                        break;      
                  
                    case ARequestType.GetContextParams:
                        AppendToOutput("Get Context Params", true);
                        string[] aParams = iArgs.Out.Split("\x7F".ToCharArray());
                        foreach (string aParam in aParams)
                        {
                            AppendToOutput(aParam, true);
                        }
                        break;
                        
                    default:
                        AppendToOutput(iArgs.RequestType.ToString(), true);
                        AppendToOutput(iArgs.Out, true);

                        break;
                }
            }
            else
            {
                uStatusError.Text = string.Format("{0}: {1}", iArgs.RequestType.ToString(), iArgs.Error);
            }
        }

        private void setContextToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestInputForm aInput = new AppTestInputForm();
                aInput.Text = "Set Context";
                aInput.Prompt = "Context Name:";
                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    _AppClient.ContextName = aInput.Value;
                }
            }
            else
            {
                uStatusError.Text = "Set context failed: Not connected.";
            }
        }

        private void Submit()
        {
            if (_AppClient != null)
            {
                string aAmpMsg = string.Empty;

                if (uCommandTextBox.Text.StartsWith("_ais"))
                {
                    aAmpMsg = uCommandTextBox.Text.Replace('|', '\x7F');
                }
                else
                {
                    aAmpMsg = string.Format(AAppMsg.Eval, ASocket.MSG_DELIM, uPrefixTextBox.Text + uCommandTextBox.Text);
                }

                AppendToOutput(uCommandTextBox.Text, true);
                _AppClient.Submit(_DefaultReceiver, ref aAmpMsg);
            }
            else
            {
                uStatusError.Text = "Submit failed: Not connected/logged on.";
            }
        }

        void uCommandTextBox_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                Submit();
                e.Handled = true;
            }
        }

        //void uEditLine_KeyDown(object sender, KeyEventArgs e)
        //{
        //    if (e.KeyCode == Keys.Enter)
        //    {
        //        if (uEditLine.Text.Length > 0 && !uEditLine.Items.Contains(uEditLine.Text))
        //        {
        //            uEditLine.Items.Insert(0, uEditLine.Text);
        //        }
        //        e.Handled = true;
        //    }
        //}

        private void uEnterButton_Click(object sender, EventArgs e)
        {
            Submit();
        }

        private void openContextToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                
            }
        }

        private void addUserToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string aUsername = string.Empty;
            string aPassword = string.Empty;
            int aSecurityLvl = 0;
            DateTime aEndDate = DateTime.Now;
            string aComment = string.Empty;

            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();

                aInput.Text = "Add User";
                aInput.SetItems(5);
                aInput.SetPrompt(0, "Username:");
                aInput.SetPrompt(1, "Password:");
                aInput.SetPrompt(2, "Security Level:");
                aInput.SetPrompt(3, "End Date:");
                aInput.SetPrompt(4, "Comment:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        aUsername = aInput.GetValue(0);
                        aPassword = aInput.GetValue(1);
                        aSecurityLvl = int.Parse(aInput.GetValue(2));
                        aEndDate = DateTime.Parse(aInput.GetValue(3));
                        aComment = aInput.GetValue(4);

                        _AppClient.AddUser(_DefaultReceiver, aUsername, aPassword, aSecurityLvl, aEndDate, aComment);
                    }
                    catch (Exception ex)
                    {
                        uStatusError.Text = "Add user failed: " + ex.Message;
                    }
                }
            }
            else
            {
                uStatusError.Text = "Add user failed: Not connected.";
            }            
        }

        private void editUserToolStripMenuItem_Click(object sender, EventArgs e)
        {
            int aUserId = 0;
            string aUsername = string.Empty;
            string aPassword = string.Empty;
            int aSecurityLvl = 0;
            DateTime aEndDate = DateTime.Now;
            string aComment = string.Empty;

            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();

                aInput.Text = "Update User";
                aInput.SetItems(6);
                aInput.SetPrompt(0, "Username:");
                aInput.SetPrompt(1, "Password:");
                aInput.SetPrompt(2, "Security Level:");
                aInput.SetPrompt(3, "End Date:");
                aInput.SetPrompt(4, "Comment:");
                aInput.SetPrompt(5, "User Id:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        aUsername = aInput.GetValue(0);
                        aPassword = aInput.GetValue(1);
                        aSecurityLvl = int.Parse(aInput.GetValue(2));
                        aEndDate = DateTime.Parse(aInput.GetValue(3));
                        aComment = aInput.GetValue(4);
                        aUserId = int.Parse(aInput.GetValue(5));

                        _AppClient.UpdateUser(_DefaultReceiver, aUserId, aUsername, aPassword, aSecurityLvl, aEndDate, aComment);
                    }
                    catch (Exception ex)
                    {
                        uStatusError.Text = "Update user failed: " + ex.Message;
                    }
                }
            }
            else
            {
                uStatusError.Text = "Update user failed: Not connected.";
            }            
        }

        private void listUsersToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetUsers(_DefaultReceiver);
            }
            else
            {
                uStatusError.Text = "Get users failed: Not connected.";
            }
        }

        private void removeUserToolStripMenuItem_Click(object sender, EventArgs e)
        {
            int aUserId = 0;

            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();

                aInput.Text = "Remove User";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "User Id:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        aUserId = int.Parse(aInput.GetValue(0));

                        _AppClient.DeleteUser(_DefaultReceiver, aUserId);
                    }
                    catch (Exception ex)
                    {
                        uStatusError.Text = "Remove user failed: " + ex.Message;
                    }
                }
            }
            else
            {

            }
        }

        private void getExtentNamesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetExtentNames(_DefaultReceiver);
            }
            else
            {
                uStatusError.Text = "Get extent names failed: Not connected.";
            }
        }

        private void todayToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppendToOutput("Today",true);
                AppendToOutput(_AppClient.Today().ToString(), true);
            }
        }

        private void setCurrentExtentToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Set Current Extent";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Current Extent");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    AppendToOutput("Set Current Extent", true);
                    bool aRes = _AppClient.SetCurrentExtent(aInput.GetValue(0));
                    AppendToOutput(aRes.ToString(), true);
                }
            }
        }

        private void showAllPropertiesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppendToOutput("Show All Properties", true);
                AppendToOutput("===================", true);
                
                string aLine = string.Format("Connection Id: {0}", _AppClient.ConnectionId);
                AppendToOutput(aLine, true);

                aLine = string.Format("Context Name: {0}", _AppClient.ContextName);
                AppendToOutput(aLine, true);

                aLine = string.Format("Current Extent Name: {0}", _AppClient.CurrentExtentName);
                AppendToOutput(aLine, true);

                aLine = string.Format("Is Connected: {0}", _AppClient.IsConnected);
                AppendToOutput(aLine, true);

                aLine = string.Format("Error Trace: {0}", _AppClient.IsErrorTrace);
                AppendToOutput(aLine, true);

                aLine = string.Format("Instruction Trace: {0}", _AppClient.IsInstructionTrace);
                AppendToOutput(aLine, true);

                aLine = string.Format("Is Jit: {0}", _AppClient.IsJit);
                AppendToOutput(aLine, true);

                aLine = string.Format("Is SysCheck: {0}", _AppClient.IsSysCheck);
                AppendToOutput(aLine, true);

                aLine = string.Format("Server Name: {0}", _AppClient.ServerName);
                AppendToOutput(aLine, true);

                aLine = string.Format("Session Id: {0}", _AppClient.SessionId);
                AppendToOutput(aLine, true);

                aLine = string.Format("User Id: {0}", _AppClient.UserId);
                AppendToOutput(aLine, true);                
            }
        }

        private void getExtentTypesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetExtentTypes(_DefaultReceiver, true);
            }
        }

        private void getNextLevelToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Next Level";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Node Name:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    string aNodeName = aInput.GetValue(0).Trim();
                    if (aNodeName.Length > 0)
                    {
                        _AppClient.GetNextLevel(_DefaultReceiver, aNodeName);
                    }
                    else
                    {
                        _AppClient.GetNextLevel(_DefaultReceiver);
                    }
                }
            }
        }

        private void getExtentTypeOptionsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Extent Type Options";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Extent Name:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    AppendToOutput("Get Extent Type Options", true);
                    AppendToOutput(_AppClient.GetExtentTypeOptions(aInput.GetValue(0)), true);
                }
            }
        }

        private void getFullNodeNameToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Full Node Name";
                aInput.SetItems(2);
                aInput.SetPrompt(0, "Extent Name:");
                aInput.SetPrompt(1, "Index:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    string aExtentName = aInput.GetValue(0).Trim();
                    int aIndex = 0;
                    string aFullNodeName = "null";

                    int.TryParse(aInput.GetValue(1), out aIndex);

                    AppendToOutput("Get Full Node Name", true);
                    if (aExtentName.Length > 0)
                    {
                        aFullNodeName = _AppClient.GetFullNodeName(aExtentName, aIndex);
                    }
                    else
                    {
                        aFullNodeName = _AppClient.GetFullNodeName(aIndex);
                    }

                    AppendToOutput(aFullNodeName, true);
                }
            }
        }

        private void getNodeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Node";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Index:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    int aIndex = 0;
                    if (int.TryParse(aInput.GetValue(0), out aIndex))
                    {
                        ANodeInfo aNodeInfo = null;
                        string aDetails = "null";

                        AppendToOutput("Get Node", true);
                        aNodeInfo = _AppClient.GetNode(aIndex);

                        if (aNodeInfo != null)
                        {
                            aDetails = string.Format("Date: {0}\nSize: {1}\nSymbol: {2}\nTime: {3}\n Type: {4}\nUnique Key: {5}\nValue: {6}\nVersion: {7}",
                            aNodeInfo.Date, aNodeInfo.Size, aNodeInfo.Symbol, aNodeInfo.Time, aNodeInfo.Type, aNodeInfo.UniqueKey, aNodeInfo.Value, aNodeInfo.Version);
                        }

                        AppendToOutput(aDetails, true);
                    }
                }
            }
        }

        private void getNodePathTreeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                string[] aPathTree = _AppClient.GetNodePathTree();

                AppendToOutput("Get Node Path Tree", true);

                if (aPathTree != null)
                {
                    foreach (string aItem in aPathTree)
                    {
                        AppendToOutput(aItem, true);
                    }
                }
            }
        }

        private void getNodeSymbolNameToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Node Symbol Name";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Index:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    int aIndex = 0;
                    if (int.TryParse(aInput.GetValue(0), out aIndex))
                    {
                        string aSymbolName = null;
                        string aDetails = "null";

                        AppendToOutput("Get Node Symbol Name", true);
                        aSymbolName = _AppClient.GetNodeSymbolName(aIndex);

                        if (aSymbolName != null)
                        {
                            aDetails = aSymbolName;
                        }

                        AppendToOutput(aDetails, true);
                    }
                }
            }
        }

        private void getNodeTypeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Node Type";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Index:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    int aIndex = 0;
                    if (int.TryParse(aInput.GetValue(0), out aIndex))
                    {
                        string aNodeType = null;
                        string aDetails = "null";

                        AppendToOutput("Get Node Type", true);
                        aNodeType = _AppClient.GetNodeType(aIndex);

                        if (aNodeType != null)
                        {
                            aDetails = aNodeType;
                        }

                        AppendToOutput(aDetails, true);
                    }
                }
            }
        }

        private void getNodeType2ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Node Type (2)";
                aInput.SetItems(2);
                aInput.SetPrompt(0, "Extent Name:");
                aInput.SetPrompt(1, "Node Name:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    string aNodeType = null;
                    string aDetails = "null";

                    AppendToOutput("Get Node Type (2)", true);
                    aNodeType = _AppClient.GetNodeType(aInput.GetValue(0), aInput.GetValue(1));

                    if (aNodeType != null)
                    {
                        aDetails = aNodeType;
                    }

                    AppendToOutput(aDetails, true);
                }
            }
        }

        private void getNumNodesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppendToOutput("Get Num Nodes", true);

                AppendToOutput(_AppClient.GetNumNodes().ToString(), true);
            }
        }

        private void getNumNodes2ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Num Nodes (2)";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Extent Name:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    AppendToOutput("Get Num Nodes (2)", true);
                    int aCnt = _AppClient.GetNumNodes(aInput.GetValue(0));

                    AppendToOutput(aCnt.ToString(), true);
                }
            }
        }

        private void getPreviousLevelToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetPreviousLevel(_DefaultReceiver);
            }
        }

        private void getRequestStatsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetRequestStats(_DefaultReceiver);
            }
        }

        private void getRootLevelToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetRootLevel(_DefaultReceiver);
            }
        }

        private void getRequestStatsToolStripMenuItem_Click_1(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetRequestStats(_DefaultReceiver);
            }
        }

        private void getServerTypeActionsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Server Type Actions";
                aInput.SetItems(2);
                aInput.SetPrompt(0, "Type:");
                aInput.SetPrompt(1, "Extent:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    AppendToOutput("Get Server Type Actions", true);

                    string[] aActions = _AppClient.GetServerTypeActions(aInput.GetValue(0), aInput.GetValue(1));
                    if (aActions != null)
                    {
                        foreach (string aAction in aActions)
                        {
                            AppendToOutput(aAction, true);
                        }
                    }
                    else
                    {
                        AppendToOutput("null", true);
                    }
                }
            }
        }

        private void getSessionStatsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetSessionStats(_DefaultReceiver);
            }
        }

        private void getSubscriptionsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetSubscriptions(_DefaultReceiver, _AppClient.ContextName);
            }
        }

        private void getWorkspaceStatsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                _AppClient.GetWorkspaceStatistics(_DefaultReceiver);
            }
        }

        private void getNodeActionsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Node Actions";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Index:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    int aIndex = 0;
                    if (int.TryParse(aInput.GetValue(0), out aIndex))
                    {
                        AppendToOutput("Get Node Actions", true);
                        string[] aActions = _AppClient.GetNodeActions(aIndex);

                        if (aActions != null)
                        {
                            foreach (string aAction in aActions)
                            {
                                AppendToOutput(aAction, true);
                            }
                        }
                        else
                        {
                            AppendToOutput("null", true);
                        }
                    }
                }
            }
        }

        private void getDirInfoToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Dir Info";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Path:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    _AppClient.GetDirInfo(_DefaultReceiver, aInput.GetValue(0));
                }
            }
        }

        private void getContextParamsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Context Params";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Context:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    _AppClient.GetContextParams(_DefaultReceiver, aInput.GetValue(0));
                }
            }
        }

        private void getContextParamToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_AppClient != null)
            {
                AppTestGenericInputForm aInput = new AppTestGenericInputForm();
                aInput.Text = "Get Context Param";
                aInput.SetItems(1);
                aInput.SetPrompt(0, "Param Name:");

                if (aInput.ShowDialog() == DialogResult.OK)
                {
                    string aValue = "null";

                    _AppClient.GetContextParam(aInput.GetValue(0), out aValue);

                    AppendToOutput(string.Format("{0} = {1}", aInput.GetValue(0), aValue), true);
                }
            }
        }
    }
}