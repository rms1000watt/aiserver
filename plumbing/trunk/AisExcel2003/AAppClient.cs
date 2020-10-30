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

/**
 * AIS Client
 * 
 * Change History
 * Version  Date        Who      Change
 * 0.0001   05/01/2008  fchua    Initial Version
 */
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Diagnostics;
using System.IO;
using AXID = System.Int32;

namespace AppClient
{
	/// <summary>
	/// AppClient C# implementation.
	/// </summary>
	public class AAppClient
	{
		/// <summary>
		/// AAppClient constructor.
		/// </summary>
		/// <param name="irHostDnsIp">AIS host or IP address.</param>
		/// <param name="iPort">Port number.</param>
		/// <param name="iReceiver">Receiver object.</param>
		/// <param name="irName">Instance name.</param>
		public AAppClient(string irHostDnsIp, ushort iPort, AAsyncEventArgs iReceiver, string irName)
		{   _Ais = AGlobals.GetSingleton();
			_Mutex = new Mutex();
			_ClientRequests = new SortedDictionary<int, AClientRequest>();
			_Y2K = new DateTime(2000, 1, 1);
			_ContextParams = new SortedDictionary<string, string>();
			_DefaultReceiver = iReceiver;
			_EngineFlags = 0;
			_HostDnsIp = irHostDnsIp;
			_IsConnected = false;
			_LogReceiver = iReceiver;
			_Port = iPort;
			_ServerName = irName;
			_SessionId = 0;
			_Socket = null;
			_UserId = 0;
			_Xid = 0;
			_ConnectionId = 0;
			_OpenConnXid = 0;
			_CloseConnXid = 0;
			_BackHistory = new List<string>();
			_Extents = new Dictionary<string, AExtentInfo>();
			_ColNames = new List<string>();
			_RepositoryNames = new List<string>();
			_ExtentTypeOptions = new Dictionary<string, string>();
			_CurExtentName = string.Empty;
			_ContextName = string.Empty;
			_TraceFile = null;
		}
		/// <summary>
		/// Submit a request to AIS to add a new user.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="irUsername">Username.</param>
		/// <param name="irPassword">Password.</param>
		/// <param name="iSecurityLevel">Security Level (0-7).</param>
		/// <param name="irEndDate">Account expiration date.</param>
		/// <param name="irComment">Optional comment.</param>
		/// <returns>Request id.</returns>
		/// <remarks>AMP format: _ais|adduser|username|%s|password|%s|securitylevel|%d|enddate|MM/dd/yyyy|comment|%s</remarks>
		public AXID AddUser(AAsyncEventArgs iReceiver, string irUsername,
			string irPassword, int iSecurityLevel, DateTime irEndDate, string irComment)
		{
			string aAmpMsg = string.Format(AAppMsg.AddUser, ASocket.MSG_DELIM, 
				irUsername, irPassword, iSecurityLevel, irEndDate, irComment);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Submit a request to AIS to disconnect from a cabinet.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iCabName">Target cabinet.</param>
		/// <returns>Request id.</returns>
		/// <remarks>AMP format: _ais|closecabinet|cabname|%s</remarks>
		public AXID CloseCabinet(AAsyncEventArgs iReceiver, string iCabName)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.CloseCabinet, ASocket.MSG_DELIM, iCabName);
			if (iCabName != null && iCabName.Length > 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.BadCabinet, ARequestType.CloseCabinet, ref aAmpMsg);
			return aXid;
		}
		/// <summary>
		/// Submit a request to AIS to close the connection.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iConnectId">Connection id.</param>
		/// <param name="iMode">Close mode.</param>
		/// <returns>Request id.</returns>
		/// <remarks>Format: _ais|closeconnection|connectid|%d|closemode|%s|closewait|%d</remarks>
		public AXID CloseConnection(AAsyncEventArgs iReceiver, int iConnectId, ACloseMode iMode)
		{
			string aAmpMsg = string.Format(AAppMsg.CloseConnection,
				ASocket.MSG_DELIM, iConnectId, _Ais.CloseModeNames[(int)iMode]);

			_CloseConnXid = Submit(iReceiver, ref aAmpMsg);
			return _CloseConnXid;
		}
		/// <summary>
		/// Close the named context on AIS.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iContextName">Context name.</param>
		/// <param name="iMode">Close mode.</param>
		/// <returns>Request id.</returns>
		public AXID CloseContext(AAsyncEventArgs iReceiver, string iContextName, int iMode)
		{
			AXID aXid = 0;
			int aStatus = 0;
			string aAmpMsg = string.Format(AAppMsg.CloseContext, ASocket.MSG_DELIM,
				iContextName, _Ais.CloseModeNames[iMode]);
			if (iContextName == null || iContextName.Length == 0)
				aStatus = (int)AErrorCodes.UnkContextName;
			else if (!_IsConnected)
				aStatus = (int)AErrorCodes.Disconnected;
			else if (_UserId <= 0)
				aStatus = (int)AErrorCodes.NoLogon;
			else
				aXid = Submit(iReceiver, ref aAmpMsg);
			if (aStatus > 0)
				aXid = SubmitError(iReceiver, aStatus, ARequestType.CloseContext, ref aAmpMsg);
			return aXid;
		}
		/// <summary>
		/// Close an open session.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iSessionId">Session id.</param>
		/// <param name="iMode">Close mode.</param>
		/// <returns>Request id.</returns>
		public AXID CloseSession(AAsyncEventArgs iReceiver, int iSessionId, int iMode)
		{
			AXID aXid = 0;
			int aStatus = 0;
			if (iSessionId <= 0)
				iSessionId = _SessionId;
			string aAmpMsg = string.Format(AAppMsg.CloseSession, ASocket.MSG_DELIM,
				iSessionId, _Ais.CloseModeNames[iMode]);
			if (!_IsConnected)
				aStatus = (int)AErrorCodes.Disconnected;
			else if (_UserId <= 0)
				aStatus = (int)AErrorCodes.NoLogon;
			else if (iSessionId <= 0)
				aStatus = (int)AErrorCodes.SessionId;
			else
				aXid = Submit(iReceiver, ref aAmpMsg);
			if (aStatus > 0)
				aXid = SubmitError(iReceiver, aStatus, ARequestType.CloseSession, ref aAmpMsg);
			return aXid;            
		}
		/// <summary>
		/// Submit a request to AIS to comiple an agent.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iExtentAgentList">Extent agent list.</param>
		/// <returns>Request id.</returns>
		/// <remarks>Format: _ais|compileagent|extent_agents|%s</remarks>
		public AXID CompileAgent(AAsyncEventArgs iReceiver, string[] iExtentAgentList)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.CompileAgent, ASocket.MSG_DELIM);
			if (iExtentAgentList != null && iExtentAgentList.Length > 0)
			{	aAmpMsg += string.Join("\t", iExtentAgentList);
				aXid = Submit(iReceiver, ref aAmpMsg);
			}
			else
			{	aAmpMsg += "noExtents";
				aXid = SubmitError(iReceiver, (int) AErrorCodes.NoExtent, ARequestType.CompileAgent, ref aAmpMsg);
			}
			return aXid;
		}
		/// <summary>
		/// Submit request to AIS to compile all agents in a cabinet.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iExtentNames">Extent names list.</param>
		/// <returns>Request id.</returns>
		public AXID CompileCabinet(AAsyncEventArgs iReceiver, string[] iExtentNames)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.CompileCabinet, ASocket.MSG_DELIM);
			if (iExtentNames != null && iExtentNames.Length > 0)
			{	aAmpMsg += string.Join("\t", iExtentNames);
				aXid = Submit(iReceiver, ref aAmpMsg);
			}
			else
			{	aAmpMsg += "noExtents";
				aXid = SubmitError(iReceiver, (int)AErrorCodes.NoExtent, ARequestType.CompileCabinet, ref aAmpMsg);
			}
			return aXid;
		}
		/// <summary>
		/// Reconnect to a disconnected session.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iSessionId">Session id.</param>
		/// <returns>Request id.</returns>
		public AXID ConnectSession(AAsyncEventArgs iReceiver, int iSessionId)
		{
			int aXid = 0;
			int aStatus = 0;
			string aAmpMsg = string.Format(AAppMsg.ConnectSession, ASocket.MSG_DELIM, iSessionId);
			if (!_IsConnected)
				aStatus = (int)AErrorCodes.Disconnected;
			else if (_UserId <= 0)
				aStatus = (int)AErrorCodes.NoLogon;
			else if (iSessionId <= 0)
				aStatus = (int)AErrorCodes.SessionId;
			else
				aXid = Submit(iReceiver, ref aAmpMsg);
			if (aStatus > 0)
				aXid = SubmitError(iReceiver, aStatus, ARequestType.ConnectSession, ref aAmpMsg);
			return aXid;
		}
		/// <summary>
		/// Submit a debug command during a debugging session.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iExp">Expression.</param>
		/// <returns>Request id.</returns>
		public AXID Debug(AAsyncEventArgs iReceiver, string iExp)
		{
			string aAmpMsg = string.Format(AAppMsg.Debug, ASocket.MSG_DELIM, iExp);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Removes a user account from AIS.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iUserId">User id.</param>
		/// <returns>Request id.</returns>
		public AXID DeleteUser(AAsyncEventArgs iReceiver, int iUserId)
		{
			string aAmpMsg = string.Format(AAppMsg.DeleteUser, ASocket.MSG_DELIM, iUserId);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Close, suspend, enable the console log.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iEnable">Enable switch.</param>
		/// <returns>Request id.</returns>
		public AXID EnableConsoleLog(AAsyncEventArgs iReceiver, int iEnable)
		{
			string aAmpMsg = string.Format(AAppMsg.EnableConsoleLog, ASocket.MSG_DELIM, iEnable, 0);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Remove an existing agent from a cabinet.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iAgentList"></param>
		/// <returns></returns>
		public AXID EraseNode(AAsyncEventArgs iReceiver, string[] iAgentList)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.EraseNode, ASocket.MSG_DELIM, _CurExtentName);
			if (_CurExtentName.Length > 0 && iAgentList != null && iAgentList.Length > 0)
			{	aAmpMsg += string.Join("\t", iAgentList);
				aXid = Submit(iReceiver, ref aAmpMsg);
			}
			else
			{	aAmpMsg += "noExtents";
				aXid = SubmitError(iReceiver, (int)AErrorCodes.NoExtent, ARequestType.EraseNode, ref aAmpMsg);
			}
			return aXid;
		}
		/// <summary>
		/// Call server to evaluate a Lisp expression.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iExp"></param>
		/// <param name="iData"></param>
		/// <param name="iDataSize"></param>
		/// <returns></returns>
		public AXID Eval(AAsyncEventArgs iReceiver, string iExp, byte[] iData, int iDataSize)
		{
			string aAmpMsg = string.Format(AAppMsg.Eval, ASocket.MSG_DELIM, iExp);
			return Submit(iReceiver, ref aAmpMsg, "", 0, iData, iDataSize, null);
		}
		/// <summary>
		/// Call server to evaluate a Lisp expression and return a binary result.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iExp"></param>
		/// <param name="iData"></param>
		/// <param name="iDataSize"></param>
		/// <returns></returns>
		public AXID Execute(AAsyncEventArgs iReceiver, string iExp, byte[] iData, int iDataSize)
		{
			string aAmpMsg = string.Format(AAppMsg.Execute, ASocket.MSG_DELIM, iExp);
			return Submit(iReceiver, ref aAmpMsg, "", 0, iData, iDataSize, null);
		}
		/// <summary>
		/// Save a cabinet as a .sl file on the server.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iCabName"></param>
		/// <param name="iCabPath"></param>
		/// <returns></returns>
		public AXID ExportCabinetRemote(AAsyncEventArgs iReceiver, string iCabName, string iCabPath)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.ExportCabinet, ASocket.MSG_DELIM, iCabName, iCabPath);

			if (iCabPath.Length > 0 && iCabName.Length > 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.BadCabinet, ARequestType.ExportCabinet, ref aAmpMsg);
			return aXid;
		}
		/// <summary>
		/// Save an agent as a .sl file on the server.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iCabName"></param>
		/// <param name="iNodeName"></param>
		/// <param name="iFilePath"></param>
		/// <returns></returns>
		public AXID ExportNodeRemote(AAsyncEventArgs iReceiver, string iCabName, string iNodeName, string iFilePath)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.ExportNode, ASocket.MSG_DELIM, iNodeName, iFilePath, iCabName);
			if (iCabName.Length > 0 && iFilePath.Length > 0 && iNodeName.Length > 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.BadCabinet, ARequestType.ExportNode, ref aAmpMsg);

			return aXid;
		}
		/// <summary>
		/// Get connection statistics.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetConnectionStats(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetConnectionStats, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Retrieve the buffered console output.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iClear"></param>
		/// <returns></returns>
		public AXID GetConsoleLog(AAsyncEventArgs iReceiver, bool iClear)
		{
			string aAmpMsg = string.Format(AAppMsg.GetConsoleLog, ASocket.MSG_DELIM, iClear);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Returns an array of node names.
		/// </summary>
		/// <returns></returns>
		public string[] GetCurrentNodeNames()
		{
			string[] aNodeNames = null;
			if (_Extents.ContainsKey(_CurExtentName))
			{	List<ANodeInfo> aNodes = _Extents[_CurExtentName].Nodes;
				int aSize = aNodes.Count;
				aNodeNames = new string[aSize];
				for (int aNode = 0; aNode < aSize; ++aNode)
					aNodeNames[aNode] = aNodes[aNode].Value;
			}
			return aNodeNames;
		}
		/// <summary>
		/// Returns the value of the specified context parameter key.
		/// </summary>
		/// <param name="iKey"></param>
		/// <param name="iValue"></param>
		/// <returns></returns>
		public bool GetContextParam(string iKey, out string iValue)
		{
			bool aRet = false;
			iValue = "";
			if (_ContextParams.ContainsKey(iKey))
			{	iValue = _ContextParams[iKey];
				aRet = true;
			}
			return aRet;
		}
		/// <summary>
		/// Sends a GetContextParams request to the AIS server.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iContextName"></param>
		/// <returns></returns>
		public AXID GetContextParams(AAsyncEventArgs iReceiver, string iContextName)
		{
			AXID aXid = 0;
			int aStatus = 0;
			string aAmpMsg = string.Format(AAppMsg.GetContextParams, ASocket.MSG_DELIM, iContextName);
			if (iContextName.Length == 0)
				aStatus = (int)AErrorCodes.UnkContextName;
			else if (!_IsConnected)
				aStatus = (int)AErrorCodes.Disconnected;
			else if (_UserId <= 0)
				aXid = Submit(iReceiver, ref aAmpMsg);

			if (aStatus > 0)
				aXid = SubmitError(iReceiver, aStatus, ARequestType.GetContextParams, ref aAmpMsg);
			return aXid;
		}
		/// <summary>
		/// Request for a list of contexts from AIS.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetCurrentContexts(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetCurrentContexts, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Returns an array of extent names.
		/// </summary>
		/// <returns></returns>
		public string[] GetCurrentExtentNames()
		{
			string[] aExtentNames = null;
			if (_Extents.Count > 0)
			{	int aIdx = 0;
				aExtentNames = new string[_Extents.Count];

				foreach (string aName in _Extents.Keys)
					aExtentNames[aIdx++] = aName;
			}
			return aExtentNames;
		}
		/// <summary>
		/// Returns an array of repository names.
		/// </summary>
		/// <returns></returns>
		public string[] GetCurrentRepositoryNames()
		{
			string[] aRepositoryNames = null;
			if (_RepositoryNames.Count > 0)
			{	int aIdx = 0;
				aRepositoryNames = new string[_RepositoryNames.Count];
				foreach (string aName in _RepositoryNames)
					aRepositoryNames[aIdx++] = aName;
			}
			return aRepositoryNames;
		}
		/// <summary>
		/// Retrieve the Session ID of the active session for the current context.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetExeSession(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetExeSession, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Fetch a list of cabinet names from the current context.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetExtentNames(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetExtentNames, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Get the capabilities of this extent.
		/// </summary>
		/// <param name="iExtentType"></param>
		/// <returns></returns>
		public string GetExtentTypeOptions(string iExtentType)
		{
			string aOptions = string.Empty;
			if (_ExtentTypeOptions.ContainsKey(iExtentType))
				aOptions = _ExtentTypeOptions[iExtentType];
			return aOptions;
		}
		/// <summary>
		/// Fetch a list of extent types for the current extent.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iForce"></param>
		/// <returns></returns>
		public AXID GetExtentTypes(AAsyncEventArgs iReceiver, bool iForce)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.GetExtentTypes, ASocket.MSG_DELIM, _CurExtentName);
			if (_Extents.ContainsKey(_CurExtentName) &&
				(_Extents[_CurExtentName].ExtentTypes.Count == 0 || iForce))
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.NoExtent, ARequestType.GetExtentTypes, ref aAmpMsg);
			return aXid;
		}
		/// <summary>
		/// Get directory information from AIS.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iDir"></param>
		/// <returns></returns>
		public AXID GetDirInfo(AAsyncEventArgs iReceiver, string iDir)
		{
			string aAmpMsg = string.Format(AAppMsg.GetDirInfo, ASocket.MSG_DELIM, iDir);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Get the name of the variable/agent/file for the specified node.
		/// </summary>
		/// <param name="iIndex"></param>
		/// <returns></returns>
		public string GetFullNodeName(int iIndex)
		{
			return GetFullNodeName(_CurExtentName, iIndex);
		}
		/// <summary>
		/// Get the name of the variable/agent/file for the specified extent/node.
		/// </summary>
		/// <param name="iExtentName"></param>
		/// <param name="iIndex"></param>
		/// <returns></returns>
		public string GetFullNodeName(string iExtentName, int iIndex)
		{
			string aNodeName = string.Empty;
			AExtentInfo aInfo = null;
			if (_Extents.ContainsKey(iExtentName))
			{	aInfo = _Extents[iExtentName];
				if (iIndex < aInfo.Nodes.Count)
				{	aNodeName = aInfo.Nodes[iIndex].Symbol;
					if (aNodeName.Length > 0 && aInfo.NodePath.Length > 0)
						aNodeName = aInfo.NodePath + "/" + aNodeName;
				}
			}
			return aNodeName;
		}
		/// <summary>
		/// 
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetLogonStats(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetLogonStats, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Get nodes for the level above the current node.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetNextLevel(AAsyncEventArgs iReceiver)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Empty;
			if (_CurExtentName != null && _CurExtentName.Length > 0 && _Extents.ContainsKey(_CurExtentName))
			{	string aOptions = GetExtentTypeOptions(_CurExtentName);
				aAmpMsg = string.Format(AAppMsg.GetNextLevel, ASocket.MSG_DELIM,
					_CurExtentName, _Extents[_CurExtentName].NodePath, aOptions);
				aXid = Submit(iReceiver, ref aAmpMsg);
			}
			else
			{	aAmpMsg = string.Format(AAppMsg.GetNextLevel, ASocket.MSG_DELIM,
					_CurExtentName, "none", "none");
				aXid = SubmitError(iReceiver, (int)AErrorCodes.NoExtent, ARequestType.GetNextLevel, ref aAmpMsg);
			}
			return aXid;
		}
		/// <summary>
		/// Get nodes for the level below the current node.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iFullNodeName"></param>
		/// <returns></returns>
		public AXID GetNextLevel(AAsyncEventArgs iReceiver, string iFullNodeName)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.GetNextLevel, ASocket.MSG_DELIM,
				_CurExtentName, iFullNodeName, "");
			if (_CurExtentName.Length > 0 && _Extents.ContainsKey(_CurExtentName))
			{	string aOptions = GetExtentTypeOptions(_CurExtentName);
				aAmpMsg += aOptions;
				aXid = Submit(iReceiver, ref aAmpMsg);
			}
			else
			{	aXid = SubmitError(iReceiver, (int)AErrorCodes.NoExtent, ARequestType.GetNextLevel, ref aAmpMsg);
			}
			return aXid;
		}

		/// <summary>
		/// Get the selected node from array of nodes.
		/// </summary>
		/// <param name="iNodeIndex"></param>
		/// <returns></returns>
		public ANodeInfo GetNode(int iNodeIndex)
		{
			ANodeInfo aNodeInfo = null;
			AExtentInfo aExtentInfo = null;
			List<ANodeInfo> aNodes = null;            
			if (_Extents.ContainsKey(_CurExtentName))
			{	aExtentInfo = _Extents[_CurExtentName];
				aNodes = aExtentInfo.Nodes;
				if(iNodeIndex >= 0 && iNodeIndex < aNodes.Count)
					aNodeInfo = aNodes[iNodeIndex];
			}
			return aNodeInfo;
		}
		/// <summary>
		/// Get the server actions for this node.
		/// </summary>
		/// <param name="iNodeIndex"></param>
		/// <returns></returns>
		public string[] GetNodeActions(int iNodeIndex)
		{
			string[] aActions = null;
			string aType = string.Empty;
			List<ANodeInfo> aNodes = null;
			if (_Extents.ContainsKey(_CurExtentName))
			{	aNodes = _Extents[_CurExtentName].Nodes;
				if (iNodeIndex >= 0 && iNodeIndex < aNodes.Count)
				{	aType = aNodes[iNodeIndex].Type;
					aActions = GetServerTypeActions(aType, _CurExtentName);
				}
			}
			return aActions;
		}
		/// <summary>
		/// Get node path.
		/// </summary>
		/// <returns></returns>
		public string[] GetNodePathTree()
		{
			List<string> aNodePathTree = new List<string>();
			if (_Extents.ContainsKey(_CurExtentName))
			{	AExtentInfo aInfo = _Extents[_CurExtentName];
				string[] aNodePaths = aInfo.NodePath.Split("/".ToCharArray());
				int aNodeCnt = aNodePaths.Length;
				StringBuilder aPath = new StringBuilder();
				aNodePathTree.Add("/");
				for (int i = 0; i < aNodeCnt; i++)
				{	aPath.Remove(0, aPath.Length);
					for (int k = 0; k <= i; k++)
					{	aPath.AppendFormat("/{0}", aNodePaths[k]);
					}
					aNodePathTree.Add(aPath.ToString());
				}
			}
			return aNodePathTree.ToArray();
		}
		/// <summary>
		/// Get the name of the variable/agent/file for the specified extent/node.
		/// </summary>
		/// <param name="iNodeIndex"></param>
		/// <returns></returns>
		public string GetNodeSymbolName(int iNodeIndex)
		{
			string aSymbolName = string.Empty;
			if (_Extents.ContainsKey(_CurExtentName))
			{	List<ANodeInfo> aNodes = _Extents[_CurExtentName].Nodes;
				if (iNodeIndex < aNodes.Count)
					aSymbolName = aNodes[iNodeIndex].Symbol;
			}
			return aSymbolName;
		}
		/// <summary>
		/// Get the type of node for the specified node.
		/// </summary>
		/// <param name="iIndex"></param>
		/// <returns></returns>
		public string GetNodeType(int iIndex)
		{
			string aType = string.Empty;
			if (_Extents.ContainsKey(_CurExtentName))
			{	List<ANodeInfo> aNodeList = _Extents[_CurExtentName].Nodes;
				if (iIndex < aNodeList.Count)
					aType = aNodeList[iIndex].Type;
			}
			return aType;
		}
		/// <summary>
		/// Get the node type for the node given the extent and node name.
		/// </summary>
		/// <param name="iExtentName"></param>
		/// <param name="iFullNodeName"></param>
		/// <returns></returns>
		public string GetNodeType(string iExtentName, string iFullNodeName)
		{
			string aType = string.Empty;
			if (_Extents.ContainsKey(iExtentName))
			{	List<ANodeInfo> aNodeList = _Extents[iExtentName].Nodes;
				int aNodeCnt = aNodeList.Count;
				for (int i = 0; i < aNodeCnt; i++)
				{	if (GetFullNodeName(iExtentName, i) == iFullNodeName)
					{	aType = aNodeList[i].Type;
						break;
					}
				}
			}
			return aType;
		}
		/// <summary>
		/// Get the number of nodes for the current extent.
		/// </summary>
		/// <returns></returns>
		public int GetNumNodes()
		{
			return GetNumNodes(_CurExtentName);
		}
		/// <summary>
		/// Get the number of nodes for the specified extent.
		/// </summary>
		/// <param name="iExtentName"></param>
		/// <returns></returns>
		public int GetNumNodes(string iExtentName)
		{
			int aNodes = 0;
			if (_Extents.ContainsKey(iExtentName))
				aNodes = _Extents[iExtentName].Nodes.Count;
			return aNodes;
		}
		/// <summary>
		/// Get nodes for the level above the current node.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetPreviousLevel(AAsyncEventArgs iReceiver)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Empty;
			AExtentInfo aInfo = null;
			if (_CurExtentName.Length > 0 && _Extents.ContainsKey(_CurExtentName))
			{	aInfo = _Extents[_CurExtentName];
				string[] aNodes = aInfo.NodePath.Split("/".ToCharArray());
				if (aNodes.Length == 0)
					aXid = -(int)AErrorCodes.AtBeg; // already at top level
				else
				{	string aNodePath = string.Join("/", aNodes, 0, aNodes.Length - 1);
					string aOptions = GetExtentTypeOptions(_CurExtentName);
					aAmpMsg = string.Format(AAppMsg.GetNextLevel, ASocket.MSG_DELIM,
						_CurExtentName, aNodePath, aOptions);
					aXid = Submit(iReceiver, ref aAmpMsg);
				}
			}
			else
				aXid = -(int)AErrorCodes.NoExtent;
			if (aXid < 0)
			{	aAmpMsg = string.Format(AAppMsg.GetNextLevel, ASocket.MSG_DELIM, _CurExtentName, "none", "none");
				aXid = SubmitError(iReceiver, -aXid, ARequestType.GetNextLevel, ref aAmpMsg);
			}
			return aXid;
		}
		/// <summary>
		/// Return a reference to a specific item from the request queue.
		/// </summary>
		/// <param name="iXid"></param>
		/// <returns></returns>
		public AClientRequest GetRequestItem(AXID iXid)
		{
			return _ClientRequests[iXid];
		}
		/// <summary>
		/// Get the nodes at the top level.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetRootLevel(AAsyncEventArgs iReceiver)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.GetNextLevel, ASocket.MSG_DELIM,
				_CurExtentName, "", "");
			if (_CurExtentName.Length > 0 && _Extents.ContainsKey(_CurExtentName))
			{	string aOptions = GetExtentTypeOptions(_CurExtentName);
				aAmpMsg += aOptions;
				aXid = Submit(iReceiver, ref aAmpMsg);
			}
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.NoExtent, ARequestType.GetNextLevel,
					ref aAmpMsg);
			return aXid;
		}
		/// <summary>
		/// Get request statistics.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetRequestStats(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetRequestStats, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Get list of actions for a given extent and type.
		/// </summary>
		/// <param name="iType"></param>
		/// <param name="iExtentName"></param>
		/// <returns></returns>
		public string[] GetServerTypeActions(string iType, string iExtentName)
		{
			string[] aServerActionList = null;
			if (_Extents.ContainsKey(iExtentName))
			{	string aDefault = ".default.";
				AExtentInfo aExtentInfo = _Extents[iExtentName];
				Dictionary<string, AExtentTypeInfo> aServerTypes = aExtentInfo.ExtentTypes;
				AExtentTypeInfo aServerTypeInfo = null;
				if (aServerTypes.ContainsKey(iType))
					aServerTypeInfo = aServerTypes[iType];
				else if (aServerTypes.ContainsKey(aDefault))
					aServerTypeInfo = aServerTypes[aDefault];
				if (aServerTypeInfo != null)
				{	string aServerActions = aServerTypeInfo.ActionCodes;
					aServerActionList = aServerActions.Split(",".ToCharArray());
				}
			}
			return aServerActionList;
		}
		/// <summary>
		/// Get current list of sessions for the named context.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iContextName"></param>
		/// <returns></returns>
		public AXID GetSessions(AAsyncEventArgs iReceiver, string iContextName)
		{
			string aAmpMsg = string.Format(AAppMsg.GetSessions, ASocket.MSG_DELIM);
			if (iContextName != null && iContextName.Length > 0)
				aAmpMsg += string.Format(AAppMsg.GetSessionsContext, ASocket.MSG_DELIM, iContextName);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Get session statistics.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetSessionStats(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetSessionStats, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Get current list of subscriptions for the named context.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iContextName"></param>
		/// <returns></returns>
		public AXID GetSubscriptions(AAsyncEventArgs iReceiver, string iContextName)
		{
			string aAmpMsg = string.Format(AAppMsg.GetSubscriptions, ASocket.MSG_DELIM);
			if (iContextName.Length > 0)
				aAmpMsg += string.Format(AAppMsg.GetSubscriptionsContext, ASocket.MSG_DELIM,
					iContextName);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Request a list of AIS user accounts.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetUsers(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetUsers, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Retrieve workspace statistics from the AIS server.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID GetWorkspaceStatistics(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.GetWorkspaceStatistics, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Load a cabinet from a .sl file on the server.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iCabName"></param>
		/// <param name="iFilePath"></param>
		/// <returns></returns>
		public AXID ImportCabinetRemote(AAsyncEventArgs iReceiver, string iCabName, string iFilePath)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.ImportCabinet, ASocket.MSG_DELIM,
					iCabName, iFilePath);
			if (iCabName.Length > 0 && iFilePath.Length > 0 && _Extents.ContainsKey(iCabName))
			{	aXid = Submit(iReceiver, ref aAmpMsg, iCabName, 0, null, 0, null);
			}
			else
			{	aXid = SubmitError(iReceiver, (int)AErrorCodes.BadCabinet, ARequestType.ImportCabinet,
					ref aAmpMsg, iCabName, 0, null);
			}
			return aXid;
		}
		/// <summary>
		/// Determine open status of a context.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iContextName"></param>
		/// <returns></returns>
		public AXID IsContextOpen(AAsyncEventArgs iReceiver, string iContextName)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.IsContextOpen, ASocket.MSG_DELIM, iContextName);
			if (iContextName.Length > 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.UnkContextName, 
					ARequestType.IsContextOpen, ref aAmpMsg);
			return aXid;
		}
		/// <summary>
		/// Logoff from the AIS server.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <returns></returns>
		public AXID Logoff(AAsyncEventArgs iReceiver)
		{
			string aAmpMsg = string.Format(AAppMsg.Logoff, ASocket.MSG_DELIM);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Logon to the AIS server.
		/// </summary>
		/// <param name="iReceiver"></param>
		/// <param name="iUsername"></param>
		/// <param name="iPassword"></param>
		/// <returns></returns>
		public AXID Logon(AAsyncEventArgs iReceiver, string iUsername, string iPassword)
		{
			string aAmpMsg = string.Format(AAppMsg.Logon, ASocket.MSG_DELIM, iUsername, iPassword);
			return Submit(iReceiver, ref aAmpMsg);
		}
		/// <summary>
		/// Call server to create a cabinet.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iCabName">Cabinet name.</param>
		/// <param name="iCabPath">Cabinet path.</param>
		/// <returns>Request id.</returns>
		public AXID NewCabinet(AAsyncEventArgs iReceiver, string iCabName, string iCabPath)
		{
			AXID aXid = 0;
			int aStatus = 0;
			string aAmpMsg = string.Format(AAppMsg.NewCabinet, ASocket.MSG_DELIM, iCabName, iCabPath);

			if (iCabPath != null && iCabPath.Length > 0 && iCabName != null && iCabName.Length > 0)
			{
				if (_Extents.ContainsKey(iCabName))
				{
					aStatus = (int)AErrorCodes.CabinetOpen;
				}
				else
				{
					aXid = Submit(iReceiver, ref aAmpMsg);
				}
			}
			else
			{
				aStatus = (int)AErrorCodes.BadCabinet;
			}

			if (aStatus > 0)
			{
				aXid = SubmitError(iReceiver, aStatus, ARequestType.NewCabinet, ref aAmpMsg);
			}

			return aXid;
		}

		/// <summary>
		/// Connected event handler.
		/// </summary>
		/// <param name="iSource">Source object.</param>
		/// <param name="iArgs">Event arguments.</param>
		private void OnConnected(object iSource, EventArgs iArgs)
		{
			_ConnectionId = _Socket.SocketDescriptor();

			ReturnOutput(0, _OpenConnXid, 0, ARequestType.OpenConnection, 0, "true", null, "", "", "");
		}

		/// <summary>
		/// Disconnected event handler.
		/// </summary>
		/// <param name="iSource">Source object.</param>
		/// <param name="iArgs">Event arguments.</param>
		private void OnDisconnected(object iSource, EventArgs iArgs)
		{
			ConnectionClosed();
		}

		/// <summary>
		/// Response event handler.
		/// </summary>
		/// <param name="iSource">Source object.</param>
		/// <param name="iArgs">Event arguments.</param>
		private void OnResponse(object iSource, ResponseEventArgs iArgs)
		{
			int aReqId = 0;
			int aStatus = 0;
			int aRetValue = 0;
			int aOffset = 0;

			ARequestType aReqType = ARequestType.Unknown;
			byte[] aResBuffer = iArgs.ResponseBuffer;

			string aReqTypeStr = string.Empty;
			string aDisplay = string.Empty;
			string aError = string.Empty;
			string aOut = string.Empty;

			// extract request id
			aOffset = AUtilities.GetArgNumber(aResBuffer, aOffset, ref aReqId);

			// extract request type
			aOffset = AUtilities.GetArgString(aResBuffer, '\x7F', aOffset, ref aReqTypeStr);

			// get request name
			aReqTypeStr = aReqTypeStr.Trim().ToLower();
			if (_Ais.RequestTypes.ContainsKey(aReqTypeStr))
				aReqType = _Ais.RequestTypes[aReqTypeStr];

			// extract status
			aOffset = AUtilities.GetArgNumber(aResBuffer, aOffset, ref aStatus);

			// extract return value
			aOffset = AUtilities.GetArgNumber(aResBuffer, aOffset, ref aRetValue);

			// extract display
			aOffset = AUtilities.GetArgString(aResBuffer, '\x7F', aOffset, ref aDisplay);

			// extract error
			aOffset = AUtilities.GetArgString(aResBuffer, '\x7F', aOffset, ref aError);

			// extract out
			AUtilities.ByteToString(aResBuffer, aOffset, aResBuffer.Length - aOffset, ref aOut);

			ReturnOutput(0, aReqId, aStatus, aReqType, aRetValue, aOut, iArgs.DataBuffer, aDisplay, aError, "");
		}

		/// <summary>
		/// Socket error event handler.
		/// </summary>
		/// <param name="iSource">Source object.</param>
		/// <param name="iArgs">Event arguments.</param>
		public void OnSocketError(object iSource, SocketErrorEventArgs iArgs)
		{
			ARequestType aCurReqType = ARequestType.ConnectionError;
			int aCurXid = 0;

			if (_ClientRequests.Count > 0)
			{
				SortedDictionary<int, AClientRequest>.Enumerator aIter = _ClientRequests.GetEnumerator();
				
				aIter.MoveNext();
				aCurXid = aIter.Current.Key;
				aCurReqType = _ClientRequests[aCurXid].RequestType;
			}

			ReturnOutput(0, aCurXid, iArgs.Status, aCurReqType, 0, "", null, "", iArgs.Error, "");
		}
		
		/// <summary>
		/// Make contents of an existing cabinet visible.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iCabName">Cabinet name.</param>
		/// <param name="iCabPath">Cabinet path.</param>
		/// <returns>Request id.</returns>
		public AXID OpenCabinet(AAsyncEventArgs iReceiver, string iCabName, string iCabPath)
		{
			AXID aXid = 0;
			int aStatus = 0;

			string aAmpMsg = string.Format(AAppMsg.OpenCabinet, ASocket.MSG_DELIM,
				iCabName, iCabPath);

			if (iCabPath != null && iCabPath.Length > 0 && iCabName != null && iCabName.Length > 0)
			{
				if (_Extents.ContainsKey(iCabName))
				{
					aStatus = (int)AErrorCodes.CabinetOpen;
				}
				else
				{
					aXid = Submit(iReceiver, ref aAmpMsg);
				}
			}
			else
			{
				aStatus = (int)AErrorCodes.BadCabinet;
			}

			if (aStatus > 0)
			{
				aXid = SubmitError(iReceiver, aStatus, ARequestType.OpenCabinet, ref aAmpMsg);
			}

			return aXid;                
		}

		/// <summary>
		/// Establish an APP client connection to the server.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <returns>Request id.</returns>
		public AXID OpenConnection(AAsyncEventArgs iReceiver)
		{
			AXID aXid = 0;
			int aStatus = 0;
			int aPendingReq = 0;
			string aAmpMsg = string.Format(AAppMsg.OpenConnection, ASocket.MSG_DELIM);

			_Mutex.WaitOne();
			aPendingReq = _ClientRequests.Count;
			_Mutex.ReleaseMutex();

			if (_IsConnected)
				aStatus = 0;
			else if (_SessionId > 0)
				aStatus = (int)AErrorCodes.SessionOpen;
			else if (_HostDnsIp.Length == 0)
				aStatus = (int)AErrorCodes.UnkHost;
			else if (aPendingReq > 0)
				aStatus = (int)AErrorCodes.ReqPending;
			else
			{
				AClientRequest aReq = new AClientRequest();
				aReq.AmpMessage = aAmpMsg;
				aReq.Receiver = iReceiver;
				aReq.RequestType = ARequestType.OpenConnection;
				_Mutex.WaitOne();
				aXid = ++_Xid;
				_ClientRequests[aXid] = aReq;
				_Mutex.ReleaseMutex();

				_OpenConnXid = aXid;
				if (_Socket == null)
				{
					// create the socket
					_Socket = new ASocket(_ServerName, AisProtocolType.App);

					// register event handlers
					_Socket.ConnectedEvent +=new EventHandler(OnConnected);
					_Socket.DisconnectedEvent += new EventHandler(OnDisconnected);
					_Socket.ResponseEvent += new EventHandler<ResponseEventArgs>(OnResponse);
					_Socket.SocketErrorEvent += new EventHandler<SocketErrorEventArgs>(OnSocketError);
				}

				// open a connection to the server
				if (_Socket.OpenConnection(ref _HostDnsIp, _Port))
					aStatus = -1;
				else
					aStatus = (int)AErrorCodes.ReqPending;
			}

			if (aStatus >= 0)
				aXid = SubmitError(iReceiver, aStatus, ARequestType.OpenConnection, ref aAmpMsg);

			return aXid;
		}

		/// <summary>
		/// Create a new console output log for the current session.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iClear">Clear flag.</param>
		/// <param name="iRedirect">Redirect flag.</param>
		/// <param name="iSize">Maximum file size.</param>
		/// <param name="iStartAtNewLine">New line flag.</param>
		/// <returns>Request id.</returns>
		public AXID OpenConsoleLog(AAsyncEventArgs iReceiver, bool iClear, 
			bool iRedirect, int iSize, bool iStartAtNewLine)
		{
			string aAmpMsg = string.Format(AAppMsg.OpenConsoleLog, ASocket.MSG_DELIM,
				iClear, iRedirect, 0, iSize, iStartAtNewLine);

			return Submit(iReceiver, ref aAmpMsg);
		}

		/// <summary>
		/// Start up an application on the server.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iStartupPath">Start-up path.</param>
		/// <param name="iContextName">Context name.</param>
		/// <returns>Request id.</returns>
		public AXID OpenContext(AAsyncEventArgs iReceiver, string iStartupPath, string iContextName)
		{
			AXID aXid = -(int)AErrorCodes.UnkContextName;
			string aAmpMsg = string.Format(AAppMsg.OpenContext, ASocket.MSG_DELIM);

			if (iContextName != null && iContextName.Length > 0)
			{
				aXid = 0;
				aAmpMsg += string.Format(AAppMsg.OpenContextName, ASocket.MSG_DELIM, iContextName);
			}

			if (iStartupPath != null && iStartupPath.Length > 0)
			{
				aXid = 0;
				aAmpMsg += string.Format(AAppMsg.OpenContextPath, ASocket.MSG_DELIM, iStartupPath);
			}

			if (aXid == 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, -aXid, ARequestType.OpenContext, ref aAmpMsg);

			return aXid;
		}

		/// <summary>
		/// Make an existing agent or variable visible.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iExtentName">Extent name.</param>
		/// <param name="iFullNodeName">Node name.</param>
		/// <returns>Request id.</returns>
		public AXID OpenNode(AAsyncEventArgs iReceiver, string iExtentName, string iFullNodeName)
		{
			AXID aXid = 0;
			int aStatus = 0;
			string aAmpMsg = string.Format(AAppMsg.OpenNode, ASocket.MSG_DELIM, 
				iExtentName, iFullNodeName);

			if (iExtentName != null && iExtentName.Length > 0 && iFullNodeName != null && iFullNodeName.Length > 0)
			{
				if (_Extents.ContainsKey(iExtentName))
				{
					aXid = Submit(iReceiver, ref aAmpMsg, iFullNodeName, 0, null, 0, null);
				}
				else
				{
					aStatus = (int)AErrorCodes.NoExtent;
				}
			}
			else
			{
				aStatus = (int)AErrorCodes.BadNode;
			}

			if (aStatus > 0)
			{
				aXid = SubmitError(iReceiver, aStatus, ARequestType.OpenNode, ref aAmpMsg, iFullNodeName, 0, null);
			}

			return aXid;
		}

		/// <summary>
		/// Open a session on AIS.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iContextName">Context name.</param>
		/// <returns>Request id.</returns>
		public AXID OpenSession(AAsyncEventArgs iReceiver, string iContextName)
		{
			AXID aXid = 0;
			int aStatus = 0;
			string aAmpMsg = string.Format(AAppMsg.OpenSession, ASocket.MSG_DELIM,
				iContextName, _UserId);

			if (iContextName == null || iContextName.Length == 0)
				aStatus = (int)AErrorCodes.UnkContextName;
			else if (!_IsConnected)
				aStatus = (int)AErrorCodes.Disconnected;
			else if (_UserId <= 0)
				aStatus = (int)AErrorCodes.NoLogon;
			else if (_SessionId > 0)
				aStatus = 0;
			else
			{
				aStatus = -1;
				aXid = Submit(iReceiver, ref aAmpMsg);
			}

			if (aStatus >= 0)
				aXid = SubmitError(iReceiver, aStatus, ARequestType.OpenSession, ref aAmpMsg);

			return aXid;
		}

		/// <summary>
		/// Update nodes for the current extent and the current node path.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <returns>Request id.</returns>
		public AXID RefreshLevel(AAsyncEventArgs iReceiver)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Empty;

			if (_CurExtentName.Length > 0 && _Extents.ContainsKey(_CurExtentName))
			{
				string aNodePath = _Extents[_CurExtentName].NodePath;
				string aOptions = GetExtentTypeOptions(_CurExtentName);

				aAmpMsg = string.Format(AAppMsg.GetNextLevel, ASocket.MSG_DELIM,
					_CurExtentName, aNodePath, aOptions);

				aXid = Submit(iReceiver, ref aAmpMsg);
			}
			else
			{
				aAmpMsg = string.Format(AAppMsg.GetNextLevel, ASocket.MSG_DELIM,
					_CurExtentName, "none", "none");

				aXid = SubmitError(iReceiver, (int)AErrorCodes.NoExtent, ARequestType.GetNextLevel,
					ref aAmpMsg);
			}

			return aXid;
		}

		/// <summary>
		/// Sends a RegisterContext request to the AIS server.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iStartupPath">Start-up path.</param>
		/// <returns>Request id.</returns>
		public AXID RegisterContext(AAsyncEventArgs iReceiver, string iStartupPath)
		{
			string aAmpMsg = string.Format(AAppMsg.RegisterContext, ASocket.MSG_DELIM, iStartupPath);

			return Submit(iReceiver, ref aAmpMsg);
		}

		/// <summary>
		/// Returns a response to object that issued the request.
		/// </summary>
		/// <param name="iXid">Request id.</param>
		/// <param name="iStatus">Request status.</param>
		/// <param name="iReqType">Request type.</param>
		/// <param name="iRetValue">Return value.</param>
		/// <param name="iAisOut">Request output.</param>
		/// <param name="iData">Optional binary data.</param>
		/// <param name="iDisplay">Optional display message.</param>
		/// <param name="iError">Optional error message.</param>
		/// <param name="iClientData">Optional client data.</param>
		/// <param name="iClearReqQueueItem">Clear request queue item flag.</param>
		/// <remarks>The default receiver is used when the receiver was not specied.</remarks>
		private void ReturnMsg(AXID iXid, int iStatus, ARequestType iReqType, int iRetValue, 
			string iAisOut, byte[] iData, string iDisplay, 
			string iError, string iClientData, bool iClearReqQueueItem)
		{
			// get default receiver
			AAsyncEventArgs aReceiver = _DefaultReceiver;
			string aOut = iAisOut;
			string aClientData = string.Empty;
			int aStatus = 0;

			if (iXid > 0)
			{
				// check if the request id is valid
				_Mutex.WaitOne();
				if (_ClientRequests.ContainsKey(iXid))
				{
					// get the client that issued the request
					AClientRequest aReq = _ClientRequests[iXid];
					aClientData = aReq.ClientData;

					if (aReq.Receiver == null)
						aStatus = (int)AErrorCodes.NoClient;
					else
						aReceiver = aReq.Receiver;

					if (iClearReqQueueItem)
						_ClientRequests.Remove(iXid);
				}
				else
				{
					aStatus = (int)AErrorCodes.BadRequest;
				}
				_Mutex.ReleaseMutex();
			}
			else if (iStatus > 0)
			{
				SortedDictionary<int, AClientRequest>.Enumerator aEnum =
					_ClientRequests.GetEnumerator();
				int aXid = 0;
				AClientRequest aReq = null;

				_Mutex.WaitOne();
				// return an error to all pending requests
				while (aEnum.MoveNext())
				{
					aXid = aEnum.Current.Key;
					aReq = aEnum.Current.Value;

					if (aReq.Receiver == null)
					{
						aReceiver = _DefaultReceiver;
						aReceiver.Status = (int)AErrorCodes.NoClient;
					}
					else
					{
						aReceiver = aReq.Receiver;
						aReceiver.Status = iStatus;
					}

					aReceiver.ConnectionId = _ConnectionId;
					aReceiver.RequestId = aXid;
					aReceiver.RequestType = iReqType;
					aReceiver.ReturnValue = iRetValue;
					aReceiver.Out = aOut;
					aReceiver.Data = iData;
					aReceiver.Display = iDisplay;
					aReceiver.Error = iError;
					aReceiver.ClientData = aReq.ClientData;

					_Mutex.ReleaseMutex();

					// send event to subscriber
					aReceiver.OnCompleted(this);

					_Mutex.WaitOne();
				}
				_ClientRequests.Clear();
				_Mutex.ReleaseMutex();

				// quit unless error not returned
				if (aXid > 0)
					goto lEnd;
			}

			// if error, return to default receiver
			if (aStatus > 0)
			{
				string aError = _Ais.ErrorMessages[(int)AErrorCodes.Generic];
				if (_Ais.ErrorMessages.ContainsKey(iStatus))
					aError = _Ais.ErrorMessages[iStatus];

				Console.Error.WriteLine(
					"AAppClient::returnMsg(Xid:{0},Status:{1},ReqType:{2},RetValue:{3},Out:{4},Display:{5},Error:{6},ClientData:{7}) {8}",
					iXid, iStatus, _Ais.RequestNames[(int)iReqType], iRetValue, iAisOut,
					iDisplay, iError, aClientData, aError);

				aReceiver = _DefaultReceiver;
				aOut = "false";
			}

			// if there's no default receiver
			if (aReceiver == null)
			{
				Console.Error.WriteLine(
					"AAppClient::returnMsg(Xid:{0},Status:{1},ReqType:{2},RetValue:{3},Out:{4},Display:{5},Error:{6},ClientData:{7}) Null receiver",
					iXid, iStatus, _Ais.RequestNames[(int)iReqType], iRetValue, iAisOut,
					iDisplay, iError, aClientData);
			}
			else
			{
				// special handling for log request types
				switch (iReqType)
				{
					case ARequestType.LogAll:
					case ARequestType.LogAmp:
					case ARequestType.LogConsole:
					case ARequestType.LogSysMsg:
					case ARequestType.LogUserAccess:
						// emit log event
						if (_LogReceiver != null)
						{
							_LogReceiver.RequestType = iReqType;
							_LogReceiver.Out = aOut;
							_LogReceiver.OnCompleted(this);
						}
						break;
					default:
						// emit response event
						if (aReceiver != null)
						{
							aReceiver.ConnectionId = _ConnectionId;
							aReceiver.RequestId = iXid;
							aReceiver.Status = iStatus;
							aReceiver.RequestType = iReqType;
							aReceiver.ReturnValue = iRetValue;
							aReceiver.Out = aOut;
							aReceiver.Data = iData;
							aReceiver.Display = iDisplay;
							aReceiver.Error = iError;
							aReceiver.ClientData = iClientData;

							aReceiver.OnCompleted(this);
						}
						break;
				}
			}

		lEnd:
			return;
		}

		/// <summary>
		/// Handles the response from the AIS server.
		/// </summary>
		/// <param name="iConnectId">Connection id.</param>
		/// <param name="iXid">Request id.</param>
		/// <param name="iStatus">Status.</param>
		/// <param name="iReqType">Request type.</param>
		/// <param name="iRetValue">Return value.</param>
		/// <param name="iOut">Request output.</param>
		/// <param name="iData">Optional binary data.</param>
		/// <param name="iDisplay">Optional display message.</param>
		/// <param name="iError">Optional error message.</param>
		/// <param name="iClientData">Optional client data.</param>
		private void ReturnOutput(int iConnectId, AXID iXid, int iStatus, ARequestType iReqType, int iRetValue,
			string iOut, byte[] iData, string iDisplay, string iError, string iClientData)
		{
			string aOut = iOut;
			int aSessionId = _SessionId;
			bool aClearQueueItem = true;
			int aExpXid = 0;

			TraceLogWriteLine("AAppClient.ReturnOutput");
			TraceLogWriteLineIf(iConnectId > 0, "Connect Id = " + iConnectId.ToString());
			TraceLogWriteLine("Request Id = " + iXid.ToString());
			TraceLogWriteLine("Status = " + iStatus.ToString());
			TraceLogWriteLine("Request Type = " + iReqType.ToString());
			TraceLogWriteLine("Return Value = " + iRetValue.ToString());
			
			TraceLogWriteLineIf(iOut != null, "Out = " + iOut);
			TraceLogWriteLineIf(iData != null, "Data =  " + iData);

			TraceLogWriteLineIf(iDisplay != null && iDisplay.Length > 0, "Display = " + iDisplay);
			TraceLogWriteLineIf(iError != null && iError.Length > 0, "Error = " + iError);
			TraceLogWriteLineIf(iClientData != null && iClientData.Length > 0, "Client Data = " + iClientData);

			if (iXid > 0)
			{
				_Mutex.WaitOne();
				// check if we are expecting any response
				if (_ClientRequests.Count == 0)
				{
					Console.Error.WriteLine("AAppClient::ReturnOutput(), Unexpected response Xid = {0}", iXid);
				}
				else
				{
					// get the oldest request expected
					SortedDictionary<int, AClientRequest>.Enumerator aEnum = _ClientRequests.GetEnumerator();
					aEnum.MoveNext();
					aExpXid = aEnum.Current.Key;
					// CloseConnection is an expection, see ConnectionClosed() and SubmitError()
					if (aExpXid != iXid && iReqType != ARequestType.CloseConnection)
					{
						Console.Error.WriteLine("AAppClient::ReturnOutput(), Expected Xid = {0}, Received Xid = {1}", aExpXid, iXid);
					}
				}
				_Mutex.ReleaseMutex();
			}

			if (iStatus > 0)
				goto lErrorRet;

			switch (iReqType)
			{
				case ARequestType.GetContextParams:
					AUtilities.StringToStringDictionary(iOut, ref _ContextParams);
					break;
				case ARequestType.AddUser:
				case ARequestType.AmpMsg:
				case ARequestType.CloseCabinet:
				case ARequestType.CloseConnection:
				case ARequestType.CloseContext:
				case ARequestType.CloseSession:
				case ARequestType.CompileAgent:
				case ARequestType.CompileCabinet:
				case ARequestType.ConnectSession:
				case ARequestType.Debug:
				case ARequestType.DeleteUser:
				case ARequestType.EnableConsoleLog:
				case ARequestType.EraseNode:
				case ARequestType.Eval:
				case ARequestType.Execute:
				case ARequestType.ExportNode:
				case ARequestType.ExportCabinet:
				case ARequestType.GetConnectionStats:
				case ARequestType.GetConsoleLog:
				case ARequestType.GetContextId:
				case ARequestType.GetCurrentContexts:
				case ARequestType.GetDirInfo:
				case ARequestType.GetExeSession:
				case ARequestType.GetLogonStats:
				case ARequestType.GetRequestStats:
				case ARequestType.GetSessionId:
				case ARequestType.GetSessions:
				case ARequestType.GetSessionStats:
				case ARequestType.GetSessionUser:
				case ARequestType.GetSubscriptions:
				case ARequestType.GetUsers:
				case ARequestType.ImportCabinet:
				case ARequestType.IsContextBusy:
				case ARequestType.IsContextOpen:
				case ARequestType.LogSysMsg:
				case ARequestType.Noop:
				case ARequestType.OpenNode:
				case ARequestType.OpenCabinet:
				case ARequestType.OpenConsoleLog:
				case ARequestType.OpenContext:
				case ARequestType.RetFile:
				case ARequestType.RetQuery:
				case ARequestType.RetUrl:
				case ARequestType.RunScriptFile:
				case ARequestType.SaveNode:
				case ARequestType.SetBreakpoint:
				case ARequestType.SetEngineFlags:
				case ARequestType.SetErrorTrace:
				case ARequestType.SetEscape:
				case ARequestType.SetInstructionTrace:
				case ARequestType.SetJit:
				case ARequestType.SetLogLvl:
				case ARequestType.SetRules:
				case ARequestType.SetSubscriptions:
				case ARequestType.SetSysCheck:
				case ARequestType.ShowConsole:
				case ARequestType.UpdateUser:
					break;
				case ARequestType.FcnEngineState:
					_EngineFlags = (uint)iRetValue;
					aClearQueueItem = false;
					break;
				case ARequestType.Display:
				case ARequestType.LogAll:
				case ARequestType.LogAmp:
				case ARequestType.LogConsole:
				case ARequestType.LogReqHdr:
				case ARequestType.LogUserAccess:
				case ARequestType.LogStatus:
					aClearQueueItem = false;
					break;
				case ARequestType.FcnDebug:
					// Moved the debugger handling to a separate function
					if (ProcessDebug(iXid, iStatus, ref aOut, aClearQueueItem))
						return;
					else
						break;
				case ARequestType.GetExtentNames:
					ProcessGetExtentNames(ref aOut);
					break;
				case ARequestType.GetExtentTypes:
					ProcessGetExtentTypes(ref aOut);
					break;
				case ARequestType.GetNextLevel:
					ProcessGetNextLevel(ref aOut);
					break;
				case ARequestType.GetWorkspaceStatistics:
					aOut = string.Format("{0}{1}", "Workspace Statistics\n", aOut);
					break;
				case ARequestType.Logoff:
					ProcessLogoff(iStatus);
					break;
				case ARequestType.Logon:
					ProcessLogon(iStatus, iRetValue);
					break;
				case ARequestType.NewCabinet:
					break;
				case ARequestType.OpenConnection:
					ProcessOpenConnection(iStatus, ref aOut);
					break;
				case ARequestType.OpenSession:
					_Mutex.WaitOne();
					if (iRetValue > 0)
						aSessionId = _SessionId = iRetValue;
					else
						iRetValue = _SessionId;
					_Mutex.ReleaseMutex();
					break;
				case ARequestType.RegisterContext:
					ProcessRegisterContext(aOut);
					break;
				default:
					Console.Error.WriteLine(@"AAppClient::ReturnOutput(Xid:{0},Status:{1},ReqType:{2},
						RetValue:{3},Out:{4},Display:{5},Error:{6}) {7}", 
						iXid, iStatus, _Ais.RequestNames[(int)iReqType], iRetValue,
						aOut, iDisplay, iError, _Ais.ErrorMessages[(int)AErrorCodes.BadRequest]);
					break;
			}

		lErrorRet:

			// ReturnMsg
			ReturnMsg(iXid, iStatus, iReqType, iRetValue, aOut, iData, iDisplay, iError, iClientData, aClearQueueItem);

			if (iStatus < 1)
			{
				switch (iReqType)
				{
					case ARequestType.CloseSession:
						if (_SessionId == iRetValue)
							_SessionId = 0;
						break;
					case ARequestType.ConnectSession:
						_SessionId = iRetValue;
						break;
					default:
						break;
				}
			}

			return;
		}

		/// <summary>
		/// Run a script remotely.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iFilePath">File path.</param>
		/// <param name="iPrefix">Prefix.</param>
		/// <returns>Request id.</returns>
		public AXID RunRemoteScriptFile(AAsyncEventArgs iReceiver, string iFilePath, string iPrefix)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.RunScriptFile, ASocket.MSG_DELIM, iFilePath);

			if (iPrefix != null && iPrefix.Length > 0)
				aAmpMsg += string.Format(AAppMsg.RunScriptFilePrefix, ASocket.MSG_DELIM, iPrefix);
			if (iFilePath != null && iFilePath.Length > 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.BadFilename, ARequestType.RunScriptFile, ref aAmpMsg);

			return aXid;
		}

		/// <summary>
		/// Submit request to save a node.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iExtentName">Extent name.</param>
		/// <param name="iNodeName">Node name.</param>
		/// <param name="iText">Text comment.</param>
		/// <returns>Request id.</returns>
		public AXID SaveNode(AAsyncEventArgs iReceiver, string iExtentName,
			string iNodeName, string iText)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.SaveNode, ASocket.MSG_DELIM,
				iExtentName, iNodeName, iText);

			if (iExtentName != null && iExtentName.Length > 0 &&
				iNodeName != null && iNodeName.Length > 0 &&
				iText != null && iText.Length > 0)
			{
				aXid = Submit(iReceiver, ref aAmpMsg);
			}
			else
			{
				aXid = SubmitError(iReceiver, (int)AErrorCodes.BadNode,
					ARequestType.SaveNode, ref aAmpMsg);
			}

			return aXid;
		}            

		/// <summary>
		/// Set a breakpoint that suspends execution of the target agent.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iAgentName">Agent name.</param>
		/// <returns>Request id.</returns>
		public AXID SetBreakPoint(AAsyncEventArgs iReceiver, string iAgentName)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.SetBreakPoint, ASocket.MSG_DELIM, iAgentName);

			if (iAgentName != null && iAgentName.Length > 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.BadAgent, ARequestType.SetBreakpoint, ref aAmpMsg);

			return aXid;
		}

		/// <summary>
		/// Set the extent options.
		/// </summary>
		/// <param name="iExtentType">Extent type.</param>
		/// <param name="iOptions">Options.</param>
		/// <remarks>
		/// ExtentType(".Memory"), aOptions("#(1 1 1)").
		/// Note that this is a bit bogus. We don't really have "extent types" so
		/// we are using the "name" of "special" extents as the key for saving extent
		/// type options. These options are currently used only for passing optional
		/// arguments to the ais|getnextlevel command. Note that we only have this
		/// special processing for the .Memory cabinet right now and that this special
		/// handling sets the ShowLockedVariables and ShowSystemVariables options.
		/// </remarks>
		public void SetExtentTypeOptions(string iExtentType, string iOptions)
		{
			if (_ExtentTypeOptions.ContainsKey(iExtentType))
			{
				_ExtentTypeOptions[iExtentType] = iOptions;
			}
			else
			{
				_ExtentTypeOptions.Add(iExtentType, iOptions);
			}            
		}

		/// <summary>
		/// Set the current extent.
		/// </summary>
		/// <param name="iExtentName">One of the extent names loaded in AIS.</param>
		/// <returns>true if the given extent is valid.</returns>
		public bool SetCurrentExtent(string iExtentName)
		{
			bool aFound = false;
			if (_Extents.ContainsKey(iExtentName))
			{
				_CurExtentName = iExtentName;
				aFound = true;
			}
			return aFound;
		}

		/// <summary>
		/// Set one or more engine flags to bits in iFlags.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iFlags">Engine flags.</param>
		/// <returns>Request id.</returns>
		public AXID SetEngineFlags(AAsyncEventArgs iReceiver, uint iFlags)
		{
			string aAmpMsg = string.Format(AAppMsg.SetEngineFlags, ASocket.MSG_DELIM, iFlags);

			return Submit(iReceiver, ref aAmpMsg);
		}

		/// <summary>
		/// Causes the program to stop on an error.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iOnOff">Enable switch.</param>
		/// <returns>Request id.</returns>
		public AXID SetErrorTrace(AAsyncEventArgs iReceiver, bool iOnOff)
		{
			string aAmpMsg = string.Format(AAppMsg.SetErrorTrace, ASocket.MSG_DELIM, iOnOff);

			return Submit(iReceiver, ref aAmpMsg);
		}

		/// <summary>
		/// Set flag to stop execution of currently executing request of the target session.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iSessionId">Session id.</param>
		/// <returns>Request id.</returns>
		public AXID SetEscape(AAsyncEventArgs iReceiver, int iSessionId)
		{
			AXID aXid = 0;
			if (iSessionId == 0)
				iSessionId = _SessionId;

			string aAmpMsg = string.Format(AAppMsg.SetEscape, ASocket.MSG_DELIM, iSessionId);

			if (iSessionId > 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = -(int)AErrorCodes.SessionId;

			return aXid;
		}

		/// <summary>
		/// Enable debugging on the current program.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iOnoff">Switch flag.</param>
		/// <returns>Request id.</returns>
		public AXID SetInstructionTrace(AAsyncEventArgs iReceiver, bool iOnOff)
		{
			string aAmpMsg = string.Format(AAppMsg.SetInstructionTrace, ASocket.MSG_DELIM, iOnOff ? 1 : 0);

			return Submit(iReceiver, ref aAmpMsg);
		}

		/// <summary>
		/// Enable compilation to native code.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iOnOff">Switch flag.</param>
		/// <returns>Request id.</returns>
		public AXID SetJit(AAsyncEventArgs iReceiver, bool iOnOff)
		{
			string aAmpMsg = string.Format(AAppMsg.SetJit, ASocket.MSG_DELIM, iOnOff ? 1 : 0);

			return Submit(iReceiver, ref aAmpMsg);
		}

		/// <summary>
		/// Sends a SetLogLevel request to the AIS server.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iLogType">Log type.</param>
		/// <param name="iWarnLevel">Warning level.</param>
		/// <returns>Request id.</returns>
		public AXID SetLogLevel(AAsyncEventArgs iReceiver, ARequestType iLogType,
			AErrorLevel iWarnLevel)
		{
			string aAmpMsg = string.Format(AAppMsg.SetLogLevel, ASocket.MSG_DELIM,
				(int)iLogType, (int)iWarnLevel);

			return Submit(iReceiver, ref aAmpMsg);   
		}

		/// <summary>
		/// Sets the default log receiver.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		public void SetLogReceiver(AAsyncEventArgs iReceiver)
		{
			_LogReceiver = iReceiver;
		}

		/// <summary>
		/// Sends a SetRules request to the AIS server.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iRules">Rules.</param>
		/// <param name="iRemove">Remove flag.</param>
		/// <returns>Request id.</returns>
		public AXID SetRules(AAsyncEventArgs iReceiver, string iRules, bool iRemove)
		{
			string aAmpMsg = string.Format(AAppMsg.SetRules, ASocket.MSG_DELIM, iRules);

			if (iRemove)
				aAmpMsg += string.Format(AAppMsg.SetRulesMode, ASocket.MSG_DELIM, 1);

			return Submit(iReceiver, ref aAmpMsg);   
		}

		/// <summary>
		/// Sends a SetSubscriptions request to the AIS server.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iNewSessions">New sessions.</param>
		/// <param name="iOldSessions">Old sessions.</param>
		/// <returns>Request id.</returns>
		public AXID SetSubscriptions(AAsyncEventArgs iReceiver, string iNewSessions,
			string iOldSessions)
		{
			string aAmpMsg = string.Format(AAppMsg.SetSubscriptions, ASocket.MSG_DELIM,
				iNewSessions, iOldSessions);

			return Submit(iReceiver, ref aAmpMsg);
		}

		/// <summary>
		/// Enable/disable self-checking.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iOnOff">Switch flag.</param>
		/// <returns>Request id.</returns>
		public AXID SetSysCheck(AAsyncEventArgs iReceiver, bool iOnOff)
		{
			string aAmpMsg = string.Format(AAppMsg.SetSysCheck, ASocket.MSG_DELIM, iOnOff ? 1 : 0);

			return Submit(iReceiver, ref aAmpMsg);
		}

		/// <summary>
		/// Enable application tracing.
		/// </summary>
		/// <param name="iFilePath">Trace file path.</param>
		public void SetTraceFile(string iFilePath, bool iAppend)
		{
			try
			{
				if (_TraceFile != null)
				{
					_TraceFile.WriteLine("--- END ---");
					_TraceFile.Close();
					_TraceFile = null;
				}
			  
				if (iFilePath != null && iFilePath.Length > 0)
				{
					_TraceFile = new StreamWriter(iFilePath, iAppend);
					_TraceFile.AutoFlush = true;

					_TraceFile.WriteLine("--- BEGIN ---");
				}
			}
			catch (Exception)
			{

			}
		}

		/// <summary>
		/// Display results of executing an expression.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iText">Text.</param>
		/// <returns>Request id.</returns>
		public AXID ShowConsoleSelection(AAsyncEventArgs iReceiver, string iText)
		{
			AXID aXid = 0;
			string aAmpMsg = string.Format(AAppMsg.ShowConsole, ASocket.MSG_DELIM, iText);
			if (iText != null && iText.Length > 0)
				aXid = Submit(iReceiver, ref aAmpMsg);
			else
				aXid = SubmitError(iReceiver, (int)AErrorCodes.NoInput, ARequestType.ShowConsole, ref aAmpMsg);

			return aXid;
		}

		/// <summary>
		/// Submit a request to the AIS server.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="irAmpMsg">AMP message.</param>
		/// <returns>Request id.</returns>
		public AXID Submit(AAsyncEventArgs iReceiver, ref string irAmpMsg)
		{
			return Submit(iReceiver, ref irAmpMsg, null, 0, null, 0, null);
		}

		/// <summary>
		/// Submit a request to the AIS server.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="irAmpMsg">AMP message.</param>
		/// <param name="iClientData">Optional string kept locally.</param>
		/// <param name="iClientValue">Optional integer kept locally.</param>
		/// <param name="iData">Optional binary data sent to the server.</param>
		/// <param name="iDataSize">Length of optional binary data.</param>
		/// <param name="iCustomData">Optional object reference kept locally.</param>
		/// <returns>Request id.</returns>
		public AXID Submit(AAsyncEventArgs iReceiver, ref string irAmpMsg,
			string iClientData, int iClientValue, byte[] iData, int iDataSize, object iCustomData)
		{
			int aRetValue = 0;
			int aStatus = 0;
			string aOut = string.Empty;
			string aSpeechAct = string.Empty;
			ARequestType aReqType = ARequestType.AmpMsg;

			TraceLogWriteLine("AAppClient.Submit");
			TraceLogWriteLineIf(irAmpMsg.Length > 0, "AmpMsg = " + irAmpMsg);
			TraceLogWriteLineIf(iClientData != null && iClientData.Length > 0, "Client Data = " + iClientData);
			TraceLogWriteLineIf(iClientValue > 0, "Client Value = " + iClientValue);

			if (!_IsConnected)
			{
				aStatus = (int)AErrorCodes.Disconnected;
				goto lError;
			}
			else if (irAmpMsg.Length == 0)
			{
				aReqType = ARequestType.Unknown;
			}
			else if (irAmpMsg.StartsWith("_ais"))
			{
				// determine the request type
				int aBeg = irAmpMsg.IndexOf(ASocket.MSG_DELIM) + 1;
				int aEnd = irAmpMsg.IndexOf(ASocket.MSG_DELIM, aBeg);

				if (aEnd < 0)
					aEnd = irAmpMsg.Length;

				aSpeechAct = irAmpMsg.Substring(aBeg, aEnd - aBeg);

				if (aSpeechAct.Length == 0)
					aSpeechAct = "noop";

				if (_Ais.RequestTypes.ContainsKey(aSpeechAct))
					aReqType = _Ais.RequestTypes[aSpeechAct];
				else
					aReqType = ARequestType.Unknown;
			}
			else
			{
				aReqType = ARequestType.AmpMsg;
			}

			switch (aReqType)
			{
				// the following types of request are forwarded to the AIS server
				case ARequestType.AmpMsg:
				case ARequestType.AddUser:
				case ARequestType.CloseCabinet:
				case ARequestType.CloseContext:
				case ARequestType.CloseConnection:
				case ARequestType.CloseSession:
				case ARequestType.CompileAgent:
				case ARequestType.CompileCabinet:
				case ARequestType.ConnectSession:
				case ARequestType.Debug:
				case ARequestType.DeleteUser:
				case ARequestType.EnableConsoleLog:
				case ARequestType.EraseNode:
				case ARequestType.Eval:
				case ARequestType.Execute:
				case ARequestType.ExportNode:
				case ARequestType.ExportCabinet:
				case ARequestType.GetNextLevel:
				case ARequestType.GetConnectionStats:
				case ARequestType.GetConsoleLog:
				case ARequestType.GetContextId:
				case ARequestType.GetContextParams:
				case ARequestType.GetCurrentContexts:
				case ARequestType.GetDirInfo:
				case ARequestType.GetExeSession:
				case ARequestType.GetExtentNames:
				case ARequestType.GetExtentTypes:
				case ARequestType.GetLogonStats:
				case ARequestType.GetRequestStats:
				case ARequestType.GetSessions:
				case ARequestType.GetSessionStats:
				case ARequestType.GetSessionUser:
				case ARequestType.GetSubscriptions:
				case ARequestType.GetUsers:
				case ARequestType.GetWorkspaceStatistics:
				case ARequestType.ImportCabinet:
				case ARequestType.IsContextBusy:
				case ARequestType.IsContextOpen:
				case ARequestType.LogSysMsg:
				case ARequestType.Logoff:
				case ARequestType.Logon:
				case ARequestType.NewCabinet:
				case ARequestType.Noop:
				case ARequestType.OpenCabinet:
				case ARequestType.OpenNode:
				case ARequestType.OpenConsoleLog:
				case ARequestType.OpenContext:
				case ARequestType.OpenSession:
				case ARequestType.RegisterContext:
				case ARequestType.RetFile:
				case ARequestType.RunScriptFile:
				case ARequestType.SaveNode:
				case ARequestType.SetBreakpoint:
				case ARequestType.SetEngineFlags:
				case ARequestType.SetErrorTrace:
				case ARequestType.SetEscape:
				case ARequestType.SetInstructionTrace:
				case ARequestType.SetJit:
				case ARequestType.SetLogLvl:
				case ARequestType.SetRules:
				case ARequestType.SetSubscriptions:
				case ARequestType.SetSysCheck:
				case ARequestType.ShowConsole:
				case ARequestType.UpdateUser:
					break;

				default:
					// process as local request
					aStatus = -1;
					aReqType = ARequestType.AmpMsg;

					if (aSpeechAct == "getcurrentnodenames")
					{
						string[] aNodeNames = GetCurrentNodeNames();
						if (aNodeNames != null)
							aOut = string.Join("\t", aNodeNames);
					}
					else if (aSpeechAct == "getcurrentextentname")
					{
						aOut = _CurExtentName;
					}
					else if (aSpeechAct == "getcurrentextentnames")
					{
						string[] aExtentNames = GetCurrentExtentNames();
						if (aExtentNames != null)
							aOut = string.Join("\t", aExtentNames);
					}
					else if (aSpeechAct == "getcurrentrepositorynames")
					{
						string[] aRepositoryNames = GetCurrentRepositoryNames();
						if (aRepositoryNames != null)
							aOut = string.Join("\t", aRepositoryNames);
					}
					else if (aSpeechAct == "getcurrentsessionid")
					{
						aRetValue = _SessionId;
					}
					else
					{
						aOut = _Ais.ErrorMessages[(int)AErrorCodes.UnkAct];
					}
					break;
			}

		lError:
			AXID aXid = 0;
			if (aStatus > 0)
			{
				// In case of error, return error response to caller
				SubmitError(iReceiver, aStatus, aReqType, ref irAmpMsg, iClientData, iClientValue, iCustomData);
			}
			else
			{
				if (aReqType != ARequestType.SetEscape)
				{
					AClientRequest aReqSt = new AClientRequest();
					aReqSt.AmpMessage = irAmpMsg;
					aReqSt.ClientData = iClientData;
					aReqSt.ClientValue = iClientValue;
					aReqSt.Receiver = iReceiver;
					aReqSt.RequestType = aReqType;
					aXid = ++_Xid;
					_Mutex.WaitOne();
					_ClientRequests.Add(aXid, aReqSt);
					_Mutex.ReleaseMutex();
				}

				// local request, return response
				if (aStatus < 0)
				{
					ReturnOutput(0, aXid, 0, aReqType, aRetValue, aOut, null, "", "", "");
				}
				else
				{
					string aRequest = string.Format("{0}{1}{2}", aXid, ASocket.MSG_DELIM, irAmpMsg);
					
					TraceLogWriteLine(string.Copy(aRequest).Replace(ASocket.MSG_DELIM, '|'));

					_Socket.Submit(ref aRequest, iData, iDataSize);
				}
			}

			return aXid;
		}

		/// <summary>
		/// Returns an error response for a failed request.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iStatus">Status code.</param>
		/// <param name="iReqType">Request type.</param>
		/// <param name="irAmpMsg">AMP message.</param>
		/// <returns>Request id.</returns>
		private AXID SubmitError(AAsyncEventArgs iReceiver, int iStatus, ARequestType iReqType,
			ref string irAmpMsg)
		{
			return SubmitError(iReceiver, iStatus, iReqType, ref irAmpMsg, null, 0, null);
		}

		/// <summary>
		/// Returns an error reponse for a failed request.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iStatus">Status code.</param>
		/// <param name="iReqType">Request type.</param>
		/// <param name="irAmpMsg">AMP message.</param>
		/// <param name="iClientData">Optional string from the request.</param>
		/// <param name="iClientValue">Optional value from the request.</param>
		/// <param name="iCustomData">Optional object reference from the request.</param>
		/// <returns>Request id.</returns>
		private AXID SubmitError(AAsyncEventArgs iReceiver, int iStatus, ARequestType iReqType,
			ref string irAmpMsg, string iClientData, int iClientValue, object iCustomData)
		{
			string aOut = (iStatus == 0 ? "true" : "");

			AClientRequest aReqSt = new AClientRequest();
			aReqSt.AmpMessage = irAmpMsg;
			aReqSt.ClientData = iClientData;
			aReqSt.ClientValue = iClientValue;
			aReqSt.CustomData = iCustomData;
			aReqSt.Receiver = iReceiver;
			aReqSt.RequestType = iReqType;

			_Mutex.WaitOne();
			_ClientRequests.Add(++_Xid, aReqSt);
			_Mutex.ReleaseMutex();

			ReturnOutput(0, _Xid, iStatus, iReqType, 0, aOut, null, "", _Ais.ErrorMessages[iStatus], "");

			return _Xid;
		}

		/// <summary>
		/// Calculates the number of days elapsed since 1/1/2000.
		/// </summary>
		/// <returns>Number of days since 1/1/2000.</returns>
		public int Today()
		{
			return (DateTime.Today.Subtract(_Y2K)).Days;
		}

		/// <summary>
		/// Write trace log entry.
		/// </summary>
		/// <param name="iText">Trace log data.</param>
		private void TraceLogWriteLine(string iText)
		{
			if (_TraceFile != null)
			{
				_TraceFile.WriteLine("{0}: {1}", DateTime.Now.TimeOfDay, iText);
			}
		}

		/// <summary>
		/// Write trace log if condition is satisfied.
		/// </summary>
		/// <param name="iCond">Log switch.</param>
		/// <param name="iText">Trage log data.</param>
		private void TraceLogWriteLineIf(bool iCond, string iText)
		{
			if (iCond)
			{
				TraceLogWriteLine(iText);
			}
		}
		/// <summary>
		/// Request update of AIS user account.
		/// </summary>
		/// <param name="iReceiver">Response handler.</param>
		/// <param name="iUserId">User Id of account to update.</param>
		/// <param name="iUsername">New username.</param>
		/// <param name="iPassword">New password.</param>
		/// <param name="iSecurityLevel">New security level (0-7).</param>
		/// <param name="iEndDate">New expiration date.</param>
		/// <param name="iComment">New comment.</param>
		/// <returns>Request id.</returns>
		public AXID UpdateUser(AAsyncEventArgs iReceiver, int iUserId, string iUsername,
			string iPassword, int iSecurityLevel, DateTime iEndDate, string iComment)
		{
			string aAmpMsg = string.Format(AAppMsg.UpdateUser, ASocket.MSG_DELIM,
				iUserId, iUsername, iPassword, iSecurityLevel, iEndDate.ToString("MM/dd/yyyy"),
				iComment);
			return Submit(iReceiver, ref aAmpMsg);
		}
		#region Special Processing for Responses
		private bool ConnectionClosed()
		{	if (_IsConnected && _DefaultReceiver != null)
			{	if (_CloseConnXid != 0)
				{	ReturnMsg(_CloseConnXid, 0, ARequestType.CloseConnection, 0, "true", null, "", "", "", true);
				}
				else
				{	string aAmpMsg = string.Format(AAppMsg.ConnectionClosed, ASocket.MSG_DELIM);
					SubmitError(_DefaultReceiver, (int)AErrorCodes.LostConnection,
						ARequestType.CloseConnection, ref aAmpMsg, _ServerName, 0, null);
				}
			}
			_ClientRequests.Clear();
			_ContextParams.Clear();
			_Extents.Clear();
			_IsConnected = false;
			_SessionId = 0;
			_Xid = 0;
			_OpenConnXid = 0;
			_CloseConnXid = 0;
			return true;
		}

		private void ProcessGetExtentNames(ref string iOut)
		{
			string[] aExtentNames = iOut.Split("\t".ToCharArray());
			// browseAgent.getExtentNames returns #void if no extents exist
			if (aExtentNames.Length == 0 || Array.IndexOf<string>(aExtentNames, "#void") != -1)
			{	_Extents.Clear();
				_CurExtentName = string.Empty;
			}
			else
			{	// reset the internal state variable in case the list has changed
				_Extents.Clear();
				foreach (string aExtentName in aExtentNames)
					_Extents.Add(aExtentName, new AExtentInfo());
			}
		}
		private void ProcessGetExtentTypes(ref string iOut)
		{
			// make sure the return is not an error
			if (!AUtilities.ErrorReturned(iOut) && _Extents.ContainsKey(_CurExtentName))
			{	string[] aTypes = iOut.Split("\n".ToCharArray());
				AExtentInfo aExtentInfo = _Extents[_CurExtentName];
				string aTypeName = string.Empty;
				string[] aField = null;
				foreach (string aType in aTypes)
				{	aField = aType.Split("\t".ToCharArray());
					if (aField.Length > 1)
					{	aTypeName = aField[0];
						aExtentInfo.ExtentTypes[aTypeName] = new AExtentTypeInfo(ref aTypeName, ref aField[1]);
					}
				}
			}
		}

		private void ProcessGetNextLevel(ref string iOut)
		{
			if (!_Extents.ContainsKey(_CurExtentName))
				return;

			AExtentInfo aExtentInfo = _Extents[_CurExtentName];
			string[] aLines = iOut.Split("\n".ToCharArray(), StringSplitOptions.None);
			if (aLines.Length < 2)
				return;
			aExtentInfo.Nodes.Clear();
			string[] aField = null;
			foreach (string aLine in aLines)
			{	aField = aLine.Split("\t".ToCharArray(), StringSplitOptions.None);
				if (aField[0] == "Current")
					aExtentInfo.NodePath = aField[1];
				else if (aField.Length >= 8)
				{	ANodeInfo aNodeInfo = new ANodeInfo(aField[0], aField[1], aField[2], aField[3],
						aField[4], aField[5], aField[6], aField[7]);
					aExtentInfo.Nodes.Add(aNodeInfo);
				}
			}
		}

		private bool ProcessDebug(AXID iXid, int iStatus, ref string iOut, bool iClearQueueItem)
		{
			// first, make sure the debugger is active
			ReturnMsg(iXid, iStatus, ARequestType.ProcSetDebuggerActive, 0, "", null, "", "", "", iClearQueueItem);

			// convert string into list
			string[] aDebugInfo = iOut.Split("\x7F".ToCharArray());
			int aSelectedLine = 0;
			int aNumCodeLines = 0;
			int aNumInfoLines = 0;
			int aNumCodeLinesIdx = 0;
			int aNumInfoLinesIdx = 0;
			string[] aLines = null;
			int aNumLines = 0;
			int aNum = 0;
			bool aLastLine = false;
			if (aDebugInfo.Length < 5)
			{
				Console.Error.WriteLine("AAppClient::ProcessDebug(), FcnDebug - Missing Header");
				return false;
			}
			// first element contains the title
			ReturnMsg(iXid, iStatus, ARequestType.LogStatus, 0, aDebugInfo[0], 
				null, "", "", "", iClearQueueItem);

			// second element contains the prompt
			ReturnMsg(iXid, iStatus, ARequestType.ProcDebugLineEdit, 0, aDebugInfo[1], 
				null, "", "", "", iClearQueueItem);

			// third element contains the selected line in the code block
			aSelectedLine = int.Parse(aDebugInfo[2]);

			// fourth element contains the number of code lines
			aNumCodeLines = int.Parse(aDebugInfo[3]);

			// fifth element contains the number of info lines
			aNumInfoLines = int.Parse(aDebugInfo[4]);
			aNumCodeLinesIdx = 5;

			// get the next aNumCodeLines codelines
			if (aNumCodeLines > 0)
			{	iOut = string.Empty;
				if (aNumCodeLines > (aDebugInfo.Length - aNumCodeLinesIdx))
				{	iOut = "AAppClient::ProcessDebug(), FcnDebug - Missing code lines\n";
					Console.Error.Write(iOut);
					aNumCodeLines = (aDebugInfo.Length - aNumCodeLinesIdx);
				}
				for (int i = 0; i < aNumCodeLines; i++)
				{	iOut += aDebugInfo[aNumCodeLinesIdx + i];
					if (i < (aNumCodeLines - 1))
						iOut += "\x7F";
				}
				ReturnMsg(iXid, iStatus, ARequestType.ProcCodeLines, aSelectedLine, iOut, 
					null, "", "", "", iClearQueueItem);
			}
			// extract aNumInfoLines from aDebugInfo.
			// extract tab-delimited lines out of each extracted line.
			// some info lines may contain tab separators.
			// these lines need to be split into individual lines.
			// this is a hack to get around some limitations of the original engine interface.
			// work at the engine level would get rid of this problem.
			// it is caused by the way the descent through variables in the info window is processed.
			// see the ADebugPage::onVarListDoubleClicked for more info.
			aNumInfoLinesIdx = aNumCodeLinesIdx + aNumCodeLines;
			if (aNumInfoLines > 0)
			{	iOut = string.Empty;
				if (aNumInfoLines > (aDebugInfo.Length - aNumInfoLines))
				{	iOut = "AAppClient::ReturnOutput(), FcnDebug - Missing info lines\n";
					Console.Error.Write(iOut);
					aNumInfoLines = (aDebugInfo.Length - aNumInfoLines);
				}
				for (int i = 0; i < aNumInfoLines;)
				{	aLines = aDebugInfo[aNumInfoLinesIdx + i].Split("\t".ToCharArray());
					aNum = aLines.Length;
					aLastLine = (++i >= aNumInfoLines);
					for (int j = 0; j < aNum; )
					{	iOut += aLines[j];
						++aNumLines;
						if (++j < aNum || !aLastLine)
							iOut += "\x7F";
					}
				}
				ReturnMsg(iXid, iStatus, ARequestType.ProcInfoLines, aNumLines, iOut,
					null, "", "", "", iClearQueueItem);
			}
			return true;
		}

		private void ProcessLogoff(int iStatus)
		{
			if (iStatus < 1)
			{	_Mutex.WaitOne();
				_UserId = 0;
				_Mutex.ReleaseMutex();
			}
		}

		private void ProcessLogon(int iStatus, int iRetValue)
		{
			_Mutex.WaitOne();
			if (iStatus < 1 && iRetValue > 0)
				_UserId = iRetValue;
			else
				_UserId = 0;
			_Mutex.ReleaseMutex();
		}

		private void ProcessOpenConnection(int iStatus, ref string iOut)
		{
			if (iStatus > 0)
				iOut = "Unable to connect to server";
			else
			{	_Mutex.WaitOne();
				_IsConnected = true;
				_Mutex.ReleaseMutex();
			}
		}

		private void ProcessRegisterContext(string iOut)
		{
			// this block doesn't change any class variables
			if (iOut.Length > 0)
			{	string[] aTkns = iOut.Split("\x7F".ToCharArray());
				string aContextName = string.Empty;
				string aRegistrationMsgs = string.Empty;

				if (aTkns.Length > 0)
				{	aContextName = aTkns[0];
					if (aTkns.Length > 1)
						aRegistrationMsgs = aTkns[1];
				}
			}
		}
		#endregion

		#region Properties
		public int ConnectionId
		{
			get { return _ConnectionId; }
		}

		public string ContextName
		{
			get { return _ContextName; }
			set { _ContextName = value; }
		}

		public string CurrentExtentName
		{
			get { return _CurExtentName; }
		}

		public AAsyncEventArgs DefaultReceiver
		{
			set { _DefaultReceiver = value; }
		}

		public string Host
		{
			get { return _HostDnsIp; }
			set { _HostDnsIp = value; }
		}

		public bool IsConnected
		{
			get { return _IsConnected; }
		}

		public bool IsErrorTrace
		{
			get { return ((_EngineFlags & SBGLUE_ERROR_TRACE) != 0); }
		}

		public bool IsInstructionTrace
		{
			get { return ((_EngineFlags & SBGLUE_INSTRUCTION_TRACE) != 0); }
		}

		public bool IsJit
		{
			get { return ((_EngineFlags & SBGLUE_JITON) != 0); }
		}

		public bool IsSysCheck
		{
			get { return ((_EngineFlags & SBGLUE_SYSCHECKON) != 0); }
		}

		public ushort Port
		{
			get { return _Port; }
			set { _Port = value; }
		}

		public SortedDictionary<int, AClientRequest>.ValueCollection RequestQueue
		{
			get { return _ClientRequests.Values; }
		}

		public string ServerName
		{
			get { return _ServerName; }
		}

		public int SessionId
		{
			get { return _SessionId; }
		}

		public int UserId
		{
			get { return _UserId; }
		}
		#endregion

		#region Class Constants
		private static int SBGLUE_ERROR_TRACE = 0x01;
		private static int SBGLUE_INSTRUCTION_TRACE = 0x02;
		private static int SBGLUE_SYSCHECKON = 0x04;
		private static int SBGLUE_JITON = 0x10;
		#endregion

		#region Member Variables
		private AGlobals _Ais;
		private Mutex _Mutex;

		// Table of client request objects
		private SortedDictionary<int, AClientRequest> _ClientRequests;

		// Client parameter
		private DateTime _Y2K;

		// Current context name
		private string _ContextName;
		// Context-specific parameters
		private SortedDictionary<string,string> _ContextParams;
		// Default receiver for response
		private AAsyncEventArgs _DefaultReceiver;
		// Default receiver for log response
		private AAsyncEventArgs _LogReceiver;
		
		// Current engine flags
		private uint _EngineFlags;
		// Server DNS name or IP address
		private string _HostDnsIp;
		// Connection to AIS established
		private bool _IsConnected;
		// Server port
		private ushort _Port;
		// Server name
		private string _ServerName;
		// Session Id
		private int _SessionId;
		// Socket communication layer object
		private ASocket _Socket;
		// User Id (from Server)
		private int _UserId;
		// Request Id
		private int _Xid;
		// Connection Id
		private int _ConnectionId;
		// Request Id for OpenConnection
		private int _OpenConnXid;
		// Request Id for CloseConnection
		private int _CloseConnXid;
		
		// Memory tab state
		private List<string> _BackHistory;
		
		// Cabinet state
		private string _CurExtentName;
		private Dictionary<string, AExtentInfo> _Extents;

		// DatamineAgent state
		private List<string> _ColNames;
		private List<string> _RepositoryNames;
		private Dictionary<string, string> _ExtentTypeOptions;

		// Trace log object
		private StreamWriter _TraceFile;
		#endregion
	}
}
