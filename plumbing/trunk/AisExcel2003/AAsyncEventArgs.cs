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
using System;
using System.Collections.Generic;
using System.Text;

namespace AppClient
{
    /// <summary>
    /// Asynchronous response object of the AppClient that is used to deliver responses
    /// to AppClient users.
    /// </summary>
    public class AAsyncEventArgs : EventArgs
    {
        #region Constructor
        /// <summary>
        /// Constructor.
        /// </summary>
        public AAsyncEventArgs()
        {
            // Initialize member variables
            _ConnectId = 0;
            _ReqId = 0;
            _Status = 0;
            _ReqType = ARequestType.Unknown;
            _RetVal = 0;
            _Out = null;
            _Data = null;
            _Display = null;
            _Error = null;
            _ClientData = null;
        }
        #endregion

        #region Event/s
        /// <summary>
        /// Response event subscribers.
        /// </summary>
        public event EventHandler<AAsyncEventArgs> Completed;
        #endregion

        #region Methods
        /// <summary>
        /// Triggers the event handler.
        /// </summary>
        /// <param name="iSource">Event source.</param>
        internal void OnCompleted(object iSource)
        {
            Completed(iSource, this);
        }
        #endregion

        #region Properties
        /// <summary>
        /// Connection Id.
        /// </summary>
        public int ConnectionId
        {
            get { return _ConnectId; }
            set { _ConnectId = value; }
        }
        /// <summary>
        /// Request Id.
        /// </summary>
        public int RequestId
        {
            get { return _ReqId; }
            set { _ReqId = value; }
        }
        /// <summary>
        /// Request status.
        /// </summary>
        public int Status
        {
            get { return _Status; }
            set { _Status = value; }
        }
        /// <summary>
        /// Request type.
        /// </summary>
        public ARequestType RequestType
        {
            get { return _ReqType; }
            set { _ReqType = value; }
        }
        /// <summary>
        /// Return value of response.
        /// </summary>
        public int ReturnValue
        {
            get { return _RetVal; }
            set { _RetVal = value; }
        }
        /// <summary>
        /// Return output of response.
        /// </summary>
        public string Out
        {
            get { return _Out; }
            set { _Out = value; }
        }
        /// <summary>
        /// Optional data.
        /// </summary>
        public byte[] Data
        {
            get { return _Data; }
            set { _Data = value; }
        }
        /// <summary>
        /// Optional display message.
        /// </summary>
        public string Display
        {
            get { return _Display; }
            set { _Display = value; }
        }
        /// <summary>
        /// Optional error message.
        /// </summary>
        public string Error
        {
            get { return _Error; }
            set { _Error = value; }
        }
        /// <summary>
        /// Optional data the client wishes to retain.
        /// </summary>
        public string ClientData
        {
            get { return _ClientData; }
            set { _ClientData = value; }
        }
        #endregion

        #region Private Members
        private int _ConnectId;
        private int _ReqId;
        private int _Status;
        private ARequestType _ReqType;
        private int _RetVal;
        private string _Out;
        private byte[] _Data;
        private string _Display;
        private string _Error;
        private string _ClientData;
        #endregion
    }
}
