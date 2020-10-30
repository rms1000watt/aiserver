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
    /// AppClient Request Information.
    /// </summary>
    public class AClientRequest
    {
        #region Constructor
        /// <summary>
        /// Constructor.
        /// </summary>
        public AClientRequest()
        {
            _AmpMsg = null;
            _ClientData = null;
            _ClientValue = 0;
            _ReqType = ARequestType.Unknown;
            _Receiver = null;
        }
        #endregion

        #region Properties
        /// <summary>
        /// AMP Message.
        /// </summary>
        public string AmpMessage
        {
            get { return _AmpMsg; }
            set { _AmpMsg = value; }
        }
        /// <summary>
        /// Optional Client Data.
        /// </summary>
        public string ClientData
        {
            get { return _ClientData; }
            set { _ClientData = value; }
        }
        /// <summary>
        /// Optional Client Value.
        /// </summary>
        public int ClientValue
        {
            get { return _ClientValue; }
            set { _ClientValue = value; }
        }
        /// <summary>
        /// Optional Custom Data.
        /// </summary>
        public object CustomData
        {
            get { return _CustomData; }
            set { _CustomData = value; }
        }
        /// <summary>
        /// Receiver.
        /// </summary>
        public AAsyncEventArgs Receiver
        {
            get { return _Receiver; }
            set { _Receiver = value; }
        }
        /// <summary>
        /// Request Type.
        /// </summary>
        public ARequestType RequestType
        {
            get { return _ReqType; }
            set { _ReqType = value; }
        }
        #endregion

        #region Private Members
        private string _AmpMsg;
        private string _ClientData;
        private int _ClientValue;
        private object _CustomData;
        private AAsyncEventArgs _Receiver;
        private ARequestType _ReqType;
        #endregion
    }
}
