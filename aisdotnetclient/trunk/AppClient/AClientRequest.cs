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
        public AReturnReceiver Receiver
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
        private AReturnReceiver _Receiver;
        private ARequestType _ReqType;

        #endregion
    }
}
