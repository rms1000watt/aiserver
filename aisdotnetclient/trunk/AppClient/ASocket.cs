/**
 * AIS Socket
 * 
 * Copyright: Copyright 2008 Investment Science Corporation. All rights reserved.
 * 
 * Change History
 * Version  Date        Who      Change
 * 0.0001   04/28/2008  fchua    Initial Version
 */

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Diagnostics;

namespace AppClient
{
    /// <summary>
    /// Supported Protocols by the AppClient.
    /// </summary>
    public enum AisProtocolType
    {
        App = 1,
        Http = 2,
        Xml = 3
    }

    /// <summary>
    /// APP Read States.
    /// </summary>
    public enum AppReadState
    {
        None,
        ReadingResponseLength,
        ReadingDataLength,
        ReadingResponse,
        ReadingData
    }

    /// <summary>
    /// Response Event Data Container.
    /// </summary>
    public class ResponseEventArgs : EventArgs
    {
        public ResponseEventArgs()
        {
            _ResponseBuffer = null;
            _CookieBuffer = null;
            _DataBuffer = null;
        }

        public byte[] ResponseBuffer
        {
            get { return _ResponseBuffer; }
            set { _ResponseBuffer = value; }
        }

        public byte[] CookieBuffer
        {
            get { return _CookieBuffer; }
            set { _CookieBuffer = value; }
        }

        public byte[] DataBuffer
        {
            get { return _DataBuffer; }
            set { _DataBuffer = value; }
        }

        private byte[] _ResponseBuffer;
        private byte[] _CookieBuffer;
        private byte[] _DataBuffer;
    }

    /// <summary>
    /// Socket Error Event Data Container.
    /// </summary>
    public class SocketErrorEventArgs : EventArgs
    {
        public SocketErrorEventArgs()
        {
            _Status = 0;
            _Error = null;
        }

        public int Status
        {
            get { return _Status; }
            set { _Status = value; }
        }

        public string Error
        {
            get { return _Error; }
            set { _Error = value; }
        }

        private int _Status;
        private string _Error;
    }

    /// <summary>
    /// Provides the raw communication services and functionality
    /// to communicate with the AIS server through TCP.
    /// </summary>
    /// <remarks>
    /// This class is patterned after the ASocket class of aiswip.
    /// It aims to provide all the existing functionality of the
    /// original ASocket class.
    /// 
    /// 
    /// </remarks>
    public class ASocket
    {
        #region Event Handlers
        public event EventHandler<ResponseEventArgs> ResponseEvent;
        public event EventHandler<SocketErrorEventArgs> SocketErrorEvent;
        public event EventHandler ConnectedEvent;
        public event EventHandler DisconnectedEvent;
        #endregion

        /// <summary>
        /// ASocket constructor.
        /// </summary>
        /// <param name="iServerName">Name of the server associated with this object.</param>
        /// <param name="iProtocolType">Type of protocol to use.</param>
        public ASocket(String iServerName, AisProtocolType iProtocolType)
        {
            _AppReadState = AppReadState.None;
            _AsyncReadEventArgs = null;
            _AsyncConnEventArgs = null;

            _DataBfr = null;
            _DataLength = 0;
            _DataSize = 0;

            _ProtocolType = iProtocolType;
            _RemoteSocket = null;

            _RspBfr = null;
            _RspLength = 0;
            _ResponseSize = 0;

            _ServerName = iServerName;
        }

        /// <summary>
        /// Closes the active connection with the server.
        /// </summary>
        /// <returns>true if the operation is successful.</returns>
        /// <remarks>If the operation was successful, a DisconnectedEvent can be received.</remarks>
        public bool CloseConnection()
        {
            bool aRet = false;

            if (_RemoteSocket != null)
            {
                // close the socket
                _RemoteSocket.Close();
                // dispose our object
                _RemoteSocket = null;

                // notify disconnected event subscribers
                DisconnectedEvent(this, new EventArgs());

                // return success
                aRet = true;
            }

            return aRet;
        }

        /// <summary>
        /// Returns true if socket is connected to the server.
        /// </summary>
        /// <returns>true if connected to server.</returns>
        public bool IsConnected()
        {
            bool aRet = false;

            if (_RemoteSocket != null)
                aRet = _RemoteSocket.Connected;

            return aRet;
        }

        /// <summary>
        /// Opens a persistent connection to the AIS server.
        /// </summary>
        /// <param name="iHostDnsIP">Hostname or IP Address of the server.</param>
        /// <param name="iPort">Server port.</param>
        /// <returns>true if the operation is successful.</returns>
        /// <remarks>After this function is called, the following events can be received:
        /// 1. ConnectedEvent - When connection to the server is successful.
        /// 2. DisconnectedEvent - When connection to the server is closed/disconnected.
        /// 3. SocketErrorEvent - When there's an error in one of the socket operations.
        /// 4. ResponseEvent - When there's data received from the server.
        /// </remarks>
        public bool OpenConnection(ref String iHostDnsIP, UInt16 iPort)
        {
            bool aRet = false;
            // check parameters and make sure no there's no existing socket connection
            if (iHostDnsIP.Length > 0 && iPort > 0 && _RemoteSocket == null)
            {
                try
                {
                    IPAddress aIPAdd = null;

                    if (!IPAddress.TryParse(iHostDnsIP, out aIPAdd))
                    {
                        // resolve host IP
                        IPHostEntry aIPHost = Dns.GetHostEntry(iHostDnsIP);
                        // look for the first IPv4 address
                        foreach (IPAddress add in aIPHost.AddressList)
                        {
                            if (add.AddressFamily == AddressFamily.InterNetwork)
                            {
                                aIPAdd = add;
                                break;
                            }
                        }
                        //aIPAdd = aIPHost.AddressList[0];
                    }
                    
                    // create end point object
                    IPEndPoint aEndPt = new IPEndPoint(aIPAdd, iPort);

                    // create our socket object
                    _RemoteSocket = new Socket(aEndPt.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

                    /*
                    // establish connection with the server
                    _RemoteSocket.Connect(aEndPt);

                    // notify connected event subscribers
                    ConnectedEvent(this, new EventArgs());

                    // Depending on the type of protocol, the initial size of the buffer
                    // and the buffer object itself will vary.
                    // For APP messages, it is important that we identify the length of the expected message first
                    // For HTTP messages, the length is stored in the Content-Length parameter
                    // For XML messages, the end of the message is identified by a null or SOH character
                    
                    // create read buffers
                    _RspBfr = new byte[BUFFER_SIZE];
                    _DataBfr = new byte[BUFFER_SIZE];
                    Int32 cReadLength = BUFFER_SIZE;
                    byte[] aReadBuffer = _DataBfr;

                    if (_ProtocolType == AisProtocolType.App)
                    {
                        // use response read buffer
                        aReadBuffer = _RspBfr;
                        // read the length field first
                        cReadLength = LENGTH_SIZE;
                        // set APP read state to reading response length
                        _AppReadState = AppReadState.ReadingResponseLength;
                    }
                    
                    _AsyncReadEventArgs = new SocketAsyncEventArgs();
                    // assign buffer but read only the length field
                    _AsyncReadEventArgs.SetBuffer(aReadBuffer, 0, cReadLength);
                    _AsyncReadEventArgs.Completed += new EventHandler<SocketAsyncEventArgs>(ReceiveComplete);

                    // asynchronous receive
                    if (!_RemoteSocket.ReceiveAsync(_AsyncReadEventArgs))
                    {
                        // if synchronous
                        ProcessReceive(_AsyncReadEventArgs);
                    }
                    */

                    _AsyncConnEventArgs = new SocketAsyncEventArgs();
                    _AsyncConnEventArgs.RemoteEndPoint = aEndPt;
                    _AsyncConnEventArgs.Completed += new EventHandler<SocketAsyncEventArgs>(ConnectComplete);

                    if (!_RemoteSocket.ConnectAsync(_AsyncConnEventArgs))
                    {
                        // if synchronous
                        ProcessConnect(_AsyncConnEventArgs);
                    }

                    // return success
                    aRet = true;
                }
                catch (SocketException aEx)
                {
                    Debug.WriteLine(aEx.Message);

                    // if socket object was created successfully
                    if (_RemoteSocket != null)
                    {
                        // if socket is connected
                        if (_RemoteSocket.Connected)
                        {
                            // disconnect from server
                            _RemoteSocket.Close();

                            // notify disconnected event subscribers
                            DisconnectedEvent(this, new EventArgs());
                        }
                        // throw away our socket object
                        _RemoteSocket = null;
                    }
                    else
                    {
                        // create socket error event object
                        SocketErrorEventArgs aSocketErrorEventArgs = new SocketErrorEventArgs();
                        aSocketErrorEventArgs.Status = aEx.ErrorCode;
                        aSocketErrorEventArgs.Error = aEx.Message;

                        // notify socket error event subscribers
                        SocketErrorEvent(this, aSocketErrorEventArgs);
                    }

                    // return failure
                    aRet = false;
                }
            }

            return aRet;
        }

        /// <summary>
        /// Dumps all the available data in the socket buffer.
        /// </summary>
        /// <returns></returns>
        private void DumpSocketBuffer()
        {
            if (_RemoteSocket != null)
            {
                // get the no. of bytes available for reading
                byte[] aOutValue = BitConverter.GetBytes(0);
                _RemoteSocket.IOControl(IOControlCode.DataToRead, null, aOutValue);
                Int32 aLen = BitConverter.ToInt32(aOutValue, 0);

                // if data is available for reading
                if (aLen > 0)
                {
                    byte[] aBuffer = new byte[aLen];
                    _RemoteSocket.Receive(aBuffer, aLen, SocketFlags.None);
                }
            }
        }

        /// <summary>
        /// Handles the APP response.
        /// </summary>
        /// <param name="iEvent">Async receive arguments object.</param>
        private void ProcessApp(SocketAsyncEventArgs iEvent)
        {
            byte[] aBuffer = iEvent.Buffer; // buffer reference
            String aTemp = String.Empty; // temporary string
            String aErrorMessage = String.Empty; // error message
            bool aError = false; // error flag
            bool aEmit = false; // emit signal flag

            switch (_AppReadState)
            {
                case AppReadState.ReadingResponseLength:
                    // check if length field was read correctly
                    // and message delimiter is present
                    if (iEvent.BytesTransferred == LENGTH_SIZE &&
                        aBuffer[LENGTH_SIZE - 1] == MSG_DELIM)
                    {
                        // convert length field to integer
                        aTemp = Encoding.ASCII.GetString(aBuffer, 0, LENGTH_SIZE - 1);
                        try
                        {
                            // get the expected size of response
                            _ResponseSize = Int32.Parse(aTemp);
                        }
                        catch (FormatException)
                        {
                            // error in number conversion
                            aErrorMessage = "Invalid response message size format.";
                            aError = true;
                        }

                        // response size is valid
                        if (_ResponseSize > 0)
                        {
                            // set state to reading data length
                            _AppReadState = AppReadState.ReadingDataLength;
                        }
                        else
                        {
                            // response size is invalid
                            aErrorMessage = "Invalid response message size.";
                            aError = true;
                        }
                    }
                    else
                    {
                        // protocol error
                        aErrorMessage = "Unable to extract response message size.";
                        aError = true;
                    }

                    break;

                case AppReadState.ReadingDataLength:
                    // check if length field was read correctly
                    if (iEvent.BytesTransferred == LENGTH_SIZE &&
                        aBuffer[LENGTH_SIZE - 1] == MSG_DELIM)
                    {
                        // convert binary data length field to integer
                        aTemp = Encoding.ASCII.GetString(aBuffer, 0, LENGTH_SIZE - 1);
                        try
                        {
                            // get the expected size of binary data
                            _DataSize = Int32.Parse(aTemp);
                        }
                        catch (FormatException)
                        {
                            // error in number conversion
                            aErrorMessage = "Invalid binary data size format.";
                            aError = true;
                        }

                        if (_DataSize >= 0)
                        {
                            // check size of response buffer
                            if (_RspBfr.Length <= _ResponseSize)
                            {
                                // increase buffer size
                                _RspBfr = new byte[_ResponseSize + 1];
                            }

                            // read the response message (including terminator)
                            _AppReadState = AppReadState.ReadingResponse;
                            iEvent.SetBuffer(_RspBfr, 0, _ResponseSize + 1);
                        }
                        else
                        {
                            // binary data size is invalid
                            aErrorMessage = "Invalid binary data size.";
                            aError = true;
                        }
                    }
                    else
                    {
                        // protocol error
                        aErrorMessage = "Unable to extract binary data size";
                        aError = true;
                    }

                    break;

                case AppReadState.ReadingResponse:
                    // update the bytes received
                    _RspLength += iEvent.BytesTransferred;

                    // if read response is complete
                    if (_RspLength == _ResponseSize + 1)
                    {
                        // check if terminator is present
                        if (_RspBfr[_ResponseSize] > MSG_TERM)
                        {
                            // allow to continue
                            Debug.WriteLine("Encountered unterminated response in message.");
                        }
                        // put null terminator
                        _RspBfr[_ResponseSize] = (byte) MSG_NULL;

                        // if binary data is expected
                        if (_DataSize > 0)
                        {
                            // if data buffer not present or not enough
                            if (_DataBfr == null || _DataBfr.Length < _DataSize)
                            {
                                _DataBfr = new byte[_DataSize];
                            }

                            // read data
                            _AppReadState = AppReadState.ReadingData;
                            iEvent.SetBuffer(_DataBfr, 0, _DataSize);
                        }
                        else
                        {
                            // read complete, signal response event
                            aEmit = true;
                        }
                    }
                    else
                    {
                        // partial response received
                        // continue reading the remaining bytes
                        iEvent.SetBuffer(_RspLength, _ResponseSize - _RspLength + 1);
                    }

                    break;

                case AppReadState.ReadingData:
                    // update the bytes received
                    _DataLength += iEvent.BytesTransferred;

                    // if read binary data is complete
                    if (_DataLength == _DataSize)
                    {
                        // read complete, signal response event
                        aEmit = true;
                    }
                    else
                    {
                        // partial binary data received

                        // continue reading the remaining bytes
                        iEvent.SetBuffer(_DataLength, _DataSize - _DataLength);
                    }

                    break;
            }

            // on emit
            if (aEmit)
            {
                // Get ready to emit response to subscribers
                ResponseEventArgs aResEvtArgs = new ResponseEventArgs();

                aResEvtArgs.ResponseBuffer = new byte[_ResponseSize + 1];
                Array.Copy(_RspBfr, aResEvtArgs.ResponseBuffer, _ResponseSize + 1);

                // if binary data is available
                if (_DataLength > 0)
                {
                    aResEvtArgs.DataBuffer = new byte[_DataSize];
                    Array.Copy(_DataBfr, aResEvtArgs.DataBuffer, _DataSize);
                }

                // Signal response event
                ResponseEvent(this, aResEvtArgs);

                // Reset state variables
                _RspLength = 0;
                _ResponseSize = 0;
                _DataLength = 0;
                _DataSize = 0;

                _AppReadState = AppReadState.ReadingResponseLength;
                iEvent.SetBuffer(_RspBfr, 0, LENGTH_SIZE);
            }

            // on error
            if (aError)
            {
                // Display error message
                Debug.WriteLine(aErrorMessage);

                // Clear socket buffer
                DumpSocketBuffer();

                // Reset read state
                _RspLength = 0;
                _ResponseSize = 0;
                _DataLength = 0;
                _DataSize = 0;

                _AppReadState = AppReadState.ReadingResponseLength;
                iEvent.SetBuffer(_RspBfr, 0, LENGTH_SIZE);
            }

            // trigger next asynchronous read
            _RemoteSocket.ReceiveAsync(iEvent);
        }

        /// <summary>
        /// Handles the HTTP response.
        /// </summary>
        /// <param name="iEvent">Socket event arguments object.</param>
        private void ProcessHttp(SocketAsyncEventArgs iEvent)
        {
            // resize response buffer if not enough
            if ((_RspLength + iEvent.BytesTransferred) < _RspBfr.Length)
            {
                Array.Resize(ref _RspBfr, _RspLength + iEvent.BytesTransferred);
            }

            // append new data to response buffer
            Array.Copy(iEvent.Buffer, 0, _RspBfr, _RspLength, iEvent.BytesTransferred);

            // update new response length
            _RspLength += iEvent.BytesTransferred;

            Int32 aBeg = 0;
            Int32 aB = 0;
            Int32 aEnd = _RspLength;
            String aTemp = String.Empty;
            Int32 aMsgLength = 0;
            byte[] aCookie = null;
            bool aClear = true;
            
            // process response buffer loop
            while (aBeg < aEnd)
            {
                // skip whitespaces
                while ((Char.IsWhiteSpace((char)_RspBfr[aBeg])) && (aBeg < aEnd))
                {
                    aBeg++;
                }

                // buffer overflow protection
                if (aBeg == aEnd)
                {
                    aClear = false;
                    break; // not enough bytes
                }

                // make sure we can read the protocol header
                if (aBeg + HTTP_HDR.Length > aEnd)
                {
                    aClear = false;
                    break; // not enough bytes
                }
                
                // read the protocol header
                aTemp = System.Text.Encoding.ASCII.GetString(_RspBfr, aBeg, HTTP_HDR.Length);
                if (aTemp != HTTP_HDR)
                {
                    // EMIT PROTOCOL ERROR
                    SocketErrorEventArgs aSocketError = new SocketErrorEventArgs();
                    aSocketError.Error = "Unknown Procotol";
                    SocketErrorEvent(this, aSocketError);
                    break;
                }

                aB = aBeg;
                // extract Set-Cookie and Content-Length loop
                do
                {
                    // scan past next newline
                    while (aB < aEnd && ((char)_RspBfr[aB++]) != '\n') ;

                    if (aB + SET_COOKIE.Length < aEnd &&
                        _RspBfr[aB] == 'S' &&
                        _RspBfr[aB + 1] == 'e' &&
                        _RspBfr[aB + 2] == 't' &&
                        _RspBfr[aB + 3] == '-' &&
                        _RspBfr[aB + 4] == 'C' &&
                        _RspBfr[aB + 5] == 'o' &&
                        _RspBfr[aB + 6] == 'o' &&
                        _RspBfr[aB + 7] == 'k' &&
                        _RspBfr[aB + 8] == 'i' &&
                        _RspBfr[aB + 9] == 'e')
                    {
                        // scan past the colon (:)
                        while ((aB < aEnd) && (_RspBfr[aB++] != ':')) ;

                        if (aB < aEnd)
                        {
                            Int32 aEol = 0;
                            if (_RspBfr[aB] == ' ')
                                aB++;

                            for (aEol = aB; (aEol < aEnd) && (_RspBfr[aEol] != '\r')
                                && (_RspBfr[aEol] != '\n'); aEol++) ;

                            aCookie = new byte[aEol - aB];
                            Array.Copy(_RspBfr, aB, aCookie, 0, aEol - aB);
                        }
                    }
                    else if (aB + CONTENT_LENGTH.Length < aEnd &&
                        _RspBfr[aB] == 'C' &&
                        _RspBfr[aB + 1] == 'o' &&
                        _RspBfr[aB + 2] == 'n' &&
                        _RspBfr[aB + 3] == 't' &&
                        _RspBfr[aB + 4] == 'e' &&
                        _RspBfr[aB + 5] == 'n' &&
                        _RspBfr[aB + 6] == 't' &&
                        _RspBfr[aB + 7] == '-' &&
                        _RspBfr[aB + 8] == 'L' &&
                        _RspBfr[aB + 9] == 'e' &&
                        _RspBfr[aB + 10] == 'g' &&
                        _RspBfr[aB + 11] == 'n' &&
                        _RspBfr[aB + 12] == 't' &&
                        _RspBfr[aB + 13] == 'h')
                    {
                        while ((aB < aEnd) && _RspBfr[aB] < '0' || _RspBfr[aB] > '9')
                            aB++;

                        // get the content-length
                        if (aB < aEnd)
                        {
                            Int32 aEol = 0;
                            for (aEol = aB; (aEol < aEnd) && (_RspBfr[aEol] != '\r')
                                && (_RspBfr[aEol] != '\n'); aEol++) ;

                            aTemp = Encoding.ASCII.GetString(_RspBfr, aB, (aEol - aB));

                            try
                            {
                                aMsgLength = Int32.Parse(aTemp);
                            }
                            catch (FormatException)
                            {
                                Debug.WriteLine("Invalid Content-Length value.");
                            }
                        }
                    }
                    else if (_RspBfr[aB] == '\r')
                    {
                        aB++;
                    }
                } // extract Set-Cookie and Content-Length loop
                while (aB < aEnd && (_RspBfr[aB++]) != '\n');

                // partial. save the partial request. 
                // aBeg = Beginning of this message
                // aB = Beginning of the body of request
                if (aB + aMsgLength > aEnd)
                {
                    // if aBeg has been moved past the beginning of the buffer
                    // just keep the partial
                    if (aBeg > 0)
                    {
                        // create new buffer
                        byte[] aNewBfr = new byte[aEnd - aBeg];
                        Array.Copy(_RspBfr, aBeg, aNewBfr, 0, aNewBfr.Length);
                        _RspBfr = aNewBfr;
                    }

                    aBeg = aB + aMsgLength;
                    // DO NOT CLEAR BUFFER
                    aClear = false;
                    // quit the main loop
                    break;
                }
                else
                {
                    ResponseEventArgs aHtmlResponse = new ResponseEventArgs();
                    aHtmlResponse.ResponseBuffer = new byte[aMsgLength];
                    aHtmlResponse.CookieBuffer = aCookie;
                    Array.Copy(_RspBfr, aB, aHtmlResponse.ResponseBuffer, 0, aMsgLength);

                    // emit response signal
                    ResponseEvent(this, aHtmlResponse);

                    // move aBeg to end of body
                    aBeg += aMsgLength;
                }
            }

            if (aClear)
            {
                _RspLength = 0;
            }

            // call asynchronous read again
            _RemoteSocket.ReceiveAsync(iEvent);
        } // end of process response buffer loop

        /// <summary>
        /// Handles the asynchronous receive event.
        /// </summary>
        /// <param name="iEvent">Receive event object.</param>
        private void ProcessReceive(SocketAsyncEventArgs iEvent)
        {
            if (iEvent.BytesTransferred > 0)
            {
                switch (_ProtocolType)
                {
                    case AisProtocolType.App:
                        ProcessApp(iEvent);
                        break;
                    case AisProtocolType.Http:
                        ProcessHttp(iEvent);
                        break;
                    case AisProtocolType.Xml:
                        ProcessXml(iEvent);
                        break;
                }
            }
            else
            {
                if (_RemoteSocket != null)
                {
                    _RemoteSocket.Close();
                    _RemoteSocket = null;
                    DisconnectedEvent(this, new EventArgs());
                }
            }
        }

        /// <summary>
        /// Handles the XML response.
        /// </summary>
        /// <param name="iEvent">Socket event arguments object.</param>
        private void ProcessXml(SocketAsyncEventArgs iEvent)
        {
            // resize response buffer if not enough
            if ((_RspLength + iEvent.BytesTransferred) < _RspBfr.Length)
            {
                Array.Resize(ref _RspBfr, _RspLength + iEvent.BytesTransferred);
            }

            // append new data to response buffer
            Array.Copy(iEvent.Buffer, 0, _RspBfr, _RspLength, iEvent.BytesTransferred);

            // update new response length
            _RspLength += iEvent.BytesTransferred;

            Int32 aBeg = 0;
            Int32 aB = 0;
            Int32 aEnd = _RspLength;
            String aTemp = String.Empty;
            bool aClear = true;
            
            // process response buffer loop
            while (aBeg < aEnd)
            {
                // skip whitespaces
                while ((Char.IsWhiteSpace((char)_RspBfr[aBeg])) && (aBeg < aEnd))
                {
                    aBeg++;
                }

                // buffer overflow protection
                if (aBeg == aEnd)
                {
                    aClear = false;
                    break; // not enough bytes
                }

                // make sure we can read the protocol header
                if (aBeg + XML_HDR.Length > aEnd)
                {
                    aClear = false;
                    break; // not enough bytes
                }

                // read the protocol header
                aTemp = System.Text.Encoding.ASCII.GetString(_RspBfr, aBeg, XML_HDR.Length);
                if (aTemp != XML_HDR)
                {
                    // EMIT PROTOCOL ERROR
                    SocketErrorEventArgs aSocketError = new SocketErrorEventArgs();
                    aSocketError.Error = "Unknown Protocol";
                    SocketErrorEvent(this, aSocketError);
                    break;
                }

                for (aB = aBeg; aB < aEnd && _RspBfr[aB] > MSG_TERM; aB++);

                //  if read past the last character
                if (aB == aEnd)
                {
                    // DO NOT CLEAR BUFFER
                    aClear = false;
                    break;
                }

                // partial. save partial request.
                // aBeg - Beginning of next response
                // aB - end of this response
                if (_RspBfr[aB] > MSG_TERM)
                {
                    // if aBeg has been moved past the beginning of the buffer
                    if (aBeg > 0)
                    {
                        byte[] aNewBuffer = new byte[aEnd - aBeg];
                        Array.Copy(_RspBfr, aBeg, aNewBuffer, 0, aNewBuffer.Length);
                        _RspBfr = aNewBuffer;
                    }

                    // DO NOT CLEAR BUFFER
                    aClear = false;
                    break;
                }

                // NULL terminate the response
                _RspBfr[aB] = (byte) MSG_NULL;

                ResponseEventArgs aResponse = new ResponseEventArgs();
                aResponse.ResponseBuffer = new byte[aB + 1];
                Array.Copy(_RspBfr, aBeg, aResponse.ResponseBuffer, 0, aB + 1);

                // emit response signal
                ResponseEvent(this, aResponse);

                // move aBeg to 
                aBeg = aB + 1;
            }

            if (aClear)
            {
                _RspLength = 0;
            }

            // call asynchronous read again
            _RemoteSocket.ReceiveAsync(iEvent);
        }

        /// <summary>
        /// Asynchronous connect event handler.
        /// </summary>
        /// <param name="iSender"></param>
        /// <param name="iEvent"></param>
        private void ConnectComplete(object iSender, SocketAsyncEventArgs iEvent)
        {
            ProcessConnect(iEvent);
        }

        /// <summary>
        /// Handles the asynchronous connect event.
        /// </summary>
        /// <param name="iEvent"></param>
        private void ProcessConnect(SocketAsyncEventArgs iEvent)
        {
            if (iEvent.SocketError == SocketError.Success)
            {
                // notify connected event subscribers
                ConnectedEvent(this, new EventArgs());

                // Depending on the type of protocol, the initial size of the buffer
                // and the buffer object itself will vary.
                // For APP messages, it is important that we identify the length of the expected message first
                // For HTTP messages, the length is stored in the Content-Length parameter
                // For XML messages, the end of the message is identified by a null or SOH character

                // create read buffers
                _RspBfr = new byte[BUFFER_SIZE];
                _DataBfr = new byte[BUFFER_SIZE];
                Int32 cReadLength = BUFFER_SIZE;
                byte[] aReadBuffer = _DataBfr;

                if (_ProtocolType == AisProtocolType.App)
                {
                    // use response read buffer
                    aReadBuffer = _RspBfr;
                    // read the length field first
                    cReadLength = LENGTH_SIZE;
                    // set APP read state to reading response length
                    _AppReadState = AppReadState.ReadingResponseLength;
                }

                _AsyncReadEventArgs = new SocketAsyncEventArgs();
                // assign buffer but read only the length field
                _AsyncReadEventArgs.SetBuffer(aReadBuffer, 0, cReadLength);
                _AsyncReadEventArgs.Completed += new EventHandler<SocketAsyncEventArgs>(ReceiveComplete);

                try
                {
                    // asynchronous receive
                    if (!_RemoteSocket.ReceiveAsync(_AsyncReadEventArgs))
                    {
                        // if synchronous
                        ProcessReceive(_AsyncReadEventArgs);
                    }
                }
                catch (SocketException aEx)
                {
                    Debug.WriteLine(aEx.Message);

                    // if socket is connected
                    if (_RemoteSocket.Connected)
                    {
                        // disconnect from server
                        _RemoteSocket.Close();
                    }

                    // notify disconnected event subscribers
                    DisconnectedEvent(this, new EventArgs());

                    // throw away our socket object
                    _RemoteSocket = null;
                }
            }
            else
            {
                // create socket error event object
                SocketErrorEventArgs aSocketErrorEventArgs = new SocketErrorEventArgs();
                aSocketErrorEventArgs.Status = (int)iEvent.SocketError;
                aSocketErrorEventArgs.Error = "Connect failed";

                // notify socket error event subscribers
                SocketErrorEvent(this, aSocketErrorEventArgs);

                // delete socket
                _RemoteSocket = null;
            }
        }

        /// <summary>
        /// Aysnchronous receive event handler.
        /// </summary>
        /// <param name="iSender">Event source.</param>
        /// <param name="iEvent">Event arguments object.</param>
        private void ReceiveComplete(object iSender, SocketAsyncEventArgs iEvent)
        {
            ProcessReceive(iEvent);
        }

        /// <summary>
        /// Returns the socket descriptor.
        /// </summary>
        /// <returns></returns>
        public int SocketDescriptor()
        {
            return _RemoteSocket.Handle.ToInt32();
        }

        /// <summary>
        /// Sends request and binary data, if available, to the AIS server.
        /// </summary>
        /// <param name="irRequest">Reference to the request string object.</param>
        /// <param name="irData">Optional binary data array.</param>
        /// <param name="iDataLength">Length of the binary data.</param>
        public void Submit(ref String irRequest, byte[] irData, Int32 iDataLength)
        {
            // convert string to latin-1 encoding
            // refer to: http://msdn2.microsoft.com/en-us/library/system.text.encoding.aspx
            byte[] aAsciiBody = Encoding.GetEncoding(28591).GetBytes(irRequest);
            byte[] aBody = null;
            Int32 aLength = aAsciiBody.Length;
            Int32 aBytes = 0;

            try
            {
                switch (_ProtocolType)
                {
                    case AisProtocolType.App:
                        // The message body is composed of
                        // Body Length (11 Bytes)
                        // Data Length (11 Bytes)
                        // Body (Variable Length)
                        // Terminator (1 Byte)
                        aBody = new byte[LENGTH_SIZE + LENGTH_SIZE + aLength + 1];
                        byte[] aBodyLength = Encoding.ASCII.GetBytes(aLength.ToString("0000000000"));
                        byte[] aDataLength = Encoding.ASCII.GetBytes(iDataLength.ToString("0000000000"));

                        // Body Length
                        Array.Copy(aBodyLength, 0, aBody, aBytes, LENGTH_SIZE - 1);
                        aBytes += LENGTH_SIZE;
                        aBody[aBytes - 1] = (byte)MSG_DELIM;

                        // Data Length
                        Array.Copy(aDataLength, 0, aBody, aBytes, LENGTH_SIZE - 1);
                        aBytes += LENGTH_SIZE;
                        aBody[aBytes - 1] = (byte)MSG_DELIM;

                        // Body
                        Array.Copy(aAsciiBody, 0, aBody, aBytes, aLength);
                        aBytes += aLength;
                        aBody[aBytes] = (byte)MSG_TERM;

                        _RemoteSocket.Send(aBody);

                        // Data
                        if (irData != null)
                            _RemoteSocket.Send(irData, iDataLength, SocketFlags.None);

                        break;

                    case AisProtocolType.Xml:
                        // Append message terminator
                        Array.Resize(ref aAsciiBody, aAsciiBody.Length + 1);
                        aAsciiBody[aAsciiBody.Length - 1] = (byte)MSG_TERM;
                        _RemoteSocket.Send(aAsciiBody);

                        // Data
                        if (irData != null)
                            Console.Error.WriteLine("{0}, Data Dropped.", irRequest);

                        break;

                    case AisProtocolType.Http:
                        _RemoteSocket.Send(aAsciiBody);

                        // Data
                        if (irData != null)
                            Console.Error.WriteLine("{0}, Data Dropped.", irRequest);

                        break;
                }
            }
            catch (Exception e)
            {
                Debug.WriteLine(e.Message);
            }
        }

        #region Private Members
        /// <summary>
        /// APP Read State.
        /// </summary>
        private AppReadState _AppReadState;

        /// <summary>
        /// Async connect event args object.
        /// </summary>
        private SocketAsyncEventArgs _AsyncConnEventArgs;

        /// <summary>
        /// Async read event args object.
        /// </summary>
        private SocketAsyncEventArgs _AsyncReadEventArgs;

        /// <summary>
        /// Buffer to hold data bytes.
        /// </summary>
        private byte[] _DataBfr;

        /// <summary>
        /// No. of data bytes received so far.
        /// </summary>
        private Int32 _DataLength;

        /// <summary>
        /// No. of data bytes expected.
        /// </summary>
        private Int32 _DataSize;
        
        /// <summary>
        /// Protocol type.
        /// </summary>
        private AisProtocolType _ProtocolType;

        /// <summary>
        /// Remote socket object.
        /// </summary>
        private Socket _RemoteSocket;

        /// <summary>
        /// Buffer to hold partial responses received.
        /// </summary>
        private byte[] _RspBfr;

        /// <summary>
        /// No. of response bytes received so far.
        /// </summary>
        private Int32 _RspLength;

        /// <summary>
        /// No. of response bytes expected.
        /// </summary>
        private Int32 _ResponseSize;

        /// <summary>
        /// Server Name.
        /// </summary>
        private String _ServerName;
        #endregion

        #region Constants
        /// <summary>
        /// Default size of the buffer.
        /// </summary>
        private const Int32 BUFFER_SIZE = 1024;

        /// <summary>
        /// Size of the APP length field.
        /// </summary>
        private const Int32 LENGTH_SIZE = 11;

        /// <summary>
        /// DEL or \177 ASCII character.
        /// </summary>
        public const char MSG_DELIM = '\x7F';

        /// <summary>
        /// SOH or \01 ASCII character.
        /// </summary>
        public const char MSG_TERM = '\x01';

        /// <summary>
        /// NUL or \00 ASCII character.
        /// </summary>
        public const char MSG_NULL = '\x00';

        /// <summary>
        /// HTTP starting tag.
        /// </summary>
        private const String HTTP_HDR = "HTTP/";

        /// <summary>
        /// XML starting tag.
        /// </summary>
        private const String XML_HDR = "<amp";

        /// <summary>
        /// HTTP Set-Cookie tag.
        /// </summary>
        private const String SET_COOKIE = "Set-Cookie";

        /// <summary>
        /// HTTP Content-Length tag.
        /// </summary>
        private const String CONTENT_LENGTH = "Content-Length";
        #endregion
    }
}
