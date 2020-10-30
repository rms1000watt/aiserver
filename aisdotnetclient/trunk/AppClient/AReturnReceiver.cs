using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AppClient
{
    /// <summary>
    /// This class encapsulates the response event handler. Use this to assign response handler for a request.
    /// </summary>
    public class AReturnReceiver
    {
        public event EventHandler<AAsyncEventArgs> ReturnOutputEvent;

        public void ReturnOutput(object iSource,
            int iConnId,
            int iReqId,
            int iStatus,
            ARequestType iRequestType,
            int iRetVal,
            string iOut,
            byte[] iData,
            string iDisplay,
            string iError,
            string iClientData)
        {
            // copy to a temporary variable to be thread-safe
            // http://msdn.microsoft.com/en-us/library/db0etb8x.aspx
            EventHandler<AAsyncEventArgs> tempEvent = ReturnOutputEvent;
            if (tempEvent != null)
            {
                AAsyncEventArgs eventArgs = new AAsyncEventArgs();
                eventArgs.ConnectionId = iConnId;
                eventArgs.RequestId = iReqId;
                eventArgs.Status = iStatus;
                eventArgs.RequestType = iRequestType;
                eventArgs.ReturnValue = iRetVal;
                eventArgs.Out = iOut;
                eventArgs.Data = iData;
                eventArgs.Display = iDisplay;
                eventArgs.Error = iError;
                eventArgs.ClientData = iClientData;
                tempEvent(iSource, eventArgs);
            }
        }
    }
}
