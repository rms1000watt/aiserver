package com.ecwise.ais;

public interface AReturnReceiver {

	public void returnOutput(int iConnectId,
			int iReqId,
			int iStatus,
			ARequestType iReqType,
			int iRetValue,
			String iOut,
			byte[] iData,
			String iDisplay,
			String iError,
			String iClientData);
	
	public void connectionClosed(int iConnectId);
}
