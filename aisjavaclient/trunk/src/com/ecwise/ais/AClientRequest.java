package com.ecwise.ais;

public class AClientRequest {

	public AClientRequest(String iAmpMessage,
			String iClientData,
			int iClientValue,
			Object iCustomData,
			AReturnReceiver iReceiver,
			ARequestType iRequestType) {
		ampMessage = iAmpMessage;
		clientData = iClientData;
		clientValue = iClientValue;
		customData = iCustomData;
		receiver = iReceiver;
		requestType = iRequestType;
	}
	
	public String getAmpMessage() {
		return ampMessage;
	}
	
	public String getClientData() {
		return clientData;
	}
	
	public int getClientValue() {
		return clientValue;
	}
	
	public Object getCustomData() {
		return customData;
	}
	
	public AReturnReceiver getReceiver() {
		return receiver;
	}
	
	public ARequestType getRequestType() {
		return requestType;
	}

	private String ampMessage;
	private String clientData;
	private int clientValue;
	private Object customData;
	private AReturnReceiver receiver;
	private ARequestType requestType;
}
