package com.ecwise.ais;

public interface ASocketEventHandler {
	
	/**
	 * Called when the socket is already connected.
	 */
	public void onConnected();
	
	/**
	 * Called when the socket is disconnected.
	 */
	public void onDisconnected();
	
	/**
	 * Called when there's a response from the server.
	 * @param iResponse
	 * @param iCookie
	 * @param iData
	 */
	public void onResponse(byte[] iResponse, byte[] iCookie, byte[] iData);
	
	/**
	 * Called when there's a socket error.
	 * @param iStatus
	 * @param iError
	 */
	public void onSocketError(int iStatus, String iError);
}
