package com.ecwise.ais.test;

import java.io.IOException;

import com.ecwise.ais.AAppSocketNoAsync;
import com.ecwise.ais.ASocketEventHandler;

public class ASocketTest implements ASocketEventHandler {

	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		ASocketTest socketEventHandler = new ASocketTest();
		//ASocket socket = new ASocket();
		AAppSocketNoAsync socket = new AAppSocketNoAsync();
		socket.setSocketEventHandler(socketEventHandler);
				
		socket.openConnection("localhost", 8081);

		System.out.println("Press any key to continue");
		System.in.read();
	}

	@Override
	public void onConnected() {
		System.out.println("onConnected");
	}

	@Override
	public void onDisconnected() {
		System.out.println("onDisconnected");
	}

	@Override
	public void onResponse(byte[] iResponse, byte[] iCookie, byte[] iData) {
		System.out.println("onResponse");		
	}

	@Override
	public void onSocketError(int iStatus, String iError) {
		System.out.println("onSocketError: " + iError);
	}
}
