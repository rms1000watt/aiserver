package com.ecwise.ais;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;

/**
 * Uses TCP socket to communicate with AIS through the application port.
 * @author Franklin Chua
 *
 */
public class AAppSocketNoAsync implements Runnable {
	
	private Socket socket;
	private BufferedInputStream inputBuffer;
	private BufferedOutputStream outputBuffer;
	private ASocketEventHandler socketEventHandler;

	private static int LEN_BUF_SIZE = 11;
	public static char MSG_DELIM = '\177';
	public static char MSG_TERM = '\01';
	
	/**
	 * Default constructor.
	 */
	public AAppSocketNoAsync() {
		socket = new Socket();
	}
	
	/**
	 * Sets the object that handles socket events.
	 * @param iSocketEventHandler
	 */
	public void setSocketEventHandler(ASocketEventHandler iSocketEventHandler) {
		socketEventHandler = iSocketEventHandler;
	}
	
	/**
	 * Opens a connection to the server.
	 * @param iHost
	 * @param iPort
	 * @return
	 */
	public boolean openConnection(String iHost, int iPort) {
		boolean aSuccess = true;
		try {
			socket.connect(new InetSocketAddress(iHost, iPort));
			onConnected();
			// starts the socket read thread
			startSocketThread();
		} catch (IOException e) {
			System.err.println(e.toString());
			aSuccess = false;
		}
		return aSuccess;
	}
	
	/**
	 * Sends a request to the server.
	 * @param iRequest
	 * @param iData Optional binary data.
	 */
	public void submit(String iRequest, byte[] iData) {
		String aBody;
		int aBodyLen = iRequest.length();
		int aDataLen = (iData != null) ? iData.length : 0;
		
		try {
			aBody = String.format("%010d", aBodyLen) + '\177' + String.format("%010d", aDataLen) + '\177' + iRequest + '\01';
			outputBuffer.write(aBody.getBytes());
			if (iData != null) {
				outputBuffer.write(iData);
			}
			outputBuffer.flush();
		} catch (IOException e) {
			System.err.println(e.toString());
			onSocketError(AErrorCode.TcpIpError.ordinal(), e.toString());
		}		
	}

	@Override
	public void run() {
		boolean aQuit = false;
	
		int aRespLen = 0;	// total bytes read (response)
		int aRespSize = 0;	// total length (response)
		int aDataLen = 0;	// total bytes read (data)
		int aDataSize = 0;	// total length (data)
		int aReadLen = 0;	// bytes read
		int aBytesRem = 0;	// bytes remaining
		byte[] aRespBuffer = null;
		byte[] aDataBuffer = null;
		byte[] aLenBuffer = new byte[LEN_BUF_SIZE];

		try {
			inputBuffer = new BufferedInputStream(socket.getInputStream());
			outputBuffer = new BufferedOutputStream(socket.getOutputStream());
		} catch (IOException e) {
			System.err.println(e.toString());
			onSocketError(AErrorCode.TcpIpError.ordinal(), e.toString());
			onDisconnected();
			return;
		}
		
		System.out.println("AAppSocket: Socket thread started.");
		
		// continue reading from socket until the server terminates the connection
		// normally, the client sends a closeconnection request to the server
		// then the server terminates the connection
		while (!aQuit) {
		
			try {
				// check if this is a new response
				if (aRespSize == 0) {

					// read response length				
					aReadLen = inputBuffer.read(aLenBuffer, 0, LEN_BUF_SIZE);
					
					if (aReadLen == -1) {
						onDisconnected();
						break; // quit loop
					}
					
					if ((aReadLen == LEN_BUF_SIZE) && 
							(aLenBuffer[LEN_BUF_SIZE - 1] == MSG_DELIM) && 
							((aRespSize = getLength(aLenBuffer)) > 0)) {
	
						// read data length
						aReadLen = inputBuffer.read(aLenBuffer, 0, LEN_BUF_SIZE);
						
						if (aReadLen == -1) {
							onDisconnected();
							break; // quit loop
						}
						
						if ((aReadLen == LEN_BUF_SIZE) && 
								(aLenBuffer[LEN_BUF_SIZE - 1] == MSG_DELIM) &&
								((aDataSize = getLength(aLenBuffer)) >= 0)) {
							// proceed to reading content
						} else {
							System.err.println("AAppSocket: Unable to extract binary data buffer size.");
							// try to re-sync, skip remaining bytes
							inputBuffer.skip(inputBuffer.available());
							aRespSize = 0;
							aDataSize = 0;
							continue;
						}
					} else {
						System.err.println("AAppSocket: Unable to extract response message size.");
						// try to re-sync, skip remaining bytes
						inputBuffer.skip(inputBuffer.available());
						aRespSize = 0;
						aDataSize = 0;
						continue;
					}
					
					// allocate buffers
					aRespBuffer = new byte[aRespSize + 1];
					if (aDataSize > 0) {
						aDataBuffer = new byte[aDataSize];
					}
				}
				
				// check if there's a response to read
				if ((aBytesRem = aRespSize - aRespLen) > 0) {
					// read until buffer is filled
					aReadLen = inputBuffer.read(aRespBuffer, aRespLen, aBytesRem + 1);
					
					if (aReadLen == -1) {
						onDisconnected();
						break; // quit loop
					} else if (aReadLen == (aBytesRem + 1)) {
						aRespLen = aRespSize;
						if (aRespBuffer[aRespLen] > MSG_TERM) {
							System.err.println("AAppSocket: Encountered unterminated response in message.");
						}
						aRespBuffer[aRespLen] = '\0';
					} else {
						// partial read
						aRespLen += aReadLen;
						continue; // read next
					}
				}
				
				// check if there's data to read
				if ((aBytesRem = aDataSize - aDataLen) > 0) {
					// read until buffer is filled
					aReadLen = inputBuffer.read(aDataBuffer, aDataLen, aBytesRem);
					
					if (aReadLen == -1) {
						onDisconnected();
						break; // quit loop
					} else if (aReadLen < aBytesRem) {
						// partial read
						aDataLen += aReadLen;
						continue; // read next
					}
				}
				
				// read complete, return response
				onResponse(aRespBuffer, null, aDataBuffer);
				
				// reset data, read next
				aRespLen = 0;
				aRespSize = 0;
				aDataLen = 0;
				aDataSize = 0;			
				
			} catch (IOException ioEx) {
				System.err.println(ioEx.toString());
				onSocketError(AErrorCode.TcpIpError.ordinal(), ioEx.toString());
				onDisconnected();
				break;
			}
		} // end of loop
	}
	
	/**
	 * Starts a thread that reads from the socket and triggers event handlers.
	 */
	private void startSocketThread() {
		Thread socketThread = new Thread(this);
		socketThread.start();
	}
	
	private void onSocketError(int iStatus, String iSocketError) {
		if (socketEventHandler != null) {
			socketEventHandler.onSocketError(iStatus, iSocketError);
		}
	}
	
	private void onConnected() {
		if (socketEventHandler != null) {
			socketEventHandler.onConnected();
		}
	}
	
	private void onDisconnected() {
		if (socketEventHandler != null) {
			socketEventHandler.onDisconnected();
		}
	}
	
	private void onResponse(byte[] iResponse, byte[] iCookie, byte[] iData) {
		if (socketEventHandler != null) {
			socketEventHandler.onResponse(iResponse, iCookie, iData);
		}
	}
	
	/**
	 * Extracts the length field in the AMP message.
	 * @param iLengthBuffer
	 * @return
	 */
	private int getLength(byte[] iLengthBuffer) {
		String temp = new String(iLengthBuffer, 0, LEN_BUF_SIZE - 1);
		int length = 0;
		try {
			length = Integer.parseInt(temp);
		} catch (NumberFormatException numEx) {
			System.err.println(numEx.toString());
			length = -1;
		}
		return length;
	}
}
