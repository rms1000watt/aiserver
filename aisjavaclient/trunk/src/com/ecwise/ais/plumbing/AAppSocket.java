package com.ecwise.ais.plumbing;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;

import com.ecwise.ais.ASocketEventHandler;

public class AAppSocket implements Runnable {

	public AAppSocket(String iName) {
		initHandlers();
		readBuffer = ByteBuffer.allocate(4096);
	}
	
	public void setSocketEventHandler(ASocketEventHandler iSocketEventHandler) {
		socketEventHandler = iSocketEventHandler;
	}

	@Override
	public void run() {
		
	}
	
	public boolean openConnection(String iHost, int iPort) {
		if (socketChannel == null) {
			try {
				socketChannel = AsynchronousSocketChannel.open();
			} catch (IOException e) {
				System.err.println(e.toString());
				return false;
			}
		}
		socketChannel.connect(new InetSocketAddress(iHost, iPort), this, connectComplete);
		return true;
	}
	
	private void initHandlers() {
		connectComplete = new CompletionHandler<Void, Object>() {
			@Override
			public void completed(Void result, Object attachment) {
				((AAppSocket)attachment).onConnected();				
			}

			@Override
			public void failed(Throwable exc, Object attachment) {
				((AAppSocket)attachment).onSocketError(exc.toString());
			}
		};
		
		readComplete = new CompletionHandler<Integer, Object>() {
			@Override
			public void completed(Integer result, Object attachment) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void failed(Throwable exc, Object attachment) {
				((AAppSocket)attachment).onSocketError(exc.toString());
			}			
		};
	}
	
	private void onConnected() {
		if (socketEventHandler != null) {
			socketEventHandler.onConnected();
		}
		socketChannel.read(readBuffer, this, readComplete);
	}
	
	private void onRead() {
		socketChannel.read(readBuffer, this, readComplete);
	}
	
	private void onSocketError(String iError) {
		if (socketEventHandler != null) {
			socketEventHandler.onSocketError(0, iError);
		}
	}
	
	private ASocketEventHandler socketEventHandler;
	private AsynchronousSocketChannel socketChannel;
	private ByteBuffer readBuffer;
	
	private CompletionHandler<Void, Object> connectComplete;
	private CompletionHandler<Integer, Object> readComplete;
}
