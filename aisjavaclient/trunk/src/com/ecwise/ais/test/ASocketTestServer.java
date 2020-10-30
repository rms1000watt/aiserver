package com.ecwise.ais.test;

//import java.io.BufferedInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class ASocketTestServer {

	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) {
		ServerSocket serverSocket;
		try {
			serverSocket = new ServerSocket(8081);
			
			System.out.println("Waiting for client...");
			// wait for client
			Socket client = serverSocket.accept();
			
			System.out.println("Client connected");
			
			//BufferedInputStream inputStream = new BufferedInputStream(client.getInputStream());
			DataOutputStream outputStream = new DataOutputStream(client.getOutputStream());
			
			outputStream.writeBytes("0000000010\1770000000000\177HelloWorld\001");
			outputStream.flush();
			
			System.out.println("Press any key to continue");
			System.in.read();
		
			// close socket after wards
			client.close();
			
			System.out.println("Press any key to continue");
			System.in.read();
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}		
	}	
}
