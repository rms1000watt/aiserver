package com.ecwise.ais.test;

import java.util.Hashtable;

import com.ecwise.ais.AAppClient;
import com.ecwise.ais.ACloseMode;
import com.ecwise.ais.AExtentInfo;
import com.ecwise.ais.ANodeInfo;
import com.ecwise.ais.ARequestType;
import com.ecwise.ais.AReturnReceiver;

public class AAppClientTest implements AReturnReceiver {

	private AAppClient appClient = null;
	private AReturnReceiver defaultReceiver = null;
	
	public void setAppClient(AAppClient iAppClient) {
		appClient = iAppClient;
	}
	
	public void setDefaultReceiver(AReturnReceiver iReceiver) {
		defaultReceiver = iReceiver;
	}
	
	public AReturnReceiver getDefaultReceiver() {
		return defaultReceiver;
	}
	
	/**
	 * @param args
	 * @throws InterruptedException 
	 */
	public static void main(String[] args) throws InterruptedException {

		AAppClientTest aAppClientTest = new AAppClientTest();
		AAppClient aAppClient = new AAppClient("127.0.0.1", 8081, aAppClientTest, "Test");
		int aReqId = 0;
		
		aAppClientTest.setAppClient(aAppClient);
		aAppClientTest.setDefaultReceiver(aAppClientTest);
		
		aReqId = aAppClient.openConnection(aAppClientTest);
		System.out.println("openConnection() = " + aReqId);
		
		Thread.sleep(2000);
		
		aReqId = aAppClient.logon(aAppClientTest, "tmay", "abc123");
		System.out.println("logon() = " + aReqId);
		
		Thread.sleep(2000);
		
		aReqId = aAppClient.getCurrentContexts(aAppClientTest);
		System.out.println("getCurrentContexts() = " + aReqId);
		
		aReqId = aAppClient.getSessions(aAppClientTest, "Default");
		System.out.println("getSessions() = " + aReqId);
		
		aReqId = aAppClient.getWorkspaceStatistics(aAppClientTest);
		System.out.println("getWorkspaceStatistics() = " + aReqId);
		
		aReqId = aAppClient.getContextParams(aAppClientTest, "Default");
		System.out.println("getContextParams() = " + aReqId);
		
		aReqId = aAppClient.eval(aAppClientTest, "(+ 1 1)");
		System.out.println("eval() = " + aReqId);
		
		aReqId = aAppClient.eval(aAppClientTest, "(writeln \"hello world\")");
		System.out.println("eval() = " + aReqId);
		
		aReqId = aAppClient.eval(aAppClientTest, "(setq x \"hello world\")");
		System.out.println("eval() = " + aReqId);
		
		aReqId = aAppClient.showConsole(aAppClientTest, "(+ 4 4)");
		System.out.println("showConsole() = " + aReqId);
		
		aReqId = aAppClient.getExtentNames(aAppClientTest);
		System.out.println("getExtentNames() = " + aReqId);
		
		aReqId = aAppClient.getExtentStatus(aAppClientTest);
		System.out.println("getExtentStatus() = " + aReqId);
		
		aReqId = aAppClient.getDirInfo(aAppClientTest, "logs");
		System.out.println("getDirInfo() = " + aReqId);
		
		aReqId = aAppClient.getSessionStats(aAppClientTest);
		System.out.println("getSessionStats() = " + aReqId);
		
		aReqId = aAppClient.getConnectionStats(aAppClientTest);
		System.out.println("getConnectionStats() = " + aReqId);
		
		aReqId = aAppClient.getLogonStats(aAppClientTest);
		System.out.println("getLogonStats() = " + aReqId);
		
		aReqId = aAppClient.getRequestStats(aAppClientTest);
		System.out.println("getRequestStats() = " + aReqId);
		
		Thread.sleep(5000);
		
		aReqId = aAppClient.logoff(aAppClientTest);
		System.out.println("logoff() = " + aReqId);
		
		aReqId = aAppClient.closeConnection(aAppClientTest, 0, ACloseMode.Default);
		System.out.println("closeConnection() = " + aReqId);
		
		Thread.sleep(1000);
		
		System.out.println("Test complete.");
	}

	@Override
	public void returnOutput(int iConnectId, int iReqId, int iStatus,
			ARequestType iReqType, int iRetValue, String iOut, byte[] iData,
			String iDisplay, String iError, String iClientData) {

		int aReqId = 0;
		
		System.out.println("******************************");
		System.out.println("\tiConnectId = " + iConnectId);
		System.out.println("\tiReqId = " + iReqId);
		System.out.println("\tiStatus =  " + iStatus);
		System.out.println("\tiReqType = " + iReqType.toString());
		System.out.println("\tiRetValue = " + iRetValue);

		if (!iOut.isEmpty()) {
			System.out.println("\tiOut = " + iOut);
		}
		if (!iDisplay.isEmpty()) {
			System.out.println("\tiDisplay = " + iDisplay);
		}
		if (iError != null && !iError.isEmpty()) {
			System.out.println("\tiError = " + iError);
		}
		if (iClientData != null && !iClientData.isEmpty()) {
			System.out.println("\tiClientData = " + iClientData);
		}
		
		if (iReqType == ARequestType.GetContextParams) {
			System.out.println("GetContextParams");
			Hashtable<String, String> aContextParams = appClient.getContextParameters();
			for (String key : aContextParams.keySet()) {
				System.out.println("\t" + key);
			}
		} else if (iReqType == ARequestType.GetExtentNames) {
			System.out.println("GetExtentNames");
			String[] aCabinets = appClient.getCurrentExtents();
			for (String cabinet : aCabinets) {
				System.out.println("\t" + cabinet);
			}

			appClient.setCurrentExtent("Math");
			
			aReqId = appClient.getExtentTypes(getDefaultReceiver(), true);
			System.out.println("getExtentTypes() = " + aReqId);
			
			aReqId = appClient.getRootLevel(getDefaultReceiver());
			System.out.println("getRootLevel() = " + aReqId);
			
		} else if (iReqType == ARequestType.GetExtentTypes) {
			System.out.println("GetExtentTypes");
			AExtentInfo aExtentInfo = appClient.getExtentInfo("Math");
			for (String aTypes : aExtentInfo.getExtentTypes().keySet()) {
				System.out.println("\t" + aTypes);
			}
		} else if (iReqType == ARequestType.GetNextLevel) {
			System.out.println("GetNextLevel");
			AExtentInfo aExtentInfo = appClient.getExtentInfo("Math");
			for (ANodeInfo aNodeInfo : aExtentInfo.getNodes()) {
				System.out.println("\t" + aNodeInfo.getSymbol());
			}
			
			aReqId = appClient.openNode(getDefaultReceiver(), "Math", "math:arrayFillColumn");
			System.out.println("openNode() = " + aReqId);
			
			System.out.println("getNodePathTree");
			String[] aNodePathTree = appClient.getNodePathTree();
			for (String aNodePathEntry : aNodePathTree) {
				System.out.println("\t" + aNodePathEntry);
			}
		}
	}

	@Override
	public void connectionClosed(int iConnectId) {

		System.out.println("Connection was closed");		
	}

}
