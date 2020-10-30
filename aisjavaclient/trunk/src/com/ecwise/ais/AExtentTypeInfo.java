package com.ecwise.ais;

public class AExtentTypeInfo {
	
	public AExtentTypeInfo(String iTypeName, String iActionCodes) {
		typeName = iTypeName;
		actionCodes = iActionCodes;
	}
	
	public String getTypeName() {
		return typeName;
	}
	
	public String getActionCodes() {
		return actionCodes;
	}

	private String typeName;
	private String actionCodes;
}
