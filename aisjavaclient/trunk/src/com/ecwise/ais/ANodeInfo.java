package com.ecwise.ais;

public class ANodeInfo {
	
	public ANodeInfo(String iType, String iValue,
			String iSize, String iDate, String iTime, 
			String iVersion, String iSymbol, String iUniqueKey) {

		type = iType;
		value = iValue;
		size = iSize;
		date = iDate;
		time = iTime;
		version = iVersion;
		symbol = iSymbol;
		uniqueKey = iUniqueKey;
	}
	
	public ANodeInfo(String[] iParams) {

		if (iParams.length > 7) {

			type = iParams[0];
			value = iParams[1];
			size = iParams[2];
			date = iParams[3];
			time = iParams[4];
			version = iParams[5];
			symbol = iParams[6];
			uniqueKey = iParams[7];
		}
	}
	
	public String getType() {
		return type;
	}
	
	public String getValue() {
		return value;
	}
	
	public String getSize() {
		return size;
	}
	
	public String getDate() {
		return date;
	}
	
	public String getTime() {
		return time;
	}
	
	public String getVersion() {
		return version;
	}
	
	public String getSymbol() {
		return symbol;
	}
	
	public String getUniqueKey() {
		return uniqueKey;
	}

	private String type = "";
    private String value = "";
    private String size = "";
    private String date = "";
    private String time = "";
    private String version = "";
    private String symbol = "";
    private String uniqueKey = "";
	
}
