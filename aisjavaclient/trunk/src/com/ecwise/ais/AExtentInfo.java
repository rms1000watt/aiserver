package com.ecwise.ais;

import java.util.ArrayList;
import java.util.Hashtable;

public class AExtentInfo {

	private String nodePath;
	private String options;
	private ArrayList<ANodeInfo> nodes;
	private Hashtable<String, AExtentTypeInfo> extentTypes;
	
	public AExtentInfo() {
		nodePath = "";
		options = "";
		nodes = new ArrayList<ANodeInfo>();
		extentTypes = new Hashtable<String, AExtentTypeInfo>();
	}
	
	public AExtentInfo(String iNodePath, String iOptions, 
			ArrayList<ANodeInfo> iNodes, Hashtable<String, AExtentTypeInfo> iExtentTypes) {
		nodePath = iNodePath;
		options = iOptions;
		nodes = iNodes;
		extentTypes = iExtentTypes;
	}
	
	public String getNodePath() {
		return nodePath;
	}
	
	public void setNodePath(String iNodePath) {
		nodePath = iNodePath;
	}
	
	public String getOptions() {
		return options;
	}
	
	public void setOptions(String iOptions) {
		options = iOptions;
	}
	
	public ArrayList<ANodeInfo> getNodes() {
		return nodes;
	}
	
	public Hashtable<String, AExtentTypeInfo> getExtentTypes() {
		return extentTypes;
	}
}
