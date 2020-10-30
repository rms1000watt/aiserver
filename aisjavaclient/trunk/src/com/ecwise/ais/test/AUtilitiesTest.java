package com.ecwise.ais.test;

import java.util.Hashtable;

import com.ecwise.ais.AUtilities;

public class AUtilitiesTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {

		String aInput1 = "name\177value";
		String aInput2 = "name1\177value1\177name2\177value2";
		String aInput3 = "name1\177value1\177name2\177value2\177\177value3";
		String aInput4 = "name1\177value1\177name2\177value2\177name3\177";
		Hashtable<String, String> aResult;
		String[] aArray1 = {"one", "two", "three"};
		String aJoinResult;
		
		aResult = AUtilities.stringToStringDictionary(aInput1);
		
		testInteger(1, aResult.size(), "aResult.size()");
		testString("value", aResult.get("name"), "aResult.get(\"name\")");
		
		aResult = AUtilities.stringToStringDictionary(aInput2);
		
		testInteger(2, aResult.size(), "aResult.size()");
		testString("value1", aResult.get("name1"), "aResult.get(\"name1\")");
		testString("value2", aResult.get("name2"), "aResult.get(\"name2\")");
		
		aResult = AUtilities.stringToStringDictionary(aInput3);
		testInteger(2, aResult.size(), "aResult.size()");
		testString("value1", aResult.get("name1"), "aResult.get(\"name1\")");
		testString("value2", aResult.get("name2"), "aResult.get(\"name2\")");
		
		aResult = AUtilities.stringToStringDictionary(aInput4);
		testInteger(3, aResult.size(), "aResult.size()");
		testString("value1", aResult.get("name1"), "aResult.get(\"name1\")");
		testString("value2", aResult.get("name2"), "aResult.get(\"name2\")");
		testString("", aResult.get("name3"), "aResult.get(\"name3\")");
		
		aJoinResult = AUtilities.joinStringArray(aArray1, ":");
		testString("one:two:three:", aJoinResult, "aJoinResult");
	}
	
	public static void testBoolean(boolean iCondition, String iMessage) {

		System.out.println(iMessage + "..." + ((iCondition) ? "Pass" : "Fail"));
	}
	
	public static void testString(String iExpected, String iActual, String iMessage) {
		
		if (iExpected.matches(iActual)) {
			System.out.println(iMessage + "..." + "Pass");
		} else {
			System.out.println(iMessage + "..." + "Fail");
			System.out.println("\tExpected = " + iExpected + ", Actual = " + iActual);
		}
	}
	
	public static void testInteger(int iExpected, int iActual, String iMessage) {
		
		if (iExpected == iActual) {
			System.out.println(iMessage + "..." + "Pass");
		} else {
			System.out.println(iMessage + "..." + "Fail");
			System.out.println("\tExpected = " + iExpected + ", Actual = " + iActual);
		}
	}
}
