package com.ecwise.ais;

import java.util.Hashtable;

public class AUtilities {
	
	/**
	 * Joins the elements of a String array into a single String.
	 * @param iArray
	 * @param iSeparator
	 * @return
	 */
	public static String joinStringArray(String[] iArray, String iSeparator) {
		
		StringBuilder aStringBuilder = new StringBuilder();
		
		for (String iItem : iArray) {
			aStringBuilder.append(iItem + iSeparator);
		}
		
		return aStringBuilder.toString();
	}
	
	/**
	 * Joins the elements of a String array into a single String.
	 * @param iArray
	 * @param iSeparator
	 * @param iCount
	 * @return
	 */
	public static String joinStringArray(String[] iArray, String iSeparator, int iCount) {
		
		StringBuilder aStringBuilder = new StringBuilder();
		int aCount = Math.min(iCount, iArray.length);
		
		for (int i = 0; i < aCount; i++) {
			aStringBuilder.append(iArray[i] + iSeparator);
		}
		
		return aStringBuilder.toString();
	}
	
	/**
	 * Returns true if the given string is an error message.
	 * @param iStr
	 * @return
	 */
	public static boolean isError(String iStr) {
		return (iStr != null && !iStr.isEmpty() && iStr.startsWith("!") && iStr.endsWith("!"));
	}
	
	/**
	 * Breaks a DEL-delimited string into name value pairs.
	 * @param iStr
	 * @return
	 */
	public static Hashtable<String, String> stringToStringDictionary(String iStr) {
		return stringToStringDictionary(iStr, "\177", "\177");
	}
	
	/**
	 * Breaks a string into name value pairs.
	 * @param iStr
	 * @param iSep Separator between name-value pairs. Can be the same as iEnd.
	 * @param iEnd Separator between name and value. Can be the same as iSep.
	 * @return
	 */
	public static Hashtable<String, String> stringToStringDictionary(String iStr,
			String iSep, String iEnd) {
		
		String[] aPairs = iStr.split(iSep);
		String aName;
		String aValue;
		int aSize = (aPairs != null) ? aPairs.length : 0;
		Hashtable<String, String> aDictionary = new Hashtable<String, String>();
		
		if (iSep == iEnd) {
			for (int i = 0; i < aSize; i++) {
				aName = aPairs[i];
				if (!aName.isEmpty()) {
					if (++i != aSize) {
						aValue = aPairs[i];
					} else {
						aValue = "";
					}
					aDictionary.put(aName, aValue);
				} else {
					// if the name field is empty, then skip that pair
					i+=2;
				}
			}
		} else {
			for (int i = 0; i < aSize; i++) {
				String[] aPair = aPairs[i].split(iEnd);
				if (aPair != null && (aPair.length > 1)) {
					aDictionary.put(aPair[0], aPair[1]);
				}
			}
		}
		
		return aDictionary;
	}
}
