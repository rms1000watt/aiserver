/**********************************************************************************
	Copyright (C) 2009 Investment Science Corp.

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;

using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Net.Mail;

namespace AppClient
{
	public sealed class AUtilities
	{
        public static bool hasWhiteSpace(string iTestString)
        {
            foreach (char aChar in iTestString)
            {
                if (char.IsWhiteSpace(aChar))
                    return true;
            }
            return false;
        }
        /// <summary>
		/// Browse selected file system.
		/// </summary>
		/// <param name="iStr">Name of file. Empty if none</param>
		/// <returns>true iff a file is selected.</returns>
		public static bool Browse(string irFileName)
		{	OpenFileDialog aFileDialog = new OpenFileDialog();
			aFileDialog.InitialDirectory = "c:\\" ;
			aFileDialog.Filter = "txt files (*.txt)|*.txt|All files (*.*)|*.*" ;
			aFileDialog.FilterIndex = 2 ;
			aFileDialog.RestoreDirectory = true ;
			if (aFileDialog.ShowDialog() == DialogResult.OK)
			{	Stream myStream = null;
				try
				{	if ((myStream = aFileDialog.OpenFile()) != null)
						MessageBox.Show("Yes! Opened file: " + aFileDialog.FileName);
				}
				catch (Exception ex)
				{	MessageBox.Show("Error: Could not read file from disk. Original error: " + ex.Message);
				}
			}
			return (aFileDialog.FileName.Length > 0);
		}
		/// <summary>
		/// Converts and iEnd-separated string of name-value pairs into a map.
		/// </summary>
		/// <param name="iStr">String to be converted.</param>
		/// <param name="iMap">Holds the generated map.</param>
		/// <param name="iSep">Name-Value separator.</param>
		/// <param name="iEnd">Name-Value pair separator.</param>
		/// <returns></returns>
		public static bool StringToStringDictionary(string iStr, 
			ref SortedDictionary<string, string> iMap, string iSep, string iEnd)
		{	if (iStr.Length == 0)
				return false;
			string[] aPairs = iStr.Split(iEnd.ToCharArray(), StringSplitOptions.None);
			string aName;
			string aValue;
			int aSize;
			aSize = aPairs.Length;
			if (iSep == iEnd)
			{	for (int aIdx = 0; aIdx < aSize; ++aIdx)
				{	aName = aPairs[aIdx];
					if (aName.Length > 0)
					{	if (++aIdx != aSize)
							aValue = aPairs[aIdx];
						else
							aValue = "";
						iMap.Add(aName, aValue);
					}
				}
			}
			else
			{	string[] aPair = null;

				for (int aIdx = 0; aIdx < aSize; ++aIdx)
				{	aPair = aPairs[aIdx].Split(iSep.ToCharArray(), StringSplitOptions.None);
					aName = aPair[0];
					if (aName.Length != 0 && aPair.Length > 1)
						iMap.Add(aName, aPair[1]);
				}
			}
			return true;
		}

		/// <summary>
		/// Converts and DEL-separated string of name-value pairs into a map.
		/// </summary>
		/// <param name="iStr">String to be converted.</param>
		/// <param name="iMap">Holds the generated map.</param>
		/// <returns></returns>
		public static bool StringToStringDictionary(string iStr, ref SortedDictionary<string, string> iMap)
		{
			return StringToStringDictionary(iStr, ref iMap, "\x7F", "\x7F");
		}

		/// <summary>
		/// Returns true if the specified string contains an error.
		/// </summary>
		/// <param name="iRet"></param>
		/// <returns></returns>
		public static bool ErrorReturned(string iRet)
		{
			return (iRet.Length != 0 && iRet.StartsWith("!") && iRet.EndsWith("!"));
		}

		public static int GetArgNumber(byte[] iBuffer, int iOffset, ref int irOut)
		{
			bool aIsNegative = false;
			int aDigit = 0;
			irOut = 0;
			if (iBuffer == null)
			{	Console.Error.WriteLine("AUtilities::GetArgNumber, iBuffer is null");
				return -1;
			}
			if (iOffset < 0 || iOffset >= iBuffer.Length)
			{	Console.Error.WriteLine("AUtilities::GetArgNumber, iOffset is invalid");
				return -1;
			}
			// skip whitespaces
			while (iOffset < iBuffer.Length && char.IsWhiteSpace((char)iBuffer[iOffset]))
				++iOffset;
			if (iOffset == iBuffer.Length)
			{	Console.Error.WriteLine("AUtilities::GetArgNumber, buffer is empty");
				return -1;
			}
			// check for negative number
			if ((char)iBuffer[iOffset] == '-')
			{	aIsNegative = true;
				++iOffset;
				if (iOffset == iBuffer.Length)
				{	Console.Error.WriteLine("AUtilities::GetArgNumber, incomplete");
					return -1;
				}
			}
			if ((char)iBuffer[iOffset] <= '\x01')
			{	return 0;
			}
			else if (!char.IsDigit((char)iBuffer[iOffset]))
			{	Console.Error.WriteLine("AUtilities::GetArgNumber, non-numeric data");
				return -1;
			}
			else
			{	for (; (aDigit = (char)iBuffer[iOffset] - '0') >= 0 && aDigit <= 9; ++iOffset)
					irOut = irOut * 10 + aDigit;
			}
			if (aIsNegative)
				irOut = -irOut;
			return (iOffset + (iBuffer[iOffset] > 0x01 ? 1 : 0));
		}

		public static int GetArgString(byte[] iBuffer, char iDelimiter, int iOffset, ref string irOut)
		{
			char aC;
			int aEndOffset = 0;
			StringBuilder aString = new StringBuilder();
			if (iBuffer == null)
			{	Console.Error.WriteLine("AUtilities::GetArgString, iBuffer is null");
				return -1;
			}
			if (iOffset < 0 || iOffset >= iBuffer.Length)
			{	Console.Error.WriteLine("AUtilities::GetArgString, iOffset is invalid");
				return -1;
			}
			// skip whitespaces
			while (iOffset < iBuffer.Length && char.IsWhiteSpace((char)iBuffer[iOffset]))
				++iOffset;
			if (iOffset == iBuffer.Length)
			{	Console.Error.WriteLine("AUtilities::GetArgString, buffer is empty");
				return -1;
			}
			// scan for the separator, build output string
			for (aEndOffset = iOffset; (aC = (char)iBuffer[aEndOffset]) != iDelimiter && aC > '\x01'; ++aEndOffset)
			{	aString.Append(aC);
			}
			irOut = aString.ToString();
			return (aEndOffset + (iBuffer[aEndOffset] > 0x01 ? 1 : 0));
		}

		public static int ByteToString(byte[] iBuffer, int iOffset, int iLength, ref string iOut)
		{
			int aEnd = iOffset + iLength;
			if (iOffset >= iBuffer.Length || aEnd > iBuffer.Length)
			{	return -1;
			}
			StringBuilder aString = new StringBuilder();
			char aC;
			for (int i = iOffset; i < aEnd && (aC = (char)iBuffer[i]) > '\x01'; ++i)
			{	aString.Append(aC);
			}
			iOut = aString.ToString();
			return aEnd;
		}
		/// <summary>
		/// Cells in the first header row alphanumeric and unique.
		/// All cells in remaining rows are non-empty and numeric.
		/// The XXX property represents the name of ...</summary>
		/// <value>The XXX property gets/sets the _XXX member </value>
		/// <example> This example shows how to call validateSheet ..
		/// <para>Add a paragraph to this example</para>
		/// <code>test code here...</code></example>
		/// <exception cref="ErrorClass">Thrown when... Put before try...catch (Error Class)...</exception>
		/// <list type="bullet"><listheader> ... </listheader><item>...</item></item></listheader></list>
		/// <remarks> The <paramref name="iWorksheet"/> takes ...</remarks>
		/// <param name="iWorksheet">Worksheet to be validated.</param>
		/// <returns>void</returns>
		public static bool validateSheet(Excel.Worksheet iWorksheet)
		{
			// Select the smallest rectangle containing all occupied cells.
			bool aIsValid = true;
			object aUsed = iWorksheet.UsedRange.Value2;
			if (aUsed == null)
			{	MessageBox.Show("There are no values in the worksheet");
				return false;
			}
			if (aUsed.ToString() != "System.Object[,]")
			{	MessageBox.Show("The data in the worksheet is not a 2-dimensional array");
				return false;
			}
			object[,] aValues = aUsed as object[,];
			object aValue;
			Boolean aIsDouble;
			Excel.Range aRange = null;
			int aRowCount = aValues.GetUpperBound(0);
			int aColCount = aValues.GetUpperBound(1);
			string[] aHeaders = new string[aColCount + 1];
			for (int aRow = 1; aRow <= aRowCount; aRow++)
			{	for (int aCol = 1; aCol <= aColCount; aCol++)
				{	aValue = aValues[aRow, aCol];
					aIsDouble = aValue is Double;
					if (aValue == null)
					{	MessageBox.Show("Missing " + ((aRow == 1) ? "header" : "value"));
						// set cell red...
						aRange = (Excel.Range)iWorksheet.Cells[aRow, aCol];
						setRangeRed(aRange);
						aIsValid = false;
					}
					else
					{	if (aRow == 1)
						{	if (aIsDouble)
							{	MessageBox.Show("Header must be alpha-numeric. Missing header?");
								aRange = (Excel.Range)iWorksheet.Cells[aRow, aCol];
								setRangeRed(aRange);
								aIsValid = false;
							}
							else // Add this header to array of headers
							{	aHeaders[aCol] = (string)aValue;
							}
						}
						else if (!aIsDouble)
						{	MessageBox.Show("Not numeric. Data cells must be numeric");
							// set cell red
							aRange = (Excel.Range)iWorksheet.Cells[aRow, aCol];
							setRangeRed(aRange);
							aIsValid = false;
						}
					}
				}
			}
			// Unique headers.  The headers must be unique.  Compare 1st header with all the rest, 2nd header with the rest, etc.
			for (int aCol = 1; aCol < aColCount; ++aCol)
			{	for (int aNext = aCol + 1; aNext <= aColCount; ++aNext)
				{	if (aHeaders[aCol] == aHeaders[aNext])
					{	MessageBox.Show("Headers must be unique. Column header " + aCol + " matches column header " + aNext);
						aRange = (Excel.Range)iWorksheet.Cells[1, aCol];
						setRangeRed(aRange);
						aRange = (Excel.Range)iWorksheet.Cells[1, aNext];
						setRangeRed(aRange);
						aIsValid = false;
					}
				}
			}
			return aIsValid;
		}
		public static void setRangeRed(Excel.Range iRange)
		{
			iRange.Interior.Color = 255;
			// iRange.Interior.Color = System.Drawing.ColorTranslator.ToWin32(System.Drawing.Color.Red);
			// iRange.Interior.Color = System.Drawing.Color.Red.ToArgb();
			// iRange.Interior.Color = System.Drawing.Color.Red;
			// iRange.Interior.ColorIndex = 3;
		}
		/// <summary>
        /// Checks that two sets of headers are the same
        /// </summary>
        /// <param name="iWorksheet1">Worksheet containing one or more headers.</param>
		/// <param name="iWorksheet2">Worksheet containing one or more headers.</param>
        /// <returns>void</returns>
        public static bool compareHeaders(Excel.Worksheet iWorksheet1, Excel.Worksheet iWorksheet2)
        {
			bool aIsEqual = true;
            // Get the number of occupied columns in each worksheet
			object aUsed = iWorksheet1.UsedRange.Value2;
			if (aUsed == null)
			{	MessageBox.Show("There are no values in the first worksheet");
				return false;
			}
			if (aUsed.ToString() != "System.Object[,]")
			{	MessageBox.Show("The data in the first worksheet is not a 2-dimensional array");
				return false;
			}
			object[,] aValues1 = aUsed as object[,];
			int aColCount1 = aValues1.GetUpperBound(1);
			aUsed = iWorksheet2.UsedRange.Value2;
			if (aUsed == null)
			{	MessageBox.Show("There are no values in the second worksheet");
				return false;
			}
			if (aUsed.ToString() != "System.Object[,]")
			{	MessageBox.Show("The data in the second worksheet is not a 2-dimensional array");
				return false;
			}
			object[,] aValues2 = aUsed as object[,];
			int aColCount2 = aValues2.GetUpperBound(1);
			if (aColCount1 != aColCount2)
			{	MessageBox.Show("Worksheet headers differ.  First worksheet has " + aColCount1
					+ " headers and Second worksheet has " + aColCount2 + " headers.");
				return false;
			}
			string aValue1, aValue2;
			Excel.Range aRange = null;
			for (int aCol = 1; aCol <= aColCount1; ++aCol)
			{	aValue1 = (string)aValues1[1, aCol];
				aValue2 = (string)aValues2[1, aCol];
				if (aValue1 != aValue2)
				{	MessageBox.Show("Worksheet headers differ.  Headers in column " + aCol + " are not equal");
					aRange = (Excel.Range)iWorksheet1.Cells[1, aCol];
					setRangeRed(aRange);
					aRange = (Excel.Range)iWorksheet2.Cells[1, aCol];
					setRangeRed(aRange);
					aIsEqual = false;
				}
			}
			return aIsEqual;
        }

        /// <summary>
        /// Converts the used range of the given worksheet to a 2 dimensional object array
        /// It returns null if it the worksheet has invalid data
        /// </summary>
        /// <param name="iWorksheet">Worksheet to be converted.</param>
        /// <returns>object[,]</returns>
        public static object[,] worksheetToObjectArray(Excel.Worksheet iWorksheet)
        {
            // Select the smallest rectangle containing all occupied cells.
            object aUsed = iWorksheet.UsedRange.Value2;
            if (aUsed == null || aUsed.ToString() != "System.Object[,]")
            {  return null;
            }
            object[,] aValues = aUsed as object[,];
            return aValues;
        }
		/// <summary>
		/// Sends an email to the current GSM administrator
		/// </summary>
		/// <param name="iWorksheet">Worksheet to be converted.</param>
		/// <returns>true iff email is sent</returns>
		public static bool email(string iFrom, string iTo, string iSubject, string iMessage)
		{
			// Use Mail Message
			bool aRet = true;
			MailMessage aMsg = new MailMessage(new MailAddress(iFrom), new MailAddress(iTo));
			aMsg.IsBodyHtml = true;
			aMsg.Body = iMessage;
			aMsg.Subject = iSubject;
			SmtpClient aSender = new SmtpClient("smtp.west.cox.net", 25);		// First arg should be local SMTP server!
			try
			{	aSender.Send(aMsg);
			}
			catch(Exception iEx)
			{	MessageBox.Show("Unable to send email: " + iEx.ToString() + ". Please send your email directly to tedwilliams@alum.mit.edu");
				aRet = false;
			}
			return aRet;
		}
	}
}

