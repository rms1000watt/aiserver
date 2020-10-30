using System;
using System.Collections.Generic;
using System.Text;

namespace AppClient
{
    public sealed class AUtilities
    {
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
        {
            if (iStr.Length == 0)
                return false;

            string[] aPairs = iStr.Split(iEnd.ToCharArray(), StringSplitOptions.None);
            string aName;
            string aValue;
            int aSize;

            aSize = aPairs.Length;

            if (iSep == iEnd)
            {
                for (int aIdx = 0; aIdx < aSize; ++aIdx)
                {
                    aName = aPairs[aIdx];
                    if (aName.Length > 0)
                    {
                        if (++aIdx != aSize)
                            aValue = aPairs[aIdx];
                        else
                            aValue = "";

                        iMap.Add(aName, aValue);
                    }
                }
            }
            else
            {
                string[] aPair = null;

                for (int aIdx = 0; aIdx < aSize; ++aIdx)
                {
                    aPair = aPairs[aIdx].Split(iSep.ToCharArray(), StringSplitOptions.None);
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
            {
                Console.Error.WriteLine("AUtilities::GetArgNumber, iBuffer is null");
                return -1;
            }

            if (iOffset < 0 || iOffset >= iBuffer.Length)
            {
                Console.Error.WriteLine("AUtilities::GetArgNumber, iOffset is invalid");
                return -1;
            }

            // skip whitespaces
            while (iOffset < iBuffer.Length && char.IsWhiteSpace((char)iBuffer[iOffset]))
                ++iOffset;

            if (iOffset == iBuffer.Length)
            {
                Console.Error.WriteLine("AUtilities::GetArgNumber, buffer is empty");
                return -1;
            }

            // check for negative number
            if ((char)iBuffer[iOffset] == '-')
            {
                aIsNegative = true;
                ++iOffset;

                if (iOffset == iBuffer.Length)
                {
                    Console.Error.WriteLine("AUtilities::GetArgNumber, incomplete");
                    return -1;
                }
            }

            if ((char)iBuffer[iOffset] <= '\x01')
            {
                return 0;
            }
            else if (!char.IsDigit((char)iBuffer[iOffset]))
            {
                Console.Error.WriteLine("AUtilities::GetArgNumber, non-numeric data");
                return -1;
            }
            else
            {
                for (; (aDigit = (char)iBuffer[iOffset] - '0') >= 0 && aDigit <= 9; ++iOffset)
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
            {
                Console.Error.WriteLine("AUtilities::GetArgString, iBuffer is null");
                return -1;
            }

            if (iOffset < 0 || iOffset >= iBuffer.Length)
            {
                Console.Error.WriteLine("AUtilities::GetArgString, iOffset is invalid");
                return -1;
            }

            // skip whitespaces
            while (iOffset < iBuffer.Length && char.IsWhiteSpace((char)iBuffer[iOffset]))
                ++iOffset;

            if (iOffset == iBuffer.Length)
            {
                Console.Error.WriteLine("AUtilities::GetArgString, buffer is empty");
                return -1;
            }

            // scan for the separator, build output string
            for (aEndOffset = iOffset; (aC = (char)iBuffer[aEndOffset]) != iDelimiter && aC > '\x01'; ++aEndOffset)
            {
                aString.Append(aC);
            }

            irOut = aString.ToString();

            return (aEndOffset + (iBuffer[aEndOffset] > 0x01 ? 1 : 0));
        }

        public static int ByteToString(byte[] iBuffer, int iOffset, int iLength, ref string iOut)
        {
            int aEnd = iOffset + iLength;

            if (iOffset >= iBuffer.Length || aEnd > iBuffer.Length)
            {
                return -1;
            }

            StringBuilder aString = new StringBuilder();
            char aC;

            for (int i = iOffset; i < aEnd && (aC = (char)iBuffer[i]) > '\x01'; ++i)
            {
                aString.Append(aC);
            }

            iOut = aString.ToString();

            return aEnd;
        }
    }
}
