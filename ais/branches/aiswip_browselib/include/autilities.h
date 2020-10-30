/**********************************************************************************
    Copyright (C) 2008 Investment Science Corp.

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

#ifndef AUTILITIES_H
#define AUTILITIES_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/autilities.h
															Utilities

CHANGE HISTORY
Version	Date		Who		Change
1.0111	10/25/2006	tlw		getRecord. Gets everything up to separator including whitespace, parens.
1.0108	 9/29/2006	tlw		Add toLower to getArg
1.0100	 6/16/2005	tlw		Add documentation.
1.0062	 5/14/2005	tlw		Add formatConsoleLog to convert ConsoleLog records to XML. Add getArg and getArgValue.
1.0058	 3/28/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include	<QtCore/qdatetime.h>
#include	<QtCore/qmap.h>
#include	<QtCore/qstring.h>
#include	<QtCore/qstringlist.h>
#include	<QtCore/qregexp.h>

typedef QHash<QString, QString> AStgMap;			// Name-value pairs
typedef QMap<QString, QString> AStringMap;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
/*!
\brief AUtil provides a few general-purpose utilities that are widely used by other classes.  

These utilities could just as well be a
set of stand-alone functions rather than a class.  The only reason that they are in a class is for organizational and naming
purposes.  Just one instance of this class is required for each application.

Applications have some high-level objects that are designated as "controllers".  Controllers establish policy that may be used
by all other objects in their implementation.  The controller is the place to declare this class.

\par Multitasking.
These methods are thread safe.  Since these utilities require no shared data, one global instance may be used in a multi-threaded
environment.

\par Usage
 1.	Create one instance of AUtil in application controller.
 2.	Call static methods using the scope resolution operator. For example, AUtil::chop(aString)
 */
class AUtil
{
public:
	static bool		ampmsgToXml(const QString& irAmpMsg, AStringMap& irAttribs, QString& orXml);
	static void		bitClear(QByteArray* opBitVector);
	static void		bitFill(long iBeg, long iEnd, uchar iValue, QByteArray* opBitVector);
	static void		bitSet(long iIndex, uchar iValue, QByteArray* opBitVector);
	static long		bitValue(long iIndex, const QByteArray* ipBitVector);
	static bool		chop(QString& iorIn);
	static bool		compareMaps(bool iDoInSrc, AStgMap& irDst, AStgMap& irSrc, QString& orInDst, QString& orInSrc);
	static bool		expandList(const QString& irItems, AStringMap& irPairs, AStringMap& irParams,QStringList& orList,char iSep=';');
	static bool		formatConsoleLog(const QString& irRecords, QString& orXml);
	static long		getArg(const char* ipArg, char iSep, QString& orArg, bool iTrim = false, bool iToLower = false);
	static long		getArgValue(const char *ipArg, long* opValue);
	static long		getRecord(const char* ipArg, char iSep, QString& orArg);
	static long		hexToInt(char** ippStg);
	static bool		isBlank(const QString& irIn);
	static bool		isComment(const QString& irIn);
	static bool		makePath(const QString& irRootPath, const QString& irPath, bool iIgnoreFile, QString& orFullPath);
	static bool		override(bool iDoGlobals, AStgMap& irDst, AStgMap& irSrc, QString& orInSrc);
	static bool		readFile(const QString& irFilePath, QString& orContents);
	static bool		startStg(const QChar& irCh, QString& iorStg);
	static bool		stringToDate(const QString& irDate, QDate& orDate);
	static bool		stringCompare(const char* ipStg1, const char* ipStg2);
	static bool		stringToStringMap(const QString& irStg, AStringMap& orMap, char iSep = '\177', char iEnd = '\177');
	static bool		stringMapToString(AStringMap& irMap, QString& orStg, char iSep = '\177', char iEnd = '\177');
	static bool		terminateStg(const QChar& irCh, QString& iorStg);
	static long		toLong(const char* ipStg, long* opLgth = NULL);
	static QString&	urlDecode(QString& iorStg, bool iCnvPlus = true);
	static QByteArray& urlDecode(QByteArray& iorStg, bool iCnvPlus = true);
	static QByteArray& urlEncode(QByteArray& iorStg, bool iUsePlus = true);
	static QString&	 xmlDecode(QString& iorStg);
	static QString&	 xmlEncode(QString& iorStg);
	static bool		 xmlToAmpmsg(const QString& irXml, QString& orAmpmsg, QString& orAct, QString& orContext, QString& orMode,
					 QString& orTarget);
};

/*!
\brief bitClear - Clear all the bits in a bit vector

A bit vector is implemented as a set of bytes.
\param opBitVector -> to a vector holding the bits to be cleared
\return void
 */
inline void AUtil::bitClear(QByteArray* opBitVector)
{
	long aSize = opBitVector->size();
	for (long i = 0; i < aSize; ++i)
		opBitVector[i] = 0;
}

//   0 <= Index <= (arraySize * 8) - 1
/*!
\brief bitSet - Set a specific bit in a bit array to iValue.

A bit vector is implemented as a set of bytes.
\param iIndex - Bit offset into the bit vector supplied.
\param iValue - Bit value (>0 is one and 0 is 0)
\param opBitVector -> to a vector holding the bit to be set
\return void
\par Notes:
-# The number of bits in the bit vector must exceed iIndex.
 */
inline void AUtil::bitSet(long iIndex, uchar iValue, QByteArray* opBitVector)
{
	long aOffset = iIndex / 8;		// Calculate byte offset
	if (aOffset >= opBitVector->size()) return;

	iIndex -= aOffset * 8;			// Convert to bit offset
	uchar aByte = opBitVector->at(aOffset);
	if (iValue > 0)
		aByte |= (uchar)(1 << iIndex);
	else
		aByte &= ~(uchar)(1 << iIndex);
	(*opBitVector)[(int)aOffset] = aByte;
}

/*!
\brief bitValue - Return the bit value at iIndex

A bit vector is implemented as a set of bytes.
\param iIndex - Bit offset into the bit vector supplied.
\param ipBitVector -> to a vector holding the bit to be fetched.
\return aValue - The value of the bit (0 or 1)
\par Notes:
-# The number of bits in the bit vector must exceed iIndex.
 */
inline long AUtil::bitValue(long iIndex, const QByteArray* ipBitVector) 
{
	long aOffset = iIndex / 8;
	if (aOffset >= ipBitVector->size()) return -1;
	iIndex -= aOffset * 8;

	return (ipBitVector->at(aOffset) & (uchar)(1 << iIndex)) > 0 ? 1 : 0;
}

/*!
\brief chop - Remove last set of newline chars from a string, if any

\param iorIn - String to be chopped
\return aTrunc - True iff the string was modified
\par Notes:
-# Only the last LF, CRLF, CR, or LFCR is truncated.
 */
inline bool AUtil::chop(QString& iorIn)
{
	int last = iorIn.length() - 1;
	int pos = last;		//  Current position
	bool aTrunc = false;

	if (last >= 0)
	{	if (iorIn[pos] == '\n')
		{	if (iorIn[--pos] == '\r')
				--pos;
		}
		else if (iorIn[pos] == '\r')
		{	if (iorIn[--pos] == '\n')
				--pos;
		}
		if (pos < last)
		{	iorIn.truncate(++pos);
			aTrunc = true;
		}
	}
	return aTrunc;
}
/*!
\brief isBlank - Detect a blank line

\param irIn - String to be tested
\return True iff the line contains nothing but whitespace chars.
\par Note:
-# The QString::isEmpty() method is only true if the string contains no chars.
 */
inline bool AUtil::isBlank(const QString& irIn)
{	return irIn.indexOf("^\\s*$") == 0;}

/*!
\brief isComment - Detect a blank line or a line that begins with #

\param irIn - String to be tested
\return True iff the line contains just whitespace or begins with #
 */
inline bool AUtil::isComment(const QString& irIn)
{	return irIn.indexOf(QRegExp(QString("^\\s*(#|$)")), 0/*from*/) >= 0; }

/*!
\brief stringCompare - Return true iff two cstrings are identical

\param ipStg1 -> First string to be compared
\param ipStg2 -> Second string to be compared
\return True iff the strings are identical
\par Notes:
-# The comparison is case sensitive.
 */
inline bool AUtil::stringCompare(const char* ipStg1, const char* ipStg2)
{
	while (*ipStg1 != '\0' && *ipStg2 != '\0' && *ipStg1++ == *ipStg2++)
		;
		
	return (*ipStg1 == '\0' && *ipStg2 == '\0');
}
#endif // AUTILITIES_H

