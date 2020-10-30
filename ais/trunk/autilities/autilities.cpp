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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/autilities/autilities.cpp
															Utilities

CHANGE HISTORY
Version	Date		Who		Change
1.0111	10/25/2006	tlw		getRecord. Gets everything up to separator including whitespace, parens.
1.0108	 9/28/2006	tlw		getArg, Add separator to args, ToLower
1.0104	 9/10/2006	tlw		toLong, getArg, getArgValue. argLgth now includes separator but not a terminator.
1.0065	 6/22/2005	tlw		formatConsoleLog. Modify to fix lingering problems with generated XML.
1.0062	 5/15/2005	tlw		formatConsoleLog. Format console records into multiple XML elements
1.0061	 5/5/2005	tlw		expandList().Convert $param$ to lower case in expandList. Add compareMaps(), override()
1.0061	 5/4/2005	tlw		ampmsgToXml: Handcraft, do not use QDomDocument. Add new xmlEncode().
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
DOCUMENTATION
 1.	See code below for more details

NOTES
 1.	Requires a getenv system call for all supported platforms.
 2.	Use tail recursion instead of recursive function calls to improve performance.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <ctype.h>				// toupper
#include <stdlib.h>				// getenv
#include <QtCore/QDir>
#include <QtCore/QIODevice>
#include <QtCore/QtDebug>
#include <QtXml/QDomElement>

#include "autilities.h"			// AUtil
#include "aringfile.h"			// RINGFILE_SOC

//	-------------------------------------------------- MEMBER FUNCTIONS -------------------------------------------------------
/*!
\brief ampmsgToXml - Convert DEL-delimited string of name-value pairs into an XML document

\param irAmpmsg - DEL-delimited string
\param irAttribs - Attributes to be added to root element tag
\param orXml - Generated XML document
\return true
\par Notes:
-# Note that QDomDocument::aDoc.toString() adds in extra newlines between elements which can cause the client heartburn.
Do NOT use QT for this task. QT is not up to the job.
\par Format:
\verbatim
<amp  act="Act" status="Status" target="Target" xid="123" xtype="return"><Arg>...</Arg></amp>
\endverbatim
 */
bool AUtil::ampmsgToXml(const QString& irAmpmsg, AStringMap& irAttribs, QString& orXml)
{
	// Root Tag. Build the root tag
	QString aElem;			// Xml child elements to be added to returned document.
	QString aEncoded;		// Xml-encoded child element to be added to the returned XML document
	QString aXml("<amp");	// Defer writing to orXml in case orXml also refers to irAmpmsg

	// Attributes. Add the attributes to the root element, xid, target Lambda, speech act, status.
	QTextStream aOut(&aXml, QIODevice::WriteOnly);
	AStringMap::ConstIterator apIt;
	for (apIt = irAttribs.constBegin(); apIt != irAttribs.constEnd(); ++apIt)
		aOut << ' ' << apIt.key() << "=\"" << apIt.value() << '"';

	if (irAmpmsg.isEmpty())
		aOut << "/>";
	else
	{	// Child Elements. Add the name-value pairs in msg as child elements of the root
		QStringList aTkns(irAmpmsg.split('\177', QString::KeepEmptyParts));
		aOut << '>';
		long aEnd = aTkns.count();
		for (long aTkn =0; aTkn < aEnd; ++aTkn)
		{	QString arTag = aTkns[aTkn];
			QString aTag =  arTag.isEmpty() ? "noTag" : arTag;
			if (++aTkn >= aEnd || (aElem = aTkns[aTkn]).isEmpty())
			{	aOut << '<' << aTag << "/>";
				break;
			}
			else if (aElem.at(0) == RINGFILE_SOC)
			{	formatConsoleLog(aElem, aEncoded);
				aOut << aEncoded;
			}
			else
			{	xmlEncode(aElem);
				aOut << '<' << aTag << '>' << aElem << "</" << aTag << '>';
			}
		}
		aOut << "</amp>";
	}
	orXml = aXml;
	return true;
}

/*
\brief buildAmpmsg - Appends the name-value pairs from child elements of the root element of an XML doc. to a DEL-delimited string

\param irElement - Root element
\param iorAmpMsg - Holds the name-value pairs extracted from the child elements
\return void
\par Notes:
-# Assumes the content of this element contains a text value.
-# The document consists of a root element containing zero or more child elements containing text.
 */
static void buildAmpmsg(QDomElement& irElement, QString& iorAmpMsg)
{
	// Move to the next element in this sub tree
    QDomNode aNode = irElement.firstChild();

	// Convert siblings to name-value pairs added to AMP message.
	QDomText aDomText;
	QString aName, aValue, aPair;
    for (; !aNode.isNull(); aNode = aNode.nextSibling())
	{	if (aNode.isElement())
		{	aName = aNode.nodeName();
			aDomText = aNode.firstChild().toText();
			if (!aDomText.isNull())
				aValue = aDomText.nodeValue();
			else
				aValue = "";			// Allow an empty field in the doc.
			aPair = QString("\177%1\177%2").arg(aName, aValue);
			iorAmpMsg += aPair;
		}
    }
    // Strip off leading DEL characters
	while (iorAmpMsg.at(0) == '\177')
		iorAmpMsg = iorAmpMsg.mid(1);
}
/*!
\brief bitFill - Fill all the bits in a byte array from iBeg to iEnd with the binary value iValue

\param iBeg - Bit offset from the lsb of the multi-byte value in opBigVector
\param iEnd - Bit offset from the lsb of the multi-byte value in opBigVector
\param iValue - Binary fill value
\param opBitVector - Holds the multi-byte value to be filled
\return void
\par Note:
-# Least-significant byte in at position 0 of byte array (little Endian order).
 */
void AUtil::bitFill(long iBeg, long iEnd, uchar iValue, QByteArray* opBitVector)
{
	uchar aBit, aResult;
	long aBegOffs = iBeg / 8;		// Beg. byte offset into opBitVector
	long aEndOffs = iEnd / 8;		// End byte offset into opBitVector
	if (iBeg >= iEnd || aBegOffs > aEndOffs || aEndOffs >= opBitVector->size())
		return;
	iBeg -= aBegOffs * 8;			// Set iBeg to bit offset in beg byte
	iEnd -= aEndOffs * 8;			// Set iEnd to bit offset in end byte

	// Set the bits in the first partial byte from aBeg up to aEnd1
	if (iBeg > 0)
	{	long aEnd1 = (aEndOffs == aBegOffs) ? iEnd : 8;
		aResult = opBitVector->at(aBegOffs);
		aBit = 1 << iBeg;
		for (long i = iBeg; i < aEnd1; ++i, aBit <<= 1)
			if (iValue > 0)
				aResult |= aBit;
			else
				aResult &= ~aBit;
		(*opBitVector)[(int)aBegOffs++] = aResult;
	}
	// Fill whole bytes in the range
	aBit = (iValue > 0) ? ~0 : 0;
	for (; aBegOffs < aEndOffs; ++aBegOffs)
		(*opBitVector)[(int)aBegOffs] = aBit;

	// Set the bits at the end of the last partial byte from 0 up to aEnd
	if (iEnd > 0)
	{	aBit = 1;
		aResult = opBitVector->at(aEndOffs);
		for (long i = 0; i < iEnd; ++i, aBit <<= 1)
			if (iValue > 0)
				aResult |= aBit;
			else
				aResult &= ~aBit;
		(*opBitVector)[(int)aEndOffs] = aResult;
	}
}

/*!
\brief compareMaps - Note differences in two maps.

\param iDoInSrc - If true, just find the parameters in irSrc but not in irDst; else, do both.
\param irDst - First map to be compared. Key: param name, Value: param value.
\param irSrc - Second map to be compared. Key: param name, Value: param value.
\param orInDst - Comma-delimited list of param names in irDst but not in irSrc
\param orInSrc - Comma-delimited list of param names in irSrc but not in irDst
\return aDiff - Returns true if any differences found
\par Notes:
-# If a config file just contains a subset, set iInSrc to true, put default params in irDst and file params in irSrc.
compareMaps will return the parameter names in the config file but not in the current set.
 */
bool AUtil::compareMaps(bool iDoInSrc, AStgMap& irDst, AStgMap& irSrc, QString& orInDst, QString& orInSrc)
{
	// In Source. Find params in irSrc but not in irDst.
	bool aSrcDiff = false;		// True iff at least one difference found in irSrc.
	QString aInSrc;				// Params in irSrc but not in irDst
	AStgMap::iterator apIt, apEnd = irSrc.end();
	for (apIt = irSrc.begin(); apIt != apEnd; ++apIt)
	{	const QString& irKey = apIt.key();
		if (!irDst.contains(irKey))
		{	aInSrc += irKey + ',';
			aSrcDiff = true;
		}
	}
	// Terminate. Remove final comma.
	if (aSrcDiff)
		orInSrc = aInSrc.left(aInSrc.length() - 1);
	
	// In Destination. Find params in irDst but not in irSrc.
	bool aDstDiff = false;		// True iff at least one difference found in irSrc
	if (!iDoInSrc)
	{	QString aInDst;			// Params in irDst but not in irSrc
		for (apIt = irDst.begin(), apEnd = irDst.end(); apIt != apEnd; ++apIt)
		{	const QString& irKey = apIt.key();
			if (!irSrc.contains(irKey))
			{	aInDst += irKey + ',';
				aDstDiff = true;
			}
		}
		// Terminate. Remove final comma
		if (aDstDiff)
			orInDst = aInDst.left(aInDst.length() - 1);
	}
	return (aSrcDiff || aDstDiff);
}

/*!
\brief expandList - Expand variables of the form $(var) and $param$ in irIn.

\param irIn - An iSep-delimited list of one or more tokens containing zero or more variables to be expanded
\param irPairs - Key: param name, Value: param value. Holds parameter definitions used to evaluate $param$
\param irParams	- Key: param name, Value: param value. Holds additional definitions used to evaluate $param$.
\param orList - Holds the expanded tokens extracted from irIn
\param iSep - Token separator in lists. Defaults to semicolon.
\return aNoErrs - Returns true if no suspicious constructs encountered.
\par Notes:
-# Environment variables in irIn are represented by $(var) wher var is the name of the environment variable.
-# Parameter names in irIn are represented by $param$ where param is parameter name.
-# An environment variable value is a semicolon delimited list of one or more tokens. A token may contain an env variable.
-# Parameter names are converted to lower case so that parameter names are not case sensitive.
-# Looks up parameter values in irPairs and irParams.  If parameter is defined in both, the value in irParams trumps.
-# Sets value to ?envVar? or ?param? if name is not found where envVar is variable name and param is the parameter name.
 */
bool AUtil::expandList(const QString& irIn, AStringMap& irPairs, AStringMap& irParams, QStringList& orList, char iSep)
{
	bool aNoErrs = true;
	QString aSep("\\s*;\\s*");	// Ignore leading and trailing spaces
	if (iSep > ' ' && iSep < '\177')
		aSep[3] = iSep;
	QRegExp aRxSep(aSep);		// Regular expression for splitting strings

	// Split the input string into tokens.
	QStringList aInList = irIn.split(aRxSep, QString::KeepEmptyParts);
	// Reverse the token order to improve performance of the value list
	QStringList aTokList;
	QStringList::Iterator itp;
	if (!aInList.isEmpty())
	{	itp = aInList.end();
		do
		{	aTokList << *(--itp);
		} while (itp != aInList.begin());
	}
	long aStart, aLgth, aPos;					// Index into the next token
	char* apEnvVar;
	QRegExp aRxEnv("\\$\\(\\s*(\\w*)\\s*\\)");	// Matches $(NAME)
	QRegExp aRxParam("\\$\\s*(\\w*)\\s*\\$");	// Matches $NAME$
	QString aName, aValue;
	while (!aTokList.isEmpty())
	{	// Pop the next input token off of the end of the list
		itp = aTokList.end();
		QString aTok = *(--itp);
		aTokList.erase(itp);

		//  If a name found, substitute each value into the token and put token back on input.
		if ((aStart = aTok.indexOf('$')) >= 0)
		{	aValue = "";
			if ((aPos = aRxEnv.indexIn(aTok, aStart, QRegExp::CaretAtOffset)) >= 0)
			{	aName = aRxEnv.cap(1);
				aLgth = 3;			// Length of delimiters $()
				if ((apEnvVar = getenv(aName.toLatin1().data())) != NULL)
					aValue = apEnvVar;
			}
			else if ((aPos = aRxParam.indexIn(aTok, aStart, QRegExp::CaretAtOffset)) >= 0)
			{	aName = aRxParam.cap(1).toLower();
				aLgth = 2;			// Length of delimiters $$
				if (irParams.contains(aName))
					aValue = irParams[aName];
				 else if (irPairs.contains(aName))
					aValue = irPairs[aName];
			}
			if (aPos >= 0)
			{	// Remove the name from the token
				aLgth += aName.length();
				aTok.remove(aPos, aLgth);

				// If name not resolved into a value, substitute ?name? for the value.
				if (aValue.isEmpty())
				{	aNoErrs = false;
					aValue = '?' + aName + '?';
				}
				// Substitute each value into the token and push each one backwards onto
				// input for further expansion.
				QStringList aValList = aValue.split(aRxSep, QString::KeepEmptyParts);
				if (!aValList.isEmpty())
				{	itp = aValList.end();
					do	// Put the next value into token
					{	aValue = aTok;
						aValue.insert(aPos, *(--itp));
						// and push it back onto the input list
						aTokList.append(aValue);
					} while (itp != aValList.begin());
				}
			}
		}
		else
			aPos = -1;
		// Else, append token to output list
		if (aPos < 0)
			orList.append(aTok);
	}	// end while
	return aNoErrs;
}

/*!
\brief formatConsoleLog - Convert records stored in a ring file into an XML document.

\param irRecords - Console log records which may include result and display records.
\param orXml - Records reformatted as a set of XML elements with reqid and sessionid attributes.
\return aOk - True iff the conversion succeeds.
\par Notes:
-# Converts a sequence of records into a sequence of elements.
-# The converted elements are returned in iorRecords.
-# Sample returned element: \<Result reqid="4" sessionid="3"\>Some &amp; result\</Result\>
-# The Body is XML encoded.
-# A record is a DEL-delimited set of fields: VLength|Type|RqId|SessionId|Body
\verbatim
V		RINGFILE_SOC - Start-of-record character (vertical tab)
Length	One or more digits representing the length of Type|RqId|SessionId|Body.
Type	R for a result, D for a display. The tag name for R records is result, and display for D records.
RqId	ID of the originating request.
SessionId	ID of the originating session.
Body	The content of the resulting element in the XML document.
\endverbatim
 */
bool AUtil::formatConsoleLog(const QString& irRecords, QString& orXml)
{
	// Validate input.
	if (irRecords.isEmpty() || irRecords.at(0) != RINGFILE_SOC)
		return false;

	// Record. Get list of records from the input stream. For now, split on V instead of extracting length.
	bool aOk = true;			// False iff conversion fails.
	QString aBody;				// Body of the next record
	QString aHdr;				// Next header extracted from the record
	long aRqId;					// Message ID extracted from the header
	long aSnId;					// SessionId extracted from the header
	QChar aType;				// Type of record extracted from the header (R or D)
	const char* apType;
	long aBeg, aEnd = -1;		// Offset to beginning and end of next token.
	long aLength;				// Length as noted above.
	QStringList aTkns = irRecords.split(RINGFILE_SOC, QString::SkipEmptyParts);
	long aSz = aTkns.count();
	for (long aTkn = 0; aTkn < aSz; ++aTkn)
	{	aBody = aTkns[aTkn];
		aBeg = 0;
		// Length.
		if ((aEnd = aBody.indexOf('|', aBeg)) < 0)
			break;
		aHdr = aBody.mid(aBeg,  aEnd - aBeg);
		if ((aLength = aHdr.toUInt()) != aBody.length() - (aEnd + 1))
		{	aEnd = -1;
			break;
		}
		aBeg = aEnd + 1;	// Skip over '|'
		// Type (R or D).
		if ((aEnd = aBody.indexOf('|', aBeg)) < aBeg || ((aType = aBody[(int)aBeg]) != 'R' && aType != 'D'))
		{	aEnd = -1;		// Type mismatch
			break;
		}
		aBeg = aEnd + 1;
		// RqId.
		if ((aEnd = aBody.indexOf('|', aBeg)) < aBeg)
			break;
		aHdr = aBody.mid(aBeg, aEnd - aBeg);
		aRqId = aHdr.toLong();
		aBeg = aEnd + 1;
		// SessionId.
		if ((aEnd = aBody.indexOf('|', aBeg)) < aBeg)
			break;
		aHdr = aBody.mid(aBeg, aEnd - aBeg);
		aSnId = aHdr.toLong();
		aBeg = aEnd + 1;
		// Body.
		aBody = aBody.mid(aBeg);
		xmlEncode(aBody);

		// Generate element.  Convert next record into an element with attributes.
		apType = (aType == 'R') ? "Result" : "Display";
		aHdr = QString("<%1 reqid=\"%2\" sessionid=\"%3\">%4</%5>").arg(apType).arg(aRqId).arg(aSnId).arg(aBody, apType);
		orXml += aHdr;
	}
	if (aEnd < 0)
	{	qDebug() << "AUtil::formatConsoleLog(" << aBody << "), Unable to parse header.";
		aOk = false;
	}
	return aOk;
}

/*!
\brief getArg - Extract the next argument from a string of tokens.

The string of tokens may be "\tfoo, fu bar )\n".  In this case args are "foo" and "fu bar"
\param ipArg ->iSep-delimited string of tokens
\param iSep Separator between args
\param orArg Holds extracted argument
\param iTrim Do not include trailing white space in orArg.
\param iToLower Convert returned arg to lower case.
\return aLgth	Length of extracted arg in chars including separator and white space unless at terminator.
\par Notes:
-# Skips over leading white space. If iTrim, do not include trailing whitespace in arg.
-# Scan terminates on iSep, newline, null or ^A.
-# If iSep is comma, scan also terminates on a right paren.
-# Clears orArg if only excluded chars encountered.
-# Returns 0 if at end of the buffer.
 */
long AUtil::getArg(const char* ipArg, char iSep, QString& orArg, bool iTrim, bool iToLower)
{
	long aLgth = 0;
	char aC, aSep;				// Next char in the argument, secondary separator.
	const char* apBeg = ipArg;	// -> beg of argument
	const char *apEnd, *apEof;	// -> end of argument, -> end of field (including white space and separators)

	// Skip. Skip leading white space
	while (*apBeg == ' ' || *apBeg == '\t')
		++apBeg;

	// apEnd. Scan to a separator or to terminator at end of string.
	aSep = (iSep == ',') ? ')' : iSep;
	for (apEof = apBeg; (aC = *apEof) != iSep  && aC != aSep && aC != '\n' && aC > '\01'; ++apEof)
		;

	// Back. If iTrim, do not include trailing white space in argument
	apEnd = apEof;
	if (iTrim)
	{	while ((apEnd > apBeg) && ((aC = *(apEnd-1)) == ' ' || aC == '\t'))
			--apEnd;
	}
	// Extract. Extract the argument from the input stream.
	if (apEnd > apBeg)
	{	QByteArray aArg(apBeg, apEnd - apBeg);
		orArg = (iToLower) ? aArg.toLower() : aArg;
	}
	else
		orArg.clear();

	// Move by separator, if any
	if (*apEof > '\01')
		++apEof;
	aLgth = apEof - ipArg;
	return aLgth;
}

/*!
\brief getArgValue - Extract the numeric value of a string

\param ipArg -> DEL-delimited string of tokens
\param opValue -> Holds integer result
\return Lgth length of field including separator unless at terminator.
\par Notes:
-# Returns 0xcececece in opValue if no digits found.
 */
long AUtil::getArgValue(const char *ipArg, long* opValue)
{
	// Scan. Scan to end of argument
	long aLgth = 0;
	*opValue = toLong(ipArg, &aLgth);
	return aLgth;
}

/*!
\brief getRecord - Extract the next record from iSep-delimited string

Unlike getArg, getRecord includes everything in record up to the separator.
The scan only stops on iSep, null or ^A.
\param ipArg ->iSep-delimited string of records
\param iSep Separator between records
\param orArg Holds extracted argument
\return aLgth	Length of extracted record in chars including separator iSep but not a terminator.
\par Notes:
-# If iSep is a null or ^A, it will not be included in the returned length.
-# If no trailing separator, quits on null or ^A.
-# Includes whitespace, parens in the record.
-# Clears orArg if only terminating chars area are encountered.
-# Returns 0 if ipArg is at terminator.
 */
long AUtil::getRecord(const char* ipArg, char iSep, QString& orArg)
{
	long aLgth = 0;
	char aC;					// Next char in the record.
	const char* apBeg = ipArg;	// -> beg of argument
	const char *apEnd;			// -> end of argument

	// apEnd. Scan to separator or to terminator at end of string.
	for (apEnd = apBeg; (aC = *apEnd) != iSep && aC > '\01'; ++apEnd)
		;
	// Extract. Extract the argument from the input stream.
	if (apEnd > apBeg)
	{	QByteArray aArg(apBeg, apEnd - apBeg);
		orArg = aArg;
	}
	else
		orArg.clear();

	// Move by separator, if any
	if (*apEnd > '\01')
		++apEnd;
	aLgth = apEnd - ipArg;
	return aLgth;
}

/*!
\brief hexToInt - Convert next 2 hex chars to a 8-bit value

\param ippStg -> to 1 or 2 hex chars
\return aValue - Binary value or -1 if no hex character
\par Note:
-# Increments pointer past converted chars in string
 */
long AUtil::hexToInt(char** ippStg)
{
	static long aDigit[]={0,1,2,3,4,5,6,7,8,9,16,16,16,16,16,16,16,10,11,12,13,14,15}; 
	char* apStg = *ippStg;			// -> next char in input
	long aValue = 0;				// integer value of result
	for (long i = 0; i < 2; ++i)
	{	char aCh = *apStg;
		long aD;						// value of the next converted char

		if (islower(aCh)) aCh = _toupper(aCh);
		if (aCh >= '0' && aCh <= 'F' && (aD = aDigit[aCh - '0']) < 16)
			aValue = (aValue << 4) | aD;
		else
		{	if (i == 0)		// Nothing to convert
				aValue = -1;
			break;
		}
		++apStg;
	}
	*ippStg = apStg;
	return aValue;
}

/*!
\brief makePath - Makes subdirectories in irPath if they don't exist, starting from irRootPath

\param irRootPath - Starting point in file system. Normally an absolute path.
\param irPath - Path to be appended to the root path.
\param iIgnoreFile - If true, makePath does not make a directory for the last name in irPath.
\param orFullPath - Holds the full path that was made (irRootPath + irPath).
\return aOk - true if desired path is constructed (or already exists).
\par Notes:
-# Warning! Set iIgnoreFile to true iff a file name is on the end of irPath.
-# Separator between directories may be a slash or a backslash.
-# A new directory is not created if it already exists in the path.
-# But once a directory is created, all its subdirectories are created.
 */
bool AUtil::makePath(const QString& irRootPath, const QString& irPath, bool iIgnoreFile, QString& orFullPath)
{
	// Start with the root path
	if (irPath.isEmpty())
	{	if (!orFullPath.isNull()) orFullPath = irRootPath;
		return true;
	}
	QString aPath(irRootPath);	// Fully qualified path starting from RootPath
	QDir aDir(aPath);
	if (irRootPath.isEmpty() || !aDir.exists())
		return false;

	// Extract list of subdirectories from irPath to be appended to aPath
	QStringList aDirList(irPath.split(QRegExp("[/\\\\]"), QString::SkipEmptyParts));
	QString aLast = aDirList.last();

	// Ignore the file name on the end in making the path
	if (iIgnoreFile)
		aDirList.pop_back();
	
	// Keep adding subdirectories to aPath until end or one is missing
	bool aOk = true;				// Set to true iff makePath succeeds
	bool aPathExists = true;		// All directories in path exist
	QStringList::Iterator itp;
	QString aXtnd(aPath);			// aPath extended to next subdirectory
	for (itp = aDirList.begin(); itp != aDirList.end(); ++itp)
	{	if (aPathExists)
		{	aXtnd += "/" + *itp;
			aDir.setPath(aXtnd);
			if (aDir.exists())
				aPath = aXtnd;
			else
				aPathExists = false;
		}
		if (!aPathExists)			// Chain of subdirs in path has been broken
		{	aDir.setPath(aPath);
			if (!aDir.mkdir(*itp))
			{	aOk = false;
				break;
			}
			aPath += "/" + *itp;
		}
	}
	if (!orFullPath.isNull())
	{	orFullPath = aPath + "/";
		if (iIgnoreFile)
			orFullPath += aLast;
	}
	return aOk;
}

/*!
\brief override - Override the settings in irSrc with those in irDst

\param iDoGlobals - Include globals in irSrc if the parameter name begins with gbl and vice versa.
\param irDst - Destination map. Key: param name, Value: param value.
\param irSrc - Source map. Key: param name, Value: param value.
\param orInSrc - List of parameters in irSrc but not in irDst.
\return aOk - Returns true if no parameters found in irSrc but not in irDst
\par Notes:
-# Put default parameters in irDst and config file parameters in irSrc. override will update parameters in irDst.
-# If a parameter in irSrc but not in default list, the parameter is not added to irDst, but param name is added to orInSrc.
 */
bool AUtil::override(bool iDoGlobals, AStgMap& irDst, AStgMap& irSrc, QString& orInSrc)
{
	bool aOk = true;		// Set to false iff an entry found in irSrc is not in irDst
	AStgMap::iterator apIt, apEnd = irSrc.end();
	for (apIt = irSrc.begin(); apIt != apEnd; ++apIt)
	{	const QString& irKey = apIt.key();
		if (irKey.startsWith("gbl", Qt::CaseInsensitive) == iDoGlobals)
		{	if (irDst.contains(irKey))
			{	irDst[irKey] = *apIt;
			}
			else	// Missing. Add parameter name to orInSrc if parameter is not in the destination map.
			{	if (!aOk) orInSrc += ',';
				orInSrc += irKey;
				aOk = false;
			}
		}
	}
	return aOk;
}

/*!
\brief readFile - Read a text file given the file specification

\param irFilePath - Path plus file name.  The path may be relative or absolute
\param orContents - Holds the contents of the file.
\return aOk - true iff able to read the file.
\par Notes:
-# Translates CRLF into newlines on Windows platform and CR into newlines on Apple platform.
\verbatim
\r - Carriage return (0x0d)
\n - Line feed (0x0a)
Platform    End-of-Line Converted  End-of-Line on
            In File     To:        write back to file
--------    ----------  ---------  --------------
Windows     \r\n        \n         \r\n
            \n          \n         \r\n
            \r          (deleted)

Linux       \n          \n         \n
            \r\n		\n         \n
            \r          (deleted)
\endverbatim
 */
bool AUtil::readFile(const QString& irFilePath, QString& orContents)
{
	bool aOk = false;
	if (!QFile::exists(irFilePath))
		return aOk;

	QFile aFile(irFilePath);
	if (aFile.open(QIODevice::Text | QIODevice::ReadOnly))		// 16 | 1
	{  	QTextStream aTs(&aFile);
		orContents = aTs.readAll();
		aFile.close();
		aOk = true;
	}
	return aOk;
}

/*!
\brief startStg - If first char is not irCh, prepend string with irCh

\param irCh - Leading char
\param iorStg - String to be modified
\return aTerm - true iff string is modified
\par Notes:
-# If irCh is slash/backslash, replace leading backslash/slash w/ irCh.
 */
bool AUtil::startStg(const QChar& irCh, QString& iorStg)
{
	// Strip off leading backslashes and slashes
	bool aTerm = false;
	QChar aCh = iorStg[0];
	if (irCh != aCh)
	{	if ((irCh == '/' || irCh == '\\') && (aCh == '/' || aCh ==  '\\'))
			iorStg.replace(0, 1, irCh);
		else	// Prepend string with irCh
			iorStg.prepend(irCh);
		aTerm = true;
	}
	return aTerm;
}

/*!
\brief stringToDate - Convert a date string to a QDate

\param irDate - Holds string containing date of the form mm/dd/yyyy (04/25/1962)
\param orDate - Date to be set
\return aValid - true iff string holds a valid date
\par Notes:
-# The year must be 4 digits greater than 2000.
 */
bool AUtil::stringToDate(const QString& irDate, QDate& orDate)
{
	bool aValid = false;
	QStringList aTkns;
	long aY, aM, aD;
	aTkns = irDate.split('/', QString::KeepEmptyParts);
	if (aTkns.size() == 3)
	{	aM = aTkns[0].toInt();
		aD = aTkns[1].toInt();
		aY = aTkns[2].toInt();
		if (aM > 0 && aD > 0 && aY >= 2000 && QDate::isValid(aY, aM, aD))
		{	orDate = QDate(aY, aM, aD);
			aValid = true;
		}
	}
	return aValid;
}

/*!
\brief stringToStringMap  Converts an iEnd-separated string of name-value pairs into a map.

\param irStg - String to be converted
\param orMap - Holds generated map
\param iSep - Character separating name and values
\param iEnd - Character separating pairs
\return true iff input is not empty.
\par Note:
-# iSep and iEnd may be the same character.
-# Ignores empty entries (no pair following iEnd). Nothing added to the map.
-# If no value for name-value pair, the value in the map is set to an empty string.
 */
bool AUtil::stringToStringMap(const QString& irStg, AStringMap& orMap, char iSep, char iEnd)
{
	if (irStg.isEmpty())
		return false;

	QStringList aPairs = irStg.split(iEnd, QString::KeepEmptyParts), aPair;
	QString aName;
	QStringList::Iterator itp;
	if (iSep == iEnd)
	{	for (itp = aPairs.begin(); itp != aPairs.end(); ++itp)
		{	aName = *itp;
			if (!aName.isEmpty())
			{	if (++itp != aPairs.end())
					orMap[aName] = *itp;
				else
				{	orMap[aName] = "";
					break;
				}
			}
		}
	}
	else	// Separator between pairs is iEnd. Separator between name-value is iSep
	{	for (itp = aPairs.begin(); itp != aPairs.end(); ++itp)
		{	aPair = (*itp).split(iSep, QString::KeepEmptyParts);
			aName = aPair[0];
			if (!aName.isEmpty() && aPair.count() >= 2)
				orMap[aPair[0]] = aPair[1];
		}
	}
	return true;
}

/*!
\brief stringMapToString - Converts a string map to a iEnd-delimited set of name-value pairs

\param irMap - Map with zero or more entries
\param orStg - Holds the generated string.
\param iSep - Separator inserted between name and value in orStg
\param iEnd - Separator inserted between pairs
\return True iff map is not empty.
\par Notes:
-# A separator is not appended to the end of the result string.
-# iSep and iEnd can be the same character.
 */
bool AUtil::stringMapToString(AStringMap& irMap, QString& orStg, char iSep, char iEnd)
{
	if (irMap.isEmpty())
		return false;

	QString aPair;
	orStg.truncate(0);
	AStringMap::Iterator itp = irMap.begin();
	for (;;)
	{	aPair = QString("%1%2%3").arg(itp.key()).arg(iSep).arg(itp.value());
		orStg += aPair;
		if (++itp != irMap.end())
			orStg += iEnd;
		else
			break;
	}
	return true;
}

/*!
\brief terminateStg - Terminate a string with irCh

\param irCh - Terminating character
\param iorStg - String to be terminated
\return aTerm - true iff string is modified
\par Notes:
-# No change is made if the trailing character is already irCh, except:
-# If irCh is a slash/backslash, a trailing backslash/slash is replaced by irCh
 */
bool AUtil::terminateStg(const QChar& irCh, QString& iorStg)
{
	bool aTerm = true;
	if (iorStg.isEmpty())
		aTerm = false;

	// Treat newline as a special case
	else if (irCh == '\n' || irCh == '\r')
	{	chop(iorStg);
		iorStg.append(irCh);
	}
	else
	{	long aLast = iorStg.length() - 1;
		const QChar& arCh = iorStg.at(aLast);
		if ((irCh == '/' && arCh == '\\') || (irCh == '\\' && arCh == '/'))
			iorStg.replace(aLast, 1, arCh);
		else if (irCh != arCh)
			iorStg.append(irCh);
		else
			aTerm = false;
	}
	return aTerm;
}

/*!
\brief toLong - Convert a numeric string to an integer

\param ipStg -> base-10 numeric string of the form: -0123456789
\param opLgth -> returned length including separator (unless at end of string).
\return aRet - integer value
\par Notes:
-# opLgth may be null.
-# Skips leading whitespace.
-# Returns 0 if ipStg is empty (no error message).
-# Returns 0xcececece if ipStg is null or if it does not contain at least one leading digit.
 */
long AUtil::toLong(const char* ipStg, long* opLgth)
{
	long aDigit, aRet = 0;
	const char* apBeg = ipStg;
	bool aIsNegative = false;
	if (apBeg == NULL)
	{	qDebug("AUtil::toLong(%s), Input pointer is null", apBeg);
		if (opLgth != NULL)
			*opLgth = 0;
		return ~(0x42424242);
	}
	while (*apBeg == ' ' || *apBeg == '\t')
		++apBeg;

	if (*apBeg == '-')
	{	aIsNegative = true;
		++apBeg;
	}
	if (*apBeg <= '\01')
		aRet = 0;
	else if (*apBeg < '0' || *apBeg > '9')
	{	qDebug("AUtil::toLong(%s), Input string is not numeric", apBeg);
		aRet = 0xcececece;
	}
	else
	{	for (; (aDigit = *apBeg - '0') >= 0 && aDigit <= 9; ++apBeg)
			aRet = aRet * 10 + aDigit;
	}
	if (opLgth  != NULL)
		*opLgth = (apBeg - ipStg) + (*apBeg > '\01' ? 1 : 0);
	return (aIsNegative) ? -aRet : aRet;
}

/*!
\brief urlDecode - Decodes a URL-encoded string

\param iorStg	- String to be decoded
\param iCnvPlus - If true urlDecode converts + to a space
\return Reference to iorStg
\par Notes:
-# Warning! String modified in place.
-# Ref: HTML: The Definitive Guide
 */
QString& AUtil::urlDecode(QString& iorStg, bool iCnvPlus)
{
	if (!iorStg.isEmpty())
	{	QByteArray aStg(iorStg.toLatin1());
		urlDecode(aStg, iCnvPlus);
		iorStg = aStg;
	}
	return iorStg;
}

/*!
\brief urlDecode - Decodes a URL-encoded string

\param iorStg - String to be decoded
\param iCnvPlus - If true urlDecode convert '+' to a space
\return Reference to iorStg
\par Notes:
-# Input is QByteArray rather than a QString
-# Warning! String modified in place.
-# Ref: HTML: The Definitive Guide
 */
QByteArray& AUtil::urlDecode(QByteArray& iorStg, bool iCnvPlus)
{
	char* apBfr = iorStg.data();	// -> current input char

	if (apBfr != NULL)
	{	char aCh;
		long aValue;					// Integer value of 2 hex chars
		char* apOut = apBfr;			// -> current output char
		while (*apBfr != '\0')
		{	// Replace + with a space
			if ((aCh = *apBfr++) == '+' && iCnvPlus)
				aCh = ' ';
			else if (aCh == '%')
			{	if (*apBfr == '%')
					++apBfr;
				else if ((aValue = hexToInt(&apBfr)) < 0)
					break;			// Nothing to convert
				else				// Convert next 2 hex chars to equivalent ASCII code
					aCh = (char)aValue;
			}
			*apOut++ = aCh;
		}
		*apOut = '\0';
	}
	return iorStg;
}

/*!
\brief urlEncode - URL encodes a string

\param iorStg		String to be encoded
\param iUsePlus	If true urlEncode converts spaces to '+' rather than %20
\return Reference to iorStg
\par Notes:
-# Warning! String modified in place.
-# Ref: HTML: The Definitive Guide
 */
QByteArray& AUtil::urlEncode(QByteArray& iorStg, bool iUsePlus)
{
	long aLgth = iorStg.length();	// Length of the input string
	if (aLgth <= 0) return iorStg;

	// Determine total result length (to improve performance)
	int j = 0, i;
	for (i = 0; i < aLgth; ++i)
	{	char aCh = iorStg[i];
		switch (aCh)
		{// Reserved characters (vf. HTML Def. Guide, p196)
		case ' ':
			if (iUsePlus)
			{	++j;
				break;
			}
		case ';':
		case '/':
		case '?':
		case ':':
		case '@':
		case '=':
		case '&':
		// Unsafe characters
		case '<':
		case '>':
		case '"':
		case '#':
		case '%':
		case '{':
		case '}':
		case '|':
		case '\\':
		case '^':
		case '~':
		case '[':
		case ']':
		case '`':	// backquote
			j += 3;
			break;
		default:
			++j;
			break;
		}
	}
	QByteArray aOut;
	aOut.resize(j+1);		// Holds result + null
	for (i = 0, j = 0; i < aLgth; ++i)
	{	char aCh = iorStg[i], aHex;
		switch (aCh)
		{// Reserved characters (vf. HTML Def. Guide, p196)
		case ' ':
			if (iUsePlus)
			{	aOut[j++] = '+';
				break;
			}
		case ';':
		case '/':
		case '?':
		case ':':
		case '@':
		case '=':
		case '&':
		// Unsafe characters
		case '<':
		case '>':
		case '"':
		case '#':
		case '%':
		case '{':
		case '}':
		case '|':
		case '\\':
		case '^':
		case '~':
		case '[':
		case ']':
		case '`':	// backquote
			aOut[j++] = '%';
			aHex = aCh >> 4;
			aHex += (aHex <= 9) ? '0' : 'A' - 10;
			aOut[j++] = aHex;
			aHex = aCh & 0xF;
			aHex += (aHex <= 9) ? '0' : 'A' - 10;
			aOut[j++] = aHex;
			break;
		default:
			aOut[j++] = aCh;
			break;
		}
	}
	aOut[j++] = '\0';
	iorStg.resize(j);
	iorStg = aOut;
	return iorStg;
}

/*!
\brief xmlDecode - Decodes an XML-encoded string. Converts the 5 predefined entities in XML to their values

\param iorStg - String to be decoded
\return Reference to iorStg
\par Notes:
-# Warning! Be sure to avoid converting &amp;lt; into &lt; and then converting it.
-# Ref: The XML Handbook, by Goldfarb.
 */
QString& AUtil::xmlDecode(QString& iorStg)
{
	// Decode predefined entities
	long aPos;
	for (aPos = 0; (aPos = iorStg.indexOf('&', aPos)) >= 0 ; ++aPos)
	{	long aLgth;
		if ((aLgth = iorStg.indexOf(';', aPos)) > 0 && (aLgth -= aPos - 1) <= 6)
		{	QString aTok = iorStg.mid(aPos, aLgth);
			if (aTok == "&amp;")
				iorStg.replace(aPos, aLgth, "&");
			else if (aTok == "&lt;")
				iorStg.replace(aPos, aLgth, "<");
			else if (aTok == "&gt;")
				iorStg.replace(aPos, aLgth, ">");
			else if (aTok == "&apos;")
				iorStg.replace(aPos, aLgth, "'");
			else if (aTok == "&quot;")
				iorStg.replace(aPos, aLgth, "\"");
		}
	}
	return iorStg;
}

/*!
\brief xmlEncode - Encode an XML-encoded string. Converts the 5 symbols to their predefined entities

\param iorStg		String to be enccoded
\return Reference to iorStg
\par Notes:
-# Convert & first to avoid changing leading & in predefined entities.
-# Ref: The XML Handbook, by Goldfarb.
-# Converts the following characters to their predefined entities:
\verbatim
&	&amp;
'	&apos;
<	&lt;
>	&gt;
'	&quot;
\endverbatim
 */
QString& AUtil::xmlEncode(QString& iorStg)
{
	// Encode predefined entities
	iorStg.replace('&', "&amp;");
	iorStg.replace('\'', "&apos;");
	iorStg.replace('<', "&lt;");
	iorStg.replace('>', "&gt;");
	iorStg.replace('"', "&quot;");
	return iorStg;
}

/*!
\brief xmlToAmpmsg - Converts XML document to an DEL-delimited string

\param irXml - XML document
\param orAmpmsg - Holds generated DEL-delimited string
\param orAct - Holds speech act extracted from the root element attributes
\param orContext - Holds context name extracted from the root element attributes
\param orMode - Holds mode value extracted from the root element attributes
\param orTarget - Holds target Lambda name extracted from the root element attributes
\return aOk - true iff able to read the file.
\par Notes:
-# The target and act attributes are prepended to the returned AMP message: MyTarget|MyAct|Arg0|Value0|...
-# XML format:
\verbatim
 <amp  act="MyAct" status="0" target="MyTarget" context="MyContext" mode="async"><Arg0>Value0</Arg0>...</amp>
\endverbatim
 */
bool AUtil::xmlToAmpmsg(const QString& irXml,QString& orAmpmsg,QString& orAct,QString& orContext,QString& orMode,QString& orTarget)
{
	bool aConvert = true;
	QDomDocument aDoc;
	if (!aDoc.setContent(irXml))
	{	return false;
		//aConvert = false;				// Unable to parse input
	}
	
	// Extract the root name (should be amp) from the XML 
    QDomElement aRoot = aDoc.documentElement();
	QString aRootName = aRoot.tagName();

	// Extract the attributes from the root element: act, context, mode, target
	QDomAttr aAttr = aRoot.attributeNode("act");
	if (!aAttr.isNull())
		orAct = aAttr.value();
	aAttr = aRoot.attributeNode("context");
	if (!aAttr.isNull())
		orContext = aAttr.value();
	aAttr = aRoot.attributeNode("mode");
	if (!aAttr.isNull())
		orMode = aAttr.value();
	aAttr = aRoot.attributeNode("target");
	if (!aAttr.isNull())
		orTarget = aAttr.value();

	orAmpmsg.truncate(0);
	if (!orTarget.isEmpty())
		orAmpmsg = QString("%1\177%2").arg(orTarget, orAct);

	// Add any child elements to the AMP message
	buildAmpmsg(aRoot, orAmpmsg);
	if (aRootName.toLower() != "amp")
		aConvert = false;			// Not an AMP msg

	return aConvert;
}
