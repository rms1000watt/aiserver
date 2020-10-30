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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AUtilities.cpp															tlwms 2/7/02

									General Utilities

ANcsaLog records web server activity in a log file using the NCSA Common Log Forat (CLF).


DOCUMENTATION
 1.	See Include/utilities.h for class specification.

NOTES
 1.	Requires a getenv system call for all supported platforms.
 2.	Use tail recursion instead of recursive function calls to improve performance.

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


//	------------------------------ IMPORTS ---------------------------------------------------
#include <ctype.h>				// toupper
#include <stdlib.h>				// getenv
#include <qdir.h>				// QDir
#include <qdom.h>				// QDomDocument
#include "autilities.h"			// AUtil


//	----------------------------- DEFINITIONS ------------------------------------------------

//	--------------------------- MEMBER FUNCTIONS ---------------------------------------------

bool AUtil::ampmsgToXml(const QString& irAmpMsg, AStgMap& irAttribs,
					QString& orXml) throw()
{
	// Build the root agent
	QDomDocument aDoc;
	QDomElement aRoot = aDoc.createElement("amp");
	aDoc.appendChild(aRoot);

	// Add the attributes to the root element, xtype, target agent, speech act, ...
	AStgMap::Iterator itp;
	for (itp = irAttribs.begin(); itp != irAttribs.end(); ++itp)
	{	aRoot.setAttribute(itp.key(), itp.data());
	}
	aRoot.setAttribute("xtype", "return");
	
	if (!irAmpMsg.isEmpty())
	{	// Add the name-value pairs in msg as child elements of the root
		QStringList aTkns(QStringList::split('\177', irAmpMsg, true));
		QStringList::Iterator jtp;
		QDomText aNode;
		for (jtp = aTkns.begin(); jtp != aTkns.end(); ++jtp)
		{	QDomElement aElem = aDoc.createElement((*jtp).stripWhiteSpace());
			aRoot.appendChild(aElem);
			if (++jtp == aTkns.end())
			{	// Tack an element with empty content on the end.
				// This may not be what the caller had in mind.
				aNode = aDoc.createTextNode("");
				aElem.appendChild(aNode);
				break;
			}
			aNode = aDoc.createTextNode(*jtp);
			aElem.appendChild(aNode);
		}
	}
	// Note - Annoying extra newlines are sprinkled throughout the document.
	orXml = aDoc.toString(0).stripWhiteSpace();
	return true;
}

void buildAmpmsg(QDomElement& irElement, QString& iorAmpMsg)
{
	/* dead code
	// Expect the next node to be the content (ie. the value) for this element
	QDomText aTextChild = irElement.firstChild().toText();
	if (!aTextChild.isNull())
	{	iorAmpMsg += DELSTG + aTextChild.nodeValue().stripWhiteSpace();
	}
			HANDLE CASE OF CHILD ELEMENTS IN SUBTREE
	 */
	// Move to the next element in this sub tree
    QDomNode aNode = irElement.firstChild();

	// Convert siblings to name-value pairs added to AMP message.
	QDomText aDomText;
	QString aName, aValue, aPair;
    for (aNode; !aNode.isNull(); aNode = aNode.nextSibling())
	{	if (aNode.isElement())
		{	aName = aNode.nodeName();
			aDomText = aNode.firstChild().toText();
			if (!aDomText.isNull())
				aValue = aDomText.nodeValue().stripWhiteSpace();
			else
				aValue = "";			// Allow an empty field in the doc.
			aPair.sprintf("\177%s\177%s", (const char*)aName, (const char*)aValue);
			iorAmpMsg += aPair;
		}
    }
	while (iorAmpMsg.at(0) == '\177')
		iorAmpMsg = iorAmpMsg.mid(1);
}

// Fill all the bits from iBeg up to (but not including) iEnd with iValue
void AUtil::bitFill(int iBeg, int iEnd, uchar iValue, QByteArray* opBitVector) throw()
{
	uchar aBit, aResult;
	int aBegOffs = iBeg / 8;
	int aEndOffs = iEnd / 8;
	if (iBeg >= iEnd || aBegOffs > aEndOffs || aEndOffs >= (int)opBitVector->size())
		return;
	iBeg -= aBegOffs * 8;
	iEnd -= aEndOffs * 8;

	// Set the bits in the first partial byte from aBeg up to aEnd1
	if (iBeg > 0)
	{	int aEnd1 = (aEndOffs == aBegOffs) ? iEnd : 8;
		aResult = opBitVector->at(aBegOffs);
		aBit = 1 << iBeg;
		for (int i = iBeg; i < aEnd1; ++i, aBit <<= 1)
			if (iValue > 0)
				aResult |= aBit;
			else
				aResult &= ~aBit;
		opBitVector->at(aBegOffs++) = aResult;
	}
	// Fill whole bytes in the range
	aBit = (iValue > 0) ? ~0 : 0;
	for (; aBegOffs < aEndOffs; ++aBegOffs)
		opBitVector->at(aBegOffs) = aBit;

	// Set the bits at the end of the last partial byte from 0 up to aEnd
	if (iEnd > 0)
	{	aBit = 1;
		aResult = opBitVector->at(aEndOffs);
		for (int i = 0; i < iEnd; ++i, aBit <<= 1)
			if (iValue > 0)
				aResult |= aBit;
			else
				aResult &= ~aBit;
		opBitVector->at(aEndOffs) = aResult;
	}
}

// Expands $(envVar) and $param$ in irIn.  Looks up values in irNameValues and irGbPairs.
// Sets value to ?envVar? or ?param? if name is not found 
bool AUtil::expandList(const QString& irIn, AStgMap& irNameValues, AStgMap& irGbPairs,
				QStringList& orList, char iSep) throw()
{
	bool aNoErrs = true;
	QString aSep("\\s*;\\s*");	// Ignore leading and trailing spaces
	if (iSep > ' ' && iSep < '\177')
		aSep[3] = iSep;
	QRegExp aRxSep(aSep);		// Regular expression for splitting strings

	// Split the input string into tokens.
	QStringList aInList = QStringList::split(aRxSep, irIn);
	// Reverse the token order to improve performance of the value list
	QStringList aTokList;
	QStringList::Iterator itp;
	if (!aInList.isEmpty())
	{	itp = aInList.end();
		do
		{	aTokList << *(--itp);
		} while (itp != aInList.begin());
	}
	int aStart, aLgth, aPos;					// Index into the next token
	char* apEnvVar;
	QRegExp aRxEnv("\\$\\(\\s*(\\w*)\\s*\\)");	// Matches $(NAME)
	QRegExp aRxParam("\\$\\s*(\\w*)\\s*\\$");	// Matches $NAME$
	QString aName, aValue;
	while (!aTokList.isEmpty())
	{	// Pop the next input token off of the end of the list
		itp = aTokList.end();
		QString aTok = *(--itp), a;
		aTokList.remove(itp);

		//  If a name found, substitute each value into the token and put token back on input.
		if ((aStart = aTok.find('$')) >= 0)
		{	aValue = "";
			if ((aPos = aRxEnv.search(aTok, aStart, QRegExp::CaretAtOffset)) >= 0)
			{	aName = aRxEnv.cap(1);
				aLgth = 3;			// Length of delimiters $()
				if ((apEnvVar = getenv((const char*)aName)) != NULL)
					aValue = apEnvVar;
			}
			else if ((aPos = aRxParam.search(aTok, aStart, QRegExp::CaretAtOffset)) >= 0)
			{	aName = aRxParam.cap(1);
				aLgth = 2;			// Length of delimiters $$
				if (irGbPairs.contains(aName))
					aValue = irGbPairs[aName];
				else if (irNameValues.contains(aName))
					aValue = irNameValues[aName];
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
				QStringList aValList = QStringList::split(aRxSep, aValue, false);
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
/*	-----------------------------------------------------------------------------------------------
	hexToInt - Convert next 2 hex chars to a 8-bit value

	ippStg		-> -> to 1 or 2 hex chars
	NOTE:		Increments pointer past converted chars in string
	RETURNS:	Hex value or -1 if no hex character
	-------------------------------------------------------------------------------- */
long AUtil::hexToInt(char** ippStg)
{
	static long aDigit[]={0,1,2,3,4,5,6,7,8,9,16,16,16,16,16,16,16,10,11,12,13,14,15}; 
	char* apStg = *ippStg;			// -> next char in input
	long aValue = 0;				// integer value of result
	for (int i = 0; i < 2; ++i)
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
// Makes subdirectories in irPath if they don't exist, starting from irRootPath.
// Warning! set iIgnoreFile to true iff a file name is on the end of irPath.
bool AUtil::makePath(const QString& irRootPath, const QString& irPath,
			bool iIgnoreFile, QString& orFullPath) throw()
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
	QStringList aDirList(QStringList::split(QRegExp("[/\\\\]"), irPath));
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
			if (!aDir.mkdir(*itp, false))
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

bool AUtil::readFile(const QString& irFilePath, QString& orContents)
{
	bool aOk = false;
	if (!QFile::exists(irFilePath))
		return aOk;

	QFile aFile(irFilePath);
	if (aFile.open(IO_Translate | IO_ReadOnly))
	{  	QTextStream aTs(&aFile);
		orContents = aTs.read();
		aFile.close();
		aOk = true;
	}
	return aOk;
}
// If irCh is slash/backslash replace leading backslash/slash w/ irCh
// Else if first char not already irCh, prepend w/ irCh
// Return true iff string is modified.
bool AUtil::startStg(const QChar& irCh, QString& iorStg) throw()
{
	// Strip off leading backslashes and extra slashes
	bool aTerm = false;
	const QChar& arCh = iorStg[0];
	if ((irCh == '/' || irCh == '\\') && (arCh == '/' || arCh == '\\'))
	{	// If not a single slash/backslash, strip leading slashes/backslashes
		if (irCh != arCh || irCh == iorStg[1])
		{	int i;
			for (i = 0; iorStg[i] == '/' || iorStg[i] == '\\'; ++i)
				;
			iorStg = iorStg.mid(i);
		}
	}
	if (irCh != iorStg[0])
	{	iorStg.prepend(irCh);
		aTerm = true;
	}
	return aTerm;
}
// Date is of the form MM/dd/yyyy  e.g. 04/25/1962
// Returns true iff a valid date in irDate.
bool AUtil::string2Date(const QString& irDate, QDate& orDate) throw()
{
	bool aValid = false;
	QStringList aTkns;
	int aY, aM, aD;
	aTkns = QStringList::split('/', irDate, true);
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
// Convert a string list into a Sep-delimited string. Return true iff string modified.
QString AUtil::stringListToString(QString& irSep, const QStringList& irList)
{
	return irList.join(irSep);	
}

// Converts a Sep-separated string of name-value pairs into a map

bool AUtil::stringToStringMap(char iSep, const QString& irStg, AStgMap& orMap)
{
	if (irStg.isEmpty())
		return false;

	QStringList aStgList = QStringList::split(iSep, irStg, true);
	QString aName;
	QStringList::Iterator itp;
	for (itp = aStgList.begin(); itp != aStgList.end(); ++itp)
	{	aName = *itp;
		if (++itp != aStgList.end())
			orMap[aName] = *itp;
		else
		{	orMap[aName] = "";
			break;
		}
	}
	return true;
}
// Converts a string map to a iSep delimited set of name-value pairs
bool AUtil::stringMapToString(AStgMap& irMap, QString& orStg, char iSep, char iEnd)
{
	if (irMap.isEmpty())
		return false;

	QString aPair;
	AStgMap::Iterator itp;
	for (itp = irMap.begin(); itp != irMap.end(); ++itp)
	{	aPair.sprintf("%s%c%s%c", (const char*)itp.key(), iSep,
				(const char*)itp.data(), iEnd);
		orStg += aPair;
	}
	return true;
}
bool AUtil::terminateStg(const QChar& irCh, QString& iorStg) throw()
{
	bool aTerm = true;
	// Treat newline as a special case
	if (irCh == '\n' || irCh == '\r')
	{	chop(iorStg);
		iorStg.append(irCh);
	}
	else
	{	uint aLast = iorStg.length() - 1;
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

/*	-----------------------------------------------------------------------------------
	urlDecode - Decode a url string
	Warning: String modified in place.

	irStg		Ref to string to be decoded
	RETURNS:	Ref to modified string 
	-------------------------------------------------------------------------------- */
QString& AUtil::urlDecode(QString& iorStg, bool iCnvPlus)
{
	if (!iorStg.isEmpty())
	{	QCString aStg(iorStg);
		urlDecode(aStg, iCnvPlus);
		iorStg = aStg;
	}
	return iorStg;
}

QCString& AUtil::urlDecode(QCString& iorStg, bool iCnvPlus)
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

/*	-----------------------------------------------------------------------------------
	urlEncode - Encode a url string

	iorStg		String to be encoded. This string is modified in place.
	iUsePlus	Replace a space with plus sign, else, use %20.
	RETURNS:	Reference to encoded string.
	-------------------------------------------------------------------------------- */
QCString& AUtil::urlEncode(QCString& iorStg, bool iUsePlus)
{
	int aLgth = (int)iorStg.length();	// Length of the input string
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
	QCString aOut(j+1);					// Holds result + null
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

QString& AUtil::xmlDecode(QString& iorStg) throw()
{
	// Decode predefined entities
	int aPos;
	for (aPos = 0; (aPos = iorStg.find('&', aPos)) >= 0 ; ++aPos)
	{	int aLgth;
		if ((aLgth = iorStg.find(';', aPos)) > 0 && (aLgth -= aPos - 1) <= 6)
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

bool AUtil::xmlToAmpmsg(const QString& irXml, QString& orAmpmsg,
			AStgMap& orAttribs) throw()
{
	bool aConvert = true;
	QDomDocument aDoc;
	if (!aDoc.setContent(irXml))
		aConvert = false;				// Unable to parse input
	
	// Extract the name and attributes from the root element, xid, target, act
    QDomElement aRoot = aDoc.documentElement();
	QString aRootName = aRoot.tagName();
	QDomAttr aAttr = aRoot.attributeNode("xid");
	if (!aAttr.isNull())
		orAttribs["xid"] = aAttr.value();
	aAttr = aRoot.attributeNode("connectID");
	if (!aAttr.isNull())
		orAttribs["connectID"] = aAttr.value();
	orAmpmsg.truncate(0);
	QString aTarget, aAct;
	aAttr = aRoot.attributeNode("target");
	if (!aAttr.isNull())
		orAttribs["target"] = aTarget = aAttr.value();	
	aAttr = aRoot.attributeNode("act");
	if (!aAttr.isNull())
		orAttribs["act"] = aAct = aAttr.value();

	if (!aTarget.isEmpty())
		orAmpmsg.sprintf("%s\177%s", (const char*)aTarget, (const char*)aAct);
	// Add any child elements to the AMP message
	buildAmpmsg(aRoot, orAmpmsg);
	if (aRootName.lower() != "amp")
		aConvert = false;			// Not an AMP msg

	return aConvert;
}
