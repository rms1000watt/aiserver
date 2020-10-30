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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/autilities/namedValues.cpp
									Named Values

CHANGE HISTORY
Version	Date		Who		Change
1.0061	5/4/2005	tlw		readPairs. Remove notInMap and notInFile args.
1.0057	3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS -----------------------------------------------------------
#include <QtCore/QDir>		// QDir
#include <QtCore/QFile>		// QFile
#include <QtCore/QFileInfo>	// QFileInfo
#include <QtCore/QTextStream>	// QTextStream
#include "anamedvalues.h"		// ANamedValues
#include "autilities.h"			// AUtil 

//	------------------------------------------------- LOCAL DECLARATIONS ------------------------------------------------------

//	-------------------------------------------------- MEMBER FUNCTIONS -------------------------------------------------------
/*!
\brief ANamedValues fetches a set of name-value pairs from persistent storage and returns them in a StringMap.

The string map key is the name of the parameter and the string map value is the value (a string) of the parameter.
\param irFileName - The path + file name of the file that holds the name value pairs on disk
\return void
 */
ANamedValues::ANamedValues(const QString& irFileName)
{
	cFileName = irFileName;
}

/*!
\brief readPairs - Extract the name value pairs from the currently selected configuration file

\param iExpand - If true, expand env variables and $param$ found in file name and in values
\param iDoGlobals - Include config file definitions if name begins with gbl; else, skip parameters not beginning with gbl.
\param iorPairs - List of default name-value pairs. Updated with values from configuration file
\param irParams - Auxililiary map holding parameter definitions for use in expanding $param$.
\param orInFile - Comma-separated list of parameter names that are in config file, but not in iorPairs.
\return aOk - true iff no parameter names missing.
\par Notes:
-# Expanded values containing variables are expanded ad infinitum.
-# If current file spec expands to multiple files, the first readable file in the list is used.
-# If DoGlobals, parameters in config file whose name starts with gbl are included and vice versa.
-# A parameter in the config file but not in iorPairs is not added to iorPairs, but name is added to orInFile.
-# A variable encountered before it is defined is not expanded.
-# Environment variables are of the form $(NAME).
-# Parameter values may include variables of the form $NAME$ where NAME is any previously encountered name.
 */
bool ANamedValues::readPairs(bool iExpand, bool iDoGlobals, AStringMap& iorPairs, AStringMap& irParams, QString& orInFile)
{
	// File Name. Expand the environment variables in cFileName
	QFile aIniFile;				// File to be used to fetch values.
	QStringList aFileList;		// List of file specifications, names in qmPairs
	bool aFoundOne = false;		// Found a parameter file
	bool aOk = true;			// No parameters in config file that are not in iorPairs.
	orInFile.truncate(0);
	if (iExpand && cFileName.indexOf('$') >= 0)
	{	// Iterate over the files in the list.  Stop on first success, else, return 0
		if (AUtil::expandList(cFileName, iorPairs, irParams, aFileList, ';'))
		{	QStringList::Iterator itp;
			for (itp = aFileList.begin(); itp != aFileList.end(); ++itp)
			{	aIniFile.setFileName(*itp);
				if (aIniFile.open(QIODevice::Text | QIODevice::ReadOnly))	// 16|1
				{	aFoundOne = true;
					break;
				}
			}
		}
	}
	else
	{	aIniFile.setFileName(cFileName);
		if (aIniFile.open(QIODevice::Text | QIODevice::ReadOnly))	// 16|1
			aFoundOne = true;
	}
	if (aFoundOne)
	{	// Read. Read lines in file.
		QTextStream aInput(&aIniFile);	// Byte input stream.
		QString aLine, aName, aPrefix, aValue, aSep;	// Line,name,prefix,value,sep of a name-value pair
		QRegExp aPair("(\\w*:)?(\\w+)\\s*(\\+?=)(.*)$");
		
		// Parse. Extract the name-value pairs if not a comment line.
		while (!aInput.atEnd())
		{	// Comments. Skip blank lines and comments.
			aValue = aInput.readLine(1024);
			if (AUtil::isComment(aValue))
				continue;
			aLine = aValue.simplified();
			if (aPair.indexIn(aLine) >= 0)
			{	// Prefix. Extract the prefix if any
				aPrefix = aPair.cap(1).toLower();
				if (!aPrefix.isEmpty())
				{
#ifdef _WIN
					aValue = "win:";
#endif
#ifdef _LINUX
					aValue = "linux:";
#endif
					if (aPrefix != aValue)
						continue;
				}
				// DoGlobals. Include globals if iDoGlobals and vice versa
				aName = aPair.cap(2).toLower();
				if (aName.startsWith("gbl") != iDoGlobals)
					continue;

				// Missing. Note parameters that are in config file but not in iorPairs
				if (!iorPairs.contains(aName))
				{	if (!aOk) orInFile += ',';
					orInFile += aName;
					aOk = false;
				}
				else // Value. Extract value from line.
				{	aSep = aPair.cap(3);
					aValue = aPair.cap(4).simplified();
					
					// Expand. Expand environment vars and $param$ found in the value.
					if (iExpand && aValue.indexOf('$') >= 0)
					{	aFileList.clear();
						AUtil::expandList(aValue, iorPairs, irParams, aFileList, ';');
		
						// Use the first element in the list even if it is bogus (?name?)
						if (!aFileList.isEmpty())
							aValue = aFileList.first();
					}
					// Set. Add this name-value pair to result.
					if (aSep == "+=" )
						iorPairs[aName] +=  ',' + aValue;
					else
						iorPairs[aName] = aValue;
				}
			}
			else	// Format. Format is not [prefix:]name=value
			{	if (!aOk) orInFile += ',';
				orInFile += aLine;
				aOk = false;
			}
		}
		aIniFile.close();
	}
	return aOk;
}

/*!
\brief setFile - Set a new file path + file names

\param irFileName - A list of files, possibly containing environment variables
\return void
 */
void ANamedValues::setFile(QString& irFileName)
{
	// Set the parameter file name (may include environment variables)
	cFileName = irFileName;
}

