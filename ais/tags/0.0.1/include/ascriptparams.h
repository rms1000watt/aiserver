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

#ifndef ASCRIPTPARAMS_H
#define ASCRIPTPARAMS_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/ascriptparams.h
														Script Parameters

Scriptparams reads through a string containing the source of a script file and extracts each
parameter found. The parameters are stored in the Params property of the class.
Parameters occupy a single line and have the form:
;#name=value

CHANGE HISTORY
Version	Date		Who		Change
1.0057	 3/18/2005	tlw		Update documentation.
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include	<QtCore/QMap>
#include	<QtCore/QString>
#include	<QtCore/QStringList>
#include	"autilities.h"

//	------------------------------------------------------ METHODS ------------------------------------------------------------
class AScriptParams
{
public:
	static void parse(const QString& irScriptSource, AStringMap& iorPairMap, bool iDoGlobals)
	{
		// Parse each line of script looking for parameters denoted by ;#name=value
		QStringList aPair, aScript = irScriptSource.split("\n", QString::KeepEmptyParts);
		QStringList::Iterator apIt;
		QString aLine, aName, aValue;
		long aCount;
		for (apIt = aScript.begin(); apIt != aScript.end(); ++apIt)
		{	aLine = *apIt;
			if (aLine.startsWith(";#") && aLine.length() > 2)
			{	aLine = aLine.mid(2).simplified().toLower();
				aPair = aLine.split("=", QString::KeepEmptyParts);
				if ((aCount = aPair.count()) == 2)
				{	aName = aPair[0], aValue = aPair[1];
					if (aName.startsWith("gbl") == iDoGlobals)
					{	if (aValue.indexOf('$') >= 0)
						{	aPair.clear();
							AUtil::expandList(aValue, iorPairMap, gAis.mGblParams, aPair, ',');
							if (!aPair.isEmpty())
								aValue = aPair.first();
						}
						iorPairMap[aName] = aValue;
					}
				}
			}
		}
	};
};

#endif	// ASCRIPTPARAMS_H
