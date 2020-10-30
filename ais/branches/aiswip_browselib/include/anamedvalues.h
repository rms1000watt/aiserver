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

#ifndef ANAMEDVALUES_H
#define ANAMEDVALUES_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/anamedvalues.h
												Named Values Class Specification 		

CHANGE HISTORY
Version	Date		Who		Change
1.0100	6/15/2006	tlw		Add prefixes.
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include	<QtCore/qstring.h>
#include	"aglobals.h"		// AStringMap

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ANamedValues provides persistent storage for a set of name-value pairs.

\par Ini-file Specifecation
Name-value pairs are saved in a text file in the format:
\verbatim
[prefix:]name=value
\endverbatim
\par Configuration File Entries:
-# An entry may begin with an optional prefix of the form Win: or Linux: to specify
the platform.  The entry is only used if the server is running on the specified platform.
-# Compose/edit the configuration file using a text editor,e.g. vi.
-# One name-value pair per line.
-# Spaces that precede or succeed the equal sign are ignored.
-# Numeric values are saved as text.
-# Blank lines are ignored.
-# Comment lines starting with an octothorpe (#) are ignored.
-# The value of environment values of the form $(EnvVar) in a value are substituted for the
environment value. If the EnvVar is a list, the first value in the list is used. 

\par Environment.
Typically the environment is under construction when this constructor and readPairs
are called.  Probably no logger is available and the display options may be limited.
So, no exceptions are thrown and no display is assumed.  Be sure to check the return
code form readPairs and tailor the response to the application capabilities.
 */
class ANamedValues
{
public:
	ANamedValues(const QString& irFileName);
	bool	readPairs(bool iExpand, bool iDoGlobals, AStringMap& iorPairs, AStringMap& irParams, QString& orInFile);
	void	setFile(QString& irFileName);
private:
	QString	cFileName;
};
#endif // ANAMEDVALUES_H
