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
aisdev/autilities/atimestamp.cpp

								 Time Zone Offset

ATimeStamp is a platform-independent class that provides the local time, Greenwich Mean
Time (GMT or UTC), and the offset for the local time in seconds from the Coordinated
Universal Time (UTC) which is the same (for our purposes) as GMT.

CHANGE HISTORY
Version	Date		Who		Change
2.0001	1/6/2007	tlw		Update documentation.
1.0057	3/18/2005	tlw		Update documentation
												---------------------------------
DOCUMENTATION
 1.	See Include/timezone.h for class specification.

NOTES
 1.	Tested on Microsoft Windows 2000.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS -----------------------------------------------------------
#include <QtCore/QDateTime>
#include "atimestamp.h"

long ATimeStamp::scTimeZone = 0;
bool ATimeStamp::scInitStaticDefs = false;

//	-------------------------------------------------- MEMBER FUNCTIONS -------------------------------------------------------
/*!
\brief initTimeZone - Initialize local time offset from GMT in seconds

\par Notes:
-# Used to initialize scTimeZone
-# All other methods are required to call this method if scInitStaticDefs is false.
-# It is a bit ugly, but the time zone is not directly available from Qt.
 */
long ATimeStamp::initTimeZone()
{
	QDateTime aLocalTime(QDateTime::currentDateTime());
	QDateTime aGmtTime(aLocalTime.toUTC());
	QString aGmt(aGmtTime.toString(Qt::TextDate));
	QDateTime aGmtTime2(QDateTime::fromString(aGmt, Qt::TextDate));
	aGmtTime = QDateTime::fromString(aGmt, Qt::TextDate);
	return aGmtTime.secsTo(aLocalTime) - 1;
}

/*!
\brief localOffset - Get the local time offset from GMT in seconds
\return cTimezone - Number of seconds to add to GMT to get the local time.

\par Notes:
-# The sun rises later in the USA than in Greenwich, England, so cTimezone is negative by several hours in the USA.
That is, local time in the USA is behind GMT.
/verbatim
PDT = GMT - 7hours (25200)
PST = GMT - 8hours (28800)
\endverbatim
 */
long ATimeStamp::localOffset()
{
	if (!scInitStaticDefs)
	{	scTimeZone = initTimeZone();
		scInitStaticDefs = true;
	}
	return scTimeZone;
}

/*!
\brief localTime - Local time in seconds since 1/1/2000
\return Time in seconds since 1/1/2000
\par Notes:
-# It is much easier to make time calculations using seconds rather than hours,minutes,seconds
-# The local time is offset from GMT.  In the USA, local time is several hours behind GMT.
 */
long ATimeStamp::localTime()
{
	// Current time in secs since 1/1/2000
	return QDateTime::currentDateTime().toTime_t() - SECSTO2000;
}

/*!
\brief timeStamp - Returns local time in either GMT or Local format
\param iGmt - If true, show time in GMT format; else, show time in NCSA format
\return
	GMT:  Sun, 06 Nov 1994 08:49:37 GMT 
	NCSA: 03/Mar/2002:08:50:40 -0800  
\par Notes:
-# GMT format as per RFC 1123 of HTTP specification
-# NCSA format as per National Center for Supercomputing Applications HTTPd standard
-# Pacific Standard Time = GMT - 8:00:00 
 */
QString ATimeStamp::timeStamp(bool iGmt)
{
	// Get the local date-time
	QString aTime, aTmp;
	QDateTime aDt = QDateTime::currentDateTime();
	long aTimeZone = localOffset();
	if (iGmt)				// Sun, 06 Nov 1994 08:49:37 GMT - GMT format as per RFC 1123
	{	aDt = aDt.toUTC();	// GMT = LocalTime + _timezone
		aTime = aDt.toString("ddd, dd MMM yyyy hh:mm:ss") + " GMT";
	}
	else		// 03/Mar/2002:08:50:40 -0800 -  Local time format 
	{	long aZoneHrs = aTimeZone / 3600;	// LocalTime = GMT + zoneHrs
		aTmp.sprintf("  %02ld%s", aZoneHrs, "00");		// PST = GMT - 08:00
		aTime = aDt.toString("dd/MMM/yyyy:hh:mm:ss") + aTmp;
	}
	return aTime;
}
// end
