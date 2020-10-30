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

#ifndef ATIMESTAMP_H
#define ATIMESTAMP_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/atimestamp.h
													Setting Time Stamps

CHANGE HISTORY
Version	Date		Who		Change
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------
#define SECSTO2000  946684800	// (30*365+7)*24*3600  Seconds from 1/1/1970 to 1/1/2000 (includes 7 leap-years)
#define SECSPERDAY  86400;		// Secs in one day 3600 * 24

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ATimeStamp - Supplies time zone, Greenwich Mean Time, and local date-time

The implementation of this class is platform independent.  Qt does not support time zones, but we managed to extract it from the
supplied methods.  For the purposes of this discussion UTC is the same as Greenwich Mean Time (GMT).  Since these methods are all
static, no instance of ATimeStamp is required.

\par Notes:
-# localOffset fetches the offset in seconds from Coordinated Universal Time (UTC) to local time.  Thus, LocalTime = UTC + offset.
For example, Pacific Standard Time (PST) is 8 hours behind UTC and Pacific Daylight Time (PDT) is 7 hours behind UTC.

\par Usage:
-# You do NOT need to create an instance of the ATimeStamp.
-# To invoke a method, use the scope resolution operator ::. For example ATimeStamp::localOffset().

\par Timestamp Formats
Sun, 03 Mar 2002 12:50:40 GMT	; GMT as per RFC 822, updated by RFC 1123
03/Mar/2002:04:50:40 -0800		; Local time in NCSA Common Log Format
 */
class ATimeStamp
{
public:
	// GMT offset in seconds from local time
	static long		localTime();				// Current time in secs since 1/1/2000
	static QString	timeStamp(bool iGmt=false);	// Local/GMT datetime in NCSA CLF/HTTP format

private:
	static long		initTimeZone();				// Initialize scTimeZone
	static long		localOffset();				// Offset of local time from GMT [secs]
	static bool		scInitStaticDefs;			// >0 iff static defs have been initialized
	static long		scTimeZone;					// Difference [secs] between local time and GMT
};

#endif // ATIMESTAMP_H
