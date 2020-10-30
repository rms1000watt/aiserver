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

#ifndef AGLOBALDEFS_H
#define AGLOBALDEFS_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aglobaldefs.h
													Global Defined Constants

CHANGE HISTORY
Version	Date		Who		Change
3.0005	 8/12/2007	rca		Bugfix for Windows 64bit port.
3.0004	 8/06/2007	rca		Used %i64d for NUM formatting in Windows 64-bit build.
3.0003	 7/25/2007	tlw		Incorporate the 64-bit jit into the build.
3.0000	 7/8/2007	tlw		Add in changes made to smtbase by mfk
3.0000	 6/26/2007	mfk		Beta Release New Class Lisp language features for 32bit architectures
2.0005	 5/2/2007	pmk		Update AISVERSION
2.0004	 2/9/2007	tlw		Update AISVERSION
2.0003	 1/26/2007	tlw		Update AISVERSION
2.0002	 1/7/2007	tlw		Update AISVERSION
2.0001	12/28/2006	tlw		Update AISVERSION
1.0120	12/19/2006	tlw		Update AISVERSION
1.0119	12/17/2006	tlw		Update AISVERSION
1.0118	12/11/2006	tlw		Update AISVERSION
1.0117	12/6/2006	tlw		Update AISVERSION
1.0116	11/26/2006	tlw		Update AISVERSION
1.0115	11/16/2006	tlw		Update AISVERSION
1.0114	11/13/2006	tlw		Update AISVERSION
1.0113	11/6/2006	tlw		Update AISVERSION
1.0112	10/27/2006	tlw		Update AISVERSION
1.0111	10/23/2006	tlw		Update AISVERSION
1.0110	10/16/2006	tlw		Update AISVERSION
1.0109	10/5/2006	tlw		Update AISVERSION
1.0108	 9/28/2006	tlw		Update AISVERSION
1.0107	 9/18/2006	tlw		Update AISVERSION
1.0106	 9/14/2006	tlw		Update AISVERSION
1.0105	 9/11/2006	tlw		Update AISVERSION
1.0104	 9/8/2006	tlw		Udpate AISVERSION
1.0070	11/3/2005	tlw		Add global definitions
												---------------------------------
NOTES
Aglobaldefs contains the globally defined constants that span all applications.  This may be the only global included in some
applications.  Other globals are not always needed.  For ease in management and troubleshooting, globally defined constants are
all encapsulated in this file.

Since this file is always included in aglobals.h, if a globals.h is included, aglobaldefs.h does not need to be included as
well.

The initial cut of this file defines version numbers that are used in nearly every application.  We may want to add other
definitions here as well.

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// ************************************************* GLOBAL DEFINED CONSTANTS *************************************************
extern "C" { // includes for modules written in C
	#include "fsmtbase.h" // SmartBase engine declarations
	}
#define AGBL_AISVERSION			CURRENT_VERSION	// AIS version corresponds to the current Smartbase Engine (should match the StarTeam Version Label)
#endif // AGLOBALDEFS_H
// end

