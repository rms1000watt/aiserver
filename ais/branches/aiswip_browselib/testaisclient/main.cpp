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
testaisclient/main.cpp
															AIS Test
AisTest implements an HTTP client for testing AIS (Analytic Information Server)

CHANGE HISTORY
Version	Date		Who		Change
1.0104	 9/8/2006	tlw		Add gAis to support error definitions.
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- ---------------
DOCUMENTATION
1.	See aistestnotes.txt for project setup information.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QObject>
#include <QtGui/QApplication>

#include "aistest.h"
#include "aglobals.h"
#include "appsvr.h"

#ifdef _MSVC
#pragma warning(push)
#pragma warning(disable : 4100)  // Disable "unreferenced formal parameter" warning
#endif
// submit - Just a dummy routine added in to satisfy the linker.  This routine is never actually called.
void AAppSvr::submit(long iXid, const QString& irAmpmsg, AReturnRcvr* ipInProcClient, QTcpSocket* ipSocket, char* ipData)
{
    Q_UNUSED(iXid);
    Q_UNUSED(irAmpmsg);
    Q_UNUSED(ipInProcClient);
    Q_UNUSED(ipSocket);
    Q_UNUSED(ipData);
}
#ifdef _MSVC
#pragma warning(pop)			// Restore standard settings
#endif

extern int qInitResources_aistest();

//	------------------------------------------------- DEFINED CONSTANTS -------------------------------------------------------
//	We look for the config file in the following places (in left-right order):
// ;$(AIS)/aistest.ini;$(AIS)/abasedev/aistest.ini
AGlobals gAis;				// Define ONE global instance of AGlobals per application.

//	------------------------------------------------ FUNCTION DEFINITIONS -----------------------------------------------------
int main(int argc, char* argv[])
{
	QApplication aApp(argc, argv);
	qInitResources_aistest();
	const char* apTestFile = (argc > 1) ? argv[1] : AISTEST_FILESPEC;
	AAisTest aMainWindow(NULL, "AISTestSuite", apTestFile);
	QObject::connect(&aApp, SIGNAL(lastWindowClosed()), &aApp, SLOT(quit()));
	aMainWindow.show();
	return aApp.exec();
}

// end
