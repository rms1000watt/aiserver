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
aisdev/webide/main.cpp

														WEBIDE MAIN

CHANGE HISTORY
Version	Date		Who		Change
3.1004	10/31/2007	fchua	Removed call to system log since it was already fixed in the AServerForm.
1.0116	12/4/2006	tlw		Connect aboutToQuit signal to AisSvr::onQuit slot.
1.0113	11/6/2006	tlw		Quit if initializeParameters fails to find the install path.
1.0063	 5/26/2005	tlw		Move all the GUI elements out of webide into aforms.
1.0057	 3/21/2005	tlw		Move common code for main in webide, aissvc, ride to ASys
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- ---------------
NOTE
See asys.h for details on the system startup.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QApplication>
#include <QtGui/QMessageBox>

#include "aglobals.h"			// AGlobals
#include "alogmgr.h"			// ALogMgr
#include "aissvr.h"				// AAisSvr
#include "aismgr.h"				// AAisMgr
#include "appsvr.h"				// AAppSvr
#include "axmlsvr.h"			// AXmlSvr
#include "asys.h"				// ASys
#include "amainwindow.h"		// AMainWindow

// We are granted special dispensation to access asessionmgr.h for the sole purpose of creating an instance of asessionmgr. 
#include "../asessionmgr/asessionmgr.h"	// SessionMgr

//	------------------------------------------------------ GLOBALS ------------------------------------------------------------
AGlobals gAis;				// Define ONE global instance of AGlobals per application.
extern int qInitResources_aforms();

//	------------------------------------------------ FUNCTION DEFINITIONS  ----------------------------------------------------
int main(int argc, char ** argv)
{
	QString aStartupCmd;
	QString aStartupScript((argc > 1) ? argv[1] : "");
	QString aStartupMsgs("Starting up AIS.\n");
	QApplication aApp(argc, argv);

	aStartupCmd = aApp.applicationFilePath();

	// ************************************ THE FOLLOWING STEPS ARE ORDER SENSITIVE! ******************************************
	// Global Settings. Search for the ais.ini initialization file.
	if (!ASys::initializeParameters(aStartupCmd, aStartupScript, aStartupMsgs))
	{	if (QMessageBox::critical(NULL/*Parent*/, "Installation path not found", aStartupMsgs) != QMessageBox::Cancel)
			return -1;
	}
	// Log Manager
	AStringMap& arGblParams = gAis.mGblParams;
	ALogMgr aLogMgr(arGblParams);
	gAis.init(&aLogMgr);

	// Session Manager
	ASessionManager aSessionMgr;

	// Ais Manager
	AAisMgr aAisMgr(&aSessionMgr);

	// User Manager
	AUsrMgr aUsrMgr(arGblParams);

	// Ais Server
	AAisSvr aAisSvr(&aAisMgr, &aSessionMgr, &aUsrMgr);
	aSessionMgr.setAisMgr(&aAisMgr, &aAisSvr);
	aLogMgr.setAisSvr(&aAisSvr);
	aAisMgr.setAisSvr(&aAisSvr);
	gAis.mpAisSvr = &aAisSvr;

	// Startup
	ASys::startup(&aSessionMgr, aStartupMsgs, aStartupScript);

	// In-process App Server. Open an in-process protocol server if one not already open
	ushort aPort = arGblParams.contains("gblappport") ? arGblParams["gblappport"].toUShort() : 0;
	if (aPort == 0)
		new AAppSvr(aPort, gAis.mGblParams["gblaisdefaultcontextname"]);

	// Debug. Uncomment in case AIS does not start, also, check system message log
	// QMessageBox::information(0,"AIS main.cpp", aStartupMsgs);

	// ***************************************************** GUI **************************************************************
	// Start application
	qInitResources_aforms();

	// AMainWindow. Start MDI containing ConnectMgr Dialog, Session Form, Server Form.
	AMainWindow aMainWindow("AIS - Analytic Information Server");
	QString aDefContextName(gAis.mGblParams["gblaisdefaultcontextname"]);
	aMainWindow.startup(aDefContextName, aStartupMsgs);
	aMainWindow.show();
	aApp.connect(&aApp, SIGNAL(lastWindowClosed()), &aApp, SLOT(quit()), Qt::DirectConnection);
	aApp.connect(&aApp, SIGNAL(aboutToQuit()), &aAisSvr, SLOT(onQuit()), Qt::DirectConnection);
	return aApp.exec();
}
// end
