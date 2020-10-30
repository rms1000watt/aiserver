/**********************************************************************************
    Copyright (C) 2013 AIS Foundation.

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
aisdev/radide/radide.cpp

														radide MAIN

CHANGE HISTORY
Version	Date		Who		Change
1.0000	 1/26/2013	mfk		Started this Widows Lisp IDE AIS interface (based on Qt so it might work on non Windows platforms also?).
												--------------- ---------------
NOTE
See asys.h for details on the system startup.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#include <QtGui/QApplication>
#include <QtGui/QMenuBar>
#include <QtGui/QMenu>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QMessageBox>
#include <QtGui/QTextEdit>
#include <QtGui/QLineEdit>
#include <QtGui/QHBoxLayout>
#include <QtGui/QvBoxLayout>
#include <QtGui/QScrollArea>

#include "aglobals.h"			// AGlobals
#include "alogmgr.h"			// ALogMgr
#include "aissvr.h"				// AAisSvr
#include "aismgr.h"				// AAisMgr
#include "appsvr.h"				// AAppSvr
#include "axmlsvr.h"			// AXmlSvr
#include "asys.h"				// ASys
#include "aradwindow.h"		// ALispWindow

// We are granted special dispensation to access asessionmgr.h for the sole purpose of creating an instance of asessionmgr. 
#include "../asessionmgr/asessionmgr.h"	// SessionMgr


//	------------------------------------------------------ GLOBALS ------------------------------------------------------------
AGlobals gAis;				// Define ONE global instance of AGlobals per application.
int main(int argc, char *argv[])
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
#if 1
	ARadWindow mainWindow("AIS - Analytic Information Server");
	mainWindow.show();
#else
	QWidget *window = new QWidget;
	window->setWindowTitle("AIS - Analytic Information Server");
	QLineEdit *command = new QLineEdit;
	QPushButton *runButton = new QPushButton("Run");
	QHBoxLayout *commandLayout = new QHBoxLayout;
	commandLayout->addWidget(runButton);
	commandLayout->addWidget(command);

	QScrollArea *scrollArea = new QScrollArea;
	QTextEdit *console = new QTextEdit;
	scrollArea->setWidget(console);
	scrollArea->setWidgetResizable(true);
	QVBoxLayout *mainLayout = new QVBoxLayout;
	mainLayout->addLayout(commandLayout);
	mainLayout->addWidget(scrollArea);

	window->setLayout(mainLayout);
	window->show();
#endif
	// ***************************************************** GUI **************************************************************

	// Connect Application signals to slots so that a graceful exit is possible.
	aApp.connect(&aApp, SIGNAL(lastWindowClosed()), &aApp, SLOT(quit()), Qt::DirectConnection);
	aApp.connect(&aApp, SIGNAL(aboutToQuit()), &aAisSvr, SLOT(onQuit()), Qt::DirectConnection);
	return aApp.exec();
}
