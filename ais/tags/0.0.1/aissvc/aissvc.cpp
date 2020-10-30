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

// Imports
#include <QtCore/QCoreApplication>
#include <QtCore/QFileInfo>

#include "aglobals.h"			// AGlobals
#include "aismgr.h"				// AAisMgr
#include "aissvr.h"				// AAisSvr
#include "aissvc.h"				// AAisSvc
#include "asys.h"				// ASys

// We are granted special dispensation to access asessionmgr.h for the sole purpose of creating
// an instance of asessionmgr in run.  Otherwise, we just use the interface.
#include "../asessionmgr/asessionmgr.h"	// SessionMgr

AAisSvc::AAisSvc(const QString &irStartupCmd, const QString &irStartupScript):
	cpAisMgr(NULL), cpAisSvr(NULL), cpLogMgr(NULL), cpSessionMgr(NULL), cpUsrMgr(NULL),
	cStartupCmd(irStartupCmd), cStartupScript(irStartupScript), cpApp(NULL)
{

}

AAisSvc::~AAisSvc()
{
    delete cpApp;
}

void AAisSvc::start()
{
    QString aStartupMsgs;

    int aArgc = 1;
    QVector<char*> aArgv(aArgc);
    aArgv[0] = cStartupCmd.toAscii().data();

    // If QCoreApplication is created in main(), it gets deallocated and exec() cannot be called.
    // So, we will create our QCoreApplication instance here.
    cpApp = new QCoreApplication(aArgc, aArgv.data());
    cStartupCmd = cpApp->applicationFilePath();

    // **************** THE FOLLOWING STEPS ARE ORDER SENSITIVE! ****************
    // Global Settings. Search for the ais.ini initialization file. Fails if unable to locate the install path.
    // If aStartupScript is empty, it is set to GblAisDefaultStartupScript.
    if (!ASys::initializeParameters(cStartupCmd, cStartupScript, aStartupMsgs))
    {
        qDebug("Install path not found. %s", aStartupMsgs.toAscii().data());
        return;
    }

    // Log Manager
    AStringMap& arGblParams = gAis.mGblParams;
    cpLogMgr = new ALogMgr(arGblParams);
    gAis.init(cpLogMgr);

    // Session Manager
    cpSessionMgr = new ASessionManager;

    // Ais Manager
    cpAisMgr = new AAisMgr(cpSessionMgr);

    // User Manager
    cpUsrMgr = new AUsrMgr(arGblParams);

    // Ais Server
    cpAisSvr = new AAisSvr(cpAisMgr, cpSessionMgr, cpUsrMgr);
    cpSessionMgr->setAisMgr(cpAisMgr, cpAisSvr);
    cpLogMgr->setAisSvr(cpAisSvr);
    cpAisMgr->setAisSvr(cpAisSvr);
    gAis.mpAisSvr = cpAisSvr;

    // Startup
    ASys::startup(cpSessionMgr, aStartupMsgs, cStartupScript);

    // Open Context. Open the default context. Run startup script.
    QString aDefaultContextName(arGblParams["gblaisdefaultcontextname"]);
    long aSessionId;
    if (!aDefaultContextName.isEmpty())
    {
        QString aMsgs;
        if ((aSessionId = cpSessionMgr->openContext(QString::null, aDefaultContextName, aMsgs)) < 0)
        {	aMsgs = QString("openContext(DefaultContextName:%1), %2").arg(aDefaultContextName, gAis.mpErrMsgs[-aSessionId]);
                aStartupMsgs += aMsgs;
        }
        // Run. Run the startup script for the default context in Context Admin Session.
        else
        {
            long aRetValue;
            aMsgs = QString("Opened context %1\n%2Launching the context startup script.").arg(aDefaultContextName, aMsgs);
            aStartupMsgs += aMsgs;
            QString aAmpmsg("_ais\177runscriptfile\177file\177_startupscript"), aDisplay, aOut;
            cpAisMgr->submit(aSessionId, 0/*Xid*/, aAmpmsg, false/*IsAsync*/, &aRetValue, aOut, aDisplay, NULL/*Data*/);
        }
    }

    // Startup Msgs. Log the startup messages.
    LOGSYSMSG(geInfo, aStartupMsgs);

    // Start the Event Loop
    // This call will no return until cpApp->quit() is called.
    cpApp->exec();

    // deallocate heap objects
    delete cpAisSvr;
    delete cpUsrMgr;
    delete cpAisMgr;
    delete cpSessionMgr;
    delete cpLogMgr;
}

void AAisSvc::stop()
{
    // Stop the Event Loop
    if(cpApp)
        cpApp->quit();
}
