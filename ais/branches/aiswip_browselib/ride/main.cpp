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
aisdev/ride/main.cpp
														REMOTE IDE
The Remote Integrated Developement Environment (RIDE) implements the GUI but omits the server portion of AIS.  It may be
connected locally or remotely (via a TCP socket connection) to a server.  

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Quit if initializeParameters fails to find install path.
1.0063	 3/19/2005	tlw		Add ride project to aisdev. Add dummy AAppSvr::submit function.
												---------------------------------


	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QApplication>
#include <QtGui/QFileDialog>
#include <QtGui/QMessageBox>

#include "aglobals.h"			// AGlobals
#include "amainwindow.h"		// AMainWindow
#include "anamedvalues.h"		// ANamedValues
#include "autilities.h"			// AUtil

//	------------------------------------------------------ GLOBALS ------------------------------------------------------------
AGlobals gAis;				// Define ONE global instance of AGlobals per application.
extern int qInitResources_aforms();

//	----------------------------------------------- FUNCTION DECLARATIONS -----------------------------------------------------
static bool initializeParameters(const QString& irStartupCmd, QString& orMsgs);
static void setGblParamDefaults(AStringMap& orGblParams);

//	------------------------------------------------ FUNCTION DEFINITIONS  ----------------------------------------------------
int main(int argc, char **argv)
{
	// Parameters. Set client-side parameters.
	QApplication aApp(argc,argv);
    QString aStartupCmd = aApp.applicationFilePath();
	QString aStartupMsgs("Starting up Remote IDE.\n");
	qInitResources_aforms();
	if (!initializeParameters(aStartupCmd, aStartupMsgs))
		return -1;

	// MainWindow. Launch the IDE
	AMainWindow aMainWindow("Remote IDE ");
	QString aDefContextName("_Default"); 
	aMainWindow.startup(aDefContextName, aStartupMsgs);
	aMainWindow.show();

	// Start. Wait for events from the IDE or AIS.
	aApp.connect(&aApp, SIGNAL(lastWindowClosed()), &aApp, SLOT(quit()));
	return aApp.exec();
}

/*!
 * \brief Initializes ride client-side parameters.
 */
bool initializeParameters(const QString& irStartupCmd, QString& orMsgs)
{
    // rideinstall.ini
    // Possible locations:
    // 1. Install/Binaries Path
    // 2. /etc/ais/rideinstall.ini

    // ride.ini
    // Possible locations:
    // 1. Install/Binaries Path
    // 2. $AISDIR
    // 3. /home/user/ais/ride.ini
    Q_UNUSED(orMsgs);

    QString aExePath;
    QString aAisPath;
	QFileInfo aFileInfo;
    QString aIniFile;
    QString aMsg;
    char* apAisDir = NULL;
	bool aExpand = true;			// Expand variables found in values and file spec
	bool aDoGlobals = true;			// If true, include global definitions whose name starts with gbl.
	AStringMap aParams;				// Empty parameter list.
	AStringMap& arGblParams = gAis.mGblParams;
    ANamedValues aNamedValues("");

    if (!irStartupCmd.isEmpty())
    {
        aFileInfo.setFile(irStartupCmd);
        aExePath = aFileInfo.absolutePath();
        AUtil::terminateStg(QDir::separator().toAscii(), aExePath);
	}

    if ((apAisDir = getenv(AGLBLS_AISDIR)) != NULL)
    {
        aAisPath = apAisDir;
        AUtil::terminateStg(QDir::separator().toAscii(), aAisPath);
	}

    // rideinstall.ini
    // 1st location
    aIniFile = aExePath + AGLBLS_RIDEINSTALLFILENAME;
    aFileInfo.setFile(aIniFile);

    // 2nd location
    if (!aFileInfo.isFile())
    {
        // try the default location
        aIniFile = QString(AGLBLS_SYSCONFDIR) + AGLBLS_RIDEINSTALLFILENAME;
        aFileInfo.setFile(aIniFile);
    }

    // load default parameters
    setGblParamDefaults(arGblParams);
    arGblParams["gblaisinstallpath"] = aExePath;

    if (aFileInfo.isFile())
    {
        //qDebug("%s found. Loading parameters.",qPrintable(aIniFile));
        // load configuration file
        aNamedValues.setFile(aIniFile);
        aNamedValues.readPairs(aExpand, aDoGlobals, arGblParams, aParams, aMsg);
    }
    else
    {
        //qDebug("%s not found. Skipping file.",qPrintable(aIniFile));
    }

    // ride.ini
    // 1st location
    aIniFile = aExePath + AGLBLS_RIDEINIFILENAME;
    aFileInfo.setFile(aIniFile);

    // 2nd location
    if (!aFileInfo.isFile())
    {
        if (!aAisPath.isEmpty())
            aIniFile = aAisPath + AGLBLS_RIDEINIFILENAME;
        else
            aIniFile = QDir::homePath() + QDir::separator() + AGLBLS_AISUSERDIR + AGLBLS_RIDEINIFILENAME;

        aFileInfo.setFile(aIniFile);
    }

    if (aFileInfo.isFile())
    {
        //qDebug("%s found. Loading parameters.",qPrintable(aIniFile));
        // load configuration file
        aNamedValues.setFile(aIniFile);
        aNamedValues.readPairs(aExpand, aDoGlobals, arGblParams, aParams, aMsg);
    }
    else
    {
        //qDebug("%s not found. Skipping file.",qPrintable(aIniFile));
    }

	return true;
}

void setGblParamDefaults(AStringMap& orGblParams)
{
	orGblParams["gblaismachinename"] = "localhost";			// May be used to define URL
	orGblParams["gblclientbrowser"] = APPCLIENT_BROWSER;	// Path to client browser, C:/PROGRA~1/INTERN~1/IEXPLORE.EXE
	orGblParams["gblclienthelpurl"] = APPCLIENT_HELPURL;	// http://$GblAisMachineName$/!Help/index.html
	orGblParams["gblclientserverformrules"] = "";			// context.protocol list of server-form subscritptions
}

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
// end
