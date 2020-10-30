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
aisdev/asysmgr/asys.cpp
													AIS System Startup

CHANGE HISTORY
Version	Date		Who		Change
2.0001	1/6/2007	tlw		gbllogncsaminlevel. Remove unused parameters.
1.0113	11/6/2006	tlw		setCtxParamDefaults. Include the CloseWait parameter. initializeParameters. Return false on error.
1.0061	5/5/2005	tlw		initializeParameters. Read globals and server-wide parameters separately.
1.0058	3/22/2005	tlw		If command-line arg specifies a startup file, set overrides from context.ini and startup script.
							Set default context name to context	specified by the startup script.
1.0057	3/20/2005	tlw		Split out common routines for webide, ride, aissvc
												---------------------------------
DOCUMENTATION
See aisdev/include/asys.h
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <stdlib.h>			// getenv
#include <QtCore/QFileInfo>
#include <QtGui/QFileDialog>
#include <QtGui/QMessageBox>
#include "aglobals.h"		// ACHECK,AIS_,APPCLIENT_,APPSVR_,AHTTPSVR_,LOGMGR_,AUSRMGR_,AXMLSVR_
#include "aissvr.h"			// AAisSvr
#include "appsvr.h"			// AAppSvr
#include "anamedvalues.h"	// ANamedValues
#include "ascriptparams.h"	// AScriptParams
#include "asys.h"			// ASys
#include "atextedit.h"		// ATED_...
#include "autilities.h"		// AUtil
#include "../asessionmgr/asessionmgr.h"	// ASessionManager
extern "C" { // includes for modules written in C
	#include "fsmtbase.h" // SmartBase engine declarations
	}

//	--------------------------------------------------- CLASS METHODS  --------------------------------------------------------
/*!
\brief initializeParameters - Initialize global & server-wide context-specific parameters on startup

\param irStartupCmd - Command that started this executable. Holds installPath/aissvc.exe
\param iorStartupScript - Optional argument passed to aissvc.exe on the command line of the form workDir/astartup.sl where
workdir is the path to the startup script.
\param orMsgs - Holds startup messages generated in this routine and elsewhere
\return aCode. True iff initialization at least found the installation directory.
\par Notes:
-# ais.exe may also be webide.exe or ride.exe.
-# See parameters.txt for more details on the order of overrrides.
 */
bool ASys::initializeParameters(const QString& irStartupCmd, QString& iorStartupScript, QString& orMsgs)
{
    // Startup message
    orMsgs += "AIS started at " + ATimeStamp::timeStamp() + "\n";

    // The $AISDIR environment variable overrides the location of the configuration files:
    // 1. redirect.ini
    // 2. ais.ini (if #1 does not exist)

    // The file aisinstall.ini will be loaded from one of the following locations:
    // 1. install/binaries path
    // 2. /etc/ais/aisinstall.ini (if #1 does not exist)

    // The file ais.ini will be loaded from one of the following locations:
    // 1. install/binaries path
    // 2. ais instance path (if #1 does not exist)

    // The install path points the location of the executables. For binary distributions,
    // it won't contain any of the application files or directories.
    // aExePath - Path to the executables
    // aAppPath - Path to the application files and directories (also known as ais instance path)
    //
    // The aAppPath mainly depends on one of the following:
    // 1. start-up file
    // 2. $AISDIR, if start-up file was not specified
    // 3. install/binaries path
    //
    // aAppPath is where the following directories will be created or loaded:
    // 1. usr/
    // 2. logs/
    // 3. mysqldata/
    //
    // If neither the start-up file nor $AISDIR were specified, aAppPath will be the same as aExePath.
    // ais.ini will be loaded from the install/binaries path,
    // and the default start-up file will also be loaded relative to that directory.
    // aAppPath should be updated accordingly if the default start-up file was loaded.

    QString aExePath;
    QString aAppPath;
    QString aAisDir;
    QString aMsg;
    QFileInfo aFileInfo;
    QFile aRedirectFile;
    char *apAisDir = NULL;
    char aDirSeparator = QDir::separator().toAscii();

    // We will determine aExePath using irStartupCmd
    if (!irStartupCmd.isEmpty())
    {
        aFileInfo.setFile(irStartupCmd);
        if (aFileInfo.exists())
        {
            aExePath = aFileInfo.absolutePath();
            AUtil::terminateStg(aDirSeparator, aExePath);
            aMsg = QString("BinPath set to %1.\n").arg(aExePath);
        }
        else
            aMsg = QString("Command not found: %1.\n").arg(irStartupCmd);

            orMsgs += aMsg;
    }

    // Check if start-up file is defined
    if (!iorStartupScript.isEmpty())
    {
        aFileInfo.setFile(iorStartupScript);

        // If directory was passed, this will become our application path
        // If file was passed, the location of the file would become our application path
        if (aFileInfo.isDir())
        {
            aAppPath = aFileInfo.absoluteFilePath();
            AUtil::terminateStg(aDirSeparator, aAppPath);
            aMsg = QString("AppPath set to %1.\n").arg(aAppPath);
        }
        else if (aFileInfo.isFile())
        {
            aAppPath = aFileInfo.absolutePath();
            AUtil::terminateStg(aDirSeparator, aAppPath);
            aMsg = QString("AppPath set to %1.\n").arg(aAppPath);
        }
        else
            aMsg = QString("Start-up file not found: %1.\n").arg(iorStartupScript);
    }
    else if ((apAisDir = getenv(AGLBLS_AISDIR)) != NULL)
    {
        // if start-up file is not defined, consider checking $AISDIR

        // Conditions:
        // 1. No start-up file
        // 2. $AISDIR is defined in environment variables

        aAisDir = apAisDir;
        aFileInfo.setFile(aAisDir);
        // Check if $AISDIR points to a directory
        if (aFileInfo.isDir())
        {
            // $AISDIR will assume our aAppPath for now
            aAppPath = aFileInfo.absolutePath();
            AUtil::terminateStg(aDirSeparator, aAppPath);
            aMsg = QString("AppPath set to %1.\n").arg(aAppPath);
        }
        else
            aMsg = QString("$AISDIR is invalid: %1.\n").arg(aAisDir);
    }
    else
    {
        // Conditions:
        // 1. No start-up file
        // 2. No $AISDIR in environment variables

        // aExePath will assume our aAppPath
        aAppPath = aExePath;
        aMsg = QString("AppPath set to %1.\n").arg(aAppPath);
    }

    orMsgs += aMsg;

    // Load redirect.ini from aExePath, if it exists
    aRedirectFile.setFileName(aExePath + AGLBLS_REDIRECTFILENAME);
    if (aRedirectFile.exists())
    {
        aMsg = QString ("redirect.ini found in: %1.\n").arg(aAppPath);
        orMsgs += aMsg;

        aRedirectFile.open(QIODevice::ReadOnly);
        QString aRedirPath = aRedirectFile.readLine().simplified();

        // Check if redirect path is valid
        aFileInfo.setFile(aRedirPath);
        if (aFileInfo.isDir())
        {
            aExePath = aFileInfo.absoluteFilePath();
            AUtil::terminateStg(aDirSeparator, aExePath);
            aAppPath = aExePath;
            aMsg = QString("BinPath/AppPath reset to %1.\n").arg(aExePath);
        }
        else
            aMsg = QString("Redirect Path is invalid: %1.\n").arg(aRedirPath);

        orMsgs += aMsg;
    }

    // this part may not be necessary any more
    if (aAppPath.isEmpty())
    {
        qDebug("ASys::initializeParameters(), Unable to locate application directory");
        orMsgs += "Unable to locate application directory";
        return false;
    }

    bool aExpand = true;			// Expand variables found in values and file spec
    bool aDoGlobals = true;			// If true, include global definitions whose name starts with gbl.
    AStringMap aParams;				// Empty parameter list.
    AStringMap& arGblParams = gAis.mGblParams;
    AStringMap& arCtxParams = gAis.mCtxParams;
    QString aIniFile;
    ANamedValues aNamedValues(aIniFile);

    // Set Defaults
    setGblParamDefaults(arGblParams);
    setCtxParamDefaults(arCtxParams);
    arGblParams["gblaisinstallpath"] = aExePath;     // Set the global install directory
    arGblParams["gblaisapplicationpath"] = aAppPath; // Set the global instance directory

    // aisinstall.ini. Set global and server-wide parameter values from aisinstall.ini.
    // Load locations, whichever is available first.
    // 1. aExePath
    // 2. /etc/ais/aisinstall.ini

    // 1st location
    aIniFile = aExePath + AGLBLS_INSTALLFILENAME;
    aFileInfo.setFile(aIniFile);

    // 2nd location
    if (!aFileInfo.isFile())
    {
        aIniFile = QString(AGLBLS_SYSCONFDIR) + AGLBLS_INSTALLFILENAME;
        aFileInfo.setFile(aIniFile);
    }

    if (!aFileInfo.isFile())
    {
        aMsg = QString("%1 not found. Setting parameters to default values.\n").arg(aIniFile);
        orMsgs += aMsg;
    }
    else
    {
        aMsg = QString("%1 found. Loading parameters.\n").arg(aIniFile);
        orMsgs += aMsg;

        aNamedValues.setFile(aIniFile);
        if (!aNamedValues.readPairs(aExpand, aDoGlobals, arGblParams, aParams, aMsg))
        {
            aMsg.prepend("Unexpected parameters:");
            orMsgs += aMsg + '\n';
        }
    }

    // ais.ini. Override globals and server-wide context parameters from aisinstall.ini.
    // Load locations, whichever is available first.
    // 1. aExePath
    // 2. aAppPath

    // 1st location
    aIniFile = aExePath + AGLBLS_INIFILENAME;
    aFileInfo.setFile(aIniFile);

    // 2nd location
    if (!aFileInfo.isFile())
    {
        aIniFile = aAppPath + AGLBLS_INIFILENAME;
        aFileInfo.setFile(aIniFile);
    }

    if (!aFileInfo.isFile())
    {
        aMsg = QString("%1 not found. Parameters were not updated.\n").arg(aIniFile);
        orMsgs += aMsg;
    }
    else
    {
        aMsg = QString("%1 found. Loading parameters.\n").arg(aIniFile);
        orMsgs += aMsg;

        aNamedValues.setFile(aIniFile);
        if (!aNamedValues.readPairs(aExpand, aDoGlobals, arGblParams, aParams, aMsg))
        {
            aMsg.prepend("Unexpected parameters:");
            orMsgs += aMsg + '\n';
        }
        if (!aNamedValues.readPairs(aExpand, !aDoGlobals, arCtxParams, arGblParams, aMsg))
        {
            aMsg.prepend("Unexpected parameters:");
            orMsgs += aMsg + '\n';
        }
    }

    // Startup Script. If startup script defined, set workDir and override selected globals.
    if (iorStartupScript.isEmpty())
    {
        // Startup script not specified, try global default value.
        iorStartupScript = arGblParams["gblaisdefaultstartupscript"];

        if (!iorStartupScript.isEmpty())
        {
            aFileInfo.setFile(iorStartupScript);
            // If relative path, load it from aAppPath.
            if (aFileInfo.isRelative())
			{
                iorStartupScript.prepend(aAppPath);
				aFileInfo.setFile(iorStartupScript);
			}

            aMsg = QString("Default startup script: %1.\n").arg(iorStartupScript);
            orMsgs += aMsg;

            // aAppPath should be updated accordingly
            if (aFileInfo.isDir())
                aAppPath = aFileInfo.absoluteFilePath();
            else if (aFileInfo.isFile())
                aAppPath = aFileInfo.absolutePath();
            else
                aAppPath = ""; // Default startup script is not a file nor directory

            if (!aAppPath.isEmpty())
            {
                AUtil::terminateStg(aDirSeparator, aAppPath);
                aMsg = QString("AppPath reset to %1.\n").arg(aAppPath);
                orMsgs += aMsg;
                arGblParams["gblaisapplicationpath"] = aAppPath;

                // reload ais.ini from the new aAppPath
                aIniFile = aAppPath + AGLBLS_INIFILENAME;
                aFileInfo.setFile(aIniFile);

                if (!aFileInfo.isFile())
                {
                    aMsg = QString("%1 not found. Parameters were not updated.\n").arg(aIniFile);
                    orMsgs += aMsg;
                }
                else
                {
                    aMsg = QString("%1 found. Loading parameters.\n").arg(aIniFile);
                    orMsgs += aMsg;

                    aNamedValues.setFile(aIniFile);
                    if (!aNamedValues.readPairs(aExpand, aDoGlobals, arGblParams, aParams, aMsg))
                    {
                        aMsg.prepend("Unexpected parameters:");
                        orMsgs += aMsg + '\n';
                    }
                    if (!aNamedValues.readPairs(aExpand, !aDoGlobals, arCtxParams, arGblParams, aMsg))
                    {
                        aMsg.prepend("Unexpected parameters:");
                        orMsgs += aMsg + '\n';
                    }
                }
            }
        }
    }

    if (iorStartupScript.isEmpty())
        orMsgs += "No startup script provided. Context initialization is being skipped.\n";
    else
    {
        QString aCtxIniFile, aWorkDir;
        bool aIsFile = false;	// true iff iorStartupScript is a valid file spec
        aFileInfo.setFile(iorStartupScript);

        // WorkDir. Extract the workDir from workDir/startup.
        if (aFileInfo.isFile())
        {
            aWorkDir = aFileInfo.absolutePath();
            // ASessionMgr::registerContext() expects the filename only.
            iorStartupScript = aFileInfo.fileName();
            aIsFile = true;
        }
        else if (aFileInfo.isDir())
        {
            aWorkDir = aFileInfo.absoluteFilePath();
            // This is necessary for ASessionMgr::registerContext().
            iorStartupScript = ".";
        }
        else	// Oops, command-line argument is not a file and not a dir.
        {
            aMsg = QString("Can't locate startup script: %1.\n").arg(iorStartupScript);
            orMsgs += aMsg;
        }

        // Current Working Directory (PWD). Set the current working directory and set startupfile relative to PWD.
        if (aWorkDir != QDir::currentPath())
            QDir::setCurrent(aWorkDir);

        orMsgs += QString("Current working directory: %1.\n").arg(aWorkDir);

        // context.ini. Override selected global values defined in context.ini
        AStringMap aScriptParams(arCtxParams);
        AUtil::terminateStg(aDirSeparator, aWorkDir);

        aFileInfo.setFile(aCtxIniFile = aWorkDir + AGLBLS_APPINIFILENAME);
        if (aFileInfo.isFile())
        {
            aMsg = aCtxIniFile + " found. Loading parameters.\n";
            orMsgs += aMsg;

            aNamedValues.setFile(aCtxIniFile);
            if (!aNamedValues.readPairs(aExpand, aDoGlobals, arGblParams, arCtxParams, aMsg))
                    orMsgs += "Unexpected parameters:" + aMsg;
            // Also, get context name for later use.
            aNamedValues.readPairs(aExpand, !aDoGlobals, aScriptParams, arGblParams, aMsg);
        }

        // Read Startup Script. Extract selected global overrides from script file
        if (aIsFile)
        {
            QString aScript;
            AUtil::readFile(iorStartupScript, aScript);
            if (!aScript.isEmpty())
            {
                AScriptParams::parse(aScript, arGblParams, aDoGlobals);
                // Also, get context name for later use
                AScriptParams::parse(aScript, aScriptParams, !aDoGlobals);
            }
        }

        // Set GblAisDefaultContextName.
        if (aScriptParams.contains("contextname"))
            arGblParams["gblaisdefaultcontextname"] = aScriptParams["contextname"];
    }

    return true;
}

/*	---------------------------------------------------------------------------------------------------------------------------
initializePorts - Initialize the global protocol servers if global port value is non-zero
Args:
	irContextName	Global default context name used to set default context name for each server.
	orMsgs			Holds startup messages generated in this routine and elsewhere
Returns:
	nothing
Notes:
 1.	A single AppSvr can listen for both in-process requests  and remote requests
 2. An in-process server is added if not running as a service an no AppSvr already opened
	------------------------------------------------------------------------------------------------------------------------ */
void ASys::initializePorts(const QString& irContextName, QString& orMsgs)
{
	long aProtocol;
	const char *apPortName;
	ushort aPort;
	long aStatus;
	QString aMsg, aPortName;
	AStringMap& arGblParams = gAis.mGblParams;
	for (aProtocol = 1; aProtocol <= AISSVR_XML; ++aProtocol)
	{	aPortName = "gbl";
		aPortName += (apPortName = gAis.mpPortNames[aProtocol]);
		aPort = arGblParams.contains(aPortName) ? arGblParams[aPortName].toUShort() : 0;
		if (aPort > 0)
		{	if ((aStatus = gAis.mpAisSvr->openPort(aProtocol, aPort, irContextName)) > 0)
			{	aMsg = QString("Open port %1 on %2 fails: %3\n").arg(aPort).arg(apPortName).arg(gAis.mpErrMsgs[aStatus]);
				aPort = 0;
			}
			else
				aMsg = QString("Global %1 listening on %2=%3 with default context: %4.\n").arg(gAis.mpSvrNames[aProtocol])
				.arg(apPortName).arg(aPort).arg(irContextName);
			orMsgs += aMsg;
		}
		// In-process App Server. Open an in-process protocol server if not running as a service.
		if (aProtocol == AISSVR_APP && !gAis.mIsService && aPort == 0)
			new AAppSvr(aPort, irContextName);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
setCtxParamDefaults - Set server-wide context params to hard-coded defaults
Args:
	orCtxParams		Map of server-wide context-specific name-value pairs
Returns:
	nothing
Notes:
 1.	If a startup script specified on command-line, context.ini and startup script may override.
 2.	registerContext initializes context params to this list and then overrides these settings from definitions in context.ini
	and in the startup script for the context.
	------------------------------------------------------------------------------------------------------------------------ */
void ASys::setCtxParamDefaults(AStringMap& orCtxParams)
{
	orCtxParams.clear();
	orCtxParams["appautologon"] = AAPPSVR_AUTOLOGON;
	orCtxParams["appdefaultpage"]= AHTTPSVR_DEFAULT;
	orCtxParams["appdir"] =  AAPPSVR_DIR;
	orCtxParams["appport"] = AAPPSVR_PORT;
	orCtxParams["appsecuredir"] = AAPPSVR_SECUREDIR;
	orCtxParams["clientsessionformrules"] = "";
	orCtxParams["clientviewurl"] = AIS_CLIENTVIEWURL;
	orCtxParams["closemode"] = AIS_CLOSEMODE;
	orCtxParams["closewait"] = "0";
	orCtxParams["contextname"] = "";
	orCtxParams["defaultjitmode"] = "on";
	orCtxParams["edfont"] = "Courier New";
	orCtxParams["edfontsize"] = QString::number(10);
	orCtxParams["httpautologon"]= AHTTPSVR_AUTOLOGON;
	orCtxParams["httpdefaultpage"]= AHTTPSVR_DEFAULT;
	orCtxParams["httpdir"] =  AHTTPSVR_DIR;
	orCtxParams["httpport"] = AHTTPSVR_PORT;
	orCtxParams["httpsecuredir"] =  AHTTPSVR_SECUREDIR;
	orCtxParams["httpuploaddir"] = AHTTPSVR_UPLOADDIR;
	orCtxParams["memory"] = AIS_MINCONTEXTPOOL;
	orCtxParams["memoryobjectheaders"] = "0";
	orCtxParams["minsecuritylevel"] = AIS_MINSECURITYLEVEL;
	orCtxParams["ondisconnect"] = QString::null;
	orCtxParams["startupprefix"] = QString::null;
	orCtxParams["usridlesecs"] = AUSRMGR_IDLESECS;
	orCtxParams["xmlautologon"] = AXMLSVR_AUTOLOGON;
	orCtxParams["xmldefaultpage"]= AHTTPSVR_DEFAULT;
	orCtxParams["xmldir"] = AXMLSVR_DIR;
	orCtxParams["xmlport"] = AXMLSVR_PORT;
	orCtxParams["xmlsecuredir"] = AXMLSVR_SECUREDIR;
}

/*	---------------------------------------------------------------------------------------------------------------------------
setGblParamDefaults - Set global params to hard-coded defaults
Args:
	orGblParams		Map of global name-value pairs
Returns:
	nothing
Notes:
 1.	The settings in aisinstall.ini and in ais.ini override these settings.
 2.	If a startup script is specified on command-line, context.ini and startup script override
	------------------------------------------------------------------------------------------------------------------------ */
void ASys::setGblParamDefaults(AStringMap& orGblParams)
{
	orGblParams["gblaiscontextlist"] = "";				// AIS may start with no contexts at all.
	orGblParams["gblaisdefaultcontextname"] = "";		// If not set in ais.ini, set by initial startup.sl
	orGblParams["gblaisdefaultstartupscript"] = "";		// Set by aisinstall.ini and used by aissvc.
	orGblParams["gblaishostaddress"] = AIS_HOSTADDRESS;
	orGblParams["gblaismachinename"] = AIS_MACHINENAME;
	orGblParams["gblaismaxbuffersize"] = AIS_MAXBUFFERSIZE;
	orGblParams["gblaismemorypool"] = AIS_MINCONTEXTPOOL;		// Minimum memory for use by AIS itself
	orGblParams["gblaismincontextpool"] = AIS_MINCONTEXTPOOL;	// Minimum memory for use by default contexts
	orGblParams["gblappport"] = AAPPSVR_PORT;
	orGblParams["gblclientbrowser"] = APPCLIENT_BROWSER;	// Location of browser on client machine
	orGblParams["gblclienthelpurl"] = APPCLIENT_HELPURL;	// URL location of help HTML files
	orGblParams["gblclientserverformrules"] = "";			// Comma-delimited list of contextName.protocols
	orGblParams["gblhttpport"] = AHTTPSVR_PORT;
	orGblParams["gbllogaccessfilename"] = LOGMGR_ACCESSFILENAME;
	orGblParams["gbllogaccessminlevel"] = LOGMGR_ACCESSMINLEVEL;
	orGblParams["gbllogampfilename"] = LOGMGR_AMPFILENAME;
	orGblParams["gbllogampminlevel"] = LOGMGR_AMPMINLEVEL;
	orGblParams["gbllogconsolefilename"] = LOGMGR_CONSOLEFILENAME;
	orGblParams["gbllogconsoleminlevel"] = LOGMGR_CONSOLEMINLEVEL;
	orGblParams["gbllogfiledir"] = LOGMGR_FILEDIR;	// Folder for log files
	orGblParams["gbllogsize"] = LOGMGR_LOGSIZE;
	orGblParams["gbllogreqhdrfilename"] = LOGMGR_REQHDRFILENAME;
	orGblParams["gbllogreqhdrminlevel"] = LOGMGR_REQHDRMINLEVEL;
	orGblParams["gbllogsysmsgfilename"] = LOGMGR_SYSMSGFILENAME;
	orGblParams["gbllogsysmsgminlevel"] = LOGMGR_SYSMSGMINLEVEL;
	orGblParams["gblmysqldatadir"] = AGLBLS_MYSQLDATADIR;
	orGblParams["gblusrfiledir"] = AUSRMGR_FILEDIR;		// usr
	orGblParams["gblusrmaxlogon"] = AUSRMGR_MAXLOGON;		// 4
	orGblParams["gblxmlport"] = AXMLSVR_PORT;
}

/*!
startup - Common code for use by webide or aissvc to startup a server.

\param ipSessionMgr -> Session manager for this instance of AIS
\param iorStartupMsgs - Place to add to collection of pending messages.
\param irStartupScript - Startup script to be submitted to the engine.
\return void
\par Notes:
-# Any classes defined in this routine will be destroyed on the return from this call.
-# Put servers that must live as long as the server is running in main or in AisSvc.run
 */
void ASys::startup(ASessionManager* ipSessionMgr, QString& iorStartupMsgs, const QString& irStartupScript)
{
	// Get MemoryPool. Get default minimum amount of memory to set aside for contexts
	// See also check for higher value set by application below in read of Default Context
	AStringMap& arGblParams = gAis.mGblParams;
	NUM aMemoryPool = arGblParams["gblaismemorypool"].toLongLong();

	// Initialize Embedded MySQL
	ipSessionMgr->initEmbeddedMySQL(iorStartupMsgs);

	// System Context
	QString& arApplicationDir = arGblParams["gblaisapplicationpath"];

	QString aDefaultContextName, aSystemContextName(AGLBLS_SYSTEMCONTEXTNAME);
	ipSessionMgr->registerContext(arApplicationDir, aSystemContextName, iorStartupMsgs);

	// Default Context
	if (!irStartupScript.isEmpty())
	{	ipSessionMgr->registerContext(irStartupScript, aDefaultContextName, iorStartupMsgs);
		if (aDefaultContextName.isEmpty() || aDefaultContextName == AGLBLS_SYSTEMCONTEXTNAME)
		{	iorStartupMsgs += "Invalid or missing context name for startup path " + irStartupScript + "\n";
			aDefaultContextName.truncate(0);
		}
		else	// Got something to chew on. If DefaultContextName not set, set it now.
		{	if (arGblParams["gblaisdefaultcontextname"].isEmpty())
				arGblParams["gblaisdefaultcontextname"] = aDefaultContextName;

			// Check if higher memory pool requested in appini file or startup script.
			AStringMap *apParams = ipSessionMgr->getContextParams(aDefaultContextName);
			if (apParams->contains("gblaismincontextpool"))
			{	QString aContextMemoryPoolStr = (*apParams)["gblaismincontextpool"];
				NUM aContextMemoryPool = aContextMemoryPoolStr.toLongLong();
				if (aContextMemoryPool > aMemoryPool)
					aMemoryPool = aContextMemoryPool;
			}
			// Check if memory pool needs to be larger for startup context
			if (apParams->contains("memory"))
			{	QString aContextMemoryStr = (*apParams)["memory"];
				NUM aContextMemory = aContextMemoryStr.toLongLong();
				if (aContextMemory > aMemoryPool)
					aMemoryPool = aContextMemory;
			}
		}
	}

	// System-wide Ports
	initializePorts(aDefaultContextName, iorStartupMsgs);

	// MemoryPool
	aMemoryPool *= (1024*1024);
	if (!ipSessionMgr->initMemoryMgr(aMemoryPool))
		throw QString("SessionMgr::initMemoryMgr, Allocation of MemoryPool fails");

	// Register Contexts
	QString aContextName;
	if (arGblParams.contains("gblaiscontextlist"))
	{	QString& arContextList = arGblParams["gblaiscontextlist"];
		if (!arContextList.isEmpty())
		{	QStringList aTkns = arContextList.split(',', QString::KeepEmptyParts);
			long aSize = aTkns.size();
			for (long aId = 0; aId < aSize; ++aId)
				ipSessionMgr->registerContext(aTkns[aId].trimmed(), aContextName, iorStartupMsgs);
		}
	}

}
// end
