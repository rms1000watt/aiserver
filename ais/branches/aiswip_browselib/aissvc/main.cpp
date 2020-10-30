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
#include <iostream>
#include <QtCore/QFileInfo>

#include "aglobals.h"			// AGlobals
#include "aissvc.h"				// AAisSvc
#include "aservice.h"

#ifdef Q_OS_LINUX
#include <signal.h>
#include <getopt.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <pwd.h>
#endif

// Globals
AGlobals gAis;				// Define ONE global instance of AGlobals per application.
AAisSvc *gAisSvc = NULL;

using namespace std;

/**
 * \brief Initializes the AIS service instance.
 *
 * \param[in] irStartupCmd Contains the path to the executable, usually argv[0].
 * \param[in] irStartupScript Optional path to the startup script, absolute path.
 */
void initAisSvc(const QString &irStartupCmd, const QString &irStartupScript);

/**
 * \brief Deallocates the AIS service instance.
 */
void cleanAisSvc();

/**
 * \brief Starts the AIS service instance.
 * This function blocks until stopAisSvc() is called.
 */
void startAisSvc();

/**
 * \brief Stops the AIS service instance.
 */
void stopAisSvc();

#ifdef Q_OS_WIN

// The following table shows a mapping of a Control Event to its Control Code
// CTRL_C_EVENT			0
// CTRL_BREAK_EVENT		1
// CTRL_CLOSE_EVENT		2
// CTRL_LOGOFF_EVENT	5
// CTRL_SHUTDOWN_EVENT	6

// We are defining this here since we are avoiding conflicts between our fsmtbase.h and windows.h
#define CTRL_C_EVENT	 0
#define CTRL_CLOSE_EVENT 2

/**
 * \brief Console control event handler for Windows.
 *
 * \param[in] iCtrlCode Contains the code for the type of console event that occured.
 */
int consoleEventHandler(long iCtrlCode)
{
	// if the user types in Ctrl+C,
	// terminates the process using Task Manager,
	// or closes the Console window
	if(iCtrlCode == CTRL_C_EVENT || iCtrlCode == CTRL_CLOSE_EVENT)
	{
		stopAisSvc();
	}

	return 1;
}

#endif

#ifdef Q_OS_LINUX
/**
 * \brief Signal handler for Unix.
 *
 * \param[in] Signal code.
 */
void signalEventHandler(int)
{
    stopAisSvc();
}

/**
 * \brief Setup signal handlers.
 *
 */
void initSignalHandler()
{
    int aSignals[] = {SIGINT, SIGTERM, SIGABRT};
    int aCnt = sizeof(aSignals)/sizeof(int);

    for (int i = 0; i < aCnt; i++)
    {
        signal(aSignals[i], signalEventHandler);
    }
}

/**
 * \brief Create PID file.
 *
 * \param[in] irPidFile Path to PID file.
 *
 * \return true if the PID file was created successfully.
 */
bool createPidFile(const QString &irPidFile)
{
    QFile aPidFile(irPidFile);

    if (aPidFile.open(QFile::WriteOnly))
    {
        QTextStream aStream(&aPidFile);
        aStream << (long)getpid();
        return true;
    }

    cerr << "Error: Could not create PID file." << endl;
    return false;
}

/**
 * \brief Erase PID file.
 *
 * \param[in] Path to PID file.
 */
void erasePidFile(const QString &irPidFile)
{
    if (irPidFile.length() > 0)
    {
        QFile aPidFile(irPidFile);
        aPidFile.remove();
    }
}

/**
 * \brief Sets the effective user of the process.
 *
 * \param[in] irUser Target user account.
 *
 * \return true if the operation was completed successfully.
 */
bool setEffectiveUser(const QString &irUser)
{
    struct passwd *apUserInfo = NULL;
    uid_t aUserId = geteuid();

    // if effective user id is 0, then we are running as root user.
    if (aUserId)
    {
        // only the root user can change the effective user of the process
        // however, we will not issue an error if the current effective user
        // is the same as the target effective user

        // retrieve user info
        apUserInfo = getpwnam(irUser.toLatin1().data());
        if (!apUserInfo || (apUserInfo->pw_uid != aUserId))
        {
            cerr << "Error: Only the root user can change the effective user." << endl;
            // no permission to change to a different user, return false.
            return false;
        }

        // effective user is same as target, ignore.
        return true;
    }

    if (!irUser.compare(QString("root")))
        return true;

    apUserInfo = getpwnam(irUser.toLatin1().data());
    if (!apUserInfo)
    {
        cerr << "Error: Invalid effective user." << endl;
        // invalid target user, return false.
        return false;
    }

    if (setgid(apUserInfo->pw_gid) == -1)
    {
        cerr << "Error: Failed to change the effective group." << endl;
        // failed to change to target group, return false.
        return false;
    }
    if (setuid(apUserInfo->pw_uid) == -1)
    {
        cerr << "Error: Failed to change the effective user." << endl;
        // failed to change to target user, return false.
        return false;
    }

    return true;
}

#endif

//	------------------------------------------------ FUNCTION DEFINITIONS  ----------------------------------------------------
/**
 * \brief AIS Service Application.
 *
 * \note
 * The Windows version of AIS Service is designed to be installed and executed as a Windows Service.
 * The Linux version is designed to run as daemon process (using nohup called from a system init script.)
 * The usage of the application is unique for each platform.
 *
 * \par
 * In Windows, the following syntax is supported:
 * - aissvc32.exe [command] [arguments]
 * The following commands and the corresponding options are supported:
 * # --install [service name] [display name] [startup script]
 * # --install-manual [service name] [display name] [startup script]
 * # --remove [service name]
 * # --start [service name]
 * # --stop [service name]
 * # --stand-alone [startup script]
 *
 * \par
 * In Linux, the following syntax is supported:
 * - aissvcdevexe [-u username] [-p pid_file] [startup script]
 * - aissvcdevexe [--user username] [--pidfile pid_file] [startup script]
 * - aissvcdevexe [startup script]
 *
 * \par
 * In Windows, if the command "--stand-alone" is used, AIS Service will run as a normal application.
 * It can be terminated using Ctrl+C. To install the AIS Service as a Windows Service, the commands
 * "--install" or "--install-manual" can be used. If successful, AIS Service should appear in the
 * Service Control Manager list, identified by the Service Name or Display Name arguments. Using
 * "--install" will cause AIS Service to be started at boot time. Use "--install-manual" to disable
 * auto-start at boot.
 *
 * \par
 * The "--start", "--stop", and "--remove" requires an AIS Service to be installed (as a Windows Service).
 * "--start" and "--stop" can be used to execute and terminate the AIS Service, while "--remove" is
 * used to remove the AIS Service from the Service Control Manager list. If the Service is running, it
 * will stop it first before removing it.
 *
 */
int main(int iArgc, char **ipArgv)
{
#ifdef Q_OS_WIN
    // The following block of code is Windows specific.
    
	QString aCommand;
	QString aServiceName(AIS_DEFAULT_SERVICE_NAME);
	QString aDisplayName(AIS_DEFAULT_DISPLAY_NAME);
	QString aStartupScript;
	QString aFilePath;
	QFileInfo aPathInfo;
	int aStartupType = 1;
	AService aService;

	// process command-line arguments here:
	// "--install"
	// "--install-manual"
	// "--remove"
	// "--stand-alone"
	// "--start"
	// "--stop"
    // "--exec"
    
    // Note: --exec is reserved for the call by Service Control Manager

	if (iArgc > 1)
	{
		// 1st parameter is the command
		aCommand = ipArgv[1];

		if (aCommand.compare("--install", Qt::CaseInsensitive) == 0)
		{
process_install:
			if (iArgc >= 5)
			{
				// --install [service name] [display name] [startup script]
				aServiceName = ipArgv[2];
				aDisplayName = ipArgv[3];
				aStartupScript = ipArgv[4];
			}
			else if (iArgc >= 4)
			{
				// --install [service name] [startup script]
				aServiceName = ipArgv[2];
				aStartupScript = ipArgv[3];
			}
			else if (iArgc >= 3)
			{
				// --install [startup script]
				aStartupScript = ipArgv[2];
			}

			cout << "Command: install" << endl;
			cout << "Service Name: " << qPrintable(aServiceName) << endl;
			cout << "Display Name: " << qPrintable(aDisplayName) << endl;
			cout << "Startup Script: " << qPrintable(aStartupScript) << endl;

			if (aService.isService(aServiceName))
			{
				cerr << "Error: Service already installed." << endl;
			}
			else
			{
				aPathInfo.setFile(QString(ipArgv[0]));
				
				// build the file path
				// start with the absolute file path
				// append --exec
				// append service name
				// append startup script

				// we also need to quote the parameters
				aFilePath.append('"' + aPathInfo.absoluteFilePath() + '"');
				aFilePath.append(" --exec ");
				aFilePath.append('"' + aServiceName + '"');
			
				if (aStartupScript.length())
				{
					aFilePath.append(' ');
					aFilePath.append('"' + aStartupScript + '"');
				}

				// prepend "--exec" to the list of parameters.
				// when this application is executed by SCM, it will be handled in the "--exec" case
				// and the application will formally start.
				// at this point, the Service is just installed.
				// use SCM to verify if the service was correctly installed
				aService.install(aStartupType, aServiceName, aDisplayName, aFilePath);
			}
		}
		else if (aCommand.compare("--install-manual", Qt::CaseInsensitive) == 0)
		{
			aStartupType = 0;
			goto process_install;
		}
		else if (aCommand.compare("--remove", Qt::CaseInsensitive) == 0)
		{
			if (iArgc >= 3)
			{
				// --remove [service name]
				aServiceName = ipArgv[2];
			}

			cout << "Command: remove" << endl;
			cout << "Service Name: " << qPrintable(aServiceName) << endl;
			
			if (aService.isService(aServiceName))
			{
				// the Service might still be running
				// stop it first before removing it
				if (aService.isRunning(aServiceName))
				{
					aService.stop(aServiceName);
				}

				aService.remove(aServiceName);
			}
			else
			{
				cerr << "Error: Service not installed." << endl;
			}
		}
		else if (aCommand.compare("--start", Qt::CaseInsensitive) == 0)
		{
			if (iArgc >= 3)
			{
				// --start [service name]
				aServiceName = ipArgv[2];
			}

			cout << "Command: start" << endl;
			cout << "Service Name: " << qPrintable(aServiceName) << endl;

			if (aService.isService(aServiceName))
			{
				if (!aService.isRunning(aServiceName))
				{
					aService.start(aServiceName);
				}
				else
				{
					cerr << "Error: Service already started." << endl;
				}
			}
			else
			{
				cerr << "Error: Service not installed." << endl;
			}
		}
		else if (aCommand.compare("--stop", Qt::CaseInsensitive) == 0)
		{
			if (iArgc >= 3)
			{
				// --stop [service name]
				aServiceName = ipArgv[2];
			}

			cout << "Command: stop" << endl;
			cout << "Service Name: " << qPrintable(aServiceName) << endl;

			if (aService.isService(aServiceName))
			{
				if (aService.isRunning(aServiceName))
				{
					aService.stop(aServiceName);
				}
				else
				{
					cerr << "Error: Service already stopped." << endl;
				}
			}
			else
			{
				cerr << "Error: Service not installed." << endl;
			}
			
			// Note: the Service should only be stopped using the SCM
			// The behavior is unknown if the process is killed
		}
		else if (aCommand.compare("--exec", Qt::CaseInsensitive) == 0)
		{
			// --exec [service name] [startup script]
			if (iArgc >= 4)
			{
				aStartupScript = ipArgv[3];
			}
			if (iArgc >= 3)
			{
				aServiceName = ipArgv[2];
			}

			// start the service normally
			if (aService.isService(aServiceName))
			{
				initAisSvc(QString(ipArgv[0]), aStartupScript);
				aService.init(aServiceName, (void*)startAisSvc, (void*)stopAisSvc);
			}
		}
		else if (aCommand.compare("--stand-alone", Qt::CaseInsensitive) == 0)
		{
			// --stand-alone [startup script]
			
			// start the service as a regular console application
			// which can be terminated using ctrl+c
			// there's no need to call any methods of AService

			if (iArgc >= 3)
			{
				aStartupScript = ipArgv[2];
			}

			// initialize AisSvc object
			initAisSvc(QString(ipArgv[0]), aStartupScript);

			// install signal (ctrl+c) handler
			aService.setConsoleControlHandler(consoleEventHandler);

			// start AisSvc processing
			startAisSvc();
		}
		else
		{
			cerr << endl << "Error: Unsupported option." << endl;
			goto display_usage;
		}

		return 0;
	}
	else
	{
display_usage:
		// display usage
		cout << endl
			 << "Controls the Analytic Information System (AIS) Service." << endl << endl
			 << "\t" << "--install" << endl
			 << "\t\t" << "- Installs this program as a Windows Service (Startup Type: Automatic)." << endl << endl
			 << "\t" << "--install-manual" << endl
			 << "\t\t" << "- Install this program as a Windows Service (Startup Type: Manual)." << endl << endl
			 << "\t" << "--remove" << endl
			 << "\t\t" << "- Uninstalls this program as a Windows Service." << endl << endl
			 << "\t" << "--start" << endl
			 << "\t\t" << "- Starts the Windows Service." << endl << endl
			 << "\t" << "--stop" << endl
			 << "\t\t" << "- Stops the Windows Service." << endl << endl
			 << "\t" << "--stand-alone" << endl
			 << "\t\t" << "- Runs this program as a stand-alone application." << endl << endl
			 << "Command Parameters:" << endl << endl
			 << "\t" << "--install [startup script]" << endl
			 << "\t" << "--install [service name] [startup script]" << endl
			 << "\t" << "--install [service name] [display name] [startup script]" << endl << endl
			 << "\t" << "--install-manual [startup script]" << endl
			 << "\t" << "--install-manual [service name] [startup script]" << endl
			 << "\t" << "--install-manual [service name] [display name] [startup script]" << endl << endl
			 << "\t" << "--remove [service name]" << endl << endl
			 << "\t" << "--start [service name]" << endl << endl
			 << "\t" << "--stop [service name]" << endl << endl
			 << "\t" << "--stand-alone [startup script]" << endl << endl
			 << "Notes:" << endl << endl
			 << "\t" << "- The [service name] parameter can be used to install the service under a different identifier." << endl
			 << "\t" << "- The [startup script] parameter can be used to specify a different startup script (use only absolute path)." << endl
			 << "\t" << "- The [display name] parameter can be used to change the display name of a service during install."
			 << endl;
	}
#endif

#ifdef Q_OS_LINUX
    // The following block of code is Linux specific.
    
    QString aCommand;
	QString aStartupScript;
    QString aPidFile;
    QString aUser;
    QFileInfo aFileInfo;
    int aOpt;

    // struct option details
    // name     - option name
    // has_args - 0 (no arguments), 1 (argument required), 2 (argument optional)
    // flag     - NULL (returns val), address of variable (set to val)
    // val      - value to return or set to the variable pointed by flag
	struct option aOptions[] = {
			{"user", 1, NULL, 'u'},
			{"pidfile", 1, NULL, 'p'},
			{0, 0, NULL, 0}
	};

    aPidFile = QString("%1.pid").arg(ipArgv[0]);

    // optarg   - pointer to argument after option
    while ((aOpt = getopt_long(iArgc, ipArgv, "u:p:", aOptions, NULL)) != -1)
    {
		switch(aOpt)
		{
		case 'u':
			aUser = optarg;
			break;
		case 'p':
			aPidFile = optarg;
			break;
		default:
            cerr << "Error: Invalid option!" << endl;
			cerr << "Usage: " << ipArgv[0] << " [-u user] [-p pidfile] [startup file]" << endl;
            return -1;
			break;
		}
    }

    // optind   - index in argv of the first argv-element that is not an option
    if (optind < iArgc)
    {
        aStartupScript = ipArgv[optind];
    }

    // initialize AisSvc instance
    initAisSvc(QString(ipArgv[0]), aStartupScript);

    // setup signal handler
    initSignalHandler();

    // set the effective user (if --user or -u was used)
    if ((aUser.length() > 0) && (!setEffectiveUser(aUser)))
    {
        cleanAisSvc();
        return -1;
    }

    aFileInfo.setFile(aPidFile);
    aPidFile = aFileInfo.absoluteFilePath();

    // create PID file (if --pidfile or -p was used)
    if (!createPidFile(aPidFile))
    {
        cleanAisSvc();
        return -1;
    }

    // start service instance
    startAisSvc();

    // remove pid file
    erasePidFile(aPidFile);

    // cleanup service instance
    cleanAisSvc();

#endif

	return 0;
}

void initAisSvc(const QString &irStartupCmd, const QString &irStartupScript)
{
	gAisSvc = new AAisSvc(irStartupCmd, irStartupScript);
}

void cleanAisSvc()
{
    if (gAisSvc)
    {
        delete gAisSvc;
        gAisSvc = NULL;
    }
}   

void startAisSvc()
{
	if (gAisSvc)
    {
		gAisSvc->start();
        cleanAisSvc();
    }
}

void stopAisSvc()
{
	if (gAisSvc)
		gAisSvc->stop();
}

// end
