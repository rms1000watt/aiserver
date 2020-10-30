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

#ifndef AISSVC_H
#define AISSVC_H

// Imports
#include "aservice.h"

// Macro definitions
#define AIS_DEFAULT_SERVICE_NAME "AIS"
#define AIS_DEFAULT_DISPLAY_NAME "Analytic Information System"

// Forward Declarations
class	AAisMgr;
class	AAisSvr;
class	ALogMgr;
class	ASessionManager;
class	AUsrMgr;
class	QCoreApplication;

/**
 * \brief AIS Service.
 *
 * This class contains the implementation of the main processing for AIS Service.
 * This does not include platform-specific calls (e.g. Windows Service).
 */
class AAisSvc
{
public:
	/**
	 * \brief Class constructor.
	 *
	 * \param[in] irStartupCmd Startup command, usually the 1st argument in main().
	 * \param[in] irStartupScript Startup script.
	 */
	AAisSvc(const QString &irStartupCmd, const QString &irStartupScript = "");

	/**
	 * \brief Class destructor.
	 */
	virtual ~AAisSvc();

	/**
	 * \brief Sets the startup script.
	 */
	void setStartupScript(const QString &irStartupScript);
	
	/**
	 * \brief Starts the service application.
	 *
	 * This function does not return until stop() is called.
	 */
	void start();

	/**
	 * \brief Stops the service application.
	 *
	 * This function causes start() to return.
	 */
	void stop();

private:
	AAisMgr				*cpAisMgr;
	AAisSvr				*cpAisSvr;
	ALogMgr				*cpLogMgr;
	ASessionManager		*cpSessionMgr;
	AUsrMgr				*cpUsrMgr;
	QString				cStartupCmd;
	QString				cStartupScript;
	QCoreApplication    *cpApp;
};

#endif // AISSVC_H
