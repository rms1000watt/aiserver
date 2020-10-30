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

#pragma once

#include <QtCore/QString>

/**
 * \brief Control Event Handler type defintion.
 */
typedef int (*RoutineHandle)(long iType);

class NTService;

class AService
{
public:

	/**
	 * \brief Constructor.
	 */
	AService();

	/**
	 * \brief Destructor.
	 */
	virtual ~AService();

	/**
	 * \brief Installs a Windows Service.
	 *
	 * \param[in] iStartupType Service Startup Mode.
	 * \param[in] irServiceName Name to identify the Service.
	 * \param[in] irDisplayName Display name of the Service.
	 * \param[in] irFullPath Path to the Service Executable.
	 * \param[in] ipUsername Optional account username.
	 * \param[in] ipPassword Optional account password.
	 */
	bool install(int iStartupType, const QString &irServiceName,
		const QString &irDisplayName, const QString &irFullPath,
		QString *ipUsername = NULL, QString *ipPassword = NULL);

	/**
	 * \brief Removes an installed Windows Service.
	 *
	 * \param[in] irServiceName Name of the installed Service.
	 */
	bool remove(const QString &irServiceName);

	/**
	 * \brief Checks if a Service is installed.
	 *
	 * \param[in] irServiceName Name of the Service.
	 */
	bool isService(const QString &irServiceName);

	/**
	 * \brief Checks if a Service is running.
	 *
	 * \param[in] irServiceName Name of the Service.
	 */
	bool isRunning(const QString &irServiceName);

	/**
	 * \brief Initializes a Service.
	 * 
	 * \param[in] irServiceName Name of the Service.
	 * \param[in] ipStartFunction Pointer to the Service entry point.
	 * \param[in] ipStopFunction Pointer to the Service stop handler.
	 *
	 * \notes
	 * - The ipStopFunction will be invoked when the Service is stopped (e.g. from the Service Control Manager).
	 * - This function would normally contain the cleanup/termination code for the application (e.g. event loop termination, memory deallocation).
	 */
	long init(const QString &irServiceName, void *ipStartFunction, void *ipStopFunction);

	/**
	 * \brief Starts a Service.
	 *
	 * \param[in] irServiceName Name of the installed Service.
	 */
	void start(const QString &irServiceName);

	/**
	 * \brief Stops a Service.
	 *
	 * \param[in] irServiceName Name of the installed Service.
	 */
	void stop(const QString &irServiceName);

	/**
	 * \brief Sets the Console Control Handler.
	 */
	void setConsoleControlHandler(RoutineHandle ipHandlerRoutine);

private:

	NTService *cpNTService;
	RoutineHandle cpConsoleCtrlHandler;

};
