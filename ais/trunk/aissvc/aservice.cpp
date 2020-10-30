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

#include <windows.h>
#include <process.h>
#include "nt_servc.h"
#include "aservice.h"

AService::AService(): cpNTService(NULL), cpConsoleCtrlHandler(NULL)
{
	cpNTService = new NTService();
}

AService::~AService()
{
	delete cpNTService;
	/*
	WSACleanup();
	*/
}

bool AService::install(int iStartupType, const QString &irServiceName, 
					   const QString &irDisplayName, const QString &irFullPath, 
					   QString *ipUsername, QString *ipPassword)
{
	TCHAR *apUsername = (ipUsername) ? (TCHAR*)ipUsername->utf16() : NULL;
	TCHAR *apPassword = (ipPassword) ? (TCHAR*)ipPassword->utf16() : NULL;

	return cpNTService->Install(iStartupType, (TCHAR*)irServiceName.utf16(), 
		(TCHAR*)irDisplayName.utf16(), (TCHAR*)irFullPath.utf16(),
		apUsername, apPassword);
}

bool AService::remove(const QString &irServiceName)
{
	return cpNTService->Remove((TCHAR*)irServiceName.utf16());
}

bool AService::isService(const QString &irServiceName)
{
	return cpNTService->IsService((TCHAR*)irServiceName.utf16());
}

bool AService::isRunning(const QString &irServiceName)
{
	return cpNTService->IsRunning((TCHAR*)irServiceName.utf16());
}

long AService::init(const QString &irServiceName,
					void *ipStartFunction, void *ipStopFunction)
{
	if (ipStopFunction)
		cpNTService->SetShutdownHandler(ipStopFunction);

	return cpNTService->Init((TCHAR*)irServiceName.utf16(), ipStartFunction);
}

void AService::stop(const QString &irServiceName)
{
	cpNTService->Stop((TCHAR*)irServiceName.utf16());
}

void AService::start(const QString &irServiceName)
{
	cpNTService->Start((TCHAR*)irServiceName.utf16());
}

void AService::setConsoleControlHandler(RoutineHandle ipHandlerRoutine)
{
	// The Console Control Handler can also be set in the main source file.
	// However, our fsmtbase.h conflicts with some of the header files included in windows.h.
	// Instead of changing the type definitions with fsmtbase.h and all throughout the solution,
	// the call to SetConsoleCtrlHandler was hidden inside the AService implementation.
	SetConsoleCtrlHandler((PHANDLER_ROUTINE)ipHandlerRoutine, 1);
}
