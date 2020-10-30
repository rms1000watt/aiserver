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

#ifndef ASYS_H
#define ASYS_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/asys.h
													AIS System Startup Routines
ASys contains common startup routines used by main.cpp for the aisSvc, webide, and ride projects.

CHANGE HISTORY
Version	Date		Who		Change
2.0002	 1/23/2007	tlw		Add documentation on Default Startup Script.
1.0113	11/6/2006	tlw		Return error code if initialize fails.
1.0100	 6/15/2006	tlw		Convert documentation to doxygen
1.0058	 3/22/2005	tlw		Modify note on contexts.
1.0057	 3/20/2005	tlw		Split out the common files
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
//	Forward Declarations:
class AAisSvr;
class ASessionManager;

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ASys contains common startup routines used by aisSvc, webide and ride.

\par Contexts.
\par Registered Contexts.
Registered contexts are initialized and they are made visible to users.  A file specification is passed
to registerContext built-in command to identify the path and name of a startup script.  The path/name is saved, but the startup
script is not executed at this point.  All configurable parameters are set using parameters extracted from the startup script
and from any ini files found in the startup script folder.  The context name is set by this initialization process. A
registered context does not have an execution thread or any memory allocated.  See ASessionManager::registerContext for more
details.

\par System Context.
One System Context is registered for every instance of AIS, but it is not directly accessible to users. It
handles housekeeping tasks and provides support for those tasks not associated with any particular context (such as
registering or opening other contexts).  The System Context can do anything any other context can do except access the engine
(it cannot run an AisLisp script or process an AMP message).  

\par Default Context Name.
One default context may be specified during startup.  The default context name is set by the global parameter
GblAisDefaultContext.  If a startup script is specified on the command-line, this setting is overwritten with the context
name associated with the context specified the startup script. This allows a user to automatically set the default context
without editing a parameter file every time the user clicks on a startup script. The default context is used whenever an
incoming request does not specify a context.

\par Context List.
The global parameter GblAisContextList specifies zero or more startup scripts that are registered when the system is started.

\par Default Startup Script.  The initial working directory and startup script are set by the value passed in iorStartupScript
to ASys::initalizeParameters.  If it is empty, the value in the GblAisDefaultStartupScript is used.  The path provided by this
parameter is assumed to be relative to the install directory.  The context parameters are gleaned from the context.ini and
startup script in this working directory.  Next, ASys::startup registers this default context.  Finally, the application, such
as aissvc will open the default context.
 */
class ASys
{
public:
	static	bool	initializeParameters(const QString& irStartupCmd, QString& iorStartupScript, QString& orMsgs);
	static	void	startup(ASessionManager* ipSessionMgr, QString& iorStartupMsgs, const QString& irStartupScript);
	static	void	setGblParamDefaults(AStringMap& orGblParams);
	static	void	setCtxParamDefaults(AStringMap& orCtxParams);

private:
	static	void	initializePorts(const QString& irContextName, QString& orMsgs);
};

#endif // ASYS_H
