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

#ifndef ACONTEXTTHREAD_H
#define ACONTEXTTHREAD_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/asessionmgr/acontextthread.h
														Context Thread

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0107	 9/18/2006	tlw		Added hasQuit method. Remove cMutex.
1.0057	 3/18/2005	tlw		Update documentation.
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QMutex>
#include "asessionmgr.h"
class QTimer;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define ACONTEXTTHREAD_TIMEOUT	1	// Time to pause between checking for the next request.

//	------------------------------------------------------ CLASSES ------------------------------------------------------------
/*!
\brief AContextThread - Start a new thread to run one instance of a Context

 */
class AContextThread : public QThread
{
	Q_OBJECT
public:
	AContextThread(AContextSt *ipContext, ASessionManager *ipSessionManager, long iStackSize);
	bool		hasQuit() { return cQuit;};
	void		localSleep(long iMsecs);		// Milliseconds
	void		quit();

	AContextSt*	cpContext;

protected:
	virtual void run();

private slots:
	void		onProcessNextRequest();

private:
	bool                   cQuit;
	ASessionManager*       cpSessionMgr;
	long                   cTimerId;
	QTimer*                cpTimer;
};
#endif	// ACONTEXTTHREAD_H
