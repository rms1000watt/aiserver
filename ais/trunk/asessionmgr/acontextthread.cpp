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
aisdev/asessionmgr/acontextthread.cpp
										Context Thread

CHANGE HISTORY
Version	Date		Who		Change
5.0000   8/20/2008  fchua   MySQL Support.
1.0113	 11/7/2006	tlw		destructor. Omit unused destructor.
1.0107	 9/18/2006	tlw		Fix destructor so that thread is deleted prior to exit. Improve the stop process.
1.0056	 3/18/2005	tlw		Update documentation.
												---------------------------------
DOCUMENTATION
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QTimer>
#include "aglobals.h"
#include "alogmgr.h"
#include "asessionmgr.h"
#include "acontextthread.h"
#include "asbglue.h"

#if __EXMYSQL
// Implementation Note: If MySQL is included (fsmtbase.h), we must also
// add the following include path to the properties for this source file:
// "$MYSQLDIR\include"
typedef unsigned int  SOCKET;
extern "C" { // includes for modules written in C
	#include "mysql.h"
	}
#endif

//	------------------------------------------------------ METHODS ------------------------------------------------------------
/*!
\brief AContextThread constructor initializes the class variables

\param ipContext -> New context structure
\param ipSessionMgr -> Session manager that is starting the new thread
\param iStackSize - The size of the stack for this thread in bytes.
*/
AContextThread::AContextThread(AContextSt *ipContext, ASessionManager *ipSessionMgr, long iStackSize)
	: QThread(), cpContext(ipContext), cpSessionMgr(ipSessionMgr)
{
	cQuit = false;
	cpTimer = NULL;
	cTimerId = -1;
	setStackSize(iStackSize);
}

/*!
\brief localSleep - Suspend this thread from executing for iMsecs

\param iMsecs - Length of time to sleep [milliseconds]
return void
 */
void AContextThread::localSleep(long iMsecs)
{
	msleep((ulong)iMsecs);
}

/*	-------------------------------------------------------------------------------------------------------------------
onProcessNextRequest - A process loop that waits for new requests and then processes them.

It is essential that this routine be called from the run method so that it runs on the new thread, not the main thread.
Otherwise, this routine will bring the main event loop to its knees since this routine implements an endless loop.
Even though AContextThread was created in the main thread, it is possible to call one of AContextThread's methods, such
as this one, on the new thread.
The rules are:
 1. The child of aQObject must be created in the thread of the parent QObject. For example, AContextThread cannot be
	the parent of an object that runs in the new thread since AContextThread was created in the parent thread.
 2.	All objects that live in a thread must be deleted before the thread itself is deleted.  One way to do this is
	to create the objects as automatic variables in run.
 3. QObjects must be deleted in the thread that created them.  If an object must be deleted that lives in a different
	thread, call QObject::deleteLater() which posts an event on the object's own thread.  This assumes that event
	processing on the object's thread is still active.

Notes:
 1. It would be better if this method used QWaitCondition to wait for a new request rather than polling.
 2. A loop that just sleeps will not allow events to be processed
 3. Quit sets cQuit to true which causes onProcessNextRequest to call quit which causes run to terminate, terminating
	the thread.
 4. Since cQuit is a boolean it does not require synchronization.  cpSessionMgr, cpContext, and cpTimer are read-only.
	---------------------------------------------------------------------------------------------------------------- */
void AContextThread::onProcessNextRequest()
{
    if (!cQuit && cpSessionMgr->isContextAlive(cpContext))
        cpSessionMgr->processNextRequest(cpContext);
    else
    {
        cpTimer->stop();
        quit();         // this will not tell the event loop to quit
        exit(0);        // this will terminate the event loop
    }
}

/*!
\brief run - Begin accepting requests on the new thread.

Each context runs on a separate thread. QtThread provides an abstract virtual function, run() called from start() which
executes in the new thread. run opens a context, sets a timer, and then falls into the new thread's event loop.  The timer
is a direct connection, so it calls onProcessNextRequest on the same thread as the new timer.  This is important
because the receiver is "this" which refers to this instance of AContextThread which lives on the main thread. Note
that only run and those methods called from run actually run on the new thread.
\return void
\par Notes:
-# Synchronization. Since no events are possible until exec is started, there is no need to use a mutex lock here.
*/
void AContextThread::run()
{
    if (cpContext != NULL)
    {
        long aIx;	// Context Index
        QTimer aTimer;	// Top-level object. NOT a child of AContextThread which lives on the main thread.
        aTimer.setObjectName("AContextThread");
        aTimer.setInterval(100);        // Time to wait before starting the process-requests loop [msec].
        QObject::connect(&aTimer, SIGNAL(timeout()), this, SLOT(onProcessNextRequest()), Qt::DirectConnection);
        cQuit = false;
        QString& arCtxName = cpContext->mContextName;
        const char* apScript = cpContext->mRunScript.data();
        cpContext->mpCP = NULL;
        if ((aIx=SBGlue_OpenContext(arCtxName.toLatin1().data(),apScript,&cpContext->mpCP,cpContext->mMemorySize,cpContext->mObjHdrSize,this)) >= 0)
        {
            cpContext->mContextIndex = aIx;
            cQuit = false;
            cpTimer = &aTimer;
            aTimer.start();			// Calls onProcessNextRequest on this thread after the event loop is started.
            exec();				// No return until this thread's event loop is terminated by a call to quit.
        }
        else
        {
            qDebug("AContextThread::run(), Open fails, ret=%ld Context=%s", aIx, arCtxName.toLatin1().data());
            // Note: To keep the system from looping forever with no user context open we must exit here.
            if (SBGlue_CountOfOpenContexts() <= 1) ::exit(-1);
        }

		// Terminate the context on termination of thread execution. 
		// Note: We only arrive here after return from the thread exec() method.
		//ThreadTermination:
		//if ((cpContext != NULL) && (cpContext->mType == CONTEXTTYPECODE) && (cpContext->mAlive >= 0))
		//	SBGlue_FlushLispCodeInContext(cpContext->mpCP);

		// FCC: This should be the only place we release the context memory
		SBGlue_FlushLispCodeInContext(cpContext->mpCP);

        if (cpTimer != NULL)
            cpTimer->stop();		// Stop timer used to continuously check for new requests.
        cpTimer = NULL;

        cQuit = true;			// Just in case it was not already set to true.
    }
}

void AContextThread::quit()
{
	cQuit = true;
}

// end
