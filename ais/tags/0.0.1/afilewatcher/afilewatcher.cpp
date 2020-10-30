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
#include "afilewatcher.h"
#include "asbglue.h"

// gpFileWatcher class supports functions in the glue layer.  This is initialized in the Constructor and should only have one 
// instance per server.
AFileWatcher* gpFileWatcher;

AFileWatcher::AFileWatcher(ASessionManager* ipSessionMgr)
{
	cpSessionMgr = ipSessionMgr;
	cpWatcher = new QFileSystemWatcher();
	connect(cpWatcher, SIGNAL(fileChanged(const QString&)), this, SLOT(onFileChangedEvent(const QString&)));

	cpTimer = new QTimer();
	connect(cpTimer, SIGNAL(timeout()), this, SLOT(onTimeout()));
	cpTimer->start(FILEWATCHER_TIMEOUT);

	gpFileWatcher = this;
}

AFileWatcher::~AFileWatcher()
{
	if(cpWatcher != NULL)
		delete cpWatcher;

	if (cpTimer != NULL)
	{
		cpTimer->stop();
		delete cpTimer;
	}

	gpFileWatcher = NULL;
}

void AFileWatcher::onFileChangedEvent(const QString& irFileName)
{
	/*
	char* ipData = NULL;
	long aSessionId;
	QByteArray aAmpReq;
	QString aMsgs;
	cFileMgrMutex.lock();
	aSessionId = cFileWatcherMap.value(irFileName);
	if (aSessionId > 0)
	{
		QString aContextName;
		QStringList aSessionList;
		QString aRecord;
		QStringList aRecordList;
		bool aOk;
		aContextName = cpSessionMgr->getContextName( aSessionId );
		long aSessionCount = cpSessionMgr->getSessions( aContextName, aSessionList );
		for(int aCtr = 0; aCtr < aSessionCount; ++aCtr)
		{
			// Each Record appears in this format: ContextName\\tSessionId\\tAdmin\\tUsrName\\tCloseMode
			aRecord = aSessionList[aCtr];
			aRecordList = aRecord.split( "\t" );
			aSessionId = aRecordList[1].toInt( &aOk );
			// Send events to client sessions only, skip admin session
			if(cpSessionMgr->isAdminSession(aSessionId) == 0)
			{
				aMsgs = "(browseLib.setWatchedFileFlag {" + irFileName + "} true)";
				aAmpReq = aMsgs.toLatin1();
    			long aReqId = cpSessionMgr->submit(aSessionId, SBGLUE_EVENTCMD, SBGLUE_NO_DEBUG, aAmpReq, QDateTime::currentDateTime(), ipData);
                Q_UNUSED(aReqId);
                // FC - Should we be using aReqId?
			}
		}
	}
	cFileMgrMutex.unlock();
	*/

	cFileMgrMutex.lock();
	// Add event only if it doesn't exist
	if (!cEventList.contains(irFileName))
	{
		cEventList.append(irFileName);
	}
	cFileMgrMutex.unlock();
}

void AFileWatcher::addPath(const long iSessionID, const QString& irPath)
{
	cFileMgrMutex.lock();
	cpWatcher->addPath(irPath);

	// it might be better to add the context name instead of the session id
	// to prevent duplicate notifications, since all notifications will
	// go to the same parent context
	cFileWatcherMap.insert(irPath, iSessionID);
	cFileMgrMutex.unlock();
}

void AFileWatcher::removePath(const long iSessionID, const QString& irPath)
{
    cFileMgrMutex.lock();
	cFileWatcherMap.remove(irPath, iSessionID);

	// Remove only when there are no more entries in the registry
	if (cFileWatcherMap.value(irPath) == 0)
	{
		cpWatcher->removePath(irPath);
	}
	cFileMgrMutex.unlock();
}

void AFileWatcher::sendFileChangedEvent(const long iSessionId, const QString& irPath, const QString& irCabinetName)
{
	QString aContextName;
	QString aMsgs;
	QString aRecord;
	QStringList aRecordList;
	QStringList aSessionList;
	long aSessionId;
	bool aOk;

	cFileMgrMutex.lock();
	aContextName = cpSessionMgr->getContextName(iSessionId);
	long aSessionCount = cpSessionMgr->getSessions(aContextName, aSessionList);
	for(int aCtr = 0; aCtr < aSessionCount; ++aCtr)
	{
		// Each Record appears in this format: ContextName\\tSessionId\\tAdmin\\tUsrName\\tCloseMode
		aRecord = aSessionList[aCtr];
		aRecordList = aRecord.split("\t");
		aSessionId = aRecordList[1].toInt(&aOk);
		// Send events to client sessions only, skip admin session
		if(cpSessionMgr->isAdminSession(aSessionId) == 0)
		{
			aMsgs = irPath + '\t' + irCabinetName;
   			cpSessionMgr->cbSendToClient(aSessionId, aMsgs);
		}
	}
	cFileMgrMutex.unlock();
/*
	QString aMsgs = irPath + '\t' + irCabinetName;
	QList<long> aSessIdList;

	if (irPath.isEmpty())
	{
		// only the session who triggered the "browseLib.checkin" operation should
		// received this message, otherwise all sessions will prompted
		// to export the same cabinet

		// other sessions will be notified when the file is actually written
		// and the file change event is processed
		cpSessionMgr->cbSendToClient(iSessionId, aMsgs);
	}
	else
	{
		// if irPath is specified, it means that the call was made from
		// (browseLib.setWatchedFileFlag ...) which provides us with the actual file monitored
		// we then notify sessions registered with the path (or all sessions from the same context)
		// see addPath() and removePath()

		cFileMgrMutex.lock();
		// go through each registered session and deliver the notification
		aSessIdList = cFileWatcherMap.values(irPath);
		if (!aSessIdList.isEmpty())
		{
			QList<long>::iterator aIdIter;
			for (aIdIter = aSessIdList.begin(); aIdIter != aSessIdList.end(); ++aIdIter)
			{
				cpSessionMgr->cbSendToClient(*aIdIter, aMsgs);
			}
		}
		cFileMgrMutex.unlock();
	}
*/
}

// signal handler for cpTimer
void AFileWatcher::onTimeout()
{
	processEvents();
}

// this function causes the context admin session
// to invoke browseLib.setWatchedFileFlag for each of the
// files modified within the last 1000ms
// the browseLib.setWatchFileFlag will eventually
// cause sendFileChangeEvent to be invoked and
// all the registered sessions will receive the notification
void AFileWatcher::processEvents()
{
	cFileMgrMutex.lock();
	if (!cEventList.isEmpty())
	{
		// go through each file
		QStringList::iterator aIter;
		for (aIter = cEventList.begin(); aIter != cEventList.end(); ++aIter)
		{
			// no need to go through each registered session
			// since the command will be processed in the admin session (because of SBGLUE_EVENTMSG)
			// we only need to do this once for every file

			long aSessId;
			QByteArray aAmpReq;
			QString aMsgs;

			// we still need one valid session id for submit to work properly
			aSessId = cFileWatcherMap.value(*aIter);
			if (aSessId > 0)
			{
				aMsgs = "(browseLib.setWatchedFileFlag {" + *aIter + "} true)";
				aAmpReq = aMsgs.toLatin1();

				// note the aSessId is not really used here
				// internally it will be replaced by the admin session id
				cpSessionMgr->submit(aSessId, SBGLUE_EVENTCMD, SBGLUE_NO_DEBUG, aAmpReq, QDateTime::currentDateTime(), NULL);
			}
		}
		// clear the event list
		cEventList.clear();
	}
	cFileMgrMutex.unlock();
}
