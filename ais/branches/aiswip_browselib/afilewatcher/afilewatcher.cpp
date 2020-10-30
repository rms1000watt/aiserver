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
#include <iostream>
using namespace std;

// gpFileWatcher class supports functions in the glue layer.  This is initialized in the Constructor and should only have one 
// instance per server.
AFileWatcher* gpFileWatcher;

AFileWatcher::AFileWatcher(ASessionManager* ipSessionMgr)
	: QObject(NULL)
{
	cpSessionMgr = ipSessionMgr;
	cpWatcher = new QFileSystemWatcher();
	connect( cpWatcher, SIGNAL( fileChanged(const QString&)), this, SLOT(onFileChangedEvent(const QString&)));
	gpFileWatcher = this;
}

AFileWatcher::~AFileWatcher()
{
	if(cpWatcher != NULL)
		delete cpWatcher;
	gpFileWatcher = NULL;
}

void AFileWatcher::onFileChangedEvent(const QString& irFileName)
{
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
			if( cpSessionMgr->isAdminSession(aSessionId) == 0 )
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
}

void AFileWatcher::addPath(const long iSessionID, const QString& irPath)
{
	QString aContextName = "";
	cFileMgrMutex.lock();
	aContextName = cpSessionMgr->getContextName(iSessionID);
	cpWatcher->addPath(irPath);
	cFileWatcherMap.insertMulti(irPath, iSessionID);
	cFileMgrMutex.unlock();
}

void AFileWatcher::removePath(const long iSessionID, const QString& irPath)
{
    Q_UNUSED(iSessionID);
	cFileMgrMutex.lock();
	cpWatcher->removePath(irPath);
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
	aContextName = cpSessionMgr->getContextName( iSessionId );
	long aSessionCount = cpSessionMgr->getSessions( aContextName, aSessionList );
	for(int aCtr = 0; aCtr < aSessionCount; ++aCtr)
	{
		// Each Record appears in this format: ContextName\\tSessionId\\tAdmin\\tUsrName\\tCloseMode
		aRecord = aSessionList[aCtr];
		aRecordList = aRecord.split( "\t" );
		aSessionId = aRecordList[1].toInt( &aOk );
		// Send events to client sessions only, skip admin session
		if( cpSessionMgr->isAdminSession(aSessionId) == 0 )
		{
			aMsgs = irPath + '\t' + irCabinetName;
   			cpSessionMgr->cbSendToClient(aSessionId, aMsgs);
		}
	}
	cFileMgrMutex.unlock();
}
