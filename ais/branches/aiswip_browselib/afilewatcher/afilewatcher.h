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
#ifndef AFILEWATCHER_H
#define AFILEWATCHER_H

/*      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
							File Watcher Class

CHANGE HISTORY
Version Date            Who             Change
1.0001  9/29/2008       rca             Add Doxygen documentation

        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//      ------------------------------------------------------ IMPORTS ------------------------------------------------------------


#include <QtCore/QFileSystemWatcher>
#include "../asessionmgr/asessionmgr.h"

//      ------------------------------------------------------ CLASSES ------------------------------------------------------------

class AFileWatcher: public QObject
{
	Q_OBJECT
public:
    AFileWatcher(ASessionManager* ipSessionMgr);
    ~AFileWatcher();
	void addPath(const long iSessionID, const QString& irPath);
	void removePath(const long iSessionID, const QString& irPath);
	void sendFileChangedEvent(const long iSessionID, const QString& irPath, const QString& irCabinetName);


public slots:
    void onFileChangedEvent(const QString& irFileName);

private:
	QFileSystemWatcher*	cpWatcher;			// Watcher class
	ASessionManager*	cpSessionMgr;			// Used to call browseLib functions
	QMutex			cFileMgrMutex;			// Mutex lock for this class
	QMap< QString, long >	cFileWatcherMap;		// Mapping of file to a session ID, this ID will get mapped to a context
};

#endif
