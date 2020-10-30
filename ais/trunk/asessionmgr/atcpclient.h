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

#ifndef ATCPCLIENT_H
#define ATCPCLIENT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aiswip/asessionmgr/atcpclient.h
														Tcp Client

CHANGE HISTORY
Version	Date		Who		Change
1.1000   4/05/2007  jmo     removed unnecessary classes and variables
1.0000	 3/27/2007	jmo		Initial implementation.
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------

#include <QtCore/QMutex>
#include "asocket.h"

typedef long AXID;
typedef long ATcpTaskId;
/*!
\brief ATcpTask - Holds information of the current task
\par Usage.
For every action, provide an ATcpTask with the current task ID and
append it to the ATcpTasks QHash. The current task ID will be used
to indexed that task that has been added into ATcpTasks later when
the main thread will check for the mTcpTaskComplete, a status flag
that identifies whether the task is done or not.
*/
class ATcpTask
{
public:
    ATcpTask(){} 
    ATcpTask(ATcpTaskId iTcpTaskId):mTcpTaskId(iTcpTaskId),mTcpTaskComplete(false){}
    ATcpTaskId      mTcpTaskId;
    bool            mTcpTaskComplete;
	QString         mAisOut;
};
typedef QHash<long, ATcpTask> ATcpTasks; // long value is the key to an ATcpTask

/*!
\brief ATcpClient - Implements a TCP client connection to a server (e.i. HTTP server)
\par Usage.
A typical scenario:
-# Create an instance of ATcpClient specifying the server to connect to
-# Perform OpenConnection to connect to (an HTTP) server
-# Perform IsConnected to check whether the TCP client is successfully connected to a server
-# Call submit to send (HTTP) request to be processed by the server
-# Call CloseConnection to disconnect from the server
-# Destroy the ATcpClient instance
NOTES:
In each of these actions performed a new task ID is generated and cXid holds the current
task ID which is returned by the operations in the typical scenarios. The cXid is being 
passed in returnMsg() calls since it is used to index the ATcpTasks. The cXid which is 
a long serves as a key to the ATcpTasks QHash.
 */
class ATcpClient : public QObject
{
    Q_OBJECT
public:
    ATcpClient(long iProtocolId);
    ~ATcpClient(void);
    ATcpTaskId    closeConnection();
    ATcpTask      getReturnResult(ATcpTaskId iTcpTaskId, bool *ipOk);
    ATcpTaskId    isConnected();
    void          makeHttpRequestMsg(const QString& irResPath, const QString& irBody, const QString& irHost);
    ATcpTaskId    openConnection(const QByteArray& irServerAddress, ushort iPort);
    QString       requestToUrl(QString& irStg, char iSep);
    void          returnMsg(AXID iXid, const QString& irAisOut);
    ATcpTaskId    submit(const QString& irResPath, const QString& irBody);
    void          updateCookie(QByteArray iCookie);
    QString       urlEncode(QString& irStg, bool iUsePlus);
signals:
    void          toOpenConnection(ATcpTask* ipTaskInfo);
    void          toIsConnected(ATcpTask* ipTaskInfo);
    void          toSubmit(const QString& irResPath, const QString& irBody, ATcpTask* ipTaskInfo);
    void          toCloseConnection(ATcpTask* ipTaskInfo);
private slots:
	void          onConnected();
    void          onDisconnected();
    AXID          onCloseConnection(ATcpTask* ipTaskInfo);
    AXID          onIsConnected(ATcpTask* ipTaskInfo);
    AXID          onOpenConnection(ATcpTask* ipTaskInfo);
    void          onResponse(QByteArray iBody, QByteArray iCookie);
    void          onSocketError();
    AXID          onSubmit(const QString& irResPath, const QString& irBody, ATcpTask* ipTaskInfo);
private:
    ATcpTaskId	       getNextTcpTaskId(); // Provides a new task ID given by cTcpTaskId  
    static ATcpTaskId  cTcpTaskId;         // Increments for every new task by getNextTcpTaskId()
    QString		       cCookie;	           // Current cookie returned by server.
    QString            cCurRequest;        // Contains HTTP 1.1 header and body or for other uses
    QByteArray	       cHostDnsIp;		   // Ais DNS name or IP address.
    ushort		       cPort;			   // AIS socket number
    long               cProtocolId;        // Protocol (AISSVR_HTTP, ...) 
    ASocket*           cpTcpSocket;		   // Maintains TCP connection with server
    QMutex             cTcpMutex;          // Mutex to synchronize access to shared variables.
    ATcpTasks          cTcpTasks;          // tasks setup in every on<Action>() slot
    long		       cXid;			   // Client-generated request ID
};

#endif //ATCPCLIENT_H
