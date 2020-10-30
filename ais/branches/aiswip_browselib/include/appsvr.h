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

#ifndef APPSVR_H
#define APPSVR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/appsvr.h
													AAppSvr Specification

CHANGE HISTORY
Version	Date		Who		Change
4.0003   7/01/2008	fchua	[CR-118] Added function isInProcess to check if a connection is In-Process.
        10/16/2007  fchua   Updated Doxygen documentation.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		onSubmit, submit. Add serial binary stream argument.
1.0112	10/27/2006	tlw		AppSvr. Convert cConnectionMap, cSocketMap to QHash
1.0056	 3/10/2005	tlw		Update documentation.
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtNetwork/QTcpSocket>

#include "aglobals.h"
#include "alogmgr.h"
#include "aissvr.h"				// AAisSvr

class QTcpServer;
class AAisMgr;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define APPCLIENT_RCVBFRSIZE	4096
#define ASIZE					11

//	-------------------------------------------------- DATA STRUCTURES --------------------------------------------------------
/*!
 * \brief APP Socket Information structure.
 */
typedef struct
{	AStringIntMap	mContextMap;	/*!< Key: context name, Value: connectId. */
	long			mCurConnectId;	/*!< Last-used connectId. Use this connectId if no context name. */
	char*			mpDataBfr;		/*!< -> binary data buffer holding serialized object. */
	long			mDataLgth;		/*!< Number of data bytes received so far. */
	long			mDataSize;		/*!< Number of bytes of binary data expected. */
	AReturnRcvr*	mpInProcClient;	/*!< -> In-proc client iff in-process client. */
	QByteArray		mRcvBfr;		/*!< Buffer to hold partial requests received from the client. */
	long			mRcvLgth;		/*!< Number of bytes in mRcvBfr so far. */
	long			mRcvSize;		/*!< Size of mRcvBfr (number of request bytes expected). */
	ALongKeyMap		mReqMap;		/*!< Key: aRqId, Value: aRqId. */
	long			mRqId;			/*!< Request ID for a pending request. */
	QTcpSocket*		mpSocket;		/*!< -> client for returning async messages. */
} AAppSocketSt;

/*!
 * \brief AAppSocketSt collection.
 */
typedef QList<AAppSocketSt*> AAppSockets;		// Array indexed by SocketId

/*!
 * \brief Connection Id to Socket Id association.
 */
typedef QHash<long, long> AAppConnectMap;		// Key: ConnectId, value: SocketId

/*!
 * \brief QTcpSocket pointer to Socket Id association.
 */
typedef QHash<QTcpSocket*, long> AAppSocketMap;	// Key: apSocket, value: SocketId.
//	------------------------------------------------- CLASS DEFINITION --------------------------------------------------------

/*!
 * \brief Processes requests from remote AAppClient applications running the AIS IDE.
 *
 */
class AAppSvr : public QObject, public AReturnRcvr
{
	Q_OBJECT
public:
	AAppSvr(ushort iPort, const QString& irContextName, QObject* ipParent=0);
	~AAppSvr();
	// AReturnRcvr Methods:
	virtual bool	connectionClosed(long iConnectId);
	virtual void	returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString&
					irAisOut, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError
					, const QString iClientData = QString());
	virtual void	setContextName(long iConnectId, const QString& irContextName);

	// Public methods:
	void			submit(long iRqId, const QString& irAmpmsg, AReturnRcvr* ipInProcClient, QTcpSocket* ipSocket, char* ipData);
	bool			isInProcess(long iConnectId);

private slots:
	void			onConnected();
	void			onConnectionClosed();
	void			onDisconnected();
	void			onError(QAbstractSocket::SocketError iCode);
	void			onNewConnection();
	void			onSubmit();

private:
	void			closeSocket(long iSocketId);
	long			openSocket(long iConnectId, const QString& irContextName, AReturnRcvr* ipInProcClient, QTcpSocket* ipSocket);
	long			returnFile(long iConnectId,long iRqId,const QString& irFileSpec,AReturnRcvr* ipInProcClient,QTcpSocket* ipSocket);
	void			retOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irAisOut
					, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, AReturnRcvr* ipInProcClient
					, QTcpSocket* ipSocket);

	AAppConnectMap	cConnectionMap;		/*!< Map from ConnectId to SocketId. */
	QString			cDefaultContextName;/*!< Default context name (used by submit). */
	QString			cMt;				/*!< Empty string for arg placeholder. */
	AAppSockets		cOpenSockets;		/*!< Array of AppSocketSt structures indexed by SocketId. */
	ushort			cPort;				/*!< AAppSvr listens on this socket. 0 if disabled. */
	AAppSocketMap	cSocketMap;			/*!< Key apSocket, value: SocketId. */
	QTcpServer*		cpAppServer;		/*!< Listens on the App socket for incoming requests. */
};

#endif //APPSVR_H
