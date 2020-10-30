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

#ifndef ASOCKET_H
#define ASOCKET_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/asocket.h
														AIS Socket

CHANGE HISTORY
Version	Date		Who		Change
1.0120	12/26/2006	tlw		onRead. Recast into onReadApp, onReadHttp, onReadXml
1.0118	12/12/2006	tlw		submit. Add serial binary stream argument.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0104	 9/8/2006	tlw		Revise to support 3 protocols and use signal/slot interface
1.0057	 3/18/2005	tlw		Revise doSocketRead to allow multiple and partial responses.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QThread>
#include <QtNetwork/QTcpSocket>
#include "ais.h"				// AReqType, AReturnRcvr

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define ASOCKET_SIZE		   11		/*!< Field width of a size in the APP prefix (10 chars for size + DEL separator) */

//	------------------------------------------------------ CLASSES ------------------------------------------------------------
/*!
 * \brief Establishes a persistent TCP connection between an AIS client and a server.
 *
 * A remote client creates one instance of this class for every connection made to an AIS server. An in-process client will not
 * require an instance of this class.  Each connection supports just one of the supported protocols.  For example, every instance
 * of a remote AppClient will create an AIS connection using the AIS protocol. A call to submit causes ASocket to send a request
 * to the server.  When the server responds, readyRead intercepts the transmission. A transmission may contain a partial message,
 * a single messages, or multiple messages. Each message is extracted for the specified protocol and returned via a signal to the
 * caller.
 *
 * \par Thread
 * ASocket inherits from QTcpSocket which is a QObject that is created on a thread.  So, each instance of this class runs on the
 * thread of the caller. In AIS every thread has its own event loop which supports direct and indirect signal-slot connections.
 * ASocket implements a persistent, non-blocking TCP/IP socket connection to the server.   The thread is started by a call to
 * openConnection which calls QTcpSocket's connectToHost.
 *
 * \par Returned Messages
 * Partial transmissions are buffered.   Each message in a transmission containing multiple messages is returned back to the
 * caller via a separate signal.  It is up to the caller to unwrap the message.  The way that messages are terminated is dependent
 * upon the protocol.
 *
 * \par Protocols
 * The protocol IDs are defined in aglobals.h.  The ones used here are:
 * -# AISSVR_APP	- App (AIS IDE) protocol
 * -# AISSVR_HTTP - HTTP protocol
 * -# AISSVR_XML	- XML (Macromedia Flash) protocol
 *
 * \par Termination
 * The termination of a message is dependent upon the protocol.  The options are:
 * -# AISSVR_APP. Each message starts with a length + DEL and the message is terminated by ^A.  The length is the length of the message
 * not including the length + DEL prefix and the ^A on the end.
 * -# AISSVR_HTTP. The header is terminated by a blank line.  If the header is followed by a body, the length of the body is
 * contained in the header.
 * -# AISSVR_XML. Each XML document is terminated by a null or ^A.
 *
 * \par Disconnect
 * If the TCP connection to the server becomes disconnected, it notifies the caller by emitting a disconnected signal.  A
 * disconnected signal can be generated if the socket becomes disconnected from the server due to a connection failure or a server
 * shutdown.  It can also be generated if the connecton to this socket is closed by a call to closeConnection.
 *
 * \par Destructor
 * If the caller delete's an instance of ASocket, the ASocket destructor is called. The destructor closes QTcpSocket which
 * causes a disconnected signal.  See Disconnect discussion above for the rest of the story.
 *
 * \par Timeout
 * Since the response to a request can take an indeterminate amount of time, no timeout provision is provided except for the
 * connectToHost request.
 *
 * \par Scenario
 * A typical sequence of events when an instance of AppClient creates an instance of ASocket:
 * -# The ASocket constructor is called. It initializes its class variables and connects up the QTcp signals to local slots.
 * -# OpenConnection is called.  It establishes a connection to the server.
 * -# When the connection is made (or if it fails), a response signal is generated by connected, onError, or onTimeout.
 * -# Submit is called with a request. Requests submitted by the caller are forwarded to the server.
 * -# The server returns messages and results back via a readyRead signal.  A return from the server may contain a partial
 * message, zero or more complete messages followed by zero or one partial message.  Complete messages are assembled and returned
 * via a response signal. Partial messages are saved pending further transmissions.
 * -# CloseConnection is called by the parent or the server becomes disconnected.  If the socket is not already closed,
 * closeConnection closes the socket.  When the socket is closed, the disconnected signal may be used to notify the caller.
 *
 */
class ASocket : public QTcpSocket
{
	Q_OBJECT
public:
	// Opens connection using specified protocol for local in-process
	ASocket(QObject* ipParent, const char* ipServerName, long iProtocolId);
	bool		closeConnection();
	bool		isConnected();
	bool		openConnection(const QString& irHostDnsIp, ushort iPort);
	void		submit(const QString& irRequest, char* ipData, long iDataLgth);

signals:
	void		response(QByteArray iResponse, QByteArray iCookie, char* ipData);
	void		socketError(long iStatus, QString iError);

private slots:
	void		onError(QAbstractSocket::SocketError iErrCode);
	void		onReadApp();
	void		onReadHttp();
	void		onReadXml();

private:
	bool		isValidProtocol(const char* ipBeg);

	char*		cpDataBfr;			/*!< -> binary data buffer holding serialized object */
	long		cDataLgth;			/*!< Number of data bytes received so far */
	long		cDataSize;			/*!< Number of bytes of binary data expected */
	long		cProtocolId;		/*!< Protocol expected on this connection (AISSVR_APP, AISSVR_HTTP, AISSVR_XML) */
	QByteArray	cRspBfr;			/*!< Buffer to hold partial responses received from the server */
	char*		cpRspBfr;			/*!< -> to beginning of cRspBfr */
	long		cRspLgth;			/*!< Number of response bytes received so far */
	long		cRspSize;			/*!< Number of response bytes expected */
	QByteArray	cSizeBfr;			/*!< Place to put a size field */
	char*		cpSizeBfr;			/*!< -> to beginning of cSizeBfr */
};

#endif // ASOCKET_H
