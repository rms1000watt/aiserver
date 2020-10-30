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
aisdev/appclient/asocket.cpp
															AIS Socket

CHANGE HISTORY
Version	Date		Who		Change
2.0001	12/31/2006	tlw		onReadApp. Remove terminator from end of binary data.
2.0001	12/30/2006	tlw		onRead. Change onRead to onReadXml in XML connection.
1.0120	12/26/2006	tlw		onRead. Recast into onReadApp, onReadHttp, onReadXml
1.0118	12/12/2006	tlw		submit. Add serial binary stream argument.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0104	9/4/2006	tlw		Revise to support 3 protocols and use signal/slot interface
												--------------- ---------------
DOCUMENTATION
See include/asocket.h for specifications.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "asocket.h"			// ASocket
#include "aglobals.h"			// AERR_
#include "autilities.h"			// AUtil

//	-------------------------------------------------- DEFINED CONSTANTS ------------------------------------------------------

//	--------------------------------------------------- PUBLIC METHODS --------------------------------------------------------
/*!
 * \brief Constructor initializes the class variables and connects up the QTcpSocket signals to private slots.
 * \param[in] ipParent -> Parent that instantiated this class
 * \param[in] ipServerName -> Local name assigned to the server catching the messages on this socket.
 * \param[in] iProtocolId Specifies the protocol for this socket (AISSVR_APP, AISSVR_HTTP, AISSVR_XML).
 * \note
 * Create one instance of ASocket for each remote connection to an AIS server.
 */
ASocket::ASocket(QObject* ipParent, const char* ipServerName, long iProtocolId)
	:	QTcpSocket(ipParent), cProtocolId(iProtocolId)
{
	// Initialize. Set class variables.
	cpDataBfr = NULL;
	cDataLgth = 0;
	cDataSize = 0;
	cpRspBfr = NULL;
	cRspLgth = 0;
	cRspSize = 0;
	setObjectName(ipServerName);
	cSizeBfr.resize(ASOCKET_SIZE);
	cpSizeBfr = cSizeBfr.data();

	// Connections. Connect QTcpSocket signals to ASocket slots
	QObject::connect(this, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError(QAbstractSocket::SocketError)),
	Qt::DirectConnection);

	switch (cProtocolId)
	{
	case AISSVR_APP:
		QObject::connect(this, SIGNAL(readyRead()), this, SLOT(onReadApp()), Qt::DirectConnection);
		break;
	case AISSVR_HTTP:
		QObject::connect(this, SIGNAL(readyRead()), this, SLOT(onReadHttp()), Qt::DirectConnection);
		break;
	case AISSVR_XML:
		QObject::connect(this, SIGNAL(readyRead()), this, SLOT(onReadXml()), Qt::DirectConnection);
		break;
	default:
		qDebug("ASocket(). No protocol set for this connection.");
		break;
	}
}

/*!
 * \brief Closes the current connection which triggers a TCP disconnected notification to the server.
 * \return true
 */
bool ASocket::closeConnection()
{
	cRspBfr.clear();
	close();
	return true;
}

/*!
 * \brief Returns true if the socket is still connected to the server.
 * \return true iff the socket is still connected to the server.
 */
bool ASocket::isConnected()
{
	return (state() == QAbstractSocket::ConnectedState);
}

/*!
 * \brief Detects incoming messages with the incorrect protocol.
 * \param[in] ipBeg -> beginning of the incoming message.
 * \return true iff the message is of the correct format.
 */
bool ASocket::isValidProtocol(const char* ipBeg)
{
	bool aIsValid = false;
	switch (cProtocolId)
	{
	case AISSVR_APP:
		for (; *ipBeg >= '0' && *ipBeg <= '9'; ++ipBeg)
		 aIsValid = true;
		if (aIsValid && *ipBeg != '\177')
			aIsValid = false;
		break;
	case AISSVR_HTTP:
		aIsValid = (*ipBeg == 'H' && *(ipBeg+1) == 'T' && *(ipBeg+2) == 'T' && *(ipBeg+3) == 'P' && *(ipBeg+4) == '/');
		break;
	case AISSVR_XML:
		aIsValid = *ipBeg == '<' && *(ipBeg+1) == 'a' && *(ipBeg+2) == 'm' && *(ipBeg+3) == 'p';
		break;
	}
	return aIsValid;
}

/*!
 * \brief TCP/IP reports an error in performing an operation.
 * \param[in] iCode QTcpSocket error code.
 * \note
 * Client receives an error message in response to outstanding request.
 * Emits a socketError() signal.
 */
void ASocket::onError(QAbstractSocket::SocketError iCode)
{
	// Expect client status to be Opening, Closing, Responding
	QString aError;

	switch (iCode)
	{
	case QAbstractSocket::ConnectionRefusedError:
		aError = "Host not available";
		break;
	case QAbstractSocket::HostNotFoundError:
		aError = "Host not found";
		break;
	case QAbstractSocket::SocketAccessError:
		aError = "Unable to read from socket";
	default:
		aError = "Socket error.";
		break;
	}
	emit socketError(AERR_TCPIPERROR, aError);
}

/*!
 * \brief Unwrap returned transmission from server. Return complete messages via the response signal.
 * \note
 * 1. For AISSVR_APP protocol: ReqSize|DataSize|RqId|ReqType|Status|RetValue|Display|Error|Out\01BinaryData\0
 * 2. ReqSize is the size of the response which extends from RqId up to (but not including) the \01 terminator.
 * 3. DataLgth is the size of the binary data (not including null on end). DataLgth may be 0.
 * 4. ReqSize and DataSize are '0' filled fixed-length fields.
 * 5. onReadApp returns one response back to client for every completed message.
 * 6. Partial messages are saved in cRspBfr and cpDataBfr until next transmission comes in.
 */
void ASocket::onReadApp()
{
	QByteArray aCookie;					// Cookie extracted from the header (not used).
	long aLgth;							// Length of next arg in the message including separator, if any.
	long aNeed;							// Number of bytes needed to fill a buffer.

	// Next. Process next response.
	while (bytesAvailable() > 0)
	{	// Size.  Get the response size and binary buffer size from the prefix.
		if (cRspSize == 0)
		{	// Resync.  When getting garbage, eat first couple chars if they are not digits.
			peek(cpSizeBfr, 2);
			if (*cpSizeBfr < '0' || *cpSizeBfr > '9')
			{	aLgth = 1;
				if (*(cpSizeBfr + 1) < '0' || *(cpSizeBfr + 1) > '9')
				++aLgth;
				read(cpSizeBfr, aLgth);
			}

			if (read(cpSizeBfr, ASOCKET_SIZE) == ASOCKET_SIZE && *(cpSizeBfr + ASOCKET_SIZE - 1) == '\177'	&& (cRspSize
			= AUtil::toLong(cpSizeBfr, &aLgth)) > 0 && aLgth == ASOCKET_SIZE)
			{	if (read(cpSizeBfr, ASOCKET_SIZE) == ASOCKET_SIZE && *(cpSizeBfr + ASOCKET_SIZE - 1) == '\177' && (cDataSize
				= AUtil::toLong(cpSizeBfr, &aLgth)) >= 0 && aLgth == ASOCKET_SIZE)
					;
				else
				{	qDebug("ASocket::onReadApp, Unable to extract binary data buffer size");
					if ((aLgth = bytesAvailable()) > 0)	// Dump the rest of the message in an attempt to resync.
					{	if (cRspBfr.size() < aLgth)
							cRspBfr.resize(aLgth + 1);
						read(cRspBfr.data(), aLgth);
					}
					cDataSize = 0;
				}
			}
			else
			{		qDebug("ASocket::onReadApp, Unable to extract response message size");
					if ((aLgth = bytesAvailable()) > 0)	// Dump the rest of the message in an attempt to resync.
					{	if (cRspBfr.size() < aLgth)
							cRspBfr.resize(aLgth + 1);
						read(cRspBfr.data(), aLgth);
					}
					cRspSize = 0;
					break;
			}

			// Resize. Make sure the response buffer is large enough.
			if (cRspBfr.size() != (cRspSize + 1))
			{	cRspBfr.clear();
				cRspBfr.resize(cRspSize + 1);	// Allow for terminator on end
				cpRspBfr = cRspBfr.data();
			}
		}
		// Response. Fetch more bytes if response buffer not full.
		if ((aNeed = cRspSize - cRspLgth) > 0)
		{	// Read. Read number of bytes needed to fill response buffer.
			if ((aLgth = read(cpRspBfr + cRspLgth, aNeed + 1)) == aNeed + 1)
			{	// Terminator. Check terminator on end.
				cRspLgth = cRspSize;
				if (*(cpRspBfr + cRspLgth) > '\01')
					qDebug("ASocket::onReadApp(), Encountered unterminated response in message");
				*(cpRspBfr + cRspLgth) = '\0';
			}
			else	// Quit with partial response in cRcvBfr.
			{	cRspLgth += aLgth;
				break;
			}
		}
		// Binary Data. Fetch more bytes if binary data buffer is not full.
		if ((aNeed = cDataSize - cDataLgth) > 0)
		{	// Allocate.  Allocate a binary data buffer big enough to hold expected data.
			if (cpDataBfr == NULL)
				cpDataBfr = (char*)malloc(cDataSize);

			// Read. Read the binary data available from the socket.
			if ((aLgth = read(cpDataBfr + cDataLgth, aNeed)) < aNeed)
			{	cDataLgth += aLgth;
				break;
			}
		}
		// Got it. Return response to client.
		emit response(cRspBfr, aCookie, cpDataBfr);
		cRspSize = cRspLgth = 0;
		cDataSize = cDataLgth = 0;
		cpDataBfr = NULL;
	}
}

/*!
 * \brief Unwrap returned transmission from server. Return complete messages via the response signal.
 * \note
 * 1. For AISSVR_HTTP protcol  Header\r\nBody
 * 2. Returns one response back to client for every completed message.
 * 3. Partial messages are saved in cRspBfr until next transmission comes in.
 */
void ASocket::onReadHttp()
{
	// Append received transmission to cRspBfr.
	char aC;							// Next char in rcv buffer
	QByteArray aCookie;					// Cookie extracted from the header, if any.
	cRspBfr += readAll();
	const char*apB,*apBeg,*apBfr,*apEnd;// -> into cRspBfr;
	long aLgth = cRspBfr.length();		// Total received transmission length.
	long aMsgLgth = 0;					// Length of the body of the request (included in the header).
	apBeg = apBfr = cRspBfr.data();		// -> beginning of next message.
	apEnd = apBfr + aLgth;				// -> just past last received char

	// Next Message. Start with apBeg -> to the beginning of the next message in cRspBfr, apEnd -> past last char in cRspBfr.
	// Extract next message. This task is tricky in that a msg may span multiple transmissions or a single transmission
	// may contain multiple msgs.
	while (apBeg < apEnd)
	{	// Resync. Skip leading whitespace
		for (; (aC = *apBeg) == '\r' || aC == '\n' || aC == ' ' || aC == '\t'; ++apBeg)
			;
		// Protocol. Check incoming protocol against expected protocol.
		if (!isValidProtocol(apBeg))
		{	emit socketError(AERR_UNKPROTOCOL, apBeg);		// Unexpected protocol on this port
			cRspBfr.clear();
			break;
		}
		// Header.  Extract Cookie into aCookie and content-length into aMsgLgth. Quit on a blank line.
		aLgth = 0;						// Length of the post body if any.
		apB = apBeg;
		do
		{	// Scan past next newline
			while (apB < apEnd && *apB++ != '\n')
				;
			// Set-Cookie. Set-Cookie: abaseid=GMJBLCEBEBOAHNHMKIGLPIJL
			if (*apB == 'S' && *(apB+1) == 'e' && *(apB+2) == 't' && *(apB+3) == '-' && *(apB+4) == 'C' && *(apB+5) == 'o'
			&& *(apB+6) == 'o' && *(apB+7) == 'k' && *(apB+8) == 'i' && *(apB+9) == 'e')
			{	while (apB < apEnd && *apB++ != ':')
					;
				if (apB < apEnd)
				{	const char *apEol;
					if (*apB == ' ')
						++apB;
					for (apEol = apB; apEol < apEnd && *apEol != '\r' && *apEol != '\n'; ++apEol)
						;
					aCookie = QByteArray(apB, apEol - apB);
				}
			}
			// Content-Length . Extract the length of the post body.
			else if (*apB == 'C' && *(apB+1) == 'o' && *(apB+2) == 'n' && *(apB+3) == 't' && *(apB+4) == 'e' && *(apB+5) == 'n'
			&& *(apB+6) == 't' && *(apB+7) == '-' && *(apB+8) == 'L' && *(apB+9) == 'e' && *(apB+10) == 'n' && *(apB+11) == 'g'
			&& *(apB+12) == 't' && *(apB+13) == 'h')
			{	while ((apB < apEnd) && (*apB < '0' || *apB > '9'))
					++apB;
				AUtil::getArgValue(apB, &aMsgLgth);
			}
			else if (*apB == '\r') ++apB;	// Skip over carriage return if any.
		} while (apB < apEnd && *apB++ != '\n'); // Stop on a blank line.
		
		// 	Partial. Save partial request. apBeg->Beg of this message, apB->Beg of body of request 
		if (apB + aMsgLgth > apEnd)
		{	// If apBeg has been moved past the beginning of the buffer, just keep the partial.
			if (apBeg > apBfr)
				cRspBfr = cRspBfr.mid(apBeg - apBfr);
			apBeg = apB + aMsgLgth;
			goto lQuit;
		}		
		// Return response. apBeg -> Beg of message + aArgLgth
		apBeg = apB;					// Move apBeg to beginning of the body of request
		emit response(cRspBfr.mid(apBeg - apBfr, aMsgLgth), aCookie, NULL/*pData*/);

		// Move on. Move apBeg -> end of message
		apBeg += aMsgLgth;
	}	// end loop
	// Truncate. If no partial message, clear cRspBfr.
	cRspBfr.clear();
lQuit:
	return;
}

/*!
 * \brief Unwrap returned transmission from server. Return complete messages via the response signal.
 * \note
 * 1. For AISSVR_XML protocol: XmlDoc\01
 * 2. XmlDoc begins with <amp
 * 3. Returns one response back to client for every completed message.
 * 4. Partial messages are saved in cRspBfr until next transmission comes in.
 */
void ASocket::onReadXml()
{
	// Append received transmission to cRspBfr.
	char aC;							// Next char in rcv buffer
	QByteArray aCookie;					// Cookie extracted from the header (not used).
	cRspBfr += readAll();
	char* apB, *apBeg, *apBfr, *apEnd;	// -> into cRspBfr;
	long aLgth = cRspBfr.length();		// Total received transmission length.
	apBeg = apBfr = cRspBfr.data();		// -> beginning of next message.
	apEnd = apBfr + aLgth;				// -> just past last received char

	// Next Message. Start with apBeg -> to the beginning of the next message in cRspBfr, apEnd -> past last char in cRspBfr.
	// Extract next message. This task is tricky in that a msg may span multiple transmissions or a single transmission
	// may contain multiple msgs.  Each msg is terminated by a null or ^A (\01).
	while (apBeg < apEnd)
	{	// Resync. Skip leading whitespace
		for (; (aC = *apBeg) == '\r' || aC == '\n' || aC == ' ' || aC == '\t'; ++apBeg)
			;
		// Protocol. Check incoming protocol against expected protocol.
		if (!isValidProtocol(apBeg))
		{	emit socketError(AERR_UNKPROTOCOL, apBeg);		// Unexpected protocol on this port
			break;
		}
		// Msg Length. Scan message for next terminator (ugh!), a grim task imposed on us by the protocol.
		for (apB = apBeg; apB < apEnd && *apB > '\01'; ++apB)
			;
			
		// 	Partial. Save partial request. apBeg->Beg of next response, apB->end of this response. 
		if (*apB > '\01')
		{	// If apBeg has been moved past the beginning of the buffer, just keep the partial.
			if (apBeg > apBfr)
				cRspBfr = cRspBfr.mid(apBeg - apBfr);
			goto lQuit;
		}
		// Termination. Null terminate this response.
		*apB = '\0';

		// Return response. apBeg -> Beg of message + aArgLgth, apB -> end of message
		emit response(cRspBfr.mid(apBeg - apBfr, apB - apBeg), aCookie, NULL);

		//	Done.  Move apBeg -> beginning of next message, if any
		apBeg = apB + 1;		// Move by previous terminator.
	}
	// Truncate. If no partial message, clear cRspBfr.
	cRspBfr.clear();
lQuit:
	return;			// Some compilers need this.
}

/*!
 * \brief Establish a connection to the server.
 *
 * If a connection already exists, this routine expects the socket to be connected to the same server as the one specified by its
 * arguments.  If this is not the case, be sure to disconnect this socket from the old server before connecting it to a different
 * server.
 *
 * \param[in] irHostDnsIp Host DNS name (URL) or IP address.
 * \param[in] iPort The port (socket) that the host listens on for incoming requests.
 * \return Returns false iff already connected or if input is not valid. 
 * \note
 * -# A client may wish to test a URL to make sure that it connects to a remote server.
 * -# HostDnsIP is a DNS host name (server1.investbyLambda.com) or an IP address
 * -# Returns are asynchronous. That is, a status is returned to caller. The response to request is returned later via
 * returnOutput when a connection is established.  An error response is returned if the connection fails.
 */
bool ASocket::openConnection(const QString& irHostDnsIp, ushort iPort)
{
	// State. Don't connect if already connected, or request is already pending.
	bool aIsConnecting = false;
	if (!irHostDnsIp.isEmpty() && iPort > 0 && state() == QAbstractSocket::UnconnectedState)
	{	connectToHost(irHostDnsIp, iPort);
		aIsConnecting = true;
	}
	return aIsConnecting;
}

/*!
 * \brief Submit a request generated by an APP client using the AIS protocol
 *
 * \param[in] irRequest Request in current protocol format.
 * \param[in] ipData Binary data stream to be sent to the remote server.
 * \param[in] iDataLgth Length of binary data stream.
 * \return void
 * \note
 * -# Returns are asynchronous. That is, the response is returned via a response signal or via a socketError signal if
 * the transmission fails.
 */
void ASocket::submit(const QString& irRequest, char* ipData, long iDataLgth)
{
	QByteArray aBody(irRequest.toLatin1());
	long aLgth = aBody.length();
	switch (cProtocolId)
	{
	case AISSVR_APP:		// App (AIS IDE) protocol
		aBody = QByteArray::number((int)aLgth).rightJustified(ASOCKET_SIZE-1, '0') + '\177'
		+ QByteArray::number((int)iDataLgth).rightJustified(ASOCKET_SIZE-1, '0') + '\177' + aBody + '\01';
		write(aBody.data(), aBody.length());
		if (ipData != NULL)
			write(ipData, iDataLgth);
		break;
	case AISSVR_XML:		// XML (Macromedia Flash) protocol
		aBody += '\01', ++aLgth;
		// fall thru
	case AISSVR_HTTP:		// HTTP protocol
		write(aBody.data(), aLgth);
		if (ipData != NULL)
			qDebug("ASocket::submit(Request:%s), Data dropped.", irRequest.toAscii().data());
		break;
	}
	if (ipData != NULL)
		free(ipData);
}
// end
