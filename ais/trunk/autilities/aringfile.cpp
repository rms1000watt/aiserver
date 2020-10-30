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
aisdev/autilities/aringfile.cpp
															Ring File

Ring file is an efficient implementation of large persistent queues

CHANGE HISTORY
Version	Date		Who		Change
1.0115	11/19/2006	tlw		read. Close file after open.
1.0113	11/13/2006	tlw		Simplify constructor and destructor code.
1.0067	7/29/2005	tlw		Convert embedded documentation.
1.0066	7/ 7/2005	tlw		Rename write to append.  Use sprintf to append form feed to end of string.
1.0061	5/ 8/2005	tlw		Add clear log option to constructor. Implement record-oriented writes
1.0060	4/26/2005	tlw		Add marker in file to note position of last write. Revamp code to allow terminating char.
1.0058	3/28/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aerrdefs.h"
#include "aringfile.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief ARingFile constructor configures an instance ARingFile.

It initializes all class variables and it initializes the log from persistent store.
\param iClear If true, clear the existing log; else, read the log file \a irFilePath.
\param iPersistent If true, write messages to a file; else, keep messages in cBfr until flush or when this object is destructed.
\param iRecord If true, keep a record-oriented log (see append below)
\param iSize Maximum size of the log [bytes].  If 0, the size of the log is unlimited.
\param irFilePath Path + name of log file. Even in-memory logs save cBfr to disk from time-to-time for server-crash recovery
\return void
\sa
- QFile. For file I/O. ARingFile inherits QFile.
- ASessionMgr::saveConsoleLog() and ASessionMgr::getConsoleLog() uses record-oriented logging.
\par Notes:
 -# Records.  If writing a record, the input must start with V\%d| where V is a vertical tab,\%d is the length of the
    record [bytes] followed by a vertical bar.  The length (\%d) includes all the bytes in the record not including the
    initial V. See appendRecord() for more details.
 -# In-memory. If not a persistent log, append saves messages in cBfr. In this case, cBfr is initialized from the log file
    specified by \a irFilePath. cBfr is written to a file by flush() or when this object is destroyed.
\par Examples:
 -# ARingFile::selfTest - A wide variety of examples with expected results.
 -# ASessionMgr::openConsoleLog, ASessionMgr::saveConsoleLog - Record-oriented, in-memory log to save console output
 -# ALogMgr::sendMsg - In-memory, limited-size logs (not record-oriented).
*/
ARingFile::ARingFile(bool iClear, bool iPersistent, bool iRecord, long iSize, const QString& irFilePath)
	:
	QFile(irFilePath), cFilePath(irFilePath), cPersistent(iPersistent), cRecord(iRecord), cSize(iSize)
{
	// Record, Record-oriented log. Size, max. size of log. Persistent, write each msg to file; else, save in cBfr
	if (!cPersistent && cSize > 0)
		cBfr.resize(cSize + 2);			// text + RINGFILE_TERM + null

	// Clear File, If iClear, clear the existing log.
	cBof = cEof = 0;
	if (iClear)
	{	if (open(QIODevice::Truncate | QIODevice::WriteOnly))	// 8 | 2
			close();
	}
	// Truncate. Read first cSize + 1 bytes from the file.
	else if (open(QIODevice::ReadOnly))		// 1
	{	long aPos = -1, aSize;				// Position of terminator in file, number of bytes in file
		if ((aSize = size()) > 0)
		{	if (aSize > cBfr.size())
				cBfr.resize(aSize + 2);
			char *apBfr = cBfr.data();
			if (cSize > 0 && aSize > cSize + 1) aSize = cSize + 1;
			if (QFile::read(apBfr, aSize) > 0)
			{	// Terminate. If no terminator, add one on the end
				if ((aPos = cBfr.indexOf(RINGFILE_TERM)) < 0)
				{	if (aSize > cSize)	aSize = cSize;	// Limit length to cSize bytes
					char *apEnd = apBfr + (aPos = aSize);
					*apEnd = RINGFILE_TERM;
					*(apEnd + 1) = '\0';
				}
				// Reset. Reset cBof and cEof to end of last write.
				cBof = cEof = aPos;
			}
			close();

			// Refresh.  Rewrite truncated and terminated version of file.
			if (cPersistent)
			{	if (open(QIODevice::WriteOnly))					// 2
				{	write(apBfr, aSize + 1);
					close();
				}
			}
			// Delete. If log is not persistent, truncate file.
			else if (open(QIODevice::Truncate | QIODevice::WriteOnly))	// 8 | 2
				close();
		}
		else
			close();
	}
}

/*!
ARingFile destructor. Save the ring file with the oldest message at the top.
\return void
\sa flush()
\par Notes:
-# Both persistent and non-persistent logs are saved to a file on destruction.
*/
ARingFile::~ARingFile()
{
	/* TESTING ... Remove to see what happens.
	if (cPersistent)
		read(cBfr, false/ *clear* /);
	flush();
	*/
#ifdef AIS_DEBUG
	qDebug("%s", "~~~~ARingFile()");
#endif
}

/*!
append - Append a string or a record to the log starting at the end of the last write.
\param ipData	Ptr to beg of a buffer containing the record to be added to the ring file
\param iLgth	Number of bytes to be written.
\return	aOk		true iff seek and write succeed.
\par Notes:
 -# If writing a record, the input (\a ipData) must start with V\%d| where V is a vertical tab,\%d is the length of the
	record [bytes] followed by a vertical bar.  Length (\%d) includes all the bytes in the record not including the initial V.
	See appendRecord for another option.
 -#	The append starts at the top of the log if \a iLgth exceeds the room available from cBof to max allowed size.  This rule
	improves performance by making it possible to perform a single write (at the cost of wasting some space).
 -#	The input \a ipData must include a terminating char on end (usually a null).
 -# If cPersistent, write to file; else, save input in cBfr.
 -# cBof is reset to the offset in the file to RINGFILE_TERM char at end of last write..
 -# cEof is moved to just past the end of the last character in the log or to RINGFILE_TERM if last write on end.
 -# cPersistent, cRecord and cSize are not modified.
*/
bool ARingFile::append(const char* ipData, long iLgth)
{
	// Truncate input. If message exceeds cSize chars, just save last cSize input characters
	bool aOk = false;
	if (cSize > 0 && iLgth >= cSize)
	{	cBof = cEof = 0;
		ipData += iLgth - cSize;
		iLgth = cSize;
	}
	if (iLgth > 0)
	{	// Offset. If not enough room left from cBof to cSize in the log, start from the beginning of the log.
		long aPos = (cSize > 0 && cBof + iLgth > cSize) ? 0 : cBof;
		// Persistent. Write data directly to the file.
		if (cPersistent)
		{	if (open(QIODevice::ReadWrite))		// 2 | 1
			{	// Seek. Move file offset to terminating char of last write.
				if (aPos == 0 || seek(aPos))
				{	// Write. Write iLgth bytes starting at aPos. Tack on RINGFILE_TERM at end.
					write(ipData, iLgth);
					putChar(RINGFILE_TERM);

					// Update. Move cBof to RINGFILE_TERM at end of write. Advance cEof if cBof > cEof.
					if (cEof < (cBof = aPos + iLgth))
						cEof = cBof;
					aOk = true;
				}
				close();
			}
		}
		else	// Save msg in cBfr
		{	// Expand.  If size is unlimited, expand the buffer as needed.
			if (cSize == 0 && cBfr.size() < aPos + iLgth + 2)
				cBfr.resize(aPos + iLgth + 2);

			// Save. Add iLgth bytes starting at aPos. Tack on RINGFILE_TERM to end to be consistent with persistent image.
			char *apBfr = cBfr.data();
			char *apBeg	= apBfr + aPos;		// -> RINGFILE_TERM at end of last write
			memmove(apBeg, ipData, iLgth);
			*(apBeg += iLgth) = RINGFILE_TERM;

			// Update. Move cBof to end of write. Advance cEof if cBof > cEof
			if (cEof < (cBof = aPos + iLgth))
			{	cEof = cBof;
				*(apBfr + cEof + 1) = '\0';
			}
			aOk = true;
		}
	}
	return aOk;
}

/*!
appendRecord - Format and append a record to the file starting at the end of the last write.
\param irBody Body of record to be written
\param iRqId Unique ID associated with the originating request (0 if pushed output).
\param iSessionId ID of the session originating this record
\param iType Type of record (R for a returned result, D for display (console output).
\return aOk	Returns true iff seek and write succeed.
\sa	SessionMgr::saveConsoleLog() AUtil::formatConsoleLog()
\par Notes:
-# The body of the record is prepended with a header of the form VLength where V is RINGFILE_SOC (vertical tab).
-# Record format: VLength|Type|RqId|SessionId|Body
- V	RINGFILE_SOC (vertical tab, \\013)
- Length One or more digits representing the length of the record, including Type|RqId|SessionId|Body.
- Type R for a result record, D for a display record.
- RqId ID of the originating request
- SessionId ID of the originating session.
- Body The content of the resulting element in the XML document.
*/
bool ARingFile::appendRecord(const QString& irBody, long iRqId, long iSessionId, char iType)
{
	bool aOk = false;			// Set to true iff write succeeds.
	long aLgth;					// Length of record, no including vertical tab.
	if (!irBody.isEmpty())
	{	QString aBody = QString("%1|%2|%3|%4").arg(iType).arg(iRqId).arg(iSessionId).arg(irBody);
		aLgth = aBody.length();
		QByteArray aRecord = RINGFILE_SOC + QByteArray::number((int)aLgth) + '|' + aBody.toLatin1();
		aOk = append(aRecord.data(), aRecord.length());
	}
	return aOk;
}

/*!
flush - Write buffer to log. Equivalent to close.
\return void
\sa ~ARingFile()
\par Notes:
-# The end of the last write is terminated with a RINGFILE_TERM (form feed).
-# If last append was not at end of the file, write the file with beginning of the message at the top.
-# If a record-oriented file, strip out the gap between last record and the first remaining record.
*/
void ARingFile::flush()
{
	if (cEof > 0 && !cBfr.isEmpty() && open(QIODevice::WriteOnly))	// 2
	{	char *apBfr = cBfr.data();
		// Write head.  Write from just past cBof to the end.
		long aPos = cBof + 1;
		if (cEof > aPos)
			write(apBfr + aPos, cEof - aPos);
		// Write tail. Write from top of Bfr to cBof
		if (cBof > 0)
			write(apBfr, cBof);
		close();
	}
}

/*!
read - Read the contents of the current log starting at the position just past the last write.
\param orDst  Receives the contents of the log.
\param iClear Iff true, truncate the log after read is completed.
\return aTotal Number of bytes read or error code (<0) if read fails
\sa QFile for implementing file read operation.
\par Notes:
-# Expands orDst if it is not big enough to hold the entire log.
-# cBof is offset to RINGFILE_TERM char at end of previous write, cEof is offset to RINGFILE_TERM at end of the log.
-# The offsets cBof and cEof are the same if the ring file has not reached its maximum size.
-# cPersistent, cRecord, and cSize are not modified.
-# cBfr, cBof, and cEof are reset to zero if iClear is true.
-# If reading records and not starting at top, the characters from cBof to the beginning of the first record are discarded.
*/
long ARingFile::read(QByteArray& orDst, bool iClear)
{
	long aTotal = 0;
	if (cEof > 0)
	{	// Expand. Expand destination buffer if necessary
		if (cEof >= orDst.size())
			orDst.resize(cEof + 2);			// Allow for RINGFILE_TERM and null on end.
		char *apSrc, *apDst = orDst.data();

		// Read. If persistent log, read the file into cBfr
		if (cPersistent)
		{	if (open(QIODevice::ReadOnly))	// 1
			{	long aBytesRead;
				if (cBfr.size() < cEof + 2)
					cBfr.resize(cEof + 2);
				char *apBfr = cBfr.data();
				if ((aBytesRead = QFile::read(apBfr, cEof + 1)) <= 0)
				{	close();
					return -AERR_FILEREAD;
				}
				*(apBfr + cEof + 1) = '\0';
			}
			close();
		}
		// Start. Read head from just past cBof up to cEof.
		long aPos = cBof + 1;			// Offset to oldest char in the log
		apSrc = cBfr.data();

		// Skip. Scan to beginning of the first record starting with RINGFILE_SOC (vertical tab)
		if (cRecord)
		{	char *apBeg = apSrc + aPos;
			char *apEnd = apSrc + cEof;
			for (; apBeg < apEnd && *apBeg != RINGFILE_SOC; ++apBeg)
				++aPos;
		}
		// Read head.  Copy from aPos to the end.
		if (cEof > aPos)
		{	long aSz = cEof - aPos;
			memmove(apDst, apSrc + aPos, aSz);
			apDst += aSz;
			aTotal += aSz;
		}
		// Read tail. Read from top of Bfr to cBof
		if (cBof > 0)
		{	memmove(apDst, apSrc, cBof);
			apDst += cBof;
			aTotal += cBof;
		}
		// Null. Tack a null onto end of destination.
		*apDst = '\0';
	}
	if (iClear)
		truncate();
	return aTotal;
}

/*!
selfTest - Tests all the boundary cases.
\return void
\sa ALogMgr()
\par Notes:
-# selfTest also provides several useful examples that employ a ring file.
*/
void ARingFile::selfTest()
{
	// Save. Save and modify settings
	long aMaxSize = cSize;
	bool aRecord = cRecord;
	bool aClear = false;
	QByteArray aSavBfr;
	if (read(aSavBfr, true/*Clear*/) < 0)
		qDebug("ARingFile::selfTest(), Unable to save original file.");

	// Unlimited. Read empty file
	cSize = 0;						// Allow unlimited file sizes
	cRecord = false;				// Not record-oriented
	QByteArray aReadBfr;
	if (read(aReadBfr, aClear) != 0 || !aReadBfr.isEmpty())
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Read empty file fails", cSize);

	// Write to empty file: ABCD\f
	QByteArray aStuff("ABCD");
	long aBfrLgth = aStuff.length();	// Length of string (4)
	if (!append(aStuff.data(), aBfrLgth))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Write to empty file fails", cSize);

	// Append to end of file: ABCDABCD\f
	if (!append(aStuff.data(), aBfrLgth))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Append to file fails", cSize);

	// Read file.
	if (read(aReadBfr, aClear) != 2 * aBfrLgth || QString(aReadBfr) != "ABCDABCD")
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Read from end-of-file fails", cSize);

	// Limited. Read empty file:
	cSize = 6;
	truncate();
	aReadBfr.truncate(0);
	if (read(aReadBfr, aClear) != 0 || !aReadBfr.isEmpty())
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Read empty file fails", cSize);

	// Write to empty file: ABCD\f
	if (!append(aStuff.data(), aBfrLgth))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Write to empty file fails", cSize);

	// Read from end of file
	if (read(aReadBfr, aClear) != aBfrLgth || QString(aReadBfr) != "ABCD")
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Read from end-of-file fails", cSize);

	// Write to file with wrap: ABCD\f
	if (!append(aStuff.data(), aBfrLgth))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Append to file fails", cSize);

	// Read from middle of file
	if (read(aReadBfr, aClear) != aBfrLgth || QString(aReadBfr) != "ABCD")
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Read from middle-of-file fails", cSize);

	// Append to end of the file: ABCD12\f
	QByteArray aTail("12");
	aBfrLgth = aTail.length();		// Length (2)
	if (!append(aTail.data(), aBfrLgth))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Write to end-of-file fails", cSize);

	// Read from top of file
	if (read(aReadBfr, aClear) != cSize || QString(aReadBfr) != "ABCD12")
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Read from top-of-file fails", cSize);

	// Write entire buffer from top of file: bcdefg\f
	QByteArray aFullBfr("abcdefg");
	aBfrLgth = aFullBfr.length();	// Length (7)
	if (!append(aFullBfr.data(), aBfrLgth))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Write entire buffer fails", cSize);

	// Read truncated buffer.
	if (read(aReadBfr, aClear) != cSize || QString(aReadBfr) != "bcdefg")
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Read entire buffer fails", cSize);

	// Records. Write 6-char record. \v8|ABCDEF\f
	cSize = 9;
	cRecord = true;
	aClear = false;
	truncate();
	QByteArray aBody("\0138|ABCDEF");
	if (!append(aBody.data(), aBody.length()))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Write 6-char record fails", cSize);

	// Read 6-char record
	if (read(aReadBfr, aClear) != cSize || QString(aReadBfr) != "\0138|ABCDEF")
		qDebug("ARingFile::selfTest(). Read 6-char record fails. Bfr:%s", aReadBfr.data());

	// Write 2-char record over top leaving gap: \v4|WX\fDEF\f
	aBody = "\0134|WX";
	if (!append(aBody.data(), aBody.length()))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Write 2-char record fails", cSize);

	// Read 2-char record.
	if (read(aReadBfr, aClear) != 5 || QString(aReadBfr) != "\0134|WX")
		qDebug("ARingFile::selfTest(). Read 3-char record fails. Bfr:%s", aReadBfr.data());

	// Append 1-char record: \v4|WX\v3|Y\f
	aBody = "\0133|Y";
	if (!append(aBody.data(), aBody.length()))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Write Y record fails", cSize);

	// Read 2 short records: \v4|WX\43|Y\f
	if (read(aReadBfr, aClear) != cSize || QString(aReadBfr) != "\0134|WX\0133|Y")
		qDebug("ARingFile::selfTest(). Read 2 short records fails. Bfr:%s", aReadBfr.data());

	// Write 1 short record at top leaving no gap: \v3|Z\f\v3|Y\f
	aBody = "\0133|Z";
	if (!append(aBody.data(), aBody.length()))
		qDebug("ARingFile::selfTest(), MaxSize:%ld, Write Z record fails", cSize);

	// Read 2 short records starting in middle.
	if (read(aReadBfr, aClear) != 8 || QString(aReadBfr) != "\0133|Y\0133|Z")
		qDebug("ARingFile::selfTest(). Read 2 short records fails. Bfr:%s", aReadBfr.data());

	// Restore settings
	cSize = aMaxSize;
	cRecord = aRecord;
	truncate();
	if (!aSavBfr.isEmpty() && !append(aSavBfr.data(), aSavBfr.length()))
		qDebug("ARingFile::selfTest(), Unable to restore original file");
}

/*!
truncate - Clear the contents of the ring file.
\return aOK True iff truncation succeeds.
\par Notes:
-# Open for write-only truncates the file.
-# If not a persistent file, cBfr is not truncated for performance reasons.
-# Note that non-const QByteArray methods may resize which moves the position of the data. Be careful!
*/
bool ARingFile::truncate()
{
	bool aOk = true;
	if (cPersistent)
	{	if (open(QIODevice::Truncate | QIODevice::WriteOnly))	// 8 | 2
			close();
		else
			aOk = false;
	}
	cBof = cEof = 0;
	return aOk;
}
// end
