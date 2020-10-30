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

#ifndef ARINGFILE_H
#define ARINGFILE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aringfile.h
														Ring File 

CHANGE HISTORY
Version	Date		Who		Change
1.0115	11/20/2006	tlw		Update documentation. Expand persistent description.
1.0067	7/29/2005	tlw		Add embedded documentation.
1.0061	5/10/2005	tlw		Add record-oriented writes to the file. Include context name, msg ID, session ID in record.
1.0060	4/27/2005	tlw		Add form feed at end of last write to allow constructor to find this point in the file.
1.0058	3/28/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QFile>

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define RINGFILE_SOC	'\013'	// Start of record character (vertical tab) 0xb
#define	RINGFILE_TERM	'\014'	// Terminating char in ring file marking end of last write (form feed) 0xc

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ARingFile is an efficient implementation of a fixed maximum-size persistent buffer.

The last max size bytes are kept in the ring file.  When max size bytes have been saved, writes continue at the top of the file,
overwriting the oldest data. A read starts just past the end of the last write. If the end of the file is reached, the read
continues from the top. If Persistent option is set, append stores data directly on disk; else, data is stored in cBfr.  The
contents of cBfr is saved on disk by flush and by the destructor. The data is stored in exactly the same format in either case.

\par Notes:
 -# The text is encoded using latin-1 encoding (ISO 8859-1).
 -# It is highly recommended that unlimited size logs set the Persistent option; otherwise, an in-memory log could exhaust memory.
 -#	A beg-of-file offset, cBof, is offset in the file just past the end of the last write or 0 if file is empty. 
 -# The character at cBof is a special char, RINGFILE_TERM used to mark the end (for finding cBof when RingFile is initialized).
 -# If the size of the item to be written exceeds cSize, bytes, the size is truncated to cSize.
 -#	If the item will fit between cBof and cSize, the write begins at offset cBof in the file.  Else, the write begins at the
beginning of the file overwriting the oldest data.  This approach improves performance but wastes space at the end of the
ring file. cBof is always moved to offset of the terminating char at end of the last write.
 -# cEof is the offset to the last character in the file or 0 if file is empty.  cEof is increased up to cSize as the file
expands to its maximum size.  When the next write is moved to the top of the file, cEof is set to the cBof at the end
of the previous write.  cEof is used when reading the contents of the file.
 -#	Note that cSize does not include the terminating char, so the actual file size can be one greater than cSize.
 -#	Reads first read from cBof to cEof if cBof < cEof.  Then, the read continues from 0 to cBof. The terminating character
at cBof is not included in the read but a null is added to the end of the returned string.
 -# If performance becomes an issue, postpone the close.  Just close on switching from read to write or vice-versa.  This
allows data to be buffered in-memory between writes.
\par Records
 - A record begins with V\%d|... where V is vertical tab, \%d is the length of the record in bytes (entire record less V).
 - If the record to be written won't fit between the end of the last record and cSize, the record is written from the top.
 - A read starts at the first vertical tab past the end of the last write. That way partially overwritten records are ignored.
 - A new record starts at the end of the last record, overwritting the oldest records in the file.

\par Example
A couple of diagrams of a file are shown where cSize is 3. b is the cBof, e is cEof, , x is term char at end of last write.
First write ABC, so that the file is just full: b=e=3
\verbatim
0123
   be
ABCx

Write "D", so that file wraps at end: b=0, e=3
0123
b  e
xBCD

Write "E", so that: b=1, e=3
0123
 b e
ExCD
\endverbatim
*/
class ARingFile : public QFile
{
public:
	ARingFile(bool iClear, bool iPersistent, bool iRecord, long iSize, const QString& irFilePath);
	~ARingFile();
	bool	append(const char* ipData, long iLgth);
	bool	appendRecord(const QString& irBody, long iRqId, long iSessionId, char iType);
	void	flush();
	long	read(QByteArray& orDst, bool iClear = true);
	void	selfTest();
	bool	truncate();

private:
	QByteArray	cBfr;			// Buffer to hold last cSize bytes appended to this log.
	long		cBof;			// File offset to just past last byte written (0 if none)
	long		cEof;			// File offset to just past last byte in file (0 if empty)
	QString		cFilePath;		// Path + file name of log file holding this log
	bool		cPersistent;	// Write each message to file rather than to cBfr
	bool		cRecord;		// Write records that begin with length specification.
	long		cSize;			// Maximum file size in bytes (0 if no limit to file size)
};

#endif // ARINGFILE_H
