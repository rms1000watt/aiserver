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
aisdev/ausrmgr/ausrmgr.cpp

														User Manager

CHANGE HISTORY
Version	Date		Who		Change
3.2005	 2/18/2008	fchua	Modified readFile. Old logon counts are retained when reading.
3.2005	 2/18/2008	fchua	Added getLogonStats.
3.2001	 1/27/2008	fchua	Removed old functions. Added new functions (addUser, updateUser).
3.2001	 1/27/2008	fchua	Converted user list structure to QMap.
1.0112	10/27/2006	tlw		~AUsrMgr. Reclaim User structures on destruct.
1.0057	3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QFile>			// QFile
#include <QtCore/QFileInfo>		// QFileInfo
#include <QtCore/QDir>
#include <QtCore/QObject>		// QObject
#include <QtCore/QStringList>	// QStringList
#include <QtCore/QTextStream>	// QTextStream

#include "aglobals.h"			// ACHECK
#include "alogmgr.h"			// ALogMgr
#include "autilities.h"			// AUtil
#include "ausrmgr.h"			// UsrMgr

//	-------------------------------------------------- PUBLIC METHODS ---------------------------------------------------------
/*!
\brief AUsrMgr constructor initializes the cUsrList from the password file.

The first four entries in the user list are hand crafted for the Null, Nobody, guest and system users.
\param irParams - A string map containing configuration parameters for the User Manager.
\return void
 */
AUsrMgr::AUsrMgr(const AStringMap& irParams)
{	// Initialize class properties
	bool aOk;

	cMaxUsrs = 5000;
	cMaxLogon = 4;
	cY2K = QDate(2000, 1, 1);
	if (irParams.contains("gblusrmaxlogon"))
	{	long aMaxLogon = irParams["gblusrmaxlogon"].toLong(&aOk);
		if (aOk)
			cMaxLogon = aMaxLogon;
	}

    QString aUsrDir = irParams["gblusrfiledir"];
    QDir aDir;
    QFile aFile;

    // Check if path is relative, prepend the application directory
    if (QDir::isRelativePath(aUsrDir))
        aUsrDir = gAis.mGblParams["gblaisapplicationpath"] + aUsrDir;

    AUtil::terminateStg(QDir::separator().toAscii(),aUsrDir);

    // Create the usr directory if necessary
    aDir.setPath(aUsrDir);
    if (!aDir.exists())
        aDir.mkpath(aUsrDir);

    QString aPasswdFile = aUsrDir + USRMGR_PASSWDFILE;
	if (QFile::exists(aPasswdFile))
		cPasswdFile = aPasswdFile;
	else
    {	// File does not exist, create an empty one.
        aFile.setFileName(aPasswdFile);
        aFile.open(QIODevice::WriteOnly);

        if (aFile.exists())
            aFile.close();
        else
        {
            QString aMsg = QString("0,AUsrMgr(), Unable to find password file for context: %1, file: %2")
                       .arg(irParams["gblaisdefaultcontextname"]).arg(aPasswdFile);
            // Note that this will not appear in the server form log page.
            // This message will only appear in the console/terminal and in sysmsg.log
            LOGSYSMSG(geWarn, aMsg);
        }
    }

	// Lists initialized by readFile (below)
	cUsrIds.clear();		// QHash key=LogonName, value=UsrId
	cUsrMap.clear();

	// Handcraft the null, nobody, guest, and system entries
	cNullUsr.mActive = false;
	cNullUsr.mComment = "Null User";
	cNullUsr.mDateBeg = cNullUsr.mDateEnd = QDate(2000, 1, 1);
	cNullUsr.mLogonCount = 0;
	cNullUsr.mLogonTries = 0;
	cNullUsr.mPasswd = "*";					// Password is disabled
	cNullUsr.mSecLvl = USRMGR_NULLSECLVL;	// geNada, no access, period.
	cNullUsr.mUsrName = USRMGR_NULLNAME;	// null

	cNoUsr.mActive = true;
	cNoUsr.mComment = "Nobody Logon";
	cNoUsr.mDateBeg = cNullUsr.mDateBeg;
	cNoUsr.mDateEnd = QDate(3000,1,1);
	cNoUsr.mLogonCount = 0;
	cNoUsr.mLogonTries = 0;
	cNoUsr.mPasswd = "*";					// Password is disabled
	cNoUsr.mSecLvl = USRMGR_NOBODYSECLVL;	// geNada, no access, period.
	cNoUsr.mUsrName = USRMGR_NOBODYNAME;

	cGuestUsr.mActive = true;
	cGuestUsr.mComment = "Guest Logon";
	cGuestUsr.mDateBeg = cNoUsr.mDateBeg;
	cGuestUsr.mDateEnd = cNoUsr.mDateEnd;
	cGuestUsr.mLogonCount = 0;
	cGuestUsr.mLogonTries = 0;
	cGuestUsr.mPasswd = "";					// No password
	cGuestUsr.mSecLvl = USRMGR_GUESTSECLVL;	// geGuest, minimal access
	cGuestUsr.mUsrName = USRMGR_GUESTNAME;

	cSysUsr.mActive = true;
	cSysUsr.mComment = "System Logon";
	cSysUsr.mDateBeg = cNoUsr.mDateBeg;
	cSysUsr.mDateEnd = cNoUsr.mDateEnd;
	cSysUsr.mLogonCount = 0;
	cSysUsr.mLogonTries = 0;
	cSysUsr.mPasswd = "1NYQDD8HWQD2Z";
	cSysUsr.mSecLvl = USRMGR_SYSSECLVL;		// geSuper, total access
	cSysUsr.mUsrName = USRMGR_SYSNAME;

	// Read password file to set User list and UserId map
	readFile();
}

/*!
\brief Destructor Free allocated resources

\par Notes:
-# AUsrMgr has remote ownership of AUsrSt referents in cUsrList. 
-# Since instances of AUsrMgr are never assigned, copied or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
AUsrMgr::~AUsrMgr()
{
	AUsrSt* apUsr;

	AUsrInfoMap::const_iterator aMapIter = cUsrMap.begin();
	while (aMapIter != cUsrMap.end())
	{
		if ((apUsr = aMapIter.value()) != NULL)
			delete apUsr;
		aMapIter++;
	}

	cUsrMap.clear();
	cUsrIds.clear();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AUsrMgr()");
#endif
}

/*!
 * \brief Add a new user.
 *
 * \param[in] iSecLvl Security Level.
 * \param[in] irUsername Username.
 * \param[in] irPassword Password.
 * \parma[in] irEndDate End Date.
 * \param[in] irComment.
 *
 * \return Result code.
 */
long AUsrMgr::addUser(ASecLvl iSecLvl, const QString& irUsername, const QString& irPassword,
						const QDate& irEndDate, const QString& irComment)
{
	long aUsrId = 0;
	long aRet = 0;
	// get list of keys
	QList<long> aUsrIdList = cUsrMap.keys();
	long aListSize = aUsrIdList.size();
	QString aError;

	if (aListSize >= cMaxUsrs)
	{
		aError = "Maximum no. of users reached";
		aRet = AERR_MAXUSRREACHED;
	}
	else if (cUsrIds.contains(irUsername))
	{
		aError = "Username already exists";
		aRet = AERR_USRNAMEEXISTS;
	}
	else if (iSecLvl < geNada || iSecLvl > geSuper)
	{
		aError = "Invalid security level";
		aRet = AERR_BADSECLVL;
	}
	else
	{
		// Find the lowest available User Id starting from 4
		for (aUsrId = 4; aUsrId < aListSize && aUsrIdList.contains(aUsrId); ++aUsrId)
			;

		AUsrSt *apUsr = new AUsrSt();
		if (apUsr == NULL)
		{
			aError = "Out of memory";
			aRet = AERR_OUTMEMORY;
		}
		else
		{
			apUsr->mActive = true;
			apUsr->mComment = irComment;
			apUsr->mDateBeg = QDate::currentDate();
			apUsr->mDateEnd = irEndDate;
			apUsr->mLogonCount = 0;
			apUsr->mLogonTries = 0;
			apUsr->mPasswd = irPassword;
			apUsr->mSecLvl = iSecLvl;
			apUsr->mUsrName = irUsername;

			cUsrMap.insert(aUsrId, apUsr);
			cUsrIds[irUsername] = aUsrId;

			aRet = writeFile();
		}
	}

	if (!aError.isEmpty())
	{
		QString aEntry = QString("0,AUsrMgr.addUser, %1").arg(aError);
		LOGSYSMSG(geWarn,aEntry);
	}

	return aRet;
}

/*!
 * \brief Delete a user.
 *
 * \param[in] iUserId User Id.
 *
 * \return Result code.
 *
 * \notes
 * -# Removes entry from cUsersId, if any
 * -# Usr ID may not change. It must be fixed for any one user on any one server
 */
long AUsrMgr::deleteUser(long iUsrId)
{
	long aRet = 0;
	QString aError;
	if (iUsrId <= 3 || !cUsrMap.contains(iUsrId))
	{
		aError = "Invalid User Id";
		aRet = AERR_BADUSRID;
	}
	else
	{
		AUsrSt *apUsr = cUsrMap[iUsrId];
		cUsrIds.remove(apUsr->mUsrName);
		delete apUsr;
		cUsrMap.remove(iUsrId);
		aRet = writeFile();
	}

	if (!aError.isEmpty())
	{
		QString aEntry = QString("0,AUsrMgr.deleteUser, %1, UsrId %2").arg(aError).arg(iUsrId);
		LOGSYSMSG(geWarn, aEntry);
	}

	return aRet;
}

/*!
\brief getEndDay - The expiration day of this account.

\param iUsrId - ID assigned to this user
\return aEndDay - Number of days from Jan. 1, 2000 to the expiration day.
\par Notes:
-# The end day in the password file is in human readable format.
 */
long AUsrMgr::getEndDay(long iUsrId)
{
	long aEndDay = 0;

	if (cUsrMap.contains(iUsrId))
	{	QDate& arDateEnd = cUsrMap[iUsrId]->mDateEnd;
		aEndDay = cY2K.daysTo(arDateEnd);
		if (aEndDay < 0) aEndDay = 0;
	}	

	return aEndDay;
}

long AUsrMgr::getLogonStats(QString& orLogonStats)
{
	orLogonStats.clear();
	foreach(AUsrSt *apUsr, cUsrMap)
	{
		orLogonStats.append(apUsr->mUsrName);
		orLogonStats.append("\t");
		orLogonStats.append(QString::number(apUsr->mLogonCount));
		orLogonStats.append("\n");
	}
	return 0;
}

long AUsrMgr::getUsers(QString& orUserList)
{
	AUsrSt *apUsr = 0;
	orUserList.clear();
	AUsrInfoMap::const_iterator aMapIter = cUsrMap.begin();
	QString aTemp;

	// ingore system entries
	while (aMapIter != cUsrMap.end() && aMapIter.key() < 4)
		aMapIter++;
	while (aMapIter != cUsrMap.end())
	{
		apUsr = aMapIter.value();
		if (apUsr != NULL)
		{
			aTemp.sprintf("%ld\t%s\t%d\t%s\t%s\t%s",
                aMapIter.key(),
				qPrintable(apUsr->mUsrName),
                apUsr->mSecLvl,
				qPrintable(apUsr->mDateBeg.toString("MM/dd/yyyy")),
				qPrintable(apUsr->mDateEnd.toString("MM/dd/yyyy")),
                qPrintable(apUsr->mComment));
			orUserList.append(aTemp);
			orUserList.append("\n");
		}

		aMapIter++;
	}

	return (cUsrMap.count() - 4);
}

// getSecurityLevel is in ausrmgr.h
// getUsrId is in ausrmgr.h
// getUsrInfo(long iUsrId) is in ausrmgr.h
// getUsrName is in ausrmgr.h
// logoff (long iUsrId) is in ausrmgr.h

/*!
\brief logon - Authenticate a user

\param irUsrName - Login name
\param irPasswd - Password
\param opSecLvl - Security Level 0(lowest) - 7 (highest) assigned to this user
\param opEndDay - Day (since 1/1/2000) that account expires
\param orComment - Comment about the user
\returns aUsrId - UsrId or error code (< 0) iff an error.
\par Notes:
-# Valid user IDs are > 0.
 */
long AUsrMgr::logon(const QString& irUsrName, const QString& irPasswd,long* opSecLvl, long* opEndDay, QString& orComment)
{
	// Reread passwd file if it has changed
	QFileInfo aFi(cPasswdFile);
	QDateTime aLastModified = aFi.lastModified();
	if (cLastRead < aLastModified)
		readFile();			// Select file based upon the context.

	// Check inputs
	long aUsrId;
	if (!cUsrIds.contains(irUsrName))
		aUsrId = -AERR_UNKUSRNAME;
	else if ((aUsrId = cUsrIds[irUsrName]) <= 0 || !cUsrMap.contains(aUsrId))
		aUsrId = -AERR_BADUSRID;
	else
	{	// Look in user list for a match
		long aSecLvl;
		aUsrId = cUsrIds[irUsrName];
		AUsrSt* apUsr = cUsrMap[aUsrId];
		bool aUsrMt = apUsr->mPasswd.isEmpty();	// User entry passwd is null or zero.
		bool aInMt  = irPasswd.isEmpty();		// Supplied passwd is null or zero len.
		QDate& arDateEnd = apUsr->mDateEnd;
		long aToday = cY2K.daysTo(QDate::currentDate());
		long aEndDay =  cY2K.daysTo(arDateEnd);
		if (irUsrName != apUsr->mUsrName)
			aUsrId = -AERR_USRMISMATCH;
		else if (apUsr->mLogonTries >= cMaxLogon)
			aUsrId = -AERR_USRMISMATCH;
		else if (((!aInMt || !aUsrMt) && (aInMt || aUsrMt || irPasswd != apUsr->mPasswd))
				|| apUsr->mPasswd == "*")		// Optimized for speed
			aUsrId = -AERR_BADPASSWD;
		else if ((aSecLvl = apUsr->mSecLvl) < 0 || aSecLvl > 7)
			aUsrId = -AERR_ACCESS;
		else if (aEndDay < aToday || !apUsr->mActive)
			aUsrId = -AERR_ACCTEXPIRED;
		else		// Logon succeeds
		{	++apUsr->mLogonCount;
			apUsr->mLogonTries = 0;
			if (opSecLvl != NULL)
				*opSecLvl = aSecLvl;
			if (opEndDay != NULL)
				*opEndDay = aEndDay;
			orComment = apUsr->mComment;
		}
		if (aUsrId < 0)
			++apUsr->mLogonTries;
	}
	if (aUsrId < 0)
	{	QString aErr = QString("%1,AUsrMgr.logon(), %2, UsrName %3").arg(-aUsrId).arg(gAis.mpErrMsgs[-aUsrId], irUsrName);
		LOGSYSMSG(geInfo, aErr);
	}
	return aUsrId;
}
//	bool validate(long iUsrId) is in usrmgr.h

/*	---------------------------------------------------------------------------------------------------------------------------
readFile - Authenticate a user
Args:
	none
Returns:
	nothing
Notes:
 1.	UsrIds 0-3 are reserved.
	------------------------------------------------------------------------------------------------------------------------ */
void AUsrMgr::readFile()
{
	// Temporarily store previous logon counts
	AUsrLogonCountMap aLogonCounts;

	foreach(long aUsrId, cUsrMap.keys())
		aLogonCounts[aUsrId] = cUsrMap.value(aUsrId)->mLogonCount;

	// Initialize user lists
	cUsrIds.clear();		// Map from logon name to UsrId
	cUsrMap.clear();		// Map of Usr structures$

	// Initialize the null, nobody, guest, and system entries in the user list and map
	AUsrSt* apUsr = new AUsrSt(cNullUsr);
	cUsrMap.insert(USRMGR_NULLUSRID, apUsr);			// 0
	apUsr = new AUsrSt(cNoUsr);
	cUsrIds[cNoUsr.mUsrName] = USRMGR_NOBODYUSRID;
	cUsrMap.insert(USRMGR_NOBODYUSRID, apUsr);			// 1
	apUsr = new AUsrSt(cGuestUsr);
	cUsrIds[cGuestUsr.mUsrName] = USRMGR_GUESTUSRID;
	cUsrMap.insert(USRMGR_GUESTUSRID, apUsr);			// 2
	apUsr = new AUsrSt(cSysUsr);
	cUsrIds[cSysUsr.mUsrName] = USRMGR_SYSUSRID;
	cUsrMap.insert(USRMGR_SYSUSRID, apUsr);			// 3

	// Get old logon counts
	cUsrMap[USRMGR_NULLUSRID]->mLogonCount = aLogonCounts[USRMGR_NULLUSRID];
	cUsrMap[USRMGR_NOBODYUSRID]->mLogonCount = aLogonCounts[USRMGR_NOBODYUSRID];
	cUsrMap[USRMGR_GUESTUSRID]->mLogonCount = aLogonCounts[USRMGR_GUESTUSRID];
	cUsrMap[USRMGR_SYSUSRID]->mLogonCount = aLogonCounts[USRMGR_SYSUSRID];

	// Read the password file, if any
	if (cPasswdFile.isEmpty())
		return;
	QFile aF(cPasswdFile);
	QString aMsg;
	if (!aF.open(QIODevice::Text | QIODevice::ReadOnly))		// 16|1
		aMsg = "Can't find password file";	
	else
	{	// Each entry - UsrId:SecLvl:DateBeg:DateEnd:UserName:Passwd:Comment
		QString aLine;			// One line from passwd file
		QStringList aTkns;
		long  aSize;				// Number of tokens/line
		long aUsrId;
		QDate aDateBeg;			// Start date of account
		QDate aDateEnd;			// Account expiration date
		long aSecLvl;			// Security level 0-7
		const char* apErr = NULL;		// Reason for error.
		QTextStream aInput(&aF);
		while (!aInput.atEnd())
		{	// Skip blank lines and comments
			aLine = aInput.readLine(1024);
			if (AUtil::isComment(aLine))
				continue;
			// Chop newline on the end
			AUtil::chop(aLine);
			aTkns = aLine.split(':', QString::KeepEmptyParts);
			if ((aSize = aTkns.size()) < 6)
				apErr = " Need 6-7 fields";
			else
			{	QString& arUsrName = aTkns[4];
				if (aSize == 6)		// No comment supplied
					aTkns.append("");
	
				if ((aSecLvl = aTkns[1].toULong()) < 0 || aSecLvl > 7)
					apErr = "Invalid security level";
				else if ((aUsrId = aTkns[0].toLong()) < 2 || aUsrId >= cMaxUsrs)
					apErr = "UserId must be >= 2, < 5000";
				else if (aUsrId == 2 && arUsrName != USRMGR_GUESTNAME)
					apErr = "Can't change guest user name";
				else if (aUsrId == 3 && arUsrName != USRMGR_SYSNAME)
					apErr = "Can't change system user name";
				else if (aUsrId >= 4 && cUsrIds.contains(arUsrName))
					apErr = "Duplicate user login name";
				else		// Convert textual dates (MM/dd/yyyy) to QDate
				{	if (aTkns[2] == "0")
						aDateBeg = QDate::currentDate();
					else if (!AUtil::stringToDate(aTkns[2], aDateBeg))
						apErr = "Invalid beg. date";
					if (aTkns[3] == "0")
						aDateEnd = QDate(2000, 1, 1);
					else if (!AUtil::stringToDate(aTkns[3], aDateEnd))
						apErr = "Invalid expiration date";
				}
				if (apErr == NULL)
				{	// A valid entry. add it to list
					AUsrSt* apUsr = new AUsrSt;
					apUsr->mActive = true;
					apUsr->mComment = aTkns[6];
					apUsr->mDateBeg = aDateBeg;
					apUsr->mDateEnd = aDateEnd;
					// Get the previous logon count
					apUsr->mLogonCount = (aLogonCounts.contains(aUsrId)) ? aLogonCounts.value(aUsrId) : 0;
					apUsr->mLogonTries = 0;
					apUsr->mPasswd = aTkns[5];
					apUsr->mSecLvl = (ASecLvl)aTkns[1].toULong();
					apUsr->mUsrName = arUsrName;
					cUsrIds[apUsr->mUsrName] = aUsrId;
					cUsrMap.insert(aUsrId, apUsr);
					cUsrIds[arUsrName] = aUsrId;
				}
			}
			if (apErr != NULL)
			{	aLine = QString("\t0,%1, entry: %2\n").arg(apErr, aLine);
				aMsg += aLine;
				apErr = NULL;
			}
		}
		// Note time that in-memory list was updated
		cLastRead = QDateTime::currentDateTime();
		aF.close();
	}
	if (!aMsg.isEmpty())
	{	QString aErr = QString("0,AUsrMgr::readFile(), %1, File:%2").arg(aMsg, cPasswdFile);
		LOGSYSMSG(geSoftware, aMsg);
	}
}

/*!
 * \brief Update user information.
 *
 * \param[in] iUserId Existing User Id.
 * \param[in] irNewUsername New Username.
 * \param[in] irNewPassword New Password.
 * \param[in] iSecLvl New Security Level.
 * \param[in] irNewEndDate New End date.
 * \param[in] irNewComment New comment.
 *
 * \return Result code.
 */
long AUsrMgr::updateUser(long iUserId, const QString& irNewUsername, const QString& irNewPassword, 
							ASecLvl iSecLvl, const QDate& irNewEndDate, const QString& irNewComment)
{
	long aRet = 0;
	bool aOk = false;
	QString aError;

	if (!cUsrMap.contains(iUserId))
	{
		aError = "Invalid User Id";
		aRet = AERR_BADUSRID;
	}
	else
	{
		// Update selected fields
		AUsrSt *apUsr = cUsrMap[iUserId];
		if (!irNewUsername.isEmpty() && irNewUsername != apUsr->mUsrName)
		{
			if (cUsrIds.contains(irNewUsername))
			{
				aError = "Username already exists";
				aRet = AERR_USRNAMEEXISTS;
				goto Error;
			}

			cUsrIds.remove(apUsr->mUsrName);
			apUsr->mUsrName = irNewUsername;
			cUsrIds[irNewUsername] = iUserId;
			aOk = true;
		}

		if (iSecLvl >= geNada && iSecLvl <= geSuper && iSecLvl != apUsr->mSecLvl)
		{
			apUsr->mSecLvl = iSecLvl;
			aOk = true;
		}

		if (!irNewPassword.isEmpty() && irNewPassword != apUsr->mPasswd)
		{
			apUsr->mPasswd = irNewPassword;
			aOk = true;
		}

		if (irNewEndDate != apUsr->mDateEnd)
		{
			apUsr->mDateEnd = irNewEndDate;
			aOk = true;
		}

		if (!irNewComment.isEmpty() && irNewComment != apUsr->mComment)
		{
			apUsr->mComment = irNewComment;
			aOk = true;
		}
	}

Error:
	if (!aError.isEmpty())
	{	// Entry: UsrId:SecLvl:DateBeg:EndDay:LogonName:Passwd:Comment
		QString aEntry = QString("0,AUsrMgr.changeUser, %1, Entry: %2:%3:-:%4:%5:%6").arg(aError).arg(iUserId).arg(iSecLvl)
			.arg(irNewUsername, irNewPassword, irNewComment);
		LOGSYSMSG(geWarn, aEntry);
	}

	if (aOk)
		aRet = writeFile();

	return aRet;
}

/**
 * \brief Writes the contents of the user map to the password file.
 *
 * \return Result of write operation.
 * \retval 0 Success.
 * \retval AERR_PWDOPENFAIL Failed in opening of password file.
 * \retval AERR_PWDWRITEFAIL Failed in writing to password file.
 */
long AUsrMgr::writeFile()
{
	QFile aF(cPasswdFile);
	long aRet = 0;
	QString aErr;

	if (!aF.open(QIODevice::Text | QIODevice::WriteOnly))
	{
		aErr = "Can't open passwd file for writing!";
		aRet = AERR_PWDOPENFAIL;
	}
	else
	{
		// Entry  UsrId:SecLvl:DateBeg:DateEnd:LogonName:Passwd:Comment
		QString aLine, aDateBeg, aDateEnd;

		AUsrInfoMap::const_iterator aMapIter = cUsrMap.begin();
		AUsrSt *apUsr = 0;
		while(aMapIter != cUsrMap.end())
		{
			if (aMapIter.key() >= 4)
			{
				apUsr = aMapIter.value();
				aDateBeg = apUsr->mDateBeg.toString("MM/dd/yyyy");
				aDateEnd = apUsr->mDateEnd.toString("MM/dd/yyyy");
				aLine = QString("%1:%2:%3:%4:%5:%6:%7\n").arg(aMapIter.key()).arg(apUsr->mSecLvl).arg(aDateBeg, aDateEnd)
				.arg(apUsr->mUsrName, apUsr->mPasswd, apUsr->mComment);
				if (aF.write(aLine.toLatin1().data(), aLine.length()) < 0)
				{	
					aErr = "Can't write entry to passwd file";
					aRet = AERR_PWDWRITEFAIL;
					break;
				}
			}
			aMapIter++;
		}
		aF.close();
	}

	if (!aErr.isEmpty())
	{
		QString aEntry = QString("0,AUsrMgr::writeFile(), %1 File: %2").arg(aErr, cPasswdFile);
		LOGSYSMSG(geSoftware, aEntry);
	}

	return aRet;
}
