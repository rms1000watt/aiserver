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

#ifndef USRMGR_H
#define USRMGR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/ausrmgr.h
													User Manager Specification

CHANGE HISTORY
Version	Date		Who		Change
3.2005	 2/18/2008	fchua	Added getLogonStats.
3.2001	 1/27/2008	fchua	Removed old functions. Added new functions (addUser, updateUser).
3.2001	 1/27/2008	fchua	Converted user list structure to QMap.
1.0112	10/27/2006	tlw		AUsrNameMap. Convert to hash.
1.0057	3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QDateTime>
#include <QtCore/QMutex>

//	------------------------------------------------ DEFINED CONSTANTS --------------------------------------------------------
//	Error codes
#define USRMGR_INVALIDUSERNAME	-400

//	---------------------------------------------- FUNCTION DECLARATIONS ------------------------------------------------------

//	-------------------------------------------------- DATA STRUCTURES --------------------------------------------------------
// Info maintained for each user
typedef struct
{	bool		mActive;		// An entry that is in use
	QString		mComment;		// Comment supplied with successful logon
	QDate		mDateBeg;		// Date account initiated
	QDate		mDateEnd;		// Expiration date
	long		mLogonCount;	// Set to number of current logons.
	long		mLogonTries;	// Number of consecutive logon attempts
	QString		mPasswd;		// Encrypted password
	ASecLvl		mSecLvl;		// Security level (see globals.h)
	QString		mUsrName;		// User's logon name
} AUsrSt;
typedef QHash<QString, long> AUsrNameMap;// key=LogonName, value=UsrId
typedef QMap<long,AUsrSt*> AUsrInfoMap; // key=UsrId, value=AUsrSt*
typedef QHash<long, long> AUsrLogonCountMap; // key=UsrId, value=LogonCount

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------

/*!
\brief UsrMgr provides a set of utilties that manage system-wide user logons and user validations

UsrMgr authenticates system application users.  Logon compares the user's logon name and password submitted from a logon form
with those in the list of user structures.  A user is added to the list of logged-on users for the duration of a logon.
Validate checks the userId from the cookie on each request for a protected slot against the userId's of those that are logged in.
It is important to maintain the same userId for all users in a persistent store.  It would be a mistake to modify the user ID
while the client is logged in even if the server stopped and restarted.

\par Password File Format.
Each entry in the password file has the format
\verbatim
UsrId:SecLvl:LogonName:Passwd:Key
\endverbatim
\par Notes:
-# UsrId     - A unique number for each user > 0 that does not ever change
-# SecLvl    - A value from 0 (no access) to 7 (super user)
-# LogonName - User's logon name (e.g. tmay)
-# Passwd	  - The encrypted password
-# Key       - Key used to encrypt the cookie value.
 */
class AUsrMgr
{
public:
	AUsrMgr(const AStringMap& irParams);
	~AUsrMgr();
	
	bool		enableAccount(const QString& irName);
	AUsrSt*		getUsrInfo(long iUsrId);
	long		getUsrId(const QString& irUsrName) ;
	QString		getUsrName(long iUsrId);
	ASecLvl		getSecurityLevel(long iUsrId);
	long		getEndDay(long iUsrId);

	bool		logoff(long iUsrId);
	long		logon(const QString& irName, const QString& irPasswd, long* opSecLvl,
					  long* ipEndDay, QString& orComment);

	bool		validate(long iUsrId, ASecLvl iSecLvl);

	long		addUser(ASecLvl iSecLvl, const QString& irUsername, const QString& irPassword,
						const QDate& irEndDate, const QString& irComment);
	long		deleteUser(long iUsrId);
	long		updateUser(long iUserId, const QString& irNewUsername, const QString& irNewPassword, 
							ASecLvl iSecLvl, const QDate& irNewEndDate, const QString& irNewComment);
	long		getLogonStats(QString& orLogonStats);
	long		getUsers(QString& orUserList);

private:
	AUsrSt		cGuestUsr;	// Guest User in-memory info
	QDateTime	cLastRead;	// Last time passwd file was read
	long		cMaxLogon;	// Maximum number of logon tries before acct. locked
	long		cMaxUsrs;	// Maximum (Last) UsrId
	AUsrSt		cNoUsr;		// Nobody User in-memory info
	AUsrSt		cNullUsr;	// Null User in-memory info
	QString		cPasswdFile;// Path/password file name.
	AUsrSt		cSysUsr;	// System User in-memory info
	AUsrNameMap	cUsrIds;	// Map from logon name to UsrId
	AUsrInfoMap	cUsrMap;	// Map of Usr structures
	QDate		cY2K;		// 1/1/2000

	void		readFile(); // Read password file
	long		writeFile();				// Write cUsers map to passwd file

	// TODO: Consider adding mutex variable
};

/*!
\brief enableAccount - Reenable an account after it is blocked

\param irName - Logon name assigned to the blocked user.
\return aWasBlocked - Returns true iff blocked user is unblocked.
 */
inline bool AUsrMgr::enableAccount(const QString& irName)
{
	bool aWasBlocked = false;
	QString aReason;
	long aUsrId;

	if (!cUsrIds.contains(irName))
		aReason = "Unable to find user";
	else if ((aUsrId = cUsrIds[irName]) <= 0 || !cUsrMap.contains(aUsrId))
		aReason = "No user data available";
	else
	{	AUsrSt* apUsr = cUsrMap[aUsrId];
		aWasBlocked = (apUsr->mLogonTries > 1);
		apUsr->mLogonTries = 0;
	}
	if (!aReason.isEmpty())
	{	QString aErr = QString("0,AUsrMgr.enableAccount(), %1 for %2.").arg(aReason, irName);
		LOGSYSMSG(geWarn, aErr);
	}
	return aWasBlocked;
}

/*!
\brief getSecurityLevel - Return the security level for the specified user

\param iUsrId - The ID assigned in the password file for this user
\return ASecLvl - The security level or zero if userId not in the cUsrList.
 */
inline ASecLvl AUsrMgr::getSecurityLevel(long iUsrId)
{
	long aUsrId = (cUsrMap.contains(iUsrId)) ? iUsrId : 0;
	return cUsrMap[aUsrId]->mSecLvl;
}

/*!
\brief getUsrInfo - Get the specified user's logon info

\param iUsrId - The ID assigned in the password file for this user
\return AUsrSt* - Contains all the user's logon info.
\par Note:
-# Callers of this routine should not change the contents of the returned user structure.
 */
inline AUsrSt* AUsrMgr::getUsrInfo(long iUsrId)
{
	long aUsrId = (cUsrMap.contains(iUsrId)) ? iUsrId : 0;
	return cUsrMap[aUsrId];
}

/*!
\brief getUsrId - Returns the UsrId for the named user.

\param irUsrName - Logon name assigned to this user.
\return aUsrId - The UsrId (from the password file) for this user or -1 if user is not found.
 */
inline long AUsrMgr::getUsrId(const QString& irUsrName)
{
	long aUsrId = -1;
	if (cUsrIds.contains(irUsrName))
		aUsrId = cUsrIds.value(irUsrName);
	return aUsrId;
}

/*!
\brief getUsrName - Get the specified user's logon name

\param iUsrId - The ID assigned in the password file for this user
\return The user's logon name.
\par Note:
-# Callers of this routine should not change the contents of the returned user structure.
 */
inline QString AUsrMgr::getUsrName(long iUsrId)
{
	long aUsrId = (cUsrMap.contains(iUsrId)) ? iUsrId : 0;
	return cUsrMap[aUsrId]->mUsrName;
}
/*!
\brief logoff - Reduce the logon count for this user.

\param iUsrId - The ID assigned in the password file for this user
\return true iff the user was, indeed, logged on.
 */
inline bool AUsrMgr::logoff(long iUsrId)
{
	long aUsrId = (cUsrMap.contains(iUsrId)) ? iUsrId : 0;
	AUsrSt* apUsr = cUsrMap[aUsrId];

	long aLogonCount = --apUsr->mLogonCount;
	if (aUsrId > 0 && aLogonCount < 0)
	{	apUsr->mLogonCount = 0;
		QString aErr = QString("0,AUsrMgr.logoff(), Too many logoffs for %1").arg(apUsr->mUsrName);
		LOGSYSMSG(geWarn, aErr);
	}
	return (aLogonCount >= 0);
}

/*!
\brief validate - Check to make sure that the specified user has sufficient security level

\param iUsrId - The ID assigned in the password file to this user
\param iSecLvl - The user must have this security level or higher.
\return True iff the iUsrId is valid, logged on, and has sufficient security level.
 */
// Returns true if this user is currently logged on with the desired security level
inline bool AUsrMgr::validate(long iUsrId, ASecLvl iSecLvl)
{
	long aUsrId = (cUsrMap.contains(iUsrId)) ? iUsrId : 0;
	AUsrSt* apUsr = cUsrMap[aUsrId];

	return (apUsr->mLogonCount > 0 && apUsr->mSecLvl >= iSecLvl);
}

#endif //USRMGR_H
