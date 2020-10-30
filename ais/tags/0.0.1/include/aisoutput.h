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

#ifndef AISOUTPUT_H
#define AISOUTPUT_H

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aisoutput.h
														AIS Output Events
 
CHANGE HISTORY
Version	Date		Who		Change
1.0120	12/19/2006	tlw		Constructor. Add mpData. and mDataSize.
1.0057	 3/18/2005	tlw		Add documentation
												--------------- ---------------

DOCUMENTATION
 1.	See htmlmgrnotes.txt for project setup information.
 2.	See webpage.h for class specifications.	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QEvent>

//	------------------------------------------------- CLASS DEFINITIONS -------------------------------------------------------
// Function Types
// Intercepted by aismgr
#define AISMGROUT_CANCELREADHTMLPAGE	 0
#define AISMGROUT_DEBUG					 1
#define AISMGROUT_DISPLAY				 2
#define AISMGROUT_FILEOPEN				 3
#define AISMGROUT_CLOSESESSION			 4
#define AISMGROUT_READHTMLPAGE			 5
#define AISMGROUT_RETURNRESULT			 6
#define AISMGROUT_RETURNRESULT_FORWARDING 7
#define AISMGROUT_RINGBELL				 8
#define AISMGROUT_SENDTOCLIENT			 9
#define AISMGROUT_ENGINESTATE			10
// The following events are intercepted by Session Mgr
#define AISMGROUT_REQUESTHTTP			11
#define AISMGROUT_SIZE					12	// Set to number of function types

#define AISMGROUT_EVENT				   100

//	------------------------------------------------- CLASS DECLARATIONS ------------------------------------------------------
class AisMgrOutputEvent : public QEvent
{
public:
	AisMgrOutputEvent(long iSessionID, long iRequestID, long iStatus, QString& irEnctype, long iRetValue, QString& irAisOut
	, char* ipData, long iDataSize, QString& irDisplay, QString& irError, long iFcnType)
 :	QEvent(QEvent::Type(QEvent::User + AISMGROUT_EVENT)), mAisOut(irAisOut), mpData(ipData), mDataSize(iDataSize)
	, mDisplay(irDisplay), mEnctype(irEnctype),  mError(irError), mFcnType(iFcnType), mRequestID(iRequestID), mRetValue(iRetValue)
	, mSessionID(iSessionID), mStatus(iStatus)
	{}
	QString		mAisOut;
	char*		mpData;
	long		mDataSize;
	QString		mDisplay;
	QString		mEnctype;
	QString		mError;
	long		mFcnType;
	long		mRequestID;
	long		mRetValue;
	long		mSessionID;
	long		mStatus;
};

#endif // AISOUTPUT_H
