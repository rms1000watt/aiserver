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
#ifndef ASERVERFORM_H
#define ASERVERFORM_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/aserverform.h
															Server Form

CHANGE HISTORY
Version	Date		Who		Change
3.2009  5/08/2008	fchua	Update setParameters interface.
3.2008	4/08/2008	fchua	Added setParameters, onEditPreferences. Added SVR_LOG_PAGES define.
3.2006	3/10/2008	fchua	Added cpFindDialog member.
3.2006	3/10/2008	fchua	Added onFindPattern slot.
3.2006	3/10/2008	fchua	Added findPatternResult signal.
3.2005	2/18/2008	fchua	Added support for System Resource Monitor page.
3.2002	2/04/2008	fchua	Updated value of SVR_PAGES.
3.2001	1/27/2008	fchua	Added support for User Management page.
3.1004	11/2/2007	fchua	Added slot onReturnMsg() and signal windowClosed().
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0104	9/8/2006	tlw		Remove support for MinLevelSpinBox which is not used.
1.0070	10/8/2005	tlw		Convert to Qt4
1.0039	4/29/2004	tlw		Revise specification
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "ais.h"		// AReqType, AReturnRcvr
#include "aform.h"

#include <QtCore/QDate>
#include <QtCore/QTimer>

class AAppClient;
class APage;
class AMainWindow;
class ASysResMonPage;
class AOperationsDialog;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define SVR_PAGES 8		// Number of child pages: Monitor, AMP Log, Console Log, Request Headers, System Messages, 
						// User Access, User Management, System Monitor
#define SVR_LOG_PAGES 6

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief AServerForm - Server Monitor Form allows user to monitor a Server's activity by viewing various server logs.

 */
class AServerForm : public AForm, public AReturnRcvr
{
    Q_OBJECT
public:
	AServerForm(AAppClient* ipAppClient, AMainWindow* ipParent, const char* ipServerName);
	// ReturnRcvr Methods:
	virtual void returnOutput(long iDummyId, long iXId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
				 , char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
				 = QString());
	virtual void setContextName(long iConnectId, const QString& irContextName);

	// AForm Methods:
	virtual void getFormMenus(AMenuList& orFormMenus, AMenuList& orTabMenus, AToolList& orFormTools, AToolList& orTabTools);
	virtual void setParameters(AParameters* ipConsole, AParameters* ipEditor, AParameters* ipLog, AParameters* ipCabinet, AParameters* ipDebug);
    virtual void startup(const QString& irStartupMsgs);

	// Public Methods:
	void consoleOutput(const QString& irOut);
	void enable(bool iIsEnabled);
	void engineIdle(bool iIsEngineIdle);
	void setCursorLabel(long iCol, long iRow);
	void statusAlert(const QString& irMsg);

public slots:
	void		onReturnMsg(AReqType iReqType, const QString& irOut);
	void		onUserAdded(const QString& irUsername, const QString& irPassword,
							int iSecurityLevel, const QDate& irEndDate, const QString& irComment);
	void		onUserUpdated(long iUserId, const QString& irUsername, const QString& irPassword,
							int iSecurityLevel, const QDate& irEndDate, const QString& irComment);
	void		onUserDeleted(long iUserId);
	void		onListRefreshed();

signals:
	void		returnMsg(AReqType, const QString&);
	void		windowClosed(AAppClient*);
	void		listRefreshResult(long iRetValue, const QString& irOut);
	void		userAddResult(long iRetValue);
	void		userUpdateResult(long iRetValue);
	void		userDeleteResult(long iRetValue);
	void		monitorResult(int iResType, const QString& irOut);
	void		findPatternResult(bool iResult);

protected:
	void		closeEvent(QCloseEvent*);

private slots:
	void		onClearAll();
	void		onCopy();
	void		onCurrentTabChanged(int iTabIdx);
	void		onCut();
	void		onEditPreferences();
	void		onFind();
	void		onFindPattern(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord);
	void		onIntervalChanged();
	void		onLogEnabled(AReqType iLogType, long iMinLevel, bool iOn);
	void		onMonitorTimeout();
	void		onPaste();
	void		onPrint();
	void		onRedo();
	void		onReplace();
	void		onUndo();

private:
	void		createActions();
	void		createMenus();
	void		setLogLevel(AReqType iLogType, long iMinLevel, bool iOn);

	AAppClient*	 cpAppClient;
	AMainWindow* cpParent;
	AReturnRcvr* cpRcvr;
	QString		 cMt;
	QTimer*      cpMonitorTimer;
	ASysResMonPage* cpSysResMonPage;

	APage*		cpCurPage;
	APage*		cpServerPages[SVR_PAGES];

	QMenu*		cpEditMenu;
	AToolList	cEditTools;

	QAction*	cpCopyAct;
	QAction*	cpCutAct;
	QAction*	cpEditPrefAct;
	QAction*	cpFindAct;
	QAction*	cpPasteAct;
	QAction*	cpPrintAct;
	QAction*	cpReplaceAct;
	QAction*	cpRedoAct;
	QAction*	cpUndoAct;

	AOperationsDialog*	cpFindDialog;
};

#endif // ASERVERFORM_H

