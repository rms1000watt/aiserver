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
aisdev/aforms/aserverform.cpp
														  Server Form

CHANGE HISTORY
Version	Date		Who		Change
3.2009   5/08/2008	fchua	Added getCabinetParameters, getDebugParameters.
3.2008	 4/08/2008	fchua	Added setParameters. Updated constructor to use ATextEdit parameters.
3.2006	 3/10/2008	fchua	Added onFindPattern.
3.2006	 3/10/2008	fchua	Reimplemented onFind to fix dialog focus issue.
3.2005	 2/18/2008	fchua	Added support for System Resource Monitor page.
3.2002	 2/04/2008	fchua	Added support for User Access Log page.
3.2001	 1/27/2008	fchua	Added support for User Management page.
3.1004	11/2/2007	fchua	Modified closeEvent(). Added onReturnMsg(). Fixed Doxygen documentation.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0104	9/8/2006	tlw		Remove support for MinLevelSpinBox which is not used.
1.0070	10/8/2005	tlw		Convert to Qt4.
1.0067	 7/22/2005	tlw		CloseConnection on closeEvent.
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QtDebug>
#include <QtGui/QAction>
#include <QtGui/QMessageBox>
#include <QtGui/QMenu>
#include <Qt/qevent.h>

#include "appclient.h"		// AAppClient
#include "alogpage.h"
#include "ausermgmtpage.h"
#include "asysresmonpage.h"
#include "amainwindow.h"
#include "aserverform.h"	// AServerForm
#include "aoperationsdialog.h"

//	-------------------------------------------------- CLASS METHODS ----------------------------------------------------------
/*!
 * \brief Constructor instantiates the log pages to this tabbed widget.
 *
 * \param[in] ipAppClient -> Application client that forwards commands to the AIS server.
 * \param[in] ipParent -> Parent widget that created this instance of AServerForm.
 * \param[in] ipServerName -> Name assigned to this instance of the dialog.
 */
AServerForm::AServerForm(AAppClient* ipAppClient, AMainWindow* ipParent, const char* ipServerName)
	:	AForm((QWidget*)ipParent, ipServerName), cpAppClient(ipAppClient), cpParent(ipParent), cMt(""),
	    cpMonitorTimer(0), cpSysResMonPage(0), cpFindDialog(0)
{
	// Configure. Configure Tab Widget.
	setObjectName(QString(ipServerName));
	setWindowTitle(ipServerName);
	setAttribute(Qt::WA_DeleteOnClose);
	resize(QSize(640, 480).expandedTo(minimumSizeHint()));
	createActions();
	createMenus();

	// Connect. Initialize connections.
	connect(this, SIGNAL(currentChanged(int)), this, SLOT(onCurrentTabChanged(int)));

	// Pages. Add log pages to the form.
	ALogPage* apMonPage = NULL;
	AParameters* apTextEditParams = ipParent->getLogParameters();
	cpServerPages[0] = apMonPage = new ALogPage(geLogAll, this, this, "Monitor", apTextEditParams);
	addTab(apMonPage, QString("Monitor"));
	connect(apMonPage, SIGNAL(clearAll()), this, SLOT(onClearAll()));
	connect(this, SIGNAL(returnMsg(AReqType, const QString&)), apMonPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);

	ALogPage* apLogPage = NULL;
	cpServerPages[1] = apLogPage = new ALogPage(geLogAmp, this, this, "AMP Log", apTextEditParams);
	addTab(apLogPage, QString("AMP Log"));
	connect(apMonPage, SIGNAL(logEnabled(AReqType, long, bool)),	apLogPage, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(logEnabled(AReqType, long, bool)),	this, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(returnMsg(AReqType, const QString&)), apMonPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);
	connect(this, SIGNAL(returnMsg(AReqType, const QString&)), apLogPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);

	cpServerPages[2] = apLogPage = new ALogPage(geLogConsole, this, this, "Console Log", apTextEditParams);
	addTab(apLogPage, QString("Console Log"));
	connect(apMonPage, SIGNAL(logEnabled(AReqType, long, bool)),	apLogPage, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(logEnabled(AReqType, long, bool)),	this, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(returnMsg(AReqType, const QString&)), apMonPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);
	connect(this, SIGNAL(returnMsg(AReqType, const QString&)),	apLogPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);

	cpServerPages[3] = apLogPage = new ALogPage(geLogReqHdr, this, this, "Request Headers", apTextEditParams);
	addTab(apLogPage, QString("Request Headers"));
	connect(apMonPage, SIGNAL(logEnabled(AReqType, long, bool)),	apLogPage, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(logEnabled(AReqType, long, bool)),	this, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(returnMsg(AReqType, const QString&)), apMonPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);
	connect(this, SIGNAL(returnMsg(AReqType, const QString&)), apLogPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);

	cpServerPages[4] = apLogPage = new ALogPage(geLogSysMsg, this, this, "System Messages", apTextEditParams);
	addTab(apLogPage, QString("System Messages"));
	connect(apMonPage, SIGNAL(logEnabled(AReqType, long, bool)),	apLogPage, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(logEnabled(AReqType, long, bool)), this, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(returnMsg(AReqType, const QString&)), apMonPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);
	connect(this, SIGNAL(returnMsg(AReqType, const QString&)), apLogPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);

	cpServerPages[5] = apLogPage = new ALogPage(geLogUserAccess, this, this, "User Access", apTextEditParams);
	addTab(apLogPage, QString("User Access"));
	connect(apMonPage, SIGNAL(logEnabled(AReqType, long, bool)),	apLogPage, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(logEnabled(AReqType, long, bool)), this, SLOT(onLogEnabled(AReqType, long, bool))
	, Qt::DirectConnection);
	connect(apLogPage, SIGNAL(returnMsg(AReqType, const QString&)), apMonPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);
	connect(this, SIGNAL(returnMsg(AReqType, const QString&)), apLogPage, SLOT(onReturnMsg(AReqType, const QString&))
	, Qt::DirectConnection);

	AUserMgmtPage* apUserMgmtPage = 0;
	cpServerPages[6] = apUserMgmtPage = new AUserMgmtPage(this, this);
	addTab(apUserMgmtPage, QString("User Management"));
	connect(apUserMgmtPage, SIGNAL(userAdded(const QString&, const QString&,
		int, const QDate&, const QString&)), 
		this, SLOT(onUserAdded(const QString&, const QString&,
		int, const QDate&, const QString&)), Qt::DirectConnection);
	connect(apUserMgmtPage, SIGNAL(userUpdated(long, const QString&, const QString&, 
		int, const QDate&, const QString&)), 
		this, SLOT(onUserUpdated(long, const QString&, const QString&, 
		int, const QDate&, const QString&)), Qt::DirectConnection);	
	connect(apUserMgmtPage, SIGNAL(userDeleted(long)), 
		this, SLOT(onUserDeleted(long)), Qt::DirectConnection);
	connect(apUserMgmtPage, SIGNAL(listRefreshed()), 
		this, SLOT(onListRefreshed()), Qt::DirectConnection);
	connect(this, SIGNAL(listRefreshResult(long, const QString&)),
		apUserMgmtPage, SLOT(onListRefreshResult(long, const QString&)), Qt::DirectConnection);
	connect(this, SIGNAL(userAddResult(long)),
		apUserMgmtPage, SLOT(onUserAddResult(long)), Qt::DirectConnection);
	connect(this, SIGNAL(userUpdateResult(long)),
		apUserMgmtPage, SLOT(onUserUpdateResult(long)), Qt::DirectConnection);
	connect(this, SIGNAL(userDeleteResult(long)),
		apUserMgmtPage, SLOT(onUserDeleteResult(long)), Qt::DirectConnection);

	cpServerPages[7] = cpSysResMonPage = new ASysResMonPage(this, this);
	addTab(cpSysResMonPage, QString("System Monitor"));
	connect(cpSysResMonPage, SIGNAL(monitorIntervalChanged()), this, SLOT(onIntervalChanged()));
	connect(this, SIGNAL(monitorResult(int, const QString&)), cpSysResMonPage, SLOT(onMonitorResult(int, const QString&)));

	//cpCurPage = apLogPage;
	//setCurrentIndex(0);					// Begin at the beginning

	// create timer object for resource monitor
	cpMonitorTimer = new QTimer(this);
	connect(cpMonitorTimer, SIGNAL(timeout()), this, SLOT(onMonitorTimeout()));
	cpMonitorTimer->start(cpSysResMonPage->getMonitorInterval() * 1000);

	// Initialize. Initialize the pages after all of the connections have been established and connection made to server.
	for (long aPage = 0; aPage < SVR_PAGES; ++aPage)
	{
		apLogPage = dynamic_cast<ALogPage*>(cpServerPages[aPage]);
		if (apLogPage != NULL)
			apLogPage->init();
	}

	apUserMgmtPage->show();
	onCurrentTabChanged(0);
}

/*!
 * \brief Reimplemented from QWidget to close this form.
 *
 * If user closes form, a windowClosed() signal is emitted, which can be caught by AConnectMgr to perform
 * the necessary clean-up operations for the AAppClient instance associated with this object.
 *
 * \param[in] ipEv QCloseEvent object pointer.
 *
 */
void AServerForm::closeEvent(QCloseEvent* ipEv)
{
	if (gAis.mpInProcSvr == NULL)
	{
		QMessageBox aMsgBox(QMessageBox::Question,
							"Confirm Server Form Close", 
							"Are you sure you want to close the server form?",
							(QMessageBox::Yes | QMessageBox::No),
							this);
		if (aMsgBox.exec() == QMessageBox::Yes)
		{
			cpMonitorTimer->stop();
			emit windowClosed(cpAppClient);
			emit formClosed(true);
			ipEv->accept();
		}
		else
		{
			emit formClosed(false);
			ipEv->ignore();
		}
	}
	else
	{
		emit windowClosed(cpAppClient);
		emit formClosed(true);
		ipEv->accept();
	}
}

/*!
 * \brief Inherited from the AReturnRcvr abstract base class.
 *
 * \param[in] iConnectId The ID of the connection that was closed.
 */
bool AServerForm::connectionClosed(long iConnectId)
{
    Q_UNUSED(iConnectId);

	return false;
}

/*!
 * \brief Send console output to the Console log page for display.
 *
 * \param[in] irOut Message to be displayed.
 */
void AServerForm::consoleOutput(const QString& irOut)
{
	emit returnMsg(geLogConsole, irOut);
}

void AServerForm::createActions()
{
	cpCopyAct = new QAction(QIcon(":/images/editcopy.png"), tr("Copy"), this);
	cpCopyAct->setShortcut(tr("Ctrl+C"));
	cpCopyAct->setStatusTip("Copy highlighted text to the clipboard");
	cpCopyAct->setToolTip("Copy Selection");
	connect(cpCopyAct, SIGNAL(triggered()), this, SLOT(onCopy()));

	cpCutAct = new QAction(QIcon(":/images/editcut.png"), tr("Cut"), this);
	cpCutAct->setShortcut(tr("Ctrl+X"));
	cpCutAct->setStatusTip("Copy highlighted text to clipboard, then delete text");
	cpCutAct->setToolTip("Cut Selection");
	connect(cpCutAct, SIGNAL(triggered()), this, SLOT(onCut()));

	cpFindAct = new QAction(QIcon(":/images/editfind.png"), tr("Find..."), this);
	cpFindAct->setShortcut(tr("Ctrl+F"));
	cpFindAct->setStatusTip("Open find dialog to search document for a match to a pattern.");
	cpFindAct->setToolTip("Find Dialog");
	connect(cpFindAct, SIGNAL(triggered()), this, SLOT(onFind()));

	cpPasteAct = new QAction(QIcon(":/images/editpaste.png"), tr("Paste"), this);
	cpPasteAct->setShortcut(tr("Ctrl+V"));
	cpPasteAct->setStatusTip("Paste the clipboard into the current document");
	cpPasteAct->setToolTip("Paste");
	connect(cpPasteAct, SIGNAL(triggered()), this, SLOT(onPaste()));

	cpPrintAct = new QAction(QIcon(":/images/editprint.png"), tr("Print"), this);
	cpPrintAct->setShortcut(tr("Alt+P"));
	cpPrintAct->setStatusTip("Print the current document on the default printer.");
	cpPrintAct->setToolTip("Print Document");
	connect(cpPrintAct, SIGNAL(triggered()), this, SLOT(onPrint()));

	cpRedoAct = new QAction(QIcon(":/images/editredo.png"), tr("Redo"), this);
	cpRedoAct->setShortcut(tr("Ctrl+Y"));
	cpRedoAct->setStatusTip("Reverse the last undo operation.");
	cpRedoAct->setToolTip("Redo");
	connect(cpRedoAct, SIGNAL(triggered()), this, SLOT(onRedo()));

	cpReplaceAct = new QAction( tr("Replace..."), this);
	cpReplaceAct->setShortcut(tr("Ctrl+R"));
	connect(cpReplaceAct, SIGNAL(triggered()), this, SLOT(onReplace()));

	cpUndoAct = new QAction(QIcon(":/images/editundo.png"), tr("Undo"), this);
	cpUndoAct->setShortcut(tr("Ctrl+Z"));
	cpUndoAct->setStatusTip("Undo the last edit operation.");
	cpUndoAct->setToolTip("Undo");
	connect(cpUndoAct, SIGNAL(triggered()), this, SLOT(onUndo()));

	cpEditPrefAct = new QAction(tr("Edit Preferences"), this);
	cpEditPrefAct->setShortcut(tr("Ctrl+,"));
	cpEditPrefAct->setStatusTip("Edit Preferences Ctrl-,");
	cpEditPrefAct->setToolTip("Change Server Form Preferences");
	connect(cpEditPrefAct, SIGNAL(triggered()), this, SLOT(onEditPreferences()));
}

// Create Form-level menus here.
void AServerForm::createMenus()
{
	cpEditMenu = new QMenu(QString("Edit"), this);
	cpEditMenu->addAction(cpFindAct);
	cpEditMenu->addAction(cpReplaceAct);
	cpEditMenu->addSeparator();
	cpEditMenu->addAction(cpCutAct);
	cpEditMenu->addAction(cpCopyAct);
	cpEditMenu->addAction(cpPasteAct);
	cpEditMenu->addSeparator();
	cpEditMenu->addAction(cpUndoAct);
	cpEditMenu->addAction(cpRedoAct);
	cpEditMenu->addSeparator();
	cpEditMenu->addAction(cpPrintAct);
	cpEditMenu->addSeparator();
	cpEditMenu->addAction(cpEditPrefAct);
	cEditTools << cpFindAct << cpCutAct << cpCopyAct << cpPasteAct << cpUndoAct << cpRedoAct << cpPrintAct;
}

/*!
 * \brief Forward enable/disable notificattions back up to the MainWindow.
 *
 * \param[in] iIsEnabled Enable/disable flag.
 */
void AServerForm::enable(bool iIsEnabled)
{
	cpParent->enable(iIsEnabled);
}

/*!
 * \brief Forward idle/busy notifications back up to the MainWindow.
 *
 * \param[in] iIsEngineIdle Busy/Idle flag.
 */
void AServerForm::engineIdle(bool iIsEngineIdle)
{
	cpParent->engineIdle(iIsEngineIdle);
}

/*!
 * \brief Returns list of menus to be shown in the main menu bar for this form.
 *
 * \note
 * -# Called each time this form gets focus to provide the menus and toolbar icons for this form.
 * -# Reimplemented method supplied by the AForm base class.
 */
void AServerForm::getFormMenus(AMenuList& orFormMenus, AMenuList& orTabMenus, AToolList& orFormTools, AToolList& orTabTools)
{
	// Edit. Add a Server Form menu. No tab menus yet.
	orFormMenus.clear();
	orFormMenus << cpEditMenu;
	orTabMenus.clear();
	orFormTools = cEditTools;
	orTabTools.clear();
}

void AServerForm::onClearAll()
{
	ALogPage* apLogPage = 0;
	for (long aPage = 0; aPage < SVR_PAGES; ++aPage)
	{
		apLogPage = dynamic_cast<ALogPage*>(cpServerPages[aPage]);
		if (apLogPage != NULL)
			apLogPage->onClear();
	}
}

void AServerForm::onCopy()
{
	cpCurPage->copy();
}

void AServerForm::onCurrentTabChanged(int iTabIdx)
{
	cpCurPage = dynamic_cast<APage*>(widget(iTabIdx));
}

void AServerForm::onCut()
{
	cpCurPage->cut();
}

/*!
 * \brief Emits an editPreferences signal.
 */
void AServerForm::onEditPreferences()
{
	emit editPreferences();
}

void AServerForm::onFind()
{
	ALogPage* apLogPage = NULL;
	apLogPage = dynamic_cast<ALogPage*>(cpCurPage);

	if (apLogPage == NULL)
	{
		statusAlert("Find operation not supported for this tab");
	}
	else
	{
		// Display find dialog
		if (cpFindDialog == NULL)
		{
			cpFindDialog = ATextEdit::createFindDialog(this, "Find Dialog");

			// Attach some signals/slots
			connect(cpFindDialog, SIGNAL(findPattern(const QString&, bool, bool, bool, bool, bool, bool)),
					this, SLOT(onFindPattern(const QString&, bool, bool, bool, bool, bool, bool)));
			connect(this, SIGNAL(findPatternResult(bool)),
					cpFindDialog, SLOT(onFindPatternResult(bool)));
		}

		cpFindDialog->init(NULL, apLogPage->getTextEdit());
		cpFindDialog->showOnTop();
	}
}

void AServerForm::onFindPattern(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord)
{
	ALogPage* apLogPage = NULL;
	ATextEdit* apTextEdit = NULL;

	apLogPage = dynamic_cast<ALogPage*>(cpCurPage);

	if (apLogPage != NULL)
	{
		apTextEdit = apLogPage->getTextEdit();

		if (apTextEdit != NULL)
			emit findPatternResult(apTextEdit->editFind(irPattern, iAll, iMatchCase, iDown, iRegExp, iSelect, iMatchWord));
	}
}

/*!
 * \brief Invoked when the monitor interval was changed.
 */
void AServerForm::onIntervalChanged()
{
	cpMonitorTimer->start(cpSysResMonPage->getMonitorInterval() * 1000);
}

/*!
 * \brief Invoked when the user list is refreshed.
 */
void AServerForm::onListRefreshed()
{
	cpAppClient->getUsers(this);
}

/*!
 * \brief Invoked when a certain log type is enabled or disabled.
 */
void AServerForm::onLogEnabled(AReqType iLogType, long iMinLevel, bool iOn)
{
	setLogLevel(iLogType, iMinLevel, iOn);
}

/*!
 * \brief Invoked when the monitor time interval expires.
 */
void AServerForm::onMonitorTimeout()
{
	if (cpSysResMonPage->isResourceMonitorEnabled(geGetConnectionStats))
		cpAppClient->getConnectionStats(this);

	if (cpSysResMonPage->isResourceMonitorEnabled(geGetLogonStats))
		cpAppClient->getLogonStats(this);

	if (cpSysResMonPage->isResourceMonitorEnabled(geGetSessionStats))
		cpAppClient->getSessionStats(this);

	if (cpSysResMonPage->isResourceMonitorEnabled(geGetRequestStats))
		cpAppClient->getRequestStats(this);
}

void AServerForm::onPaste()
{
	cpCurPage->paste();
}

void AServerForm::onPrint()
{
	cpCurPage->print();
}

void AServerForm::onRedo()
{
	cpCurPage->redo();
}

void AServerForm::onReplace()
{
	cpCurPage->replace();
}

/*!
 * \brief Slot function that handles logging requests.
 *
 * \param[in] iReqType Log request type.
 * \param[in] irOut Log data.
 */
void AServerForm::onReturnMsg(AReqType iReqType, const QString &irOut)
{
	emit returnMsg(iReqType, irOut);
}

/*!
 * \brief Slot function that handles undo.
 */
void AServerForm::onUndo()
{
	cpCurPage->undo();
}

/*!
 * \brief Slot function that handles user addition requests.
 *
 * \param[in] irUsername Username.
 * \param[in] irPassword Password.
 * \param[in] iSecurityLevel Security Level.
 * \param[in] irEndDate End date.
 * \param[in] irComment Comment.
 */
void AServerForm::onUserAdded(const QString& irUsername, const QString& irPassword,
						int iSecurityLevel, const QDate& irEndDate, const QString& irComment)
{
	cpAppClient->addUser(this, irUsername, irPassword, iSecurityLevel, irEndDate, irComment);
}

/*!
 * \brief Slot function that handles user modification requests.
 *
 * \param[in] iUserId User Id.
 * \param[in] irUsername Username.
 * \param[in] irPassword Password.
 * \param[in] iSecurityLevel Security Level.
 * \param[in] irEndDate End date.
 * \param[in] irComment Comment.
 */
void AServerForm::onUserUpdated(long iUserId, const QString& irUsername, const QString& irPassword,
						int iSecurityLevel, const QDate& irEndDate, const QString& irComment)
{
	cpAppClient->updateUser(this, iUserId, irUsername, irPassword, iSecurityLevel, irEndDate, irComment);
}

/*!
 * \brief Slot function that handles user deletion requests.
 *
 * \param[in] iUserId User Id.
 */
void AServerForm::onUserDeleted(long iUserId)
{
	cpAppClient->deleteUser(this, iUserId);
}

/*!
 * \brief Update the task state and then process the returned information from the AIS server.
 *
 * \param[in] iDummyId Placeholder (used in other parts of AIS for the connectID).
 * \param[in] iXId An incrementing integer assigned by appclient to each outgoing request to the server.
 * \param[in] iStatus Zero or a positive error code if an error was generated by the server.
 * \param[in] iReqType Enum describing the type of response or pushed data from the server.
 * \param[in] iRetValue Integer return value (defaults to zero).
 * \param[in] irOut Returned message (defaults to an empty string).
 * \param[in] ipData Binary buffer used to hold serialized object closure (not used here).
 * \param[in] iDataSize Binary buffer size in bytes (not used here).
 * \param[in] irDisplay Return from writeln and display (defaults to an empty string).
 * \param[in] irError Returned error message (defaults to an empty string).
 * \param[in] iClientData Optional string submitted with a request and returned verbatim with the response.
 *
 * \note
 * -# If completing a request for a task, move to the next request in the task.
 * -# If last request of a task is completed, move to next task for this job.
 * -# Process results returned or forward results to requesting form for processing.
 */
void AServerForm::returnOutput(long iDummyId, long iXId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iDummyId);
    Q_UNUSED(iXId);
    Q_UNUSED(iStatus);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(iClientData);

    // Process events returned by AAppClient
	if (!irDisplay.isEmpty())
	{	qDebug() << "ServerForm::event, " << irDisplay;
		gAis.beep();
	}
	QString aOut(irDisplay + irOut);
	cpParent->statusMsg("");

	switch(iReqType)
	{
	// Close up shop
	case geCloseConnection:	
		if (cpAppClient != NULL)
			cpAppClient = NULL;
		close();
		break;

	// User Management Responses
	case geAddUser:
		if (iRetValue == 0)
			cpParent->statusAlert("Add user successful");
		else
			cpParent->statusAlert("Error: " + irError);
		emit userAddResult(iRetValue);
		break;
	case geDeleteUser:
		if (iRetValue == 0)
			cpParent->statusAlert("Delete user successful");
		else
			cpParent->statusAlert("Error: " + irError);
		emit userDeleteResult(iRetValue);
		break;
	case geGetUsers:
		if (iRetValue != 0)
			cpParent->statusAlert("Error: " + irError);
		emit listRefreshResult(iRetValue, irOut);
		break;
	case geUpdateUser:
		if (iRetValue == 0)
			cpParent->statusAlert("Update user successful");
		else
			cpParent->statusAlert("Error: " + irError);
		emit userUpdateResult(iRetValue);
		break;

	// System Resource Monitor Responses
	case geGetConnectionStats:
		emit monitorResult(geGetConnectionStats, irOut);
		break;
	case geGetLogonStats:
		emit monitorResult(geGetLogonStats, irOut);
		break;
	case geGetSessionStats:
		emit monitorResult(geGetSessionStats, irOut);
		break;
	case geGetRequestStats:
		emit monitorResult(geGetRequestStats, irOut);
		break;

	// Forward these messages to their respective log pages
	case geFcnError:
	case geLogAll:
	case geLogAmp:
	case geLogConsole:
	case geLogReqHdr:
	case geLogSysMsg:
	case geLogUserAccess:
		emit returnMsg(iReqType, aOut);
		break;
	case geLogStatus:
		cpParent->statusAlert(aOut);
		break;

	// No post-return processing required.
	case geOpenConnection:
	case geSetLogLvl:
	case geSetRules:
	case geSetSubscriptions:
		break;
	// We do not expect any other types of requests.
	default:
		aOut.prepend("AServerForm.returnOutput(), Unrecognized request.");
		cpParent->statusAlert(aOut);
		break;
	}
}

/*!
 * \brief Inherited from the AReturnRcvr abstract base class.
 *
 * \param[in] iConnectId The ID of the connection for this context.
 * \param[in] irContextName The new context name.
 */
void AServerForm::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irContextName);
}

/*!
 * \brief Update the cursor row/column in the status bar to the cursor position in the
 * currently active text pane.
 *
 * \param[in] iCol Column position starting from column zero on the left.
 * \param[in] iRow Row position starting from row zero at the top of the text.
 */
void AServerForm::setCursorLabel(long iCol, long iRow)
{
	cpParent->setCursorLabel(iCol, iRow);
}

/*!
 * \brief Set the minimum warning level for messages to a specified log.
 *
 * \param[in] iLogType Type of log ReqHdrs, AMP, System.
 */
void AServerForm::setLogLevel(AReqType iLogType, long iMinLevel, bool iOn)
{
	// Request messages for all sessions.
	long aXid;
	AErrLvl aMinLevel = (iOn) ? (AErrLvl)iMinLevel : geFatal;
	// Default to geSoftware.
	if (aMinLevel <= geNone)
		aMinLevel = geSoftware;
    aXid = cpAppClient->setLogLvl(this, iLogType, aMinLevel);
}

/*!
 * \brief Update the look and feel of all log pages.
 *
 * \param[in] ipConsole Not used.
 * \param[in] ipEditor Not used.
 * \param[in] ipLog Contains the new Font, Font Size, Line Number and Word Wrap settings.
 */
void AServerForm::setParameters(AParameters* ipConsole, AParameters* ipEditor, AParameters* ipLog, AParameters* ipCabinet, AParameters* ipDebug)
{
    Q_UNUSED(ipConsole);
    Q_UNUSED(ipEditor);
    Q_UNUSED(ipCabinet);
    Q_UNUSED(ipDebug);

	// Get old log parameters
	AParameters* apLogParams = cpParent->getLogParameters();
	ATextEdit* apLogTextEdit = 0;
	ALogPage* apLogPage = 0;

	if (ipLog != NULL && apLogParams != NULL)
	{
		// Loop through each log page instance
		for (long i = 0; i < SVR_LOG_PAGES; i++)
		{
			apLogPage = dynamic_cast<ALogPage*>(cpServerPages[i]);
			if (apLogPage != 0)
			{
				apLogTextEdit = apLogPage->getTextEdit();
				
				// Update modified settings only
				if (apLogParams->mFont != ipLog->mFont)
					apLogTextEdit->editSetParameter("Font", QVariant(ipLog->mFont));

				if (apLogParams->mFontSize != ipLog->mFontSize)
					apLogTextEdit->editSetParameter("FontSize", QVariant((int)ipLog->mFontSize));

				if (apLogParams->mLineNumbers != ipLog->mLineNumbers)
					apLogTextEdit->editSetParameter("LineNumbers", QVariant(ipLog->mLineNumbers));

				if (apLogParams->mWrap != ipLog->mWrap)
					apLogTextEdit->editSetParameter("WordWrap", QVariant(ipLog->mWrap));
			}
		}
	}
}

/*!
 * \brief Start up an instance of the Server Form.
 *
 * \param[in] irStartupMsgs	Pending system messages waiting to be displayed.
 * \note
 * -# Called from Connect Manager when this form is first instantiated.
 * -# Reimplemented method supplied by the AForm base class.
 */
void AServerForm::startup(const QString& irStartupMsgs)
{
	// Rules. GblClientServerFormRules is a comma-delimited list of rules of the form: ContextName.ProtocolName
	long aXid;
	if (gAis.mGblParams.contains("gblclientserverformrules"))
	{	QString aRules = gAis.mGblParams["gblclientserverformrules"];
		if (!aRules.isEmpty())
			aXid = cpAppClient->setRules(this, aRules);
	}
	// StartupMsgs. Display messages in SystemMsg page
	returnOutput(0/*DummyId*/, 0/*XId*/, 0/*Status*/, geLogSysMsg, 0/*Value*/, irStartupMsgs, NULL, 0, cMt/*Display*/,cMt);
}

/*!
 * \brief Show urgent message in the permanent section of the status bar.
 *
 * \param[in] irMsg Message to be displayed in the status bar.
 */
void AServerForm::statusAlert(const QString& irMsg)
{
	cpParent->statusAlert(irMsg);
}

// end
