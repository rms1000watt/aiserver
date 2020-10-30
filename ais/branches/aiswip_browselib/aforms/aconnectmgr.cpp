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
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/webide/aconnectmgr.cpp
													CONNECTION MANAGER

AConnectMgr manages the Server connections, Contexts, Sessions and forms.

CHANGE HISTORY
Version	Date		Who		Change
4.0002	 9/28/2008	tlw		CR128. Add subscribe to allow a connection subscribe to a session.
4.0003	 7/02/2008	fchua	[CR-122] Fixed Start/Stop context functions.
4.0002	 6/30/2008	fchua	[CR-114] Enabled automatic resizing of columns.
3.2002	 2/04/2008	fchua	Fixed support for reconnecting to In-Process server.
3.2002	 2/04/2008	fchua	Added closeAllConnections, isReadyToClose, onAllConnectionsClosed.
3.1007	 1/04/2008	fchua	onConnectSession and onCloseSession. Check for "admin" user instead of system label.
3.1006	12/21/2007	fchua	setEnable. Added enabling/disabling of new connect button.
3.1006	12/21/2007	fchua	onConnectSession. Updated function implementation.
3.1006	12/21/2007	fchua	onCloseSession. Updated function implementation.
3.1006	12/21/2007	fchua	launchTask. Updated function implementation.
3.1006	12/21/2007	fchua	launchConnectSession. Updated function implementation.
3.1006	12/21/2007	fchua	Added new member function launchCloseSession.
3.1006	12/21/2007	fchua	connectSession. Added irContextName argument and updated function implementation.
3.1006	12/21/2007	fchua	closeSession. Added function implementation.
3.1006	12/21/2007	fchua	AConnectMgr. Added signal-slot connection to the connect button.
3.1004	11/12/2007	fchua	Removed unnecessary icon setting in onOpenSessionForm(). Fixed Doxygen documentation.
3.1004	10/31/2007	fchua	Added slot function onServerFormClosed() to handle closing of the server form.
3.1004	10/30/2007	fchua	Added association of AAppClient and AServerForm for logging functionality.
3.1004	10/29/2007	fchua	Modified onOpenSessionForm. Fixed crash when 2nd column is selected.
3.1003	10/23/2007	fchua	Modified AConnectMgr. Connected signal to catch keyboard selection.
3.1002	10/22/2007	fchua	Modified setEnable. Fixed some Doxygen documentation.
2.0002	 1/25/2007	tlw		~AConnectMgr. Set DefaultRcvr to NULL for all defunct server connections.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0112	11/ 2/2006	tlw		cServerList, cSessionList. Release allocated items.
1.0111	10/25/2006	tlw		Rename AAppClient methods.
1.0067	 7/20/2005	tlw		Disallow operations on SystemContext. OpenSession, CloseSession???
1.0064	 6/ 3/2005	tlw		Open connect manager dialog if no local server.
1.0063	 5/26/2005	tlw		Move all GUI elements out of webide into aforms.
1.0059	 4/22/2005	tlw		Remove disconnect session
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QEvent>
#include <QtCore/QtDebug>
#include <QtCore/QModelIndex>
#include <QtGui/QMessageBox>
#include <QtGui/QStandardItemModel>
#include <QtGui/QTreeView>
#include <QtGui/QHeaderView>

#include "aglobals.h"
#include "alogmgr.h"
#include "aconnectmgr.h"
#include "amainwindow.h"
#include "aserverdialog.h"
#include "appclient.h"
#include "alogondialog.h"
#include "aserverform.h"
#include "asessionform.h"
#include "autilities.h"
#include "aclosedialog.h"

//	-------------------------------------------------- DATA STRUCTURES --------------------------------------------------------


//	------------------------------------------------------ METHODS ------------------------------------------------------------
/*!
 * \brief Constructor Configures the tree view and the command buttons for the connection manager.
 *
 * The connection manager manages the connections between the client (GUI) and one or more servers.  It also manages the contexts
 * and the sessions running on a server.  This dialog is launched from the main window of the IDE.
 *
 * \param[in] ipMainWindow -> Parent main window that created this instance of the dialog.
 * \param[in] ipName -> Name assigned to this instance of the dialog.
 * \param[in] iFlgs Optional flags to set the disposition of this dialog.
 */
AConnectMgr::AConnectMgr(AMainWindow* ipMainWindow, const char* ipName, Qt::WFlags iFlgs) : QDialog((QWidget*)ipMainWindow,iFlgs), cAllConnectionsClosed(false)
{
	cUi.setupUi(this);
	setObjectName(ipName);
	cpMainWindow = ipMainWindow;
	ceListLvl = emNone;

	// Server Model. Configure the server model to feed the server table view
	cpServerModel = new AServerTreeModel();
	cUi.upServerTreeView->setModel(cpServerModel);

	// Tree View. Configure the tree view
	cUi.upServerTreeView->setEditTriggers(QAbstractItemView::NoEditTriggers);
	cUi.upServerTreeView->setSelectionBehavior(QAbstractItemView::SelectRows);
	cUi.upServerTreeView->setSelectionMode(QAbstractItemView::SingleSelection);
	cUi.upServerTreeView->setItemsExpandable(true);
	cUi.upServerTreeView->setEditTriggers(QAbstractItemView::DoubleClicked);
	cUi.upServerTreeView->header()->setResizeMode(QHeaderView::ResizeToContents);

	// Set up connections
	connect(cUi.upAddServerButton, SIGNAL(clicked()), this, SLOT(onAddServer()));
	connect(cUi.upDeleteServerButton,SIGNAL(clicked()), this, SLOT(onDeleteServer()));
	connect(cUi.upEditServerButton,SIGNAL(clicked()), this, SLOT(onEditServer()));
	connect(cUi.upOpenServerButton,SIGNAL(clicked()), this, SLOT(onOpenServerForm()));
	// connect(cUi.upStartContextButton, SIGNAL(clicked()), this, SLOT(onAddContext()));
	connect(cUi.upStartContextButton, SIGNAL(clicked()), this, SLOT(onStartContext()));
	connect(cUi.upStopContextButton, SIGNAL(clicked()), this, SLOT(onStopContext()));
	connect(cUi.upOpenSessionButton, SIGNAL(clicked()), this, SLOT(onOpenSessionForm()));
	connect(cUi.upConnectSessionButton, SIGNAL(clicked()), this, SLOT(onConnectSession()));
	connect(cUi.upCloseSessionButton, SIGNAL(clicked()), this, SLOT(onCloseSession()));
	connect(cUi.upCloseDialogButton, SIGNAL(clicked()), this, SLOT(reject()));

	connect(cUi.upServerTreeView, SIGNAL(expanded(const QModelIndex&)), this, SLOT(onExpanded(const QModelIndex&)));
	connect(cUi.upServerTreeView->selectionModel(), SIGNAL(currentChanged(const QModelIndex &, const QModelIndex &)), this, SLOT(onActivated(const QModelIndex&)));

	connect(cpServerModel, SIGNAL(allConnectionsClosed()), this, SLOT(onAllConnectionsClosed()), Qt::QueuedConnection);

	// Enable selected buttons
	cUi.upCloseDialogButton->setEnabled(true);
	cUi.upAddServerButton ->setEnabled(true);
	setEnable(ceListLvl);

	// Initialize ServerList.
	cMt = "";

	// Initialize dialogs
	cpServerDialog =  new AServerDialog(this,  "Add Server Dialog");
	cpLogonDialog =  new ALogonDialog(this,  "Logon Dialog");
}

/*!
 * \brief Destructor Free allocated resources.
 *
 * \note
 * -# AConnectMgr has remote ownership of ASessionSt and AServerSt referents.
 * -# Since instances of AConnectMgr are never assigned, copied or passed by value, the copy constructor and assignment operator
 * are omitted here.  See C++ FAQS chap 30 for more info.
 */
AConnectMgr::~AConnectMgr()
{
	cTasks.clear();
	cUi.upServerTreeView->setModel(NULL);
	delete cpServerModel;

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AConnectMgr()");
#endif
}


/*!
 * \brief Put title: Message in the status bar at the bottom of the connection mgr dialog.
 *
 * \param[in] ipTitle Prefix added to the message.
 * \param[in] ipMsg Message to be displayed.
 * \param[in] iBell Sound alarm iff true.
 *
 * \note
 * -# Set breakpoint on beep() to stop on errors.
 */
void AConnectMgr::alert(const char* ipTitle, const char* ipMsg, bool iBell)
{
	QString aMsg;
	if (ipTitle != NULL && ipMsg != NULL)
	{	aMsg.sprintf("%s: %s", ipTitle, ipMsg);
		cUi.upStatusLabel->setText(aMsg);
	}
	else
		cUi.upStatusLabel->clear();
	if (iBell)
		qApp->beep();
}

/*!
 * \brief Formally closes all active connections to the AIS server.
 *
 * \note
 * After all connections have been closed, an allConnectionsClosed signal is emitted.
 *
 * \sa AConnectMgr::onAllConnectionsClosed, AConnectMgr::isReadyToClose
 */
void AConnectMgr::closeAllConnections()
{
	if (cpServerModel != NULL)
	{
		cAllConnectionsClosed = false;
		cpServerModel->closeAllConnections();
	}
}

/*!
 * \brief Submit request to AIS to close a context.
 *
 * \param[in] ipServerSt Server to receive request.
 * \param[in] irContextName Name of context to be closed.
 *
 * \return aStatus >= 0 error code.
 *
 * \note
 * launchCloseContext is called as each response is returned back from AIS.
 */
long AConnectMgr::closeContext(AServerSt* ipServerSt, const QString& irContextName)
{
	long aStatus = 0;
	if (irContextName.isEmpty())
		aStatus = AERR_UNKCONTEXTNAME;
	else
	{	ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = irContextName;
		aTaskSt.mArg1 = QString::null;
		aTaskSt.mArgValue = 0;
		aTaskSt.mPendingReq = geUnknown;	// Just getting started
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = ipServerSt;
		aTaskSt.mTask = cneCloseContext;
		cTasks.push(aTaskSt);
		launchCloseContext();
	}
	return aStatus;
}

/*!
 * \brief Close the currently open server form.
 *
 * \note Not yet implemented.
 * \todo Add implementation.
 */
void AConnectMgr::closeServerForm()
{
	// PENDING AConnectMgr::closeServerForm(). Close the currently open server form.
	alert("Close Server Form", "Not yet implemented", true/*Bell*/);
}

/*!
 * \brief Close the currently open session.
 *
 * \note Not yet implemented.
 * \todo Add implementation.
 */
long AConnectMgr::closeSession(AServerSt* ipServerSt, const QString& irContextName, long iSessionId, ACloseMode iCloseMode)
{
	long aStatus = 0;
	if (iSessionId <= 0)
		aStatus = AERR_SESSIONID;
	else
	{	ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = irContextName;
		aTaskSt.mArg1 = gAis.mpCloseMode[iCloseMode];
		aTaskSt.mArgValue = iSessionId;
		aTaskSt.mPendingReq = geUnknown;
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = ipServerSt;
		aTaskSt.mTask = cneCloseSession;
		cTasks.push(aTaskSt);
		launchCloseSession();
	}
	return aStatus;
}

/*!
 * \brief Connection is suddenly closed.
 *
 * If server suddenly goes off-line, ASocket receives a socket error signal which is returned back via Appclient::returnOutput
 * to AConnectMgr::returnOutput which calls this routine.
 *
 * \param[in] irServerName name of the server.
 * \return false
 *
 * \note Not fully implemented.
 * \todo Complete implementation.
 */
bool AConnectMgr::connectionClosed(const char* irServerName)
{
	// PENDING - connectionClosed(). If a session form or a server form is opened on this server, shut down these applications.  We need a way to
	// keep track of the sessionID for each open session form and the server for each open server form so that we can selectively
	// close them when a server becomes disconnected or when a context is stopped.
	// AppClient. Destroy the appclient.
	QModelIndex aServerIndex;
	if (cpServerModel->isServerExist(irServerName,aServerIndex))
	{	cpServerModel->removeServer(aServerIndex);
		return true;
	}
	return false;
}

/*!
 * \brief Connection is suddenly closed.
 *
 * \param[in] iConnectId Connection Id.
 *
 * \note Not yet implemented.
 */
bool AConnectMgr::connectionClosed(long iConnectId)
{
    Q_UNUSED(iConnectId);

	return false;
}

/*!
 * \brief Reconnect to a disconnected session.
 *
 * \param[in] ipServerSt Ptr to server info for target AIS.
 * \param[in] irContextName Name of the context.
 * \param[in] iSessionId ID of the disconnected session.
 *
 * \return aStatus >=0 Error code.
 */
long AConnectMgr::connectSession(AServerSt* ipServerSt, const QString& irContextName, long iSessionId)
{
	long aStatus = 0;
	if (iSessionId <= 0)
		aStatus = AERR_SESSIONID;
	else
	{	ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = irContextName;
		aTaskSt.mArg1 = QString::null;
		aTaskSt.mArgValue = iSessionId;
		aTaskSt.mPendingReq = geUnknown;	// Just getting started
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = ipServerSt;
		aTaskSt.mTask = cneConnectSession;
		cTasks.push(aTaskSt);
		launchConnectSession();
	}
	return aStatus;
}

/*!
 * \brief Called when an item in the connection tree is selected.
 */
void AConnectMgr::currentChanged()
{
	setEnable(ceListLvl);
}


/*!
 * \brief Expand a context in the tree list view.
 *
 * \param[in] irContextIdx Model index.
 */
void AConnectMgr::expandContext(const QModelIndex& irContextIdx)
{
	// Look up info for this server
	
	const QModelIndex& arServerIdx = irContextIdx.parent();

	if (!arServerIdx.isValid())
		alert("Expand Context", "No server selected.", true/*Bell*/);
	else if (!irContextIdx.isValid())
		alert("Expand Context", "No context selected.", true/*Bell*/);
	else
	{	AServerSt* apServerSt = cpServerModel->getServer(arServerIdx);
		QStandardItem* apContextNameColumn = cpServerModel->itemFromIndex(irContextIdx);
		ACnMgrTaskSt aTaskSt;

		aTaskSt.mArg0 = apContextNameColumn->data(Qt::DisplayRole).toString();
		aTaskSt.mArg1 = QString::null;
		aTaskSt.mArgValue = 0;
		aTaskSt.mPendingReq = geUnknown;
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = apServerSt;
		aTaskSt.mTask = cneExpandContext;
		cTasks.push(aTaskSt);
		launchExpandContext();
	}
}

/*!
 * \brief Launch a request to server for list of registered contexts.
 *
 * \param[in] irServerIdx ServerTreeView Index to the server being accessed.
 *
 * \note
 * -# Wait for parent to call processContexts.
 */
void AConnectMgr::expandServer(const QModelIndex& irServerIdx)
{
	// Server. Look up info for the selected server
	if (!irServerIdx.isValid())
		alert("Expand Server", "No server selected.", true/*Bell*/);
	else
	{	AServerSt* apServerSt = cpServerModel->getServer(irServerIdx);
		ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = QString::null;
		aTaskSt.mArg1 = QString::null;
		aTaskSt.mArgValue = 0;
		aTaskSt.mPendingReq = geUnknown;
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = apServerSt;
		aTaskSt.mTask = cneExpandServer;
		cTasks.push(aTaskSt);
		launchExpandServer();
	}
}

/*!
 * \brief Returns true IFF all active connections have already been closed.
 *
 * \sa AConnectMgr::onAllConnectionsClosed()
 */
bool AConnectMgr::isReadyToClose()
{
	return (cAllConnectionsClosed);
}

/*!
 * \brief Close the selected context.
 *
 * \note
 * -# Arg0 holds contextName
 * -# PENDING ConnectMgr::closeContext().  Just stop the context, but leave it registered.
 */
void AConnectMgr::launchCloseContext()
{
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	QString aContextName = arTaskSt.mArg0;
	long aStatus = arTaskSt.mRetStatus;
	long aXid;
	bool aDone = true;

	switch (aReqType)
	{
	case geUnknown:
		// Connect. Connect to a disconnected session
		if (apServerSt->mUsrId > 0)
		{	setEnable(emDisable);
			arTaskSt.mPendingReq = geCloseContext;
			aXid = apServerSt->mpAppClient->onCloseContext(this, arTaskSt.mArg0, geDefault/*CloseMode*/);
			aDone = false;
		}
		break;
	case geCloseContext:
		// CloseContext.  
		if (aStatus <= 0)
		{
			if (apServerSt->mUsrId > 0)
			{	// Launch any outstanding requests
				arTaskSt.mPendingReq = geGetCurrentContexts;
				setEnable(emDisable);
				apServerSt->mpAppClient->onGetCurrentContexts(this);
				aDone = false;
			}
			else
			{
				QModelIndex aServerIndex;
				if (cpServerModel->isServerExist(apServerSt->mServerName,aServerIndex))
					cUi.upServerTreeView->setExpanded(aServerIndex, false/*Expanded*/);
			}
			alert("Close Context", aContextName.toLatin1().data(), false/*Bell*/);
		}
		else
		{
			alert("Close Context", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		}
		break;
	case geGetCurrentContexts:
		// RetOut: new-delimited list of context names
		if (aStatus <= 0)
			processGetCurrentContexts(arTaskSt.mRetOut);
		break;	
	default:
		alert("Close Context. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
		break;
	}
	// Done. Decide what to do next.
	if (aDone)
	{	cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Launch close session.
 */
void AConnectMgr::launchCloseSession()
{
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	long aStatus = arTaskSt.mRetStatus;
	bool aDone = true;

	switch (aReqType)
	{
	case geUnknown:
		// Logon. Logon to server using existing UsrName, Passwd
		if (apServerSt->mUsrId <= 0)
		{	
			QString aUsername;
			QString aPassword;
			arTaskSt.mPendingReq = geLogon;
			
			// If server is In-Process
			if (apServerSt->mServerName == AGLBLS_INPROCSVRNAME)
			{
				aUsername = SYS_USER;
				aPassword = SYS_PASS;
			}
			
			if ((aStatus = logon(apServerSt, aUsername, aPassword)) <= 0)
				aDone = false;
			break;
		}
		// Already logged on. Fall thru.
	case geLogon:
		// Open Connection. Open a new connection to the remote server.
		if (apServerSt->mUsrId > 0)
		{	arTaskSt.mPendingReq = geOpenConnection;
			openConnection(apServerSt);
			aDone = false;
		}
		break;
	case geOpenConnection:
		// OpenSession. Add a session form on the selected context.
		if (apServerSt->mUsrId > 0)
		{
			arTaskSt.mPendingReq = geCloseSession;
			apServerSt->mpAppClient->onCloseSession(this,arTaskSt.mArgValue,gAis.mCloseModes[arTaskSt.mArg1]);
			aDone = false;
		}
		break;
	case geCloseSession:
		if (aStatus > 0)
			alert("Close Session", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		break;
	default:
		alert("Close Session. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
	}

	// Done. Decide what to do next.
	if (aDone)
	{	// This task is complete. Decide what to do next.
		cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();		// Return to application
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Connect to a disconnected session
 */
void AConnectMgr::launchConnectSession()
{
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	long aStatus = arTaskSt.mRetStatus;
	bool aDone = true;

	switch (aReqType)
	{
	case geUnknown:
		// Logon. Log on to the selected context using default UsrName and Passwd
		if (apServerSt->mpAppClient == NULL || apServerSt->mUsrId <= 0)
		{	arTaskSt.mPendingReq = geLogon;
			if ((aStatus = logon(apServerSt, cMt/*UsrName*/, cMt/*Passwd*/)) <= 0)
				aDone = false;
			break;
		}
		// Already logged on. Fall thru.
	case geLogon:
		// Open Connection. Open a new connection to the remote server.
		if (apServerSt->mUsrId > 0)
		{	arTaskSt.mPendingReq = geOpenConnection;
			openConnection(apServerSt);
			aDone = false;
		}
		break;
	case geOpenConnection:
		// OpenSession. Add a session form on the selected context.
		if (apServerSt->mUsrId > 0)
		{	// New Connection. Open a new connection for the session form.
			AAppClient* apAppClient = new AAppClient(apServerSt->mHostDnsIp,apServerSt->mPort,this,this,apServerSt->mServerName.data());

			// Arg0: ContextName, Arg1: startup msgs, ArgValue: RunScript flag
			ASessionForm* apSessionForm = new ASessionForm(apAppClient, cpMainWindow, "SessionForm");
			cpMainWindow->openForm(apSessionForm);
			apSessionForm->startup(arTaskSt.mArg0, false, arTaskSt.mArg1, apServerSt->mUsrName, apServerSt->mPasswd, arTaskSt.mArgValue);
			alert(NULL, NULL, false/*Bell*/);
		}
		// Status. Report error and abort if called task or request failed.
		if (aStatus > 0)
			alert("Open Session Form (Reconnect)", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		break;
	default:
		alert("Connect Session. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
	}

	// Done. Decide what to do next.
	if (aDone)
	{	// This task is complete. Decide what to do next.
		cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();		// Return to application
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Expand the currently selected context.
 */
void AConnectMgr::launchExpandContext()
{
	// Get the current sessions. Arg0 contains context name
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	long aStatus = arTaskSt.mRetStatus;
	long aXid;
	bool aDone = true;

	switch (aReqType)
	{
	case geUnknown:
		// GetSubscriptions. Get a list of sessions from the server.
		if (apServerSt->mUsrId > 0)
		{	setEnable(emDisable);
			arTaskSt.mPendingReq = geGetSessions;
			aXid = apServerSt->mpAppClient->onGetSessions(this, arTaskSt.mArg0);
			aDone = false;
		}
		break;
	case geGetSessions:
		// processGetSessions. Display session information
		// RetOut: newline-delimited list of session records
		if (aStatus <= 0)	
				processGetSessions(arTaskSt.mRetOut);

		// Status. Return message. Abort if called task or request failed.
		if (aStatus > 0)
			alert("Expand Context", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		break;
	default:
		alert("Expand Context. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
		break;

	}
	// Done.  Decide what to do next.
	if (aDone)
	{	cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();		// Return to application
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Expand the currently selected server in the list view.
 */
void AConnectMgr::launchExpandServer()
{
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	long aStatus = arTaskSt.mRetStatus;
	bool aDone = true;

	switch (aReqType)
	{
	case geUnknown:
		// Logon. Logon to server using existing UsrName, Passwd
		if (apServerSt->mUsrId <= 0)
		{	
			QString aUsername;
			QString aPassword;
			arTaskSt.mPendingReq = geLogon;
			
			// If server is In-Process
			if (apServerSt->mServerName == AGLBLS_INPROCSVRNAME)
			{
				aUsername = SYS_USER;
				aPassword = SYS_PASS;
			}
			
			if ((aStatus = logon(apServerSt, aUsername, aPassword)) <= 0)
				aDone = false;
			break;
		}
		// Logged on. Fall thru
	case geLogon:
		// 	GetCurrentContexts. Get a list of registered contexts from the server.
		if (apServerSt->mUsrId > 0)
		{	// Launch any outstanding requests
			arTaskSt.mPendingReq = geGetCurrentContexts;
			setEnable(emDisable);
			apServerSt->mpAppClient->onGetCurrentContexts(this);
			aDone = false;
		}
		else
		{
			QModelIndex aServerIndex;

			if (cpServerModel->isServerExist(apServerSt->mServerName,aServerIndex))
				cUi.upServerTreeView->setExpanded(aServerIndex, false/*Expanded*/);
		}
		break;
	case geGetCurrentContexts:
		// RetOut: new-delimited list of context names
		if (aStatus <= 0)
			processGetCurrentContexts(arTaskSt.mRetOut);
		break;
	default:
		// Status. Report error and abort if called task or request failed.
		if (aStatus > 0)
			alert("Expand Server", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		else
			alert("Expand Server. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
		break;

	}
	// Finished.  Decide what to do next.
	if (aDone)
	{	cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();		// Return to application
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Perform logon operation.
 *
 * \note
 * -# Arg0 holds UsrName, Arg1 holds Passwd, ArgValue: 1 iff a system logon.
 * -# User name and password args may be empty in which case, logon tries the values saved in AServerSt, if any.
 * Else, logon tries the saved values in cUsrName and cPasswd, if any.
 * Else, the user is asked for a user name and password.
 */
void AConnectMgr::launchLogon()
{
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	AAppClient* apAppClient = apServerSt->mpAppClient;
	long aStatus = arTaskSt.mRetStatus, aUsrId;
	long  aXid;
	bool aDone = true;		// True if no pending tasks left

	switch (aReqType)
	{
	case geUnknown:
		if (apServerSt->mUsrId <= 0)
		{	// AppClient. Establish instance of AAppClient to talk to server.
			if (apAppClient == NULL)
			{	apAppClient = new AAppClient(apServerSt->mHostDnsIp,apServerSt->mPort,this,this,apServerSt->mServerName.data());
				apServerSt->mpAppClient = apAppClient;
			}
			// Open Connection. Open a connection to remote server.
			if (!apAppClient->isConnected())
			{	arTaskSt.mPendingReq = geOpenConnection;
				openConnection(apServerSt);
				aDone = false;
				break;
			}
			// Connected. Fall thru.
		}
		else
			break;
	case geOpenConnection:
		// Logon. Logon to the system context for this server.
		if (aStatus == 0)
		{	QString aUsrName(arTaskSt.mArg0);
			QString aPasswd(arTaskSt.mArg1);
			if (aUsrName.isEmpty())
			{	if (!apServerSt->mUsrName.isEmpty())
				{	aUsrName = apServerSt->mUsrName;
					aPasswd = apServerSt->mPasswd;
				}
				else if (!cUsrName.isEmpty())
				{	aUsrName = cUsrName;
					aPasswd = cPasswd;
				}
			}
			// If name/passwd is still not available, get values from the user.
			while (aUsrName.isEmpty())
			{	cpLogonDialog->setValues(arTaskSt.mArg0, arTaskSt.mArg1);
				if (cpLogonDialog->exec() == QDialog::Accepted)
					cpLogonDialog->getValues(aUsrName, aPasswd);
				else	// User selected Cancel
				{	aUsrName.truncate(0);
					break;
				}
			}
			// Create a logon request and launch it. Save submitted UsrName, Passwd.
			if (!aUsrName.isEmpty())
			{	arTaskSt.mPendingReq = geLogon;
				arTaskSt.mArg0 = aUsrName;
				arTaskSt.mArg1 = aPasswd;
				aXid = apAppClient->onLogon(this, aUsrName, aPasswd);
				aDone = false;
			}
			// User Cancelled. aDone is true so task will finish.
		}
		break;
	case geLogon:
		// Process Logon. RetValue: UsrId, RetStatus: status
		if (aStatus == 0 && (aUsrId = arTaskSt.mRetValue) > 0)
		{	apServerSt->mUsrId = aUsrId;
			apServerSt->mUsrName = arTaskSt.mArg0;
			apServerSt->mPasswd  = arTaskSt.mArg1;
		
			// If not a system logon, save the name and password for use on another server
			if (aUsrId > USRMGR_SYSUSRID)
			{	cUsrName = arTaskSt.mArg0;
				cPasswd = arTaskSt.mArg1;
			}
			alert(NULL, NULL, false/*Bell*/);
		}
		else // Alert user if operation failed.  Try again.
		{	QString aUsrName, aPasswd;
			alert("Logon", gAis.mpErrMsgs[aStatus = AERR_BADPASSWD], true/*Bell*/);
			cpLogonDialog->setValues(arTaskSt.mArg0, arTaskSt.mArg1);
			if (cpLogonDialog->exec() == QDialog::Accepted)
			{	cpLogonDialog->getValues(aUsrName, aPasswd);

				// Try the logon again.
				if (!aUsrName.isEmpty())
				{	arTaskSt.mPendingReq = geLogon;
					arTaskSt.mArg0 = aUsrName;
					arTaskSt.mArg1 = aPasswd;
					aXid = apAppClient->onLogon(this, aUsrName, aPasswd);
					aDone = false;
				}
			}
			// else, User selected Cancel, so quit with error status
		}
		break;
	default:
		alert("Logon. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
		break;
	}
	// Done.  Decide what to do next.
	if (aDone)
	{	cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();		// Return to application
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Open a new connection to the named server.
 *
 * \note
 * -# Arg0 holds ServerName
 */
void AConnectMgr::launchOpenConnection()
{
	// Establish connection with the host
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	long aStatus = arTaskSt.mRetStatus;
	bool aDone = true;
	
	switch (aReqType)
	{
	case geUnknown:
		// OpenConnection. Open a connection to remote server
		setEnable(emNone);
		arTaskSt.mPendingReq = geOpenConnection;
		if (!apServerSt->mpAppClient->isConnected())
		{
			alert("Open Connection", "Connecting to server...", false/*Bell*/);
			apServerSt->mpAppClient->onOpenConnection(this);
			aDone = false;			// Attempting to open a connection
			break;
		}
		// Connected. fall thru
	case geOpenConnection:
		// Note connection established; else, report error
		if (aStatus <= 0)
		{
			apServerSt->mpAppClient->setDefaultRcvr(cpServerModel);
			alert("Connected to", arTaskSt.mArg0.toLatin1().data(), false/*Bell*/);
		}
		else
			alert("Open Connection", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		break;
	default:
		alert("Open Connecton. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
		break;
	}
	// Done. Decide what to do next.
	if (aDone)
	{	cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();		// Return to application
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Open the currently selected context.
 *
 * \note
 * -# Arg0 holds the ContextName
 */
void AConnectMgr::launchOpenContext()
{
	// Submit a request to AIS to open a context.
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	QString aContextName = arTaskSt.mArg0;
	long aStatus = arTaskSt.mRetStatus;
	long aXid;
	bool aDone = true;

	switch (aReqType)
	{
	case geUnknown:
		// Logon. Logon to server using existing UsrName, Passwd
		if (apServerSt->mUsrId <= 0)
		{	
			QString aUsername;
			QString aPassword;
			arTaskSt.mPendingReq = geLogon;
			
			// If server is In-Process
			if (apServerSt->mServerName == AGLBLS_INPROCSVRNAME)
			{
				aUsername = SYS_USER;
				aPassword = SYS_PASS;
			}
			
			if ((aStatus = logon(apServerSt, aUsername, aPassword)) <= 0)
				aDone = false;
			break;
		}
		// Logged on. Fall thru.
	case geLogon:
		// OpenContext. Open a context on the currently selected server.
		if (apServerSt->mUsrId > 0)
		{	// Arg0 contains the context name
			arTaskSt.mPendingReq = geOpenContext;
			setEnable(emDisable);
			aXid = apServerSt->mpAppClient->onOpenContext(this, QString()/*StartupPath*/, arTaskSt.mArg0);
			aDone = false;
		}
		break;
	case geOpenContext:
		// ProcessOpenContext. Return results back to calling task.
		// RetOut: ContextName, RetDisplay: startup messages
		if (aStatus <= 0)
		{
			if (apServerSt->mUsrId > 0)
			{	// Launch any outstanding requests
				arTaskSt.mPendingReq = geGetCurrentContexts;
				setEnable(emDisable);
				apServerSt->mpAppClient->onGetCurrentContexts(this);
				aDone = false;
			}
			else
			{
				QModelIndex aServerIndex;
				if (cpServerModel->isServerExist(apServerSt->mServerName,aServerIndex))
					cUi.upServerTreeView->setExpanded(aServerIndex, false/*Expanded*/);
			}
			alert("Open Context", aContextName.toLatin1().data(), false);
		}
		else // Status. Display error message. Abort if called task or request failed.
			alert("Open Context", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		break;
	case geGetCurrentContexts:
		if (aStatus <= 0)
		{
			processGetCurrentContexts(arTaskSt.mRetOut);
		}
		else
			alert("Open Context", gAis.mpErrMsgs[aStatus], true);
		break;
	default:
		alert("Open Context. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
		break;
	}
	if (aDone)
	{	cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Opens a server form.
 *
 * \note
 * -# Arg0 holds StartupMsgs
 */
void AConnectMgr::launchOpenServerForm()
{
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	long aStatus = arTaskSt.mRetStatus;
	bool aDone = true;

	switch (aReqType)
	{
	case geUnknown:
		// Logon. Logon to server using existing UsrName, Passwd
		if (apServerSt->mUsrId <= 0)
		{	
			QString aUsername;
			QString aPassword;
			arTaskSt.mPendingReq = geLogon;
			
			// If server is In-Process
			if (apServerSt->mServerName == AGLBLS_INPROCSVRNAME)
			{
				aUsername = SYS_USER;
				aPassword = SYS_PASS;
			}
			
			if ((aStatus = logon(apServerSt, aUsername, aPassword)) <= 0)
				aDone = false;
			break;
		}
		// Logged on. Fall thru.
	case geLogon:
		// OpenServerForm. Arg0 holds startup messages.
		if (aStatus <= 0)
		{	if (apServerSt->mUsrId > 0)
			{	AAppClient* apAppClient = apServerSt->mpAppClient;
				AServerForm* apServerForm = new AServerForm(apAppClient, cpMainWindow, apServerSt->mServerName.data());
				connect(apAppClient, SIGNAL(returnStuff(AReqType, const QString&)), apServerForm, SLOT(onReturnMsg(AReqType, const QString&)));
				connect(apServerForm, SIGNAL(windowClosed(AAppClient*)), this, SLOT(onServerFormClosed(AAppClient*)));
				cpMainWindow->openForm(apServerForm);
				apServerForm->startup(arTaskSt.mArg0);
				++apServerSt->mNumServerForms;
				alert(NULL, NULL, false/*Bell*/);
			}
		}
		else // Status. Report error. Abort if called task or request failed.
			alert("Open Server Form", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		break;
	default:
		alert("Open Server Form Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
		break;
	}
	// Done. Decide what to do next.
	if (aDone)
	{	cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Opens a context session.
 *
 * \note
 * -# Arg0 holds the context name
 * -# Arg1 holds the startup msgs
 * -# ArgValue holds the runscript flag
 */
void AConnectMgr::launchOpenSession()
{
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	long aStatus = arTaskSt.mRetStatus;
	bool aDone = true;

	switch (aReqType)
	{
	case geUnknown:
		// Logon. Logon to server using existing UsrName, Passwd
		if (apServerSt->mUsrId <= 0)
		{	
			QString aUsername;
			QString aPassword;
			arTaskSt.mPendingReq = geLogon;
			
			// If server is In-Process
			if (apServerSt->mServerName == AGLBLS_INPROCSVRNAME)
			{
				aUsername = SYS_USER;
				aPassword = SYS_PASS;
			}
			
			if ((aStatus = logon(apServerSt, aUsername, aPassword)) <= 0)
				aDone = false;
			break;
		}
		// Already logged on. Fall thru.
	case geLogon:
		// Open Connection. Open a new connection to the remote server.
		if (apServerSt->mUsrId > 0)
		{	arTaskSt.mPendingReq = geOpenConnection;
			openConnection(apServerSt);
			aDone = false;
		}
		break;
	case geOpenConnection:
		// OpenSession. Add a session form on the selected context.
		if (apServerSt->mUsrId > 0)
		{	// New Connection. Open a new connection for the session form.
			AAppClient* apAppClient = new AAppClient(apServerSt->mHostDnsIp,apServerSt->mPort,this,this,apServerSt->mServerName.data());

			// Arg0: ContextName, Arg1: startup msgs, ArgValue: RunScript flag
			ASessionForm* apSessionForm = new ASessionForm(apAppClient, cpMainWindow, "SessionForm");
			connect(apAppClient, SIGNAL(returnStuff(AReqType, const QString&)), apSessionForm, SLOT(onReturnMsg(AReqType, const QString&)));
			cpMainWindow->openForm(apSessionForm);
			apSessionForm->startup(arTaskSt.mArg0, arTaskSt.mArgValue, arTaskSt.mArg1, apServerSt->mUsrName, apServerSt->mPasswd);
			alert(NULL, NULL, false/*Bell*/);
		}
		// Status. Report error and abort if called task or request failed.
		if (aStatus > 0)
			alert("Open Session Form", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		break;
	case geOpenSession:
		break;
	default:
		alert("Open Session. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
	}
	// Done. Decide what to do next.
	if (aDone)
	{	// This task is complete. Decide what to do next.
		cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();		// Return to application
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Launch startup.
 */
void AConnectMgr::launchStartup()
{
	// If an in-proc server exists, open a server form.
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AReqType aReqType = arTaskSt.mPendingReq;
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	long aStatus = arTaskSt.mRetStatus;
	bool aDone = true;
	
	switch (aReqType)
	{
	case geUnknown:
		// AddServer. Add the in-process server to the server list. Create server structure.
		if (cpServerModel->addServer(AGLBLS_INPROCSVRNAME, ""/*HostDnsIp*/, 0/*Port*/))
		{
			arTaskSt.mpServerSt = cpServerModel->getServer(AGLBLS_INPROCSVRNAME);
			apServerSt = arTaskSt.mpServerSt;
			apServerSt->mDefaultContextName = arTaskSt.mArg0;
			arTaskSt.mPendingReq = geLogon;
			if ((aStatus = logon(apServerSt, SYS_USER, SYS_PASS)) <= 0)
				aDone = false;
			break;
			// Logged on. Fall thru.
		}
		else
		{	alert("Startup", "AddServer fails", true/*Bell*/);
			break;
		}
	case geLogon:
		// OpenServerForm. Open a server form for the in-process server.
		if (apServerSt->mUsrId > 0)
		{	arTaskSt.mPendingReq = geOpenNode;
			if ((aStatus = openServerForm(apServerSt, arTaskSt.mArg1)) <= 0)
				aDone = false;
		}
		break;
	case geOpenNode:
		// OpenContext.  Open the default context, if any.
		if (aStatus <= 0 && !apServerSt->mDefaultContextName.isEmpty())
		{	arTaskSt.mPendingReq = geOpenContext;
			if ((aStatus = openContext(apServerSt, apServerSt->mDefaultContextName)) <= 0)
				aDone = false;
		}
		break;
	case geOpenContext:
		// OpenSession. Open a session and a session form on the default context
		if (aStatus <= 0)
		{	// The startup messages are in Arg1
			arTaskSt.mPendingReq = geOpenSession;
			if ((aStatus = openSession(apServerSt, apServerSt->mDefaultContextName, arTaskSt.mArg1,	true/*RunScript*/)) <= 0)
				aDone = false;
		}
		break;
	case geOpenSession:
		// ProcessOpenSession.  Nothing left to do.
		alert(NULL, NULL, false/*Bell*/);
		break;
	default:
		alert("Startup. Unexpected result", gAis.mpRequestNames[aReqType], true/*Bell*/);
		break;
	}
	// Status. Report error and abort task if request failed.
	if (aStatus > 0)
		alert("Startup", gAis.mpErrMsgs[aStatus], true/*Bell*/);

	// Done. Decide what to do next.
	if (aDone)
	{	cTasks.pop();
		if (cTasks.isEmpty())
			currentChanged();		// Return to application
		else
		{	ACnMgrTaskSt& arCallingTask = cTasks.top();
			arCallingTask.mRetStatus = aStatus;
			launchTask();
		}
	}
}

/*!
 * \brief Return to processing top-of-stack task.
 *
 * \note
 * -# Called when request completes or task completes.
 */
void AConnectMgr::launchTask()
{
	if (cTasks.isEmpty())
		alert("Launch Task", "No tasks pending.", true/*Bell*/);
	else
	{	ACnMgrTaskSt& arTaskSt = cTasks.top();
		switch(arTaskSt.mTask)
		{
		case cneCloseContext:
			launchCloseContext();
			break;
		case cneCloseSession:
			launchCloseSession();
			break;
		case cneConnectSession:
			launchConnectSession();
			break;
		case cneExpandContext:
			launchExpandContext();
			break;
		case cneExpandServer:
			launchExpandServer();
			break;
		case cneLogon:
			launchLogon();
			break;
		case cneOpenConnection:
			launchOpenConnection();
			break;
		case cneOpenContext:
			launchOpenContext();
			break;
		case cneOpenServerForm:
			launchOpenServerForm();
			break;
		case cneOpenSession:
			launchOpenSession();
			break;
		case cneStartup:
			launchStartup();
			break;
		default:
			alert("Launch Task", "Unexpected task returned.", true/*Bell*/);
			break;
		}
	}
}

/*!
 * \brief Logon to selected server.
 *
 * \param[in] ipServerSt Server info for the target server.
 * \param[in] irUsrName User's logon name.
 * \param[in] irPasswd User's password.
 *
 * \return aStatus Error code.
 *
 * \note
 * -# Connect to this server if not connected. Submit logon request using server-wide usrname and password, if any.
 * -# Else, submit logon form to user.  Save usrname and passwd if not a system logon.
 */
long AConnectMgr::logon(AServerSt* ipServerSt, const QString& irUsrName, const QString& irPasswd)
{
	// Create a Logon task and launch it.
	long aStatus = 0;
	if (ipServerSt == NULL)
		aStatus = AERR_NOSERVER;
	else
	{	ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = irUsrName;
		aTaskSt.mArg1 = irPasswd;
		aTaskSt.mArgValue = 0;
		aTaskSt.mPendingReq = geUnknown;
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = ipServerSt;
		aTaskSt.mTask = cneLogon;
		cTasks.push(aTaskSt);
		launchLogon();
	}
	return aStatus;
}

/*!
 * \brief This slot-function is invoked when an allConnectionsClosed signal is received from cpServerModel.
 *
 * \sa AConnectMgr::closeAllConnections, AConnectMgr::isReadyToClose
 */
void AConnectMgr::onAllConnectionsClosed()
{
	cAllConnectionsClosed = true;
	emit allConnectionsClosed();
}

/*!
 * \brief User selected a new item in the list view.  Enable appropriate push buttons.
 *
 * \param[in] irCurIdx List view item index.
 */
void AConnectMgr::onActivated(const QModelIndex& irCurIdx)
{
	QModelIndex aCurIdx(irCurIdx), aParentIdx;
	if (!aCurIdx.isValid())
		aCurIdx = cUi.upServerTreeView->currentIndex();
	if (ceListLvl != emDisable && aCurIdx.isValid())
	{	if (!(aParentIdx = aCurIdx.parent()).isValid())
			ceListLvl = emServer;			// Servers are at top-level
		else if (!aParentIdx.parent().isValid())
			ceListLvl = emContext;			// Parent of contexts are at top-level
		else
			ceListLvl = emSession;			// Grand-parents of sessions are at top-level
		setEnable(ceListLvl);
	}
}

/*!
 * \brief Add new server to the Server List and to the list view ServerName\\tHostDnsIp\\tPort.
 *
 * \note
 * -# Only add server and close dialog are allowed while a request is pending.
 */
void AConnectMgr::onAddServer()
{
	/// cpServerDialog->setValues("", "000.000.000.000", 80);
	if (cpServerDialog->exec() == QDialog::Accepted)
	{	ushort aPort;
		QByteArray aServerName, aHostDnsIp;
		aPort = cpServerDialog->getValues(aServerName, aHostDnsIp);
		cpServerModel->addServer(aServerName, aHostDnsIp, aPort);
	}
	else
		alert("Add Server", "Request cancelled", false/*Bell*/);
}

/*!
 * \brief Close an existing session.
 */
void AConnectMgr::onCloseSession()
{
	if (ceListLvl == emDisable)
		alert("Close Session", "Busy, request pending", true);
	else
	{
		const QModelIndex& arSessionIndex = cUi.upServerTreeView->currentIndex();
		const QModelIndex& arContextIndex = arSessionIndex.parent();
		const QModelIndex& arServerIndex = arContextIndex.parent();
		ACloseDialog aCloseDialog(this);

		if (arSessionIndex.isValid() && arContextIndex.isValid() && arServerIndex.isValid())
		{
			long aStatus = 0;
			long aSessionId = 0;
			ASessionSt* apSessionSt = cpServerModel->getSession(arSessionIndex);
			AServerSt* apServerSt = cpServerModel->getServer(arServerIndex);
			QStandardItem* apContextColumn = cpServerModel->itemFromIndex(arContextIndex);
			const QString& aContextName = apContextColumn->data(Qt::DisplayRole).toString();

			if (apSessionSt != NULL && apServerSt != NULL)
			{
				// Prevent user from connecting to a closed (geSoft, geFirm, geHard)
				QStringList aInfoTkns = apSessionSt->mSessionInfo.split(QChar('\t'), QString::KeepEmptyParts);
				QString& arAdmin = aInfoTkns[0]; // contains the user label
				QString& arMode = aInfoTkns[2]; // contains the close mode

				aSessionId = apSessionSt->mSessionId.toLong();
				// Prevent user from closing the admin session
				if (arAdmin == "admin")
					alert("Close Session", "Closing of admin session is not allowed", true);
				else if(!(arMode == "default" || arMode == "disconnect" || arMode == "open"))
					alert("Close Session", "Closing of a closed session is not allowed", true);
				else if (aCloseDialog.exec() == QDialog::Accepted)
				{
					if ((aStatus = closeSession(apServerSt, aContextName,
						apSessionSt->mSessionId.toLong(),aCloseDialog.getCloseMode())) > 0)
						alert("Close Session", gAis.mpErrMsgs[aStatus], true);
					else
					{
						// Collapse. Session information should be updated.
						cUi.upServerTreeView->setExpanded(arContextIndex, false/*Expand*/);
						setEnable(emNone);
					}
				}
			}
		}
		else
			alert("Close Session", "No session currently selected.", true);
	}
}

/*!
 * \brief Connect to the currently selected disconnected session.
 */
void AConnectMgr::onConnectSession()
{
	if (ceListLvl == emDisable)
		alert("Connect Session", "Busy, request pending", true);
	else
	{
		const QModelIndex& arSessionIndex = cUi.upServerTreeView->currentIndex();
		const QModelIndex& arContextIndex = arSessionIndex.parent();
		const QModelIndex& arServerIndex = arContextIndex.parent();

		if (arSessionIndex.isValid() && arContextIndex.isValid() && arServerIndex.isValid())
		{
			long aStatus = 0;
			long aSessionId = 0;
			ASessionSt* apSessionSt = cpServerModel->getSession(arSessionIndex);
			AServerSt* apServerSt = cpServerModel->getServer(arServerIndex);
			QStandardItem* apContextColumn = cpServerModel->itemFromIndex(arContextIndex);
			const QString& aContextName = apContextColumn->data(Qt::DisplayRole).toString();

			if (apSessionSt != NULL && apServerSt != NULL)
			{
				// Prevent user from connecting to a closed (geSoft, geFirm, geHard)
				QStringList aInfoTkns = apSessionSt->mSessionInfo.split(QChar('\t'), QString::KeepEmptyParts);
				QString& arAdmin = aInfoTkns[0]; // contains the admin label
				QString& arMode = aInfoTkns[2]; // contains the close mode

				aSessionId = apSessionSt->mSessionId.toLong();
				// Prevent user from connecting to Session 1
				if (arAdmin == "admin")
					alert("Connect Session", "Connection to admin session is not allowed", true);
				else if(!(arMode == "default" || arMode == "disconnect"))
					alert("Connect Session", "Connection to an opened/closed session is not allowed", true);
				else if ((aStatus = connectSession(apServerSt, aContextName, apSessionSt->mSessionId.toLong())) > 0)
					alert("Connect Session", gAis.mpErrMsgs[aStatus], true);
				else
				{
					// Collapse. Session information should be updated.
					cUi.upServerTreeView->setExpanded(arContextIndex, false);
					setEnable(emNone);
				}
			}
		}
		else
			alert("Connect Session", "No session currently selected.", true);
	}
}

/*!
 * \brief Delete currently selected server from the local list of servers
 */
void AConnectMgr::onDeleteServer()
{
	// Remove currently selected record from the list view and from the ServerList
	QModelIndex aServerIndex = cUi.upServerTreeView->currentIndex();
	if (ceListLvl == emDisable)
		alert("DeleteServer", "Busy, request pending", true/*Bell*/);
	else if (!aServerIndex.parent().isValid()) // make sure this is a server index
	{	
		AServerSt* apServerSt = cpServerModel->getServer(aServerIndex);
	
		if (apServerSt->mServerName == AGLBLS_INPROCSVRNAME)
			alert("DeleteServer", "Can't remove in-process server", true/*Bell*/);
		else if (QMessageBox::question(this, tr("Delete Server"), tr("Permanently remove server from connection dialog?"),
		tr("&Yes")/*0*/,tr("&No")/*1*/, QString::null, 0/*default*/, 1/*escape*/) == 0)
		{
			AAppClient* apAppClient = apServerSt->mpAppClient;
			
			// PENDING AConnectMgr::onDeleteServer(). Logoff returns a result. Don't close shop until request comes back!!!
			apAppClient->onLogoff(this);
			cpServerModel->removeServer(aServerIndex);
			alert(NULL, NULL, false/*Bell*/);
		}
		else
			alert("Delete Server", "Request cancelled", false/*Bell*/);
	}
}

/*!
 * \brief Modify the properties of the currently selected server.
 */
void AConnectMgr::onEditServer()
{
	// Load the current settings for this server into the Add Server Dialog
	QModelIndex aServerIndex = cUi.upServerTreeView->currentIndex();
	if (ceListLvl == emDisable)
		alert("Edit Server", "Busy, request pending", true/*Bell*/);
	else 
	{	
		AServerSt* apServerSt = cpServerModel->getServer(aServerIndex);
		cpServerDialog->setValues(apServerSt->mServerName, apServerSt->mHostDnsIp, apServerSt->mPort);

		if (cpServerDialog->exec() == QDialog::Accepted)
		{	// Update the Server List and the list view in case the name changed.
			QByteArray aNewServerName, aNewHostname;
			ushort aNewPort = 0;
			aNewPort = cpServerDialog->getValues(aNewServerName, aNewHostname);
			if (aNewServerName.isEmpty())
				alert("Edit Server", "Must provide a server name.", true/*Bell*/);
			else if (aNewHostname.isEmpty())
				alert("Edit Server", "Must provide an IP Address.", true/*Bell*/);
			else if (aNewPort <= 0)
				alert("Edit Server:", "Must provide an non-zero port number.", true/*Bell*/);
			else
			{
				cpServerModel->updateServer(aServerIndex,aNewServerName,aNewHostname,aNewPort);
				alert(NULL, NULL, false/*Bell*/);
			}
		}
		else
			alert("Edit Server", "Edit settings cancelled.", false/*Bell*/);
	}
}

/*!
 * \brief Expand an item in the tree-list view.
 *
 * \param[in] irCurrent Item to be expanded in tree list view.
 */
void AConnectMgr::onExpanded(const QModelIndex& irCurrent)
{
	if (ceListLvl == emDisable || !irCurrent.isValid())
		alert("Expand Server", "Busy, request pending", true/*Bell*/);
	else
	{	QModelIndex aParent = irCurrent.parent();
		if (!aParent.isValid())					// Servers are at top-level
			expandServer(irCurrent);
		else if (!aParent.parent().isValid())	// Contexts are second-level
			expandContext(irCurrent);
		// else you get here if dbl-click on a session.
	}
}

/*!
 * \brief Open a Server Form for the selected server.
 */
void AConnectMgr::onOpenServerForm()
{
	QModelIndex aServerIndex = cUi.upServerTreeView->currentIndex();

	if (ceListLvl == emDisable)
		alert("Open Server Form", "Busy, request pending", true/*Bell*/);
	else 
	{	// Open a server management form. openServerForm checks to see if form is already open
		long aStatus;
		QString aStartupMsgs;
		AServerSt* apServerSt = cpServerModel->getServer(aServerIndex);
		
		aStartupMsgs.sprintf("Welcome to %s Management Form\n", QString(apServerSt->mServerName).toAscii().data());
		if ((aStatus = openServerForm(apServerSt, aStartupMsgs)) > 0)
			alert("Open Server Form:", gAis.mpErrMsgs[aStatus], true/*Bell*/);
	}
}

/*!
 * \brief Open a session form for the currently selected session.
 *
 * \note
 * -# Add a new session and a new sessionForm to the current context.
 * -# Update List View with new session, user name, sessionId.
 */
void AConnectMgr::onOpenSessionForm()
{
	// Find the selected context name and selected server name.
	QModelIndex aContextIndex = cUi.upServerTreeView->currentIndex();
	if (ceListLvl == emDisable)
		alert("Open Session Form", "Busy, request pending", true/*Bell*/);
	else if (aContextIndex.parent().isValid()) // make sure this is a context index
	{	long aStatus;
		const QModelIndex& aServerIndex = aContextIndex.parent();
		AServerSt* apServerSt = cpServerModel->getServer(aServerIndex);

		// get the 1st column of the selected context in the selected server
		QStandardItem* apContextNameColumn = cpServerModel->item(aServerIndex.row())->child(aContextIndex.row());
		const QString& arContextName = apContextNameColumn->text();
		
		if ((aStatus = openSession(apServerSt, arContextName, QString::null/*StartupMsgs*/, false/*RunScript*/)) > 0)
			alert("Open Session", gAis.mpErrMsgs[aStatus], true/*Bell*/);

		// Collapse. Collapse this context because this new session is not included in list of sessions.
		cUi.upServerTreeView->setExpanded(aContextIndex, false/*Expand*/);
	}
	else
		alert("Open Session Form", "No context currently selected.", true/*Bell*/);
}

/*!
 * \brief Invoked when a Server Form is closed.
 *
 * Performs the necessary clean-up for the AAppClient instance.
 *
 * \param[in] ipAppClient Pointer to AAppClient object.
 */
void AConnectMgr::onServerFormClosed(AAppClient* ipAppClient)
{
	if (ipAppClient != NULL)
	{
		ipAppClient->setLogLvl(cpServerModel, geLogAmp, geFatal);
		ipAppClient->setLogLvl(cpServerModel, geLogConsole, geFatal);
		ipAppClient->setLogLvl(cpServerModel, geLogReqHdr, geFatal);
		ipAppClient->setLogLvl(cpServerModel, geLogSysMsg, geFatal);
		ipAppClient->setLogLvl(cpServerModel, geLogUserAccess, geFatal);
		ipAppClient->onLogoff(cpServerModel);
	}
}

/*!
 * \brief Send request to open a registered context.
 */
void AConnectMgr::onStartContext()
{
	const QModelIndex& aContextIndex = cUi.upServerTreeView->currentIndex();
	if (ceListLvl == emDisable)
		alert("Start Context", "Busy, request pending", true/*Bell*/);
	else 
	{
		const QModelIndex& aServerIdx = aContextIndex.parent();
	
		if (aServerIdx.isValid()) // make sure this is a context index
		{
			AServerSt* apServerSt = cpServerModel->getServer(aServerIdx);
			QStandardItem* apContextNameColumn = cpServerModel->item(aServerIdx.row())->child(aContextIndex.row());
			const QString& arContextName = apContextNameColumn->text();

			openContext(apServerSt, arContextName);
		}
	}
}

/*!
 * \brief Send request to close a running context.
 */
void AConnectMgr::onStopContext()
{
	const QModelIndex& aContextIndex = cUi.upServerTreeView->currentIndex();
	if (ceListLvl == emDisable)
		alert("Stop Context", "Busy, request pending", true/*Bell*/);
	else
	{
		const QModelIndex& aServerIndex = aContextIndex.parent();
		if (aServerIndex.isValid()) // make sure this is a context index
		{
			AServerSt* apServerSt = cpServerModel->getServer(aServerIndex);
			QStandardItem* apContextNameColumn = cpServerModel->item(aServerIndex.row())->child(aContextIndex.row());
			const QString& arContextName = apContextNameColumn->text();
			long aStatus;

			if ((aStatus = closeContext(apServerSt, arContextName)) > 0)
				alert("CloseContext", gAis.mpErrMsgs[aStatus], true/*Bell*/);
		}
		else
			alert("CloseContext", "Context has no parent", true/*Bell*/);
	}
}

/*!
 * \brief Establish a connection with the designated server.
 *
 * \param[in] ipServerSt Target server info.
 */
long AConnectMgr::openConnection(AServerSt* ipServerSt)
{
	// Create an OpenContext task and launch it.
	ACnMgrTaskSt aTaskSt;
	aTaskSt.mArg0 = ipServerSt->mServerName;
	aTaskSt.mArg1 = QString::null;
	aTaskSt.mArgValue = 0;
	aTaskSt.mPendingReq = geUnknown;
	aTaskSt.mRetStatus = 0;
	aTaskSt.mpServerSt = ipServerSt;
	aTaskSt.mTask = cneOpenConnection;
	cTasks.push(aTaskSt);
	launchOpenConnection();
	return 0;
}

/*!
 * \brief Open the named context on the selected server.
 *
 * \param[in] ipServerSt Target server info.
 * \param[in] irContextName	Context to be opened.
 *
 * \return aStatus Error code.
 */
long AConnectMgr::openContext(AServerSt* ipServerSt, const QString& irContextName)
{
	// Create an OpenContext task and launch it.
	long aStatus = 0;
	if (irContextName.isEmpty())
		aStatus = AERR_UNKCONTEXTNAME;
	else
	{	ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = irContextName;
		aTaskSt.mArg1 = QString::null;
		aTaskSt.mArgValue = 0;
		aTaskSt.mPendingReq = geUnknown;
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = ipServerSt;
		aTaskSt.mTask = cneOpenContext;
		cTasks.push(aTaskSt);
		launchOpenContext();
	}
	return aStatus;
}

/*!
 * \brief Add a new server status form to the mainQtForm for this server.
 *
 * \return aStatus Error code.
 */
long AConnectMgr::openServerForm(AServerSt* ipServerSt, const QString& irStartupMsgs)
{
	// Create an OpenServerForm task and launch it.
	long aStatus = 0;
	if (ipServerSt->mNumServerForms > 0)
		aStatus = AERR_SERVEROPEN;
	else
	{	ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = irStartupMsgs;
		aTaskSt.mArg1 = QString::null;
		aTaskSt.mArgValue = 0;
		aTaskSt.mPendingReq = geUnknown;
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = ipServerSt;
		aTaskSt.mTask = cneOpenServerForm;
		cTasks.push(aTaskSt);
		launchOpenServerForm();
	}
	return aStatus;
}

/*!
 * \brief Open a new session form.
 *
 * \param[in] ipServerSt Target server info.
 * \param[in] irContextName	Name of host context.
 * \param[in] irStartupMsgs	Startup messages.
 * \param[in] iRunScript True iff run startup script.
 *
 * \return aStatus Error code.
 */
long AConnectMgr::openSession(AServerSt* ipServerSt, const QString& irContextName, const QString& irStartupMsgs,bool iRunScript)
{
	// Look up info for this server
	long aStatus = 0;
	if (irContextName.isEmpty())
		aStatus = AERR_UNKCONTEXTNAME;
	else if (irContextName == AGLBLS_SYSTEMCONTEXTNAME)
		aStatus = AERR_BADCONTEXT;
	else
	{	ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = irContextName;
		aTaskSt.mArg1 = irStartupMsgs;
		aTaskSt.mArgValue = iRunScript;
		aTaskSt.mPendingReq = geUnknown;
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = ipServerSt;
		aTaskSt.mTask = cneOpenSession;
		cTasks.push(aTaskSt);
		launchOpenSession();
	}
	return aStatus;
}

/*!
 * \brief Process return from getCurrentContexts request.
 *
 * \param[in] irContextNames List of current contexts on this server.
 */
void AConnectMgr::processGetCurrentContexts(const QString& irContextNames)
{
	if (!irContextNames.isEmpty())
	{	// Look up saved state
		QModelIndex aServerIdx;
		ACnMgrTaskSt& arTaskSt = cTasks.top();
		AServerSt* apServerSt = arTaskSt.mpServerSt;

		// Clear the current contexts in the list view
		if (cpServerModel->isServerExist(apServerSt->mServerName,aServerIdx))
		{
			cpServerModel->clearContexts(aServerIdx);

			// Display the returned contexts
			QString aName;
			QString aInfo;
			QStringList aNames;
			aNames = irContextNames.split('\n', QString::SkipEmptyParts);
			long aSize = aNames.size();
			if (aSize > 0)
			{
				for (int aIndex = 0; aIndex < aSize; aIndex++)
				{	
					const QString& arEntry = aNames.at(aIndex);

					aName = arEntry.section('\t', 0, 0);
					aInfo = arEntry.section('\t',1);

					if (!aName.startsWith('_')) // Omit reserved contexts
					{	
						cpServerModel->addContext(aServerIdx,aName,aInfo);
					}
				}
				// Resize. Expand first column to fit.
				cUi.upServerTreeView->resizeColumnToContents(0/*Column*/);
			}
			alert(NULL, NULL, false/*Bell*/);
		}
		else
			alert("GetCurrrentContexts", "Server not found", true/*Bell*/);
	}
	else
		alert("Get Current Contexts", "No registered contexts for this server.", true/*Bell*/);
}

/*!
 * \brief GetSessions. Gets newline-terminated list of records. 
 *
 * \param[in] irSessionList	Newline-terminated list of subscriptions.
 *
 * \note
 * -# Each record in irSessionList contains:
 *    ContextName\\tSessionId\\tAdmin\\tUsrName\\tProtocol\\tSubscribed
 *    Where:
 *    Admin admin iff this session is the ContextAdminSession
 *    Protocol -, ais, http, xml, disconnected, in-process
 *    Subscribed * iff this connecton subscribes to this session
 */
void AConnectMgr::processGetSessions(const QString& irSessionList)
{
	ACnMgrTaskSt& arTaskSt = cTasks.top();
	AServerSt* apServerSt = arTaskSt.mpServerSt;
	QStringList aSessionList = irSessionList.split('\n', QString::SkipEmptyParts);
	QModelIndex aContextIdx;

	if (cpServerModel->isContextExist(apServerSt->mServerName,arTaskSt.mArg0,aContextIdx) && !irSessionList.isEmpty())
	{
		// Context. Get the context index by searching this server's children
		QString aContextName = irSessionList.section('\t', 0, 0);
		long aNSessions = aSessionList.size();

		if (aContextName == arTaskSt.mArg0)
		{
			cpServerModel->clearSessions(aContextIdx);
			//qDebug() << aNSessions;
	
			// Sessions. Insert the new sessions as children of this context.
			for (int aRow = 0; aRow < aNSessions; ++aRow)
			{	
				const QString& arEntry = aSessionList.at(aRow);
				QString aSessionId = arEntry.section('\t', 1, 1);
				QString aRecord = arEntry.section('\t', 2);
	
				cpServerModel->addSession(aContextIdx,aSessionId,"",aRecord);
			}
			cUi.upServerTreeView->resizeColumnToContents(0/*Column*/);
		}
		else
		{
			cUi.upServerTreeView->setExpanded(aContextIdx, false/*Expanded*/);
			alert("processGetSessions", "Wrong context list received.", true/*Bell*/);
		}
	}
	else
	{
		if (aContextIdx.isValid())
		{
			cUi.upServerTreeView->setExpanded(aContextIdx, false/*Expanded*/);
		}

		alert("processGetSessions", "No sessions.", true/*Bell*/);
	}
}


/*!
 * \brief Update the task state and then process the returned information from the AIS server.
 *
 * \param[in] iDummyId - Placeholder (used in other parts of AIS for the connectID).
 * \param[in] iXid - an incrementing integer assigned by appclient to each outgoing request to the server.
 * \param[in] iStatus - Zero or a positive error code if an error was generated by the server
 * \param[in] iReqType - Enum describing the type of response or pushed data from the server.
 * \param[in] iRetValue - Integer return value (defaults to zero).
 * \param[in] irOut - Returned message (defaults to an empty string).
 * \param[in] ipData - Binary buffer used to hold serialized object closure (not used here).
 * \param[in] iDataSize - Binary buffer size in bytes (not used here).
 * \param[in] irDisplay - Return from writeln and display (defaults to an empty string).
 * \param[in] irError - Returned error message (defaults to an empty string).
 * \param[in] iClientData - Optional string submitted with a request and returned verbatim with the response.
 *
 * \note
 * -# If completing a request for a task, move to the next request in the task.
 * -# If last request of a task is completed, move to next task for this job.
 * -# Process results returned or forward results to requesting form for processing.
 */
void AConnectMgr::returnOutput(long iDummyId, long iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iDummyId);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(iClientData);

	// Process local requests; hand other requests back to the parent form.
	alert(NULL, NULL, false/*Bell*/);
	AReqType aReqType = iReqType;
	if (!cTasks.isEmpty())
	{	ACnMgrTaskSt& arTaskSt = cTasks.top();
		if (aReqType == arTaskSt.mPendingReq || aReqType == geFcnError)
		{	arTaskSt.mPendingReq = aReqType;
			arTaskSt.mRetDisplay = irDisplay;
			arTaskSt.mRetOut = irOut;
			arTaskSt.mRetStatus = iStatus;
			arTaskSt.mRetValue = iRetValue;
			launchTask();
			return;
		}
	}
	else
	{	// Handle Pushed output and errors.
		QString aOut;
		aOut.sprintf("Xid:%ld, ReqType:%s", iXid, REQNAME(iReqType));
		if (!irOut.isEmpty())
			aOut += ", AOut:" + irOut;
		if (!irDisplay.isEmpty())
			aOut += ", Display:" + irDisplay;
		if (!irError.isEmpty())
			aOut += ", Error:" + irError;
		alert("Return Message", aOut.toLatin1().data(), true/*Bell*/);
		qDebug() << "AConnectMgr::returnOutput(), " << aOut;
	}
}

/*!
 * \brief Inherited from the AReturnRcvr abstract base class.
 *
 * \param[in] iConnectId - The ID of the connection for this context.
 * \param[in] irContextName - The new context name.
 */
void AConnectMgr::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irContextName);
}


/*!
 * \brief Enable/disable selected buttons depending upon the current focus in the list view.
 *
 * \param[in] iListLvl Currently level from top of list view hierarchy.	
 */
void AConnectMgr::setEnable(AListLevel iListLvl)
{
	bool aEnableServerButtons = false;
	bool aEnableContextButtons = false;
	bool aEnableSessionButtons = false;
	switch (iListLvl)
	{
	case emServer:
		// enable server buttons only
		aEnableServerButtons = true;
		break;
	case emContext:
		// enable context buttons only
		aEnableContextButtons = true;
		break;
	case emSession:
		// enable session buttons only
		aEnableSessionButtons = true;
		break;
	// Deselect everything except AddServer and Close	
	default:
	case emNone:
		break;
	}
	// Server-specific buttons
	// cpAddContextPushButton->setEnabled(aEnableServerButtons);
	cUi.upDeleteServerButton->setEnabled(aEnableServerButtons);
	cUi.upEditServerButton->setEnabled(aEnableServerButtons);
	cUi.upOpenServerButton->setEnabled(aEnableServerButtons);

	// Context-specific buttons
	cUi.upOpenSessionButton->setEnabled(aEnableContextButtons);
	cUi.upStartContextButton->setEnabled(aEnableContextButtons);
	cUi.upStopContextButton->setEnabled(aEnableContextButtons);
	
	// Session-specific buttons
	cUi.upCloseSessionButton->setEnabled(aEnableSessionButtons);
	cUi.upConnectSessionButton->setEnabled(aEnableSessionButtons);
}

/*!
 * \brief Initialize the in-process resources.
 * \param[in] irDefaultContextName - The context name to be used when opening a new session on the server.
 * \param[in] irStartupMsgs - Messages saved from the startup process that are waiting for a place to be viewed.
 *
 * \note
 * The default context, if any, is already registered, but not opened.
 * Startup simulates the button presses to:
 * -# Add in-process server to the list of servers
 * -# Logon to SystemContext as "system". Set mpAppClient and connect ID Open a server status form for the in-process server which causes:
 * -# Initialize and load the default context into memory using openContext
 * -# Open a new Session Form
 * -# Establish a connection to server.
 * -# Open a session on the default context.
 * -# Run the default context startup script.
 */
void AConnectMgr::startup(const QString& irDefaultContextName, const QString& irStartupMsgs)
{
	if (gAis.mpInProcSvr != NULL)
	{	// Create a Startup task and launch it.
		ACnMgrTaskSt aTaskSt;
		aTaskSt.mArg0 = irDefaultContextName;
		aTaskSt.mArg1 = irStartupMsgs;
		aTaskSt.mArgValue = 0;
		aTaskSt.mPendingReq = geUnknown;
		aTaskSt.mRetStatus = 0;
		aTaskSt.mpServerSt = NULL;
		aTaskSt.mTask = cneStartup;
		cTasks.push(aTaskSt);
		launchStartup();
	}
	else	// Open connect manager and let the user select a server
	{	
		exec();
	}
}

// end
