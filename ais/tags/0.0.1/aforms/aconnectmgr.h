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
#ifndef CONNECTMGR_H
#define CONNECTMGR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/webide/aconnectmgr.h
														CONNECTION MANAGER

CHANGE HISTORY
Version	Date		Who		Change
3.2002	 2/04/2008	fchua	Added member cAllConnectionsClosed.
3.2002	 2/04/2008	fchua	Added allConnectionsClosed signal.
3.2002	 2/04/2008	fchua	Added closeAllConnections, isReadyToClose, and onAllConnectionsClosed functions.
3.1006	12/21/2007	fchua	Added irContextName in connectSession.
3.1006	12/21/2007	fchua	Added iSessionId and iCloseMode in closeSession.
3.1006	12/21/2007	fchua	Added onConnectSession and launchCloseSession member functions.
3.1006	12/21/2007	fchua	Added cneCloseSession in AConnectMgrTask enum.
3.1004	10/31/2007	fchua	Added slot onServerFormClosed().
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0039	4/29/2004	tlw		Revise specification
												--------------- --------------
*/
/*!
 * \file aconnectmgr.h
 */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QStack>
#include <QtGui/QDialog>
#include "aglobals.h"				// AStringIntMap
#include "ui_aconnectmgr.h"
#include "aservertreemodel.h"

class QModelIndex;
class QStandardItemModel;
class AAppClient;
class AServerDialog;
class AAppSvr;
class ALogonDialog;
class AMainWindow;
class ASessionForm;

//	-------------------------------------------------- DATA STRUCTURES --------------------------------------------------------

/*!
 * \brief Type of item selected in connection manager's list view.
 */
enum AListLevel {emDisable, emContext, emNone, emServer, emSession};

/*!
 * \brief Task types.
 */
enum AConnectMgrTask {cneStart, cneCloseSession, cneCloseContext, cneConnectSession, cneExpandContext, cneExpandServer, cneLogon,
	cneOpenConnection, cneOpenContext, cneOpenServerForm, cneOpenSession, cneStartup};

/*!
 * \brief Task information structure.
 *
 * A pending task.  Pending tasks are kept on a stack.  Here is the sequence of events:
 * -# A task, e.g. openServerForm, pushes TaskSt on stack containing selected args from call.
 * -# The task then calls launchOpenServerForm with PendingReq set to geUnknown
 * -# launchOpenServer may start another task, e.g. logon, in the same way.
 * -# A task may also launch a request by  setting the PendingReq in TaskSt and calling appClient.
 * -# When the results for this request are returned, User event puts results in top Task.
 * -# User generated event puts results in top Task and calls the launch procedure for this task.
 * -# The launch procedure goes to portion of code dedicated to process the return from this request.
 * -# When a task completes its last request, its TaskSt is popped from the task stack.
 * -# Then, the launch procedure for top task left on the stack, if any, is called.
 * -# Else, if no tasks left on stack, the procedure returns back to the application.
 */
typedef struct
{	QString			mArg0;				/*!< Argument passed to task */
	QString			mArg1;				/*!< Second arg passed to task */
	long			mArgValue;			/*!< Integer argument passed to task. */
	AReqType		mPendingReq;		/*!< Pending request */
	QString			mRetDisplay;		/*!< Returned console output from request or sub task. */
	QString			mRetOut;			/*!< Returned result from request */
	long			mRetStatus;			/*!< Return status from request */
	long			mRetValue;			/*!< Integer result from request */
	AServerSt*		mpServerSt;			/*!< -> current server structure */
	AConnectMgrTask mTask;				/*!< Task type */
} ACnMgrTaskSt;

/*!
 * \brief Task information stack.
 */
typedef QStack<ACnMgrTaskSt> ACnMgrTaskStack;

#define SYS_USER "system"			/*!< Default username for "system" user */
#define SYS_PASS "1NYQDD8HWQD2Z"	/*!< Default password for "system" user */

//	------------------------------------------------ CLASS DEFINITIONS --------------------------------------------------------
/*!
 * \brief Manages AIS server connections, contexts, and sessions for each client IDE.
 *
 * AMainWindow creates just one instance of AConnectMgr from AMainWindow::startup().   The AConnectMgr constructor initializes the
 * manager's widgets that are constructed using Qt Designer. In the special case that an in-process server exists, AMainWindow::startup
 * also calls ConnectMgr::startup() directly.
 *
 * The connection manager has a list view on the upper left.  The list view is a tree-structured view.  Initially the servers
 * listed in local servers.cfg file are shown in the list view.  The servers.cfg file contains an entry for each server in the
 * view.  It contains the local name assigned to the server, the IP address and port of the server, optional user logon name, and optional
 * user password.
 *
 * When the user selects the connection manager from the window menu,  load() is called.  Load initializes the cServerList from
 * the servercfg file.  The user may register/unregister/edit servers in the server list, or open a monitor-status form for a
 * server.  The user may also register/unregister/start/stop a context on a server or open a new session form on a context.
 * When the dialog is closed, the servercfg file is updated to capture any modifications to the server list. 
 *
 * A logon is required when a connection to the server is first established.  Every server has a built-in SystemContext. The AIS
 * client establishes a connection to this context in order to manage contexts and sessions.  This is the one and only connection
 * made from ConnectMgr to each server. (When a session form is started, this instance of ASessionForm will also logon and
 * establish a separate appClient connection to the server.)
 *
 * \par Usage.
 * -# One instance of this dialog is created at startup by AMainWindow::startup().
 * -# AMainWindow::startup() also calls startup() to manage the in-process server, if any.
 * -# User opens this dialog from the windows menu. AMainWindow::windows_ConnectMgr_activated loads the GUI.
 * -# The various management tasks noted above may be launched from a context menu, an icon on the task bar. or by double-
 * clicking on an item in the list view as noted below.
 *
 * \par Startup.
 * startup() is called by AMainWindow::startup() to initialize the in-process server forms if an in-process server is available.
 * startup() simulates the button presses to add an in-process server, open a default context, add a server form for the
 * in-process server and add a session and a session form to start a new session on the default context.  These operations are
 * carried out using the same facilities as provided for user-initiated requests.  Each of these operations is described below.
 *
 * \par Tree View.
 * The tree view has, at the top-level, zero or more servers.  Each server may have zero or more child contexts.  Each context
 * may have zero or more child sessions.  Each item in the view has a name or "Session nnn"  where nnn is the session ID.  Each
 * item also has the following properties.
 *
 * \verbatim
  Type    Property        Values      Description
  Server  Form            F           Form iff local server-status form has been open
          Port            999         Server socket port number, typically 80.
          Subscription    2,5,7       List of sessions subscribed to (might include protocol,context)
  
  Context	Started         S           Started iff the context is open.
  
  Session	Form            F           Form iff a local session form has been opened
          Context         TestAis     Context name
          UserName        MKorns      User's logon name
          Admin           A           Admin iff the admin session for the parent context
          Protocol        -           No connection
                          A           Ais interface
                          D           Disconnected session
                          H           Http connection
                          I           In-process connection
                          X           XML connection
  \endverbatim
 * 
 * An icon appears in front of each entry.  The icon for servers, contexts and sessions are all distinct.  Each of these icons
 * has two versions, the version displayed depends upon the first property listed in the above table.  For example, a server icon
 * is a picture of a computer.  When a monitor-status form is opened, the icon's background changes.
 * 
 * \par Subscriptions.
 * How to add a subscription?  Select a session form or a server form and select "Subscribe". A dialog appears with a list of
 * sessions, protocols, contexts.  Those that are currently subscribed to are marked.  User can select/deselect items in the list
 * to add/remove items that are subscribed to.  The user can only modify his own forms, not those of others.  For now, we can use
 * default config parameters to set subscriptions for ServerForms and SessionForms.
 * 
 * \par Server-related Tasks.
 * -# Register Server. Adds a remote server to list view. A NewServer Dialog is launched.  The user can add a server to the list
 * view and to the servers.cfg file by specifying a local name, the IP address, and port for the new server.  No connection is
 * established with the server.
 * -# Expand Server. Show the contexts for a server. Invoked by expanding a server node in the list view.  If a connection has
 * not been made, a logon to the SystemContext is required and a connection is established to the server.  A request is
 * submitted to get the current contexts and their run status.  The server subtree is expanded, showing each context and its
 * run status.  During logon, the user may choose to have the user name and password saved.  Subsequent logons to the
 * SystemContext for this server are automated.
 * -# Unregister Server. Remove a server from list view. The selected server is removed from the list view.  The connection to
 * the server is closed.  TCP will notify the server automatically that the connection has been lost.  The in-process server,
 * if any, cannot be removed.
 * -# Edit server properties. A Properties dialog is launched.  The user may edit the local server name, the IP address, and port
 * of the currently selected server.  No calls are made to the server.
 * -# Monitor Server. A monitor-server form is opened.  An instance of the AServerForm is created by a call back into the
 * MainWindow.  Then a call to AServerForm::startup() configures the messages to be returned to the form.  The ServerForm shares
 * the connection (via AAppClient) with the ConnectMgr.  No new logon or session is required.  Starting a new form requires no
 * interaction with the server (although the ServerForm itself does send setLogLvl requests to the server).
 * 
 * \par Context-related Tasks.
 * Context State. Context state is limited to just its name, its status (active/inactive) and number of active sessions.  Of
 * course, an inactive context has zero active sessions.
 * -# Expand Context. By expanding a context node in the list view, a request is sent to the parent server to retrieve the
 * sessions running on this context.  The request is sent via the existing AppClient connection.  The user name, sessionId,
 * and current interface (Http, Xml, App, Detached) for each session are returned.  The subtree for this context is updated
 * with the session information.
 * -# Start Context.  When a context is selected that is not running (the context is not "open"), a request is sent to the
 * server to open the context.
 * -# Stop Context. When a context is selected, and the Stop Context button is pressed, a warning dialog is launched.  The user
 * is warned that the selected Context is about to be stopped on the server.  All open sessions on this context are closed.
 * 
 * \par Session-related Tasks.
 * -# Attach Session. If a detached session shown in a context subtree in the list view is expanded, a callback to AMainWindow::
 * openSessionForm creates a new session form. Pending output may be fetched and displayed in the console tab of the form.
 * The logon, appClient connection, attach are handled by ASessionForm startup().  If the user name and password have been
 * saved for the current user, they are forwarded to ASessionForm::startup().  A logon to the context is attempted using the
 * same user name and password used to log on to the SystemContext.  If the logon fails, the user must enter a new user name
 * and password into a logon form.  ASessionForm::startup() attaches to this defunct session which is reactivated.  A user
 * cannot reattach to a session which was opened by a different user.
 * -# Start Session.  By selecting a context and selecting the Start icon or menu item, a callback to AMainWindow::openSessionForm
 * starts up a new session as described above for attach session.  If this is the first session opened on a context, it will
 * receive the pending output generated by the startup script when the context was opened.
 * 
 * \par Connections.
 * A connection has a specific context, user, and session associated with it.  The connection can be an AAppClient connection for
 * server support or a full-featured AppClient connection for session support.  A default session 0 is automatically available
 * for AppClient requests that just require immediate responses.  A user must be authenticated by a logon before a request can be
 * submitted via any connection.
 * 
 * \par SystemContext.
 * A "system" context is registered when a server is started.  The system context is not started, but it can
 * perform logons, open forms and open contexts and start sessions.  It never calls into the engine to run AisLisp.  ConnectMgr
 * establishes a client connection as the "system" user to a server when the server in the tree ListView is expanded.   This
 * connection is used by the Connection Manager for all subsequent transactions and is passed on to the AServerForm if one is
 * opened by monitorServer.  
 *  
 * \par Session Forms.
 * If an ASessionForm is opened by StartSession or AttachSession the form itself gets and maintains a separate
 * AppClient connection. If starting a new session, a logon and an open session call must precede the first request.
 */

class AConnectMgr : public QDialog, public AReturnRcvr
{
    Q_OBJECT
public:
	AConnectMgr(AMainWindow* ipMainWindow, const char* ipName = NULL, Qt::WFlags iFlgs = 0);
	~AConnectMgr();
	void closeAllConnections();
	virtual bool connectionClosed(long iServerIx);
	bool connectionClosed(const char* irServerName);
	bool isReadyToClose();
	virtual void returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
				 , char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
				 = QString());
	virtual void setContextName(long iConnectId, const QString& irContextName);

	void		startup(const QString& irDefaultContextName, const QString& irMsgs);
	AReqType	mPendingReq;		//!< Currently pending request, if any

public slots:
	void	onAllConnectionsClosed();
	void	onServerFormClosed(AAppClient* ipAppClient);

signals:
	/*!
	 * \brief This signal is emitted when all connections are closed.
	 */
	void	allConnectionsClosed();

private slots:
	void			onActivated(const QModelIndex& irCurIdx);
	void			onAddServer();
	void			onCloseSession();
	void			onConnectSession();
	void			onDeleteServer();
	void			onEditServer();
	void			onExpanded(const QModelIndex& irCurIdx);
	void			onOpenServerForm();
	void			onOpenSessionForm();
	void			onStartContext();
	void			onStopContext();

private:
	void			alert(const char* ipTitle, const char* ipMsg, bool iBell);
	long			closeContext(AServerSt* ipServerSt, const QString& irContextName);
	void			closeServerForm();
	long			closeSession(AServerSt* ipServerSt, const QString& irContextName, long iSessionId, ACloseMode iCloseMode);
	long			connectSession(AServerSt* ipServerSt, const QString& irContextName, long iSessionId);
	void			currentChanged();
	void			expandContext(const QModelIndex& irContextIdx);
	void			expandServer(const QModelIndex& irServerIdx);
	long			logon(AServerSt* ipServerSt,const QString& irUsrName=QString::null,const QString& irPasswd=QString::null);
	void			launchCloseContext();
	void			launchCloseSession();
	void			launchConnectSession();
	void			launchExpandContext();
	void			launchExpandServer();
	void			launchLogon();
	void			launchOpenConnection();
	void			launchOpenContext();
	void			launchOpenServerForm();
	void			launchOpenSession();
	void			launchStartup();
	void			launchTask();
	long			openConnection(AServerSt* ipServerSt);
	long			openContext(AServerSt* ipServerSt, const QString& irContextName);
	long			openSession(AServerSt* ipServerSt,const QString& irContextName,const QString& irStartupMsgs,bool iRunScript);
	long			openServerForm(AServerSt* ipServerSt, const QString& irStartupMsgs);
	void			processGetCurrentContexts(const QString& irContextNames);
	void			processGetSessions(const QString& irSessionList);
	void			setEnable(AListLevel iListLvl);

	AServerDialog*	cpServerDialog;
	ALogonDialog*	cpLogonDialog;

	AListLevel		ceListLvl;
	AMainWindow*	cpMainWindow;
	QString			cMt;
	QString			cPasswd;
	AServerTreeModel*	cpServerModel;
	ACnMgrTaskStack	cTasks;
	QString			cUsrName;
	bool			cAllConnectionsClosed;
	Ui::AConnectMgrClass cUi;
};

#endif // CONNECTMGR_H
