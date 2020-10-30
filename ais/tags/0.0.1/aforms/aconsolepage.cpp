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
aisdev/aforms/aconsolepage.cpp
														Console Page Widget

CHANGE HISTORY
Version	Date		Who		Change
3.2009	05/08/2007	fchua	Added setParameters().
3.2008	 4/08/2008	fchua	Modified constructor. Added support to use ipTextEditParams argument.
3.2006	 3/10/2008	fchua	Removed find and replace functions. Added getTextEdit.
3.1005	12/14/2007	fchua	Modified returnOutput. Added handling of geConnectSession, geCloseSession, geGetSessions, geGetSessionUser.
3.1004	11/12/2007	fchua	Modified returnOutput. Added handling of geOpenConsoleLog, geEnableConsoleLog, geGetConsoleLog.
3.1004	11/12/2007	fchua	Modified submit. Fixed handling of built-in _ais commands.
3.1002	10/23/2007	fchua	Modified returnOutput. Supported disconnection notification.
2.0001	12/29/2006	tmay	added geExecute
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0110	10/14/2006	tlw		Enable runCommand. Disable runCommand while request pending
1.0109	10/1/2006	tlw		editSetParameter. Modify parameter names
1.0107	9/21/2006	tlw		Add move method to catch calls from onPrev and onNext for cursor stack.
1.0100	6/9/2006	tlw		Add Doxygen documentation
1.0070	10/3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QContextMenuEvent>
#include <QtGui/QInputDialog>
#include <QtGui/QMenu>
#include <QtGui/QMessageBox>
#include "aconsolepage.h"
#include "appclient.h"
#include "asessionform.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief AConsolePage - constructor instantiates the GUI elements generated in ui_consolepage.h
 
\par Args:
\param ipAppClient -> Application client that forwards commands to the AIS server
\param ipParentForm -> Parent form that created this instance of a cabinet page
\param ipParent -> Parent widget that hosts this page
\param ipName -> Name assigned to this instance of the cabinet page
\return void
 */
AConsolePage::AConsolePage(AAppClient* ipAppClient, ASessionForm* ipParentForm,
						   QWidget* ipParent, const char* ipName)
:	 QWidget(ipParent), cpAppClient(ipAppClient), cEnable(true), cMaxLines(0), cRemoteFileDialog(ipAppClient, "&path", this, "RunRemoteScript")
{
    Q_UNUSED(ipName);

	// Setup. Establish the widgets for all types of log pages.
	cUi.setupUi(this);
	cpParentForm = ipParentForm;
	cTask = meNoTask;
	(cpCmdLineEdit = cUi.upCmdCombo->lineEdit())->setFocus();
	cpCmdLineEdit->setCompleter(NULL);			// Disable completer as it does not work properly
	cpFocusWidget = focusWidget();

	// Go Button. Set go icon.
	cUi.upGoButton->setIcon(QIcon(":images/consolego.png"));

	//  Connections. Connect up all the buttons and check boxes.
	connect(cUi.upClearButton, SIGNAL(clicked()), this, SLOT(onClear()));
	connect(cpCmdLineEdit, SIGNAL(returnPressed()), this, SLOT(onCmdReturnPressed()));
	connect(cUi.upGoButton, SIGNAL(clicked()), this, SLOT(onCmdReturnPressed()));

	// Menus. Create actions, menus. Start out with invisible tools and menus
	createActions();
	createMenus();

	// Connections. Connect up console pane to local slots.
	connect(cUi.upTextEdit, SIGNAL(statusAlert(const QString&)), this, SLOT(onStatusAlert(const QString&)));
	connect(cUi.upTextEdit, SIGNAL(cursorPositionChanged(long, long)), this, SLOT(onCursorPositionChanged(long, long)));
	connect(cUi.upTextEdit, SIGNAL(runCommand(const QByteArray&)), this, SLOT(onRunCommand(const QByteArray&)));

	// Prefix. Initialize prefix.
	updatePrefix();

	setParameters(ipParentForm->getConsoleParameters());
}

/*!
\brief append - Append a message to the console output pane

\param irMsg - The message that is to be appended
\return void
\par Notes:
-# If the current text contains more than cMaxLines the top lines are removed from the text.
-# The current MaxSpinBox setting has changed, cMaxLines is updated.
*/
void AConsolePage::append(const QString& irMsg)
{
	long aMaxLines = cUi.upMaxSpinBox->value();
	if (cMaxLines != aMaxLines)
	{	cMaxLines = aMaxLines;
		cUi.upTextEdit->editSetParameter("MaximumRows", QVariant((int)aMaxLines));
	}
	cUi.upTextEdit->editAppend(irMsg, true/*MoveToEnd*/);
}

/*!
\brief connectionClosed - Inherited from the AReturnRcvr abstract base class.

\param iConnectId  - The ID of the connection that was closed.
\return false
*/
bool AConsolePage::connectionClosed(long iConnectId)
{
    Q_UNUSED(iConnectId);

	return false;
}

void AConsolePage::contextMenuEvent(QContextMenuEvent* ipContextEvent)
{
	cpConsoleContextMenu->exec(ipContextEvent->globalPos());
}

/*!
\brief copy - Copy the currently selected text to the clipboard

\return void
\par Notes:
-# Inherited from the APage base class.
*/
void AConsolePage::copy()
{
	cUi.upTextEdit->editCopy();
}

void AConsolePage::createActions()
{
	// Actions.
	cpDebugBreakpointAct = new QAction(tr("Debug Breakpoint..."), this);
	connect(cpDebugBreakpointAct, SIGNAL(triggered()), this, SLOT(onDebugBreakpoint()));

	cpErrorTraceAct = new QAction(QIcon(":/images/consoleerrortrace.png"), tr("Error Trace"), this);
	cpErrorTraceAct->setCheckable(true);
	cpErrorTraceAct->setToolTip("Toggle Error Trace");
	cpErrorTraceAct->setStatusTip("Stop execution on an error. Switch to debugger.");
	connect(cpErrorTraceAct, SIGNAL(triggered()), this, SLOT(onErrorTrace()));

	cpInstructionTraceAct = new QAction(QIcon(":/images/consoleinstructiontrace.png"), tr("Instruction Trace"), this);
	cpInstructionTraceAct->setCheckable(true);
	cpInstructionTraceAct->setToolTip("Toggle Instruction Trace");
	cpInstructionTraceAct->setStatusTip("Stop execution at first instruction. Switch to debugger");
	connect(cpInstructionTraceAct, SIGNAL(triggered()), this, SLOT(onInstructionTrace()));

	cpJitAct = new QAction(QIcon(":/images/consolejit.png"), tr("JIT"), this);
	cpJitAct->setCheckable(true);
	cpJitAct->setChecked(true);
	cpJitAct->setToolTip("Toggle just-in-time Compilation");
	cpJitAct->setStatusTip("Just-in-time compilation executes compiled code.");
	connect(cpJitAct, SIGNAL(triggered()), this, SLOT(onJit()));

	cpRunRemoteScriptAct = new QAction(tr("Run Remote Script..."), this);
	connect(cpRunRemoteScriptAct, SIGNAL(triggered()), this, SLOT(onRunRemoteScript()));

	cpRunSelectionAct = new QAction(tr("Run Selection"), this);
	connect(cpRunSelectionAct, SIGNAL(triggered()), this, SLOT(onRunSelection()));

	cpShowSelectionAct = new QAction(tr("Show Selection"), this);
	connect(cpShowSelectionAct, SIGNAL(triggered()), this, SLOT(onShowSelection()));

	cpStopAct = new QAction(QIcon(":/images/consolestop.png"), tr("Stop"), this);
	cpStopAct->setToolTip(tr("Stop Execution"));
	cpStopAct->setStatusTip(tr("Stop execution of current task on server"));
	connect(cpStopAct, SIGNAL(triggered()), this, SLOT(onStop()));

	cpSysCheckAct = new QAction(QIcon(":/images/consolesyscheck.png"), tr("System Check"), this);
	cpSysCheckAct->setCheckable(true);
	cpSysCheckAct->setToolTip("Enable System Checks");
	cpSysCheckAct->setStatusTip("Enable system checks during execution.");
	connect(cpSysCheckAct, SIGNAL(triggered()), this, SLOT(onSysCheck()));

	// Tools. Initialize console-level tool bar
	cConsoleTools << cpInstructionTraceAct << cpErrorTraceAct << cpSysCheckAct << cpJitAct << cpStopAct;
}

void AConsolePage::createMenus()
{
	// Console Menu.
	cpConsoleMenu = new QMenu(QString("Console"), this);
	cpConsoleMenu->addAction(cpRunRemoteScriptAct);
	cpConsoleMenu->addAction(cpRunSelectionAct);
	cpConsoleMenu->addAction(cpShowSelectionAct);
	cpConsoleMenu->addSeparator();
	cpConsoleMenu->addAction(cpDebugBreakpointAct);
	cpConsoleMenu->addAction(cpInstructionTraceAct);
	cpConsoleMenu->addAction(cpErrorTraceAct);
	cpConsoleMenu->addAction(cpSysCheckAct);
	cpConsoleMenu->addAction(cpJitAct);
	cpConsoleMenu->addSeparator();
	cpConsoleMenu->addAction(cpStopAct);

	// Console Context Menu.
	cpConsoleContextMenu = new QMenu("Console Context Menu", this);
	cpConsoleContextMenu->addAction(cpRunRemoteScriptAct);
	cpConsoleContextMenu->addAction(cpRunSelectionAct);
	cpConsoleContextMenu->addAction(cpShowSelectionAct);
	cpConsoleContextMenu->addSeparator();
	cpConsoleContextMenu->addAction(cpDebugBreakpointAct);
	cpConsoleContextMenu->addAction(cpInstructionTraceAct);
	cpConsoleContextMenu->addAction(cpErrorTraceAct);
	cpConsoleContextMenu->addAction(cpSysCheckAct);
	cpConsoleContextMenu->addAction(cpJitAct);
	cpConsoleContextMenu->addSeparator();
	cpConsoleContextMenu->addAction(cpStopAct);
}

/*!
\brief cut - Move the currently selected text to the clipboard

\return void
\par Notes:
-# Inherited from the APage base class.
-# Some text must be currently selected (highlighted)
*/
void AConsolePage::cut()
{
	cUi.upTextEdit->editCut();
}

/*!
\brief enable - Enable/disable selected buttons and toolbar actions in the Console tab widget

\param iEnable - Enable/disable flag
\return void
\par Notes:
-# The stop toolbar action is enabled even if all the other widgets are disabled.
-# Child widgets are automatically enabled/disabled when their parent is enabled/disabled unless they are explicitly disabled;
however, if the parent is disabled, a child cannot be enabled.
*/
void AConsolePage::enable(bool iEnable)
{
	// Set cEnable to disable onRunCommand
	cEnable = iEnable;

	// Enable/disable command combo box.
	cUi.upCmdCombo->setEnabled(iEnable);

	// Enable/disable selected actions
	cpDebugBreakpointAct->setEnabled(iEnable);
	cpErrorTraceAct->setEnabled(iEnable);
	cpInstructionTraceAct->setEnabled(iEnable);
	cpJitAct->setEnabled(iEnable);
	cpRunRemoteScriptAct->setEnabled(iEnable);
	cpRunSelectionAct->setEnabled(iEnable);
	cpShowSelectionAct->setEnabled(iEnable);
	cpSysCheckAct->setEnabled(iEnable);
}

/*!
 * \brief Returns a pointer to the underlying ATextEdit object.
 */
ATextEdit* AConsolePage::getTextEdit()
{
	return cUi.upTextEdit;
}

/*!
\brief getFocusWidget - Returns the widget that currently has focus.

\return apFocus - The widget that currently has focus
\par Notes:
-# Inherited from the APage base class.
-# The widget that has focus receives keyboard and mouse input.
*/
QWidget* AConsolePage::getFocusWidget()
{
	if (cpFocusWidget == NULL)
		cpFocusWidget = cpCmdLineEdit;
	return cpFocusWidget;
}

/*!
\brief getTabMenus - Returns the menus and the toolbar actions for the console page

\param orTabMenus - Reference to place to put the console page menu list.
\param orTabTools - Reference to place to put the console page tool list.
\param iSelected - Tab is currently selected (not currently used).
\return void
\par Notes:
-# Inherited from the APage base class.
-# Used to update the Session Form menus and toolbar list.
*/
void AConsolePage::getTabMenus(AMenuList &orTabMenus, AToolList &orTabTools, bool iSelected)
{
    Q_UNUSED(iSelected);

	orTabMenus.clear();
	orTabMenus << cpConsoleMenu;
	orTabTools.clear();
	orTabTools = cConsoleTools;

	// Status. Initialize status messages when this tab is selected/deselected.
	cpParentForm->statusAlert("");			// Erase stale messages.
}

/*!
\brief move - Move to the next 

\return void
\par Notes:
-# Inherited from the APage base class.
*/
void AConsolePage::move(bool iNext)
{
	cUi.upTextEdit->editMove(iNext ? ATextEdit::eNextCur : ATextEdit::ePrevCur, false/*Select*/);
} 

void AConsolePage::onClear()
{
	cUi.upTextEdit->editClear();
}

void AConsolePage::onCmdReturnPressed()
{
	QString aCmd = cpCmdLineEdit->text();

	// Do we need to insert it into combo dd list?
	if (!aCmd.isEmpty())
	{	cUi.upTextEdit->editAppend(aCmd + '\n', true/*MoveToEnd*/);
		cpFocusWidget = qobject_cast<QWidget*>(cpCmdLineEdit);
		submit(aCmd);
	}
}

void AConsolePage::onCursorPositionChanged(long iCol, long iRow)
{
	cpParentForm->setCursorLabel(iCol, iRow);
}

void AConsolePage::onDebugBreakpoint()
{
	bool aOk;
	QString aAgent(QInputDialog::getText(this, "Set Debug Breakpoint", "Please enter agent name:", QLineEdit::Normal, "", &aOk));
	if (aOk && !aAgent.isEmpty() )
	{	cpParentForm->enable(false);
		cpAppClient->setBreakPoint(this, aAgent);
	}
}

void AConsolePage::onErrorTrace()
{
	cpParentForm->enable(false);
	cTask = meErrorTrace;
	cpAppClient->getExeSession(this);
}

void AConsolePage::onInstructionTrace()
{
	cpParentForm->enable(false);
	cTask = meInstructionTrace;
	cpAppClient->getExeSession(this);
}

void AConsolePage::onJit()
{
	cpParentForm->enable(false);
	cTask = meJit;
	cpAppClient->getExeSession(this);
}

void AConsolePage::onRunCommand(const QByteArray& irCurLine)
{
	QString aCurLine(irCurLine);
	if (cEnable)
	{	if (!aCurLine.isEmpty())
		{	cpFocusWidget = qobject_cast<QWidget*>(cUi.upTextEdit);
			submit(aCurLine);
		}
		else
			cpParentForm->statusAlert("Current line is empty. No Lisp expression to submit.");
	}
	else
		cpParentForm->statusAlert("System Busy. Wait for pending request to finish.");
}

void AConsolePage::onRunRemoteScript()
{
	if (cEnable)
	{	cRemoteFileDialog.setFilters(QString("AIS Lisp (*.sl);;Any (* *.*)"));
		if (cRemoteFileDialog.exec() == QDialog::Accepted)
		{	QString aFilePath = cRemoteFileDialog.filePath();
			if (!aFilePath.isEmpty())
			{	cpParentForm->enable(false);
				cpAppClient->runRemoteScriptFile(this, aFilePath);
			}
		}
	}
	else
		cpParentForm->statusAlert("System Busy. Wait for pending request to finish.");
}

void AConsolePage::onRunSelection()
{
	if (cEnable)
	{	QString aCmd(selectedText());
		if (!aCmd.isEmpty())
		{	cpFocusWidget = qobject_cast<QWidget*>(cUi.upTextEdit);
			submit(aCmd);
		}
		else
			cpParentForm->statusAlert("No Lisp expression selected (highlighted) in text area.");
	}
	else
		cpParentForm->statusAlert("System Busy. Wait for pending request to finish.");
}

void AConsolePage::onShowSelection()
{
	if (cEnable)
		cpParentForm->enable(false);
		QString aCmd(selectedText());
		if (!aCmd.isEmpty())
			cpAppClient->showConsoleSelection(this, aCmd);
	else
		cpParentForm->statusAlert("System Busy. Wait for pending request to finish.");
}

void AConsolePage::onStop()
{
	// Call setEscape to stop the current session if it is running.
	cpAppClient->setEscape(this, 0/*SessionID*/);
	/* Testing...
	cTask = meSetEscape;
	cpAppClient->getExeSession(this); */
}

void AConsolePage::onStatusAlert(const QString& irMsg)
{
	cpParentForm->statusAlert(irMsg);
}

void AConsolePage::onSysCheck()
{
	cpParentForm->enable(false);
	cTask = meSysCheck;
	cpAppClient->getExeSession(this);
}

/*!
\brief paste - Insert the text from the clipboard into the text at the cursor.

\return void
\par Notes:
-# Inherited from the APage base class.
-# The clipboard is not modified.
*/
void AConsolePage::paste()
{
	cUi.upTextEdit->editPaste();
}

/*!
\brief print - Print the text in the console pane to the default printer

\return void
\par Notes:
-# Inherited from the APage base class.
-# Requires that a printer be available to this machine.
*/
void AConsolePage::print()
{
	cUi.upTextEdit->editPrint();
}

/*!
\brief redo - Redo the last undo operation

\return void
\par Notes:
-# Inherited from the APage base class.
-# Requires that at least one undo operation was performed after the last edit operation.
*/
void AConsolePage::redo()
{
	cUi.upTextEdit->editRedo(true/*ToMark*/);
}

/*!
\brief returnOutput - Update the task state and then process the returned information from the AIS server.

\param iConnectId - Placeholder (used in other parts of AIS for the connectID).
\param iRqId - an incrementing integer assigned by appclient to each outgoing request to the server.
\param iStatus - Zero or a positive error code if an error was generated by the server
\param iReqType - Enum describing the type of response or pushed data from the server.
\param iRetValue - Integer return value (defaults to zero).
\param irOut - Returned message (defaults to an empty string).
\param ipData - Binary buffer used to hold serialized object closure (not used here)
\param iDataSize - Binary buffer size in bytes (not used here)
\param irDisplay - Return from writeln and display (defaults to an empty string).
\param irError - Returned error message (defaults to an empty string).
\param iClientData - Optional string submitted with a request and returned verbatim with the response
\return void
\par Notes:
 -# If completing a request for a task, move to the next request in the task.
 -# If last request of a task is completed, move to next task for this job.
-# Process results returned or forward results to requesting form for processing.
*/
void AConsolePage::returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(iRqId);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(iClientData);

	cpParentForm->statusMsg("");
	cpParentForm->enable(true/*IsEnabled*/);
	cpFocusWidget->setFocus();
	switch (iReqType)
	{
	case geAmpMsg:
		if (iStatus == AERR_DISCONNECTED)
		{
			cUi.upTextEdit->editAppend("Error: " + irError + ".\n", true);
			cpParentForm->statusAlert(irError);
		}
		break;
	case geConnectSession:
		if (!irError.isEmpty())
			cpParentForm->statusAlert(irError);

		if (!irOut.isEmpty())
		{
			// there are 3 possible values for irOut
			// true - takeover is successful and there is no buffered logs
			// false - takeover is unsuccessful
			// others - takeover is successful and there are some buffered logs

			if (irOut == "true")
			{
				cUi.upTextEdit->editAppend(irOut + "\n", true);
				// if session takeover was successful, update the name/title of the session form
				cpParentForm->setSessionId(iRetValue);
			}
			else if(irOut == "false")
			{
				cUi.upTextEdit->editAppend(irOut + "\n", true);
			}
			else
			{
				// split using VT (might cause problems if data to be displayed contains VT)
				QStringList aLogTkns = irOut.split(QChar('\013'), QString::KeepEmptyParts);
				long aLogTknSz = aLogTkns.size();
				for (long i = 0; i < aLogTknSz; i++)
				{
					QStringList aTkns = aLogTkns[i].split(QChar('|'), QString::KeepEmptyParts);
					long aTknSz = aTkns.size();
					if (aTknSz > 1)
					{
						if (aTknSz > 4)
							cUi.upTextEdit->editAppend(aTkns[4], true);
					}
				}
				cUi.upTextEdit->editAppend("\n",true);
			}
		}
		break;
	case geCloseSession:
	case geEnableConsoleLog:
		if (!irError.isEmpty())
			cpParentForm->statusAlert(irError);

		if (!irOut.isEmpty())
			cUi.upTextEdit->editAppend(irOut + "\n", true);
		break;
	case geEval:
		if (iStatus == 0)
			cUi.upTextEdit->editAppend(irDisplay + irOut + '\n', true/*MoveToEnd*/);
		else
			cpParentForm->statusAlert(irError);
		break;
	case geExecute:
		if (iStatus == 0)
			cUi.upTextEdit->editAppend(irDisplay + irOut + '\n', true/*MoveToEnd*/);
		else
			cpParentForm->statusAlert(irError);
		break;
	case geGetConsoleLog:
		if (!irError.isEmpty())
			cpParentForm->statusAlert(irError);

		if (!irOut.isEmpty())
		{
			if (irOut == "true" || irOut == "false")
			{
				cUi.upTextEdit->editAppend(irOut + "\n", true);
				break;
			}

			// split using VT (might cause problems if data to be displayed contains VT)
			QStringList aLogTkns = irOut.split(QChar('\013'), QString::KeepEmptyParts);
			long aLogTknSz = aLogTkns.size();
			for (long i = 0; i < aLogTknSz; i++)
			{
				QStringList aTkns = aLogTkns[i].split(QChar('|'), QString::KeepEmptyParts);
				long aTknSz = aTkns.size();
				if (aTknSz > 1)
				{
					if (aTknSz > 4)
						cUi.upTextEdit->editAppend(aTkns[4], true);

					cUi.upTextEdit->editAppend("\n", true);
				}
			}
		}
		break;
	case geGetExeSession:
	{	long aCurSessionId = cpAppClient->getSessionId();
		long aExeSessionId = iRetValue;
		bool aIsExeSession = (aCurSessionId > 0 && aExeSessionId == aCurSessionId);
		bool aIsOn;
		switch (cTask )
		{
		case meErrorTrace:
			aIsOn = cpErrorTraceAct->isChecked();
			if (aIsExeSession)
				cpAppClient->setErrorTrace(this, cpErrorTraceAct->isChecked());
			else
				cTask = meNoTask;
			// Jit. Turn off Jit if debugging and vice versa.
			cpJitAct->setChecked(!(aIsOn || cpInstructionTraceAct->isChecked()));
			updatePrefix();
			break;
		case meInstructionTrace:
			aIsOn = cpInstructionTraceAct->isChecked();
			if (aIsExeSession)
				cpAppClient->setInstructionTrace(this, aIsOn);
			else
				cTask = meNoTask;
			// Jit. Turn off Jit if debugging and vice versa.
			cpJitAct->setChecked(!(aIsOn || cpErrorTraceAct->isChecked()));
			updatePrefix();
			break;
		case meJit:
			aIsOn = cpJitAct->isChecked();
			if (aIsExeSession)
				cpAppClient->setJit(this, aIsOn);
			else
				cTask = meNoTask;
			// Debugging. Turn off tracing if jit is turned on.
			if (aIsOn)
			{	cpErrorTraceAct->setChecked(false);
				cpInstructionTraceAct->setChecked(false);
			}
			updatePrefix();
			break;
		case meSetEscape:
				if (aIsExeSession || (aExeSessionId > 0 && QMessageBox::information(this, "Stop Session",
				"Are you sure you want to stop another session?", QMessageBox::Yes, QMessageBox::No) == QMessageBox::Yes))
					cpAppClient->setEscape(this, aExeSessionId);
				else
					cpParentForm->statusMsg("No currently executing sessions");
				break;
		case meSysCheck:
			if (aIsExeSession)
				cpAppClient->setSysCheck(this, cpSysCheckAct->isChecked());
			else
				cTask = meNoTask;
			updatePrefix();
			break;
		default:
			break;
		}
		break;
	}
	case geGetSessions:
	case geGetSessionUser:
	case geOpenConsoleLog:
		if (!irError.isEmpty())
			cpParentForm->statusAlert(irError);

		if (!irOut.isEmpty())
			cUi.upTextEdit->editAppend(irOut + "\n", true);
		break;
	case geRunScriptFile:
		cUi.upTextEdit->editAppend(irOut + '\n', true/*MoveToEnd*/);
		break;
	case geSetBreakpoint:
		break;
	case geSetErrorTrace:
		cTask = meNoTask;
		break;
	case geSetEscape:
		cTask = meNoTask;
		if (!irOut.isEmpty())
			cUi.upTextEdit->editAppend(irOut, true/*MoveToEnd*/);
		cpParentForm->statusMsg("Session Stopped!");
		break;
	case geSetInstructionTrace:
		cTask = meNoTask;
		break;
	case geSetJit:
		cTask = meNoTask;
		break;
	case geSetSysCheck:
		cpParentForm->enable(true);
		cTask = meNoTask;
		break;
	case geShowConsole:
		if (!irDisplay.isEmpty())
			cUi.upTextEdit->editAppend(irDisplay, true/*MoveToEnd*/);
		if (!irOut.isEmpty())
			cUi.upTextEdit->editAppend(irOut, true/*MoveToEnd*/);
		break;
	default:
		if (!irOut.isEmpty())
			cUi.upTextEdit->editAppend(irOut, true/*MoveToEnd*/);
		cpParentForm->statusAlert("Unexpected response returned to the Console Tab");
		break;
	}
}

/*!
\brief selectedText - Returns the currently selected text in the console output pane

\return aText - The currently selected text
\par Notes:
-# Inherited from the APage base class.
-# If no text is currently selected, aText is empty.
*/
QString AConsolePage::selectedText()
{
	QByteArray aText;
	aText = cUi.upTextEdit->editSelectedText();
	return QString(aText);
}

/*!
\brief setContextName - Inherited from the AReturnRcvr abstract base class.

\param iConnectId  - The ID of the connection for this context.
\param irContextName - The new context name
\return void
*/
void AConsolePage::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irContextName);
}

/*!
\brief setEngineState - Update the engine-state toolbar actions and the update prefix.

\param iMode - -1 to update, 0 to disable, 1 to enable
\return void
\par Notes:
-# Update updates the current toobar settings and prefix to the current server values.
-# Enable/disable either enable or disable the toolbar actions without changing their settings.
*/
void AConsolePage::setEngineState(long iMode)
{
	if (iMode == -1)
	{	cpInstructionTraceAct->setChecked(cpAppClient->getInstructionTrace());
		cpErrorTraceAct->setChecked(cpAppClient->getErrorTrace());
		cpSysCheckAct->setChecked(cpAppClient->getSysCheck());
		cpJitAct->setChecked(cpAppClient->getJit());
		updatePrefix();
	}
	else
	{	bool aEnable = (iMode > 0);
		cpInstructionTraceAct->setEnabled(aEnable);
		cpErrorTraceAct->setEnabled(aEnable);
		cpSysCheckAct->setEnabled(aEnable);
		cpJitAct->setEnabled(aEnable);
	}
}

/*!
 * \brief Update widget properties.
 *
 * \param[in] ipParams New widget properties.
 */
void AConsolePage::setParameters(AParameters* ipParams)
{
	if (ipParams != NULL)
	{
		cUi.upTextEdit->editSetParameter("Font", QVariant(ipParams->mFont));
		cUi.upTextEdit->editSetParameter("FontSize", QVariant((int)ipParams->mFontSize));
		cUi.upTextEdit->editSetParameter("TabWidth", QVariant((int)ipParams->mTabWidth));
		cUi.upTextEdit->editSetParameter("LineNumbers", QVariant(ipParams->mLineNumbers));
		cUi.upTextEdit->editSetParameter("ShowWhiteSpace", QVariant(ipParams->mShowWhiteSpace));
		cUi.upTextEdit->editSetParameter("WordWrap", QVariant(ipParams->mWrap));
		cUi.upTextEdit->editSetParameter("MaximumRows", QVariant((int)ipParams->mMaxRowWidth));
		cUi.upTextEdit->editSetParameter("MaximumUndoDepth", QVariant((int)ipParams->mMaxRowWidth));

		QFont aNewFont(ipParams->mFont, ipParams->mFontSize);
		cUi.upCmdCombo->setFont(aNewFont);
		cUi.upPrefixLineEdit->setFont(aNewFont);
		cUi.upMaxSpinBox->setFont(aNewFont);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
submit - Submit an Lisp expression, AMP message or _ais built-in command.
Args:
Returns:
	nothing
Notes:
 1.	If irCmd prefix not '_ais' and debugger not active, the debug prefix is prepended to the command. Note that | stands for
	the DEL char.  A DEL char may be entered on the command line using Ctrl-|.
            Input        Submitted Format     Submitted Format
  Type      Format       (debugger active)    (debugger inactive)
  ----      ------       -----------------    -------------------
  Lisp      (...)		   _ais|eval|exp|(...)  _ais|eval|exp|(debug ...)(...)
  AMP       %target|act  target|act           _ais|eval|exp|(debug ...)%_%target|act
  Built-in  _ais|req	   _ais|req|..          _ais|req|...
  Var       path         _ais|eval|exp|path   _ais|eval|exp|(debug ...)path
	------------------------------------------------------------------------------------------------------------------------ */
void AConsolePage::submit(QString& irCmd)
{
	// IsAmpMsg. Strip off leading % from AMP commands. Translate | into DEL char
    bool aIsAmp = (irCmd[0] == '%');
    if (aIsAmp)
	{	irCmd = irCmd.mid(1);
		if (!irCmd.contains('\177'))
			irCmd.replace('|', '\177');
	}
	// Built-ins
	if (irCmd.startsWith("_ais"))
	{	if (!irCmd.contains('\177'))
			irCmd.replace('|', '\177');
	}
	else
	{	// Prefix.  Prepend prefix if not a built-in AMP command and debugger not active
		if (!cpParentForm->debuggerActive())
		{	if (aIsAmp)		// Prepend %_% to AMP commands
			{	irCmd.prepend("%_");
				aIsAmp = false;
			}
			irCmd.prepend(cUi.upPrefixLineEdit->text());
		}
		// Eval. If not plain AMP, prepend _ais|eval|exp|.
		if (!aIsAmp)
			irCmd.prepend("_ais\177eval\177exp\177");
	}
	QString aAisOut("");		// Cannot be null.
	cpParentForm->enable(false);
	long aXid = cpAppClient->submit(this, irCmd, aAisOut, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/);
    Q_UNUSED(aXid);
	if (!aAisOut.isEmpty())		// Process immediate response
	{	long aMaxLines = cUi.upMaxSpinBox->value();
		if (cMaxLines != aMaxLines)
		{	cMaxLines = aMaxLines;
			cUi.upTextEdit->editSetParameter("MaximumRows", QVariant((int)aMaxLines));
		}
		cUi.upTextEdit->editAppend(aAisOut + '\n', true/*MoveToEnd*/);
	}
}

/*!
\brief undo - Undo the last edit operation on the console output pane

\return void
\par Notes:
-# Inherited from the APage base class.
-# Requires at least one prior edit operation on the console output pane.
*/
void AConsolePage::undo()
{
	cUi.upTextEdit->editUndo(true/*ToMark*/);
}

/*!
\brief updatePrefix - Update the string in the prefix edit window based upon the current toobar settings

\return void
\par Notes:
-# Updates all the toolbar settings without modifying any other settings in the prefix string.
*/
void AConsolePage::updatePrefix()
{
	// Settings. Get tool bar toggle positions.
	QString aSettings;
	aSettings = cpInstructionTraceAct->isChecked() ? " traceon:" : " traceoff:";
	aSettings += cpErrorTraceAct->isChecked() ? " erroron:" : " erroroff:";
	aSettings += cpSysCheckAct->isChecked() ? " checkon:" : " checkoff:";
	aSettings += cpJitAct->isChecked() ? " jiton:" : " jitoff:";
	aSettings += ')';

	// Prefix. Reset settings in the existing prefix (which must start with "(debug ")
	QString aPrefix = cUi.upPrefixLineEdit->text();
	if (aPrefix.startsWith("(debug "))
	{	aPrefix.remove(" traceon:");
		aPrefix.remove(" traceoff:");
		aPrefix.remove(" erroron:");
		aPrefix.remove(" erroroff:");
		aPrefix.remove(" checkon:");
		aPrefix.remove(" checkoff:");
		aPrefix.remove(" jiton:");
		aPrefix.remove(" jitoff:");
		aPrefix.truncate(aPrefix.length()-1);	// Trailing )
	}
	else
		aPrefix = "(debug ";
	// Settings. Add in new settings.
	aPrefix += aSettings;
	cUi.upPrefixLineEdit->setText(aPrefix);
}
// end
