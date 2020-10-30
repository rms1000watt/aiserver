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
aisdev/webide/asessionform.cpp
														  Session Form

CHANGE HISTORY
Version	Date		Who		Chagge
4.0003	9/28/2008	tlw	CR128. Add Subscribe to allow a connection to catch output from another session.
4.0003	 7/12/2008	rca  	[CR-125] Removed compile on/off actions.
4.0003	 7/01/2008	fchua	[CR-121] Fixed session form close procedure.
3.2009	 5/08/2008	fchua	Updated setParameters, onCurrentTabChanged. Added getDebugParameters, getCabinetParameters.
3.2008	 4/08/2008	fchua	Update constructor to use ATextEdit parameters.
3.2008	 4/08/2008	fchua	Added setParameters, getConsoleParameters, getEditorParameters.
3.2006	 3/10/2008	fchua	Added onFindPattern, onReplaceFindPattern, onReplacePattern, onReplaceAllPattern.
3.2006	 3/10/2008	fchua	Reimplemented onFind, onReplace to fix dialog focus issue.
3.2006	 3/10/2008	fchua	Added new action in createActions.
3.2002	 2/04/2008	fchua	returnOutput. Updated implementation to support logoff.
3.2002	 2/04/2008	fchua	closeEvent. Updated implementation to support logoff.
3.1007	 1/04/2008	fchua	processStartup. Added handling of failed reconnection.
3.1006	12/21/2007	fchua	processStartup and startup. Added reconnection support.
3.1005	12/14/2007	fchua	Modified returnOutput. Added support for geCloseSession. Updated geCloseConnection case.
3.1005	12/14/2007	fchua	Modified processStartup. Updated geOpenConnection and geOpenSession case.
3.1005	12/14/2007	fchua	Added setSessionId. This function sets the name/title of the session form.
3.1005	12/14/2007	fchua	Added closeEvent. This function handles the closing of the session form.
3.1005	12/14/2007	fchua	Modified ~ASessionForm. Transferred cleanup to closeEvent.
3.1005	12/14/2007	fchua	Modified ASessionForm. Added initialization of new member variables.
3.1004	11/12/2007	fchua	Modified processStartup. Added setting of object name to a useful format.
3.1003	10/29/2007	fchua	Added clearing of cabinet page upon disconnection.
3.1003	10/23/2007	fchua	Added ~ASessionForm.
3.1002	10/21/2007	fchua	Added support to display connection error and disconnection notification in returnOutput.
2.0001	12/29/2006	tmay	added geExecute
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0115	11/20/2006	tlw		onCompile. Set cTask.mUsrName to an alert. Set alert in returnOutput.
1.0113	11/9/2006	tlw		cursorStackChanged. Enable/disable cursor stack operations.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0111	10/25/2006	tlw		Rename AAppClient methods.
1.0107	 9/21/2006	tlw		Add cursor stack operations
1.0070   8/3/2006	mfk		returnOutput. Do not display "Debug" everytime the debugger traces through an instruction line
1.0070	10/14/2005	tlw		Convert to Qt4 Designer
1.0067	 7/22/2005	tlw		CloseConnection on closeEvent.
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QtDebug>
#include <QtCore/QSignalMapper>
#include <QtGui/QAction>
#include <QtGui/QContextMenuEvent>
#include <QtGui/QMenu>
#include <QtGui/QMessageBox>

#include "acabinetpage.h"
#include "aconsolepage.h"
#include "adebugpage.h"
#include "aeditortab.h"
#include "aform.h"
#include "amainwindow.h"
#include "appclient.h"
#include "asessionform.h"
#include "aclosedialog.h"
#include "aoperationsdialog.h"

//	-------------------------------------------------- CLASS METHODS ----------------------------------------------------------
/*!
\brief ASessionForm - constructor instantiates a set of pages for this tabbed widget.

\par Args:
\param ipAppClient -> Application client that forwards commands to the AIS server
\param ipParent -> Parent main window that created this instance of AServerForm
\param ipSessionName -> Name of session used by this form.
\return void
 */
ASessionForm::ASessionForm(AAppClient* ipAppClient, AMainWindow* ipParent, const char* ipSessionName)
    : AForm((QWidget*)ipParent, ipSessionName), cpAppClient(ipAppClient), cConnected(false),
	cDebuggerActive(false), cpFindDialog(NULL), cpGoDialog(NULL), cIsClosing(false), cpParent(ipParent),
	cpEditPrefAct(0), cpReplaceDialog(NULL), cSessionId(0)
{
	// Configure. Configure Tab Widget.
	setAttribute(Qt::WA_DeleteOnClose);
	setMouseTracking(true/*Enable*/);
	resize(QSize(640, 480).expandedTo(minimumSizeHint()));

	// Disabled. Reset the disabled base color to the active base color so that disable doesn't have such a drastic visual impact.
	QPalette aPalette = palette();		// Get the current palette
	QColor aBaseColor = aPalette.color(QPalette::Active, QPalette::Base);
	QString aName = aBaseColor.name();
	aPalette.setColor(QPalette::Disabled, QPalette::Base, aBaseColor);
	setPalette(aPalette);

	// AppClient. Initialize AppClient.
	cTask.mActive = false;
	cpAppClient->setDefaultRcvr(this);

	// Pages. Add pages to the form (Console, Cabinets, Editor, Debugger). All page menus start out as not visible.
	cpConsoleTab = new AConsolePage(ipAppClient, this, this, "Console");
	cpPages[SESSION_CONSOLE] = cpConsoleTab;
	addTab(cpConsoleTab, QString("Console"));
	cpCurPage = cpConsoleTab;

	cpCabinetTab = new ACabinetPage(ipAppClient, this, this, "Cabinets");
	cpPages[SESSION_CABINET] = cpCabinetTab;
	addTab(cpCabinetTab, QString("Cabinets"));

	cpEditorTab = new AEditorTab(ipAppClient, this, this, "Editor");
	cpPages[SESSION_EDITOR] = cpEditorTab;
	addTab(cpEditorTab, QString("Editor"));

	cpDebugTab = new ADebugPage(ipAppClient, this, this, "Debugger");
	cpPages[SESSION_DEBUG] = cpDebugTab;
	addTab(cpDebugTab, QString("Debugger"));

	// Menus. Initialize Session Form menus and toolbar
	createActions();
	createMenus();

	// Connect. Initialize connections to signals from QTabWidget
	connect(this, SIGNAL(currentChanged(int)), this, SLOT(onCurrentTabChanged(int)));
}

/*!
 * \brief Destructor.
 * Make sure that the AppClient object will no longer invoke any of this object's methods.
 */
ASessionForm::~ASessionForm()
{
	enable(true);
}

/*!
 * \brief This method is invoked when the user attempts to close the session form.
 * \param ipEvent Determines whether the window should be closed or not.
 */
void ASessionForm::closeEvent(QCloseEvent *ipEvent)
{
	ipEvent->ignore();
	if (cConnected)
	{
		if (gAis.mpInProcSvr == NULL || cpAppClient->getConnectionId() != 0)
		{
			// if connected, ask user for disconnection mode
			ACloseDialog aCloseDialog(this, this->objectName());
			ACloseMode aDefMode = geSoft;

			if (cContextMap.contains("closemode"))
			{
				QString aMode = cContextMap["closemode"];
				if (gAis.mCloseModes.contains(aMode))
				{
					aDefMode = gAis.mCloseModes[aMode];
				}
			}

			aCloseDialog.setDefaultCloseMode(aDefMode);

			if (aCloseDialog.exec() == QDialog::Accepted)
			{
				// set closing flag, see also returnOutput()
				cIsClosing = true;
				cpAppClient->closeConnection(this, 0, aCloseDialog.getCloseMode());
			}
			else
			{
				emit formClosed(false);
			}
		}
		else
		{
			cpAppClient->closeConnection(this, 0, geHard);
			ipEvent->accept();
			emit formClosed(true);
		}
	}
	else
	{
		// if not connected, just close the session form
		ipEvent->accept();
		emit formClosed(true);
	}
}

/*!
\brief connectionClosed - Inherited from the AReturnRcvr abstract base class.

\param iConnectId  - The ID of the connection that was closed.
\return false
*/
bool ASessionForm::connectionClosed(long iConnectId)
{
    Q_UNUSED(iConnectId);

	return false;
}

/*!
\brief consoleOutput - Send console output to the Console log page for display

\param irMsg - Message to be displayed
\return void
*/
void ASessionForm::consoleOutput(const QString& irMsg)
{
	cpConsoleTab->append(irMsg);
}

void ASessionForm::createActions()
{
	cpCopyAct = new QAction(QIcon(":/images/editcopy.png"), tr("Copy"), this);
	cpCopyAct->setShortcut(tr("Ctrl+C"));
	cpCopyAct->setStatusTip("Copy highlighted text to the clipboard.");
	cpCopyAct->setToolTip("Copy Selection");
	connect(cpCopyAct, SIGNAL(triggered()), this, SLOT(onCopy()));

	cpCutAct = new QAction(QIcon(":/images/editcut.png"), tr("Cut"), this);
	cpCutAct->setShortcut(tr("Ctrl+X"));
	cpCutAct->setStatusTip("Copy highlighted text to clipboard then delete text.");
	cpCutAct->setToolTip("Cut Selection");
	connect(cpCutAct, SIGNAL(triggered()), this, SLOT(onCut()));

	cpFindAct = new QAction(QIcon(":/images/editfind.png"), tr("Find..."), this);
	cpFindAct->setShortcut(tr("Ctrl+F"));
	cpFindAct->setStatusTip("Open find dialog to search document for a match to a pattern.");
	cpFindAct->setToolTip("Find Dialog");
	connect(cpFindAct, SIGNAL(triggered()), this, SLOT(onFind()));

	cpNextAct = new QAction(QIcon(":/images/editforward.png"), tr("Next"), this);
	cpNextAct->setShortcut(tr("Ctrl+PgDown"));
	cpNextAct->setToolTip("Move next");
	cpNextAct->setEnabled(false);
	cpNextAct->setStatusTip("Move forward to next cursor position.");
	connect(cpNextAct, SIGNAL(triggered()), this, SLOT(onNext()));

	cpPasteAct = new QAction(QIcon(":/images/editpaste.png"), tr("Paste"), this);
	cpPasteAct->setShortcut(tr("Ctrl+V"));
	cpPasteAct->setStatusTip("Paste the clipboard into the current document");
	cpPasteAct->setToolTip("Paste");
	connect(cpPasteAct, SIGNAL(triggered()), this, SLOT(onPaste()));

	cpPrevAct = new QAction(QIcon(":/images/editback.png"), tr("Prev"), this);
	cpPrevAct->setShortcut(tr("Ctrl+PgUp"));
	cpPrevAct->setToolTip("Move previous");
	cpPrevAct->setEnabled(false);
	cpPrevAct->setStatusTip("Move back to previous cursor position.");
	connect(cpPrevAct, SIGNAL(triggered()), this, SLOT(onPrev()));

	cpPrintAct = new QAction(QIcon(":/images/editprint.png"), tr("Print"), this);
	cpPrintAct->setShortcut(tr("Ctrl+P"));
	cpPrintAct->setStatusTip("Print the current document on the default printer.");
	cpPrintAct->setToolTip("Print Document");
	connect(cpPrintAct, SIGNAL(triggered()), this, SLOT(onPrint()));

	cpRedoAct = new QAction(QIcon(":/images/editredo.png"), tr("Redo"), this);
	cpRedoAct->setShortcut(tr("Ctrl+Y"));
	cpRedoAct->setStatusTip("Reverse the last undo operation.");
	cpRedoAct->setToolTip("Redo");
	connect(cpRedoAct, SIGNAL(triggered()), this, SLOT(onRedo()));

	cpReplaceAct = new QAction(tr("Replace..."), this);
	cpReplaceAct->setShortcut(tr("Ctrl+R"));
	connect(cpReplaceAct, SIGNAL(triggered()), this, SLOT(onReplace()));

	cpSeparatorAct = new QAction(this);
	cpSeparatorAct->setSeparator(true);

	cpUndoAct = new QAction(QIcon(":/images/editundo.png"), tr("Undo"), this);
	cpUndoAct->setShortcut(tr("Ctrl+Z"));
	cpUndoAct->setStatusTip("Undo the last edit operation.");
	cpUndoAct->setToolTip("Undo");
	connect(cpUndoAct, SIGNAL(triggered()), this, SLOT(onUndo()));

	cpViewAct = new QAction(QIcon(":/images/sessionview.png"), tr("View"), this);
	cpViewAct->setShortcut(tr("Alt+V"));
	cpViewAct->setStatusTip("Launch browser to view application page.");
	cpViewAct->setToolTip("View Application Page");
	connect(cpViewAct, SIGNAL(triggered()), this, SLOT(onViewActivated()));

	QAction* apGoToAct = new QAction(this);
	apGoToAct->setShortcut(tr("Ctrl+G"));
	connect(apGoToAct, SIGNAL(triggered()), this, SLOT(onGoTo()));
	addAction(apGoToAct);

	cpEditPrefAct = new QAction(tr("Edit Preferences"), this);
	cpEditPrefAct->setShortcut(tr("Ctrl+,"));
	cpEditPrefAct->setStatusTip("Edit Preferences Ctrl-,");
	cpEditPrefAct->setToolTip("Change Editor Preferences");
	connect(cpEditPrefAct, SIGNAL(triggered()), this, SLOT(onEditPreferences()));

	// Tools. Initialize form-level tool bars
	cEditTools << cpPrevAct << cpNextAct << cpFindAct << cpCutAct << cpCopyAct << cpPasteAct << cpUndoAct << cpRedoAct << cpPrintAct;
	cSessionTools << cpViewAct;

	// View. Connect up view process to Session slot. View Button connected above.
	cpViewProcess = new QProcess(this);
	connect(cpViewProcess, SIGNAL(error(QProcess::ProcessError)), this, SLOT(onViewError(QProcess::ProcessError)));
}

// Create Form-level menus here.
void ASessionForm::createMenus()
{
	// Edit Menu.
	cpEditMenu = new QMenu(QString("Edit"), this);
	cpEditMenu->addAction(cpPrevAct);
	cpEditMenu->addAction(cpNextAct);
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

	// Context Menus.
	cpEditorContextMenu = new QMenu("Editor Context Menu", this);
	cpEditorContextMenu->addAction(cpFindAct);
	cpNodeContextMenu = new QMenu("Cabinet Node Context Menu", this);
	cpNodeContextMenu->addAction(cpFindAct);
}

/*!
\brief cursorStackChanged - Called from AEditorTab when cursor stack changes

Called when either the number of previous or forward positions in the stack changes from zero to non-zero or vice versa.
\param iForward	Enable/disable forward cursor operation
\param iPrevious	Enable/disable previous cursor operation
\return: void
*/
void ASessionForm::cursorStackChanged(bool iForward, bool iPrevious)
{
	cpNextAct->setEnabled(iForward);
	cpPrevAct->setEnabled(iPrevious);
}

/*!
\brief debuggerActive - Return debugger active flag

\return cDebuggerActive flag
*/
bool ASessionForm::debuggerActive()
{
	return cDebuggerActive;
}

/*!
\brief enable - Forward enable/disable notifications back up to the MainWindow.

\param iIsEnabled - Enable/disable flag
\return void
\sa AConsolePage::enable
\par Notes:
-# While disabled, disallow any edits to the console
-# If multiple requests are ever allowed, then the enable/disable should be converted into a stack.
*/
void ASessionForm::enable(bool iIsEnabled)
{
	// MainWindow. Relay change on up food chain
	cpParent->enable(iIsEnabled);

	// Tabs. Disable all but the current tab
	long aCurIdx = currentIndex();
	for (long aIdx = 1; aIdx < SESSION_PAGES; ++aIdx)
	{	if (iIsEnabled || aIdx != aCurIdx)
			setTabEnabled(aIdx, iIsEnabled);
	}
	// Session Acts. Enable/disable selected session form actions.
	cpCutAct->setEnabled(iIsEnabled);
	cpPasteAct->setEnabled(iIsEnabled);
	cpRedoAct->setEnabled(iIsEnabled);
	cpReplaceAct->setEnabled(iIsEnabled);
	cpUndoAct->setEnabled(iIsEnabled);
	cpEditorContextMenu->setEnabled(iIsEnabled);

	// Console Tab. Enable/disable selected portions of the console tab.
	if (cpConsoleTab != NULL)
		cpConsoleTab->enable(iIsEnabled);	// Enable/disable selected console tab widgets.
}

/*!
\brief engineIdle - Forward idle/busy notifications back up to the MainWindow

\param iIsEngineIdle - Busy/Idle flag
\return void
*/
void ASessionForm::engineIdle(bool iIsEngineIdle)
{
	cpParent->engineIdle(iIsEngineIdle);
}

/*!
 * \brief Returns a pointer to the cabinet parameters.
 */
AParameters* ASessionForm::getCabinetParameters()
{
	return cpParent->getCabinetParameters();
}


/*!
 * \brief Returns a pointer to the console parameters.
 */
AParameters* ASessionForm::getConsoleParameters()
{
	return cpParent->getConsoleParameters();
}

/*!
 * \brief Returns a pointer to the debug parameters.
 */
AParameters* ASessionForm::getDebugParameters()
{
	return cpParent->getDebugParameters();
}

/*!
 * \brief Returns a pointer to the editor parameters.
 */
AParameters* ASessionForm::getEditorParameters()
{
	return cpParent->getEditorParameters();
}

/*!
\brief getFormMenus - Returns list of menus to be shown in the main menu bar for this form.

\par Notes:
 -# Called each time this form gets focus to provide the menus and toolbar icons for this form.
 -# Reimplemented method supplied by the AForm base class.
 */
void ASessionForm::getFormMenus(AMenuList& orFormMenus, AMenuList& orTabMenus, AToolList& orFormTools, AToolList& orTabTools)
{
	// Session. Add form-level menu and session form tools.
	orFormMenus.clear();
	orFormMenus << cpEditMenu;
	orTabTools.clear();
	orFormTools = cSessionTools + cEditTools;

	// Console Tab. Promote Console menus to form-level (so they are visible)
	AMenuList aTabMenus;
	AToolList aTabTools;
	cpPages[SESSION_CONSOLE]->getTabMenus(aTabMenus, aTabTools, false/*Selected*/);
	orFormMenus << aTabMenus;
	orFormTools << aTabTools;

	// Tabs. Add menus and tools for each tab.
	orTabMenus.clear();
	orTabTools.clear();
	for (long aTab = 1; aTab < SESSION_PAGES; ++aTab)
	{	cpPages[aTab]->getTabMenus(aTabMenus, aTabTools, false/*Selected*/);
		orTabMenus << aTabMenus;
		orTabTools << aTabTools;
	}
}
/* Just to demonstrate use of Signal mapper. For now, only show page menus and page tools when the page is currently selected.
// On first call,connect up aboutToShow signal for each menu to the onCurrentChanged slot.
if (cpMenuSignalMap == NULL)
{	cpMenuSignalMap = new QSignalMapper(this);
	for (aTab = 0; aTab < SESSION_PAGES; ++aTab)
	{	cpPages[aTab]->getTabMenus(aTabMenus, aTabTools, false);
		foreach (apMenu, aTabMenus)
		{	connect(apMenu, SIGNAL(aboutToShow()), cpMenuSignalMap, SLOT(map()));
			cpMenuSignalMap->setMapping(apMenu, aTab);
		}
	}
	connect(cpMenuSignalMap, SIGNAL(mapped(long)), this, SLOT(onChangeTab(long)));
	orMenus << aMenus;
	orPageTools << aTools;
	// Also need to implement onChangeTab to call setCurrentIndex(iTabIdx) if tab is different
} */

void ASessionForm::onCopy()
{
	cpCurPage->copy();
}

/*	---------------------------------------------------------------------------------------------------------------------------
onCurrentTabChanged - Switch to selected tab.
Args:
	ipCurTab	-> selected tab
Returns:
	nothing
Notes:
	------------------------------------------------------------------------------------------------------------------------ */
void ASessionForm::onCurrentTabChanged(int iTabIdx)
{
	if (cpCurPage != cpPages[iTabIdx])
	{	// New Page. If the edit window is empty, add a new empty page
		if (iTabIdx == SESSION_EDITOR)
			cpPages[SESSION_EDITOR]->openPage(QString()/*ExtentName*/, QString()/*PageName*/, AEditPage::meDefault);

		// Edit Menu. Make the Session Form edit menu and edit tools visible just for pages with a TextEdit pane.
		QMenu* apMenu;
		QAction* apAction;
		bool aShowEdit = (iTabIdx == SESSION_CONSOLE || iTabIdx == SESSION_EDITOR);
		apAction = cpEditMenu->menuAction();
		apAction->setVisible(aShowEdit);
		foreach (apAction, cEditTools)
			apAction->setVisible(aShowEdit);
		cpReplaceAct->setVisible(aShowEdit);

		// Current Page Menus. Hide current page menus and page tools
		AMenuList aTabMenus;
		AToolList aTabTools;
		if (cpCurPage != NULL)
		{	cpCurPage->getTabMenus(aTabMenus, aTabTools, false/*Selected*/);
			foreach (apMenu, aTabMenus)
			{	apAction = apMenu->menuAction();
				apAction->setVisible(false);
			}
			foreach (apAction, aTabTools)
				apAction->setVisible(false);
			cpParent->statusAlert("");
		}
		// New Tab Menus. Restore menus and tools for the new tab.
		cpCurPage = cpPages[iTabIdx];
		cpCurPage->getTabMenus(aTabMenus, aTabTools, true/*Selected*/);
		foreach (apMenu, aTabMenus)
		{	apAction = apMenu->menuAction();
			apAction->setVisible(true);
		}
		foreach (apAction, aTabTools)
			apAction->setVisible(true);

		// Cabinet Page. Refresh display
		if (iTabIdx == SESSION_CABINET || iTabIdx == SESSION_DEBUG)
		{
			cpPages[SESSION_CABINET]->show();
			cpEditMenu->menuAction()->setVisible(true);
		}

		// Set focus. Return focus to the correct widget
		QWidget* apFocus = cpCurPage->getFocusWidget();
		if (apFocus != NULL)
			apFocus->setFocus();
	}
}

void ASessionForm::onCut()
{
	cpCurPage->cut();
}

/*!
 * \brief Emits an editPreferences signal.
 */
void ASessionForm::onEditPreferences()
{
	emit editPreferences();
}

void ASessionForm::onFind()
{
	AEditorTab* apEditorTab = NULL;
	AConsolePage* apConsolePage = NULL;
	ATextEdit* apTextEdit = NULL;

	apEditorTab = dynamic_cast<AEditorTab*>(cpCurPage);
	apConsolePage = dynamic_cast<AConsolePage*>(cpCurPage);

	// Find is only supported in the console page and editor tab
	if (apEditorTab == NULL && apConsolePage == NULL)
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

		if (apEditorTab != NULL)
			apTextEdit = apEditorTab->getTextEdit();
		else if (apConsolePage != NULL)
			apTextEdit = apConsolePage->getTextEdit();

		cpFindDialog->init(NULL, apTextEdit);
		cpFindDialog->showOnTop();
	}
}

/*!
 * \brief Handles the find signal from the Find Dialog.
 */
void ASessionForm::onFindPattern(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord)
{
	AEditorTab* apEditorTab = NULL;
	AConsolePage* apConsolePage = NULL;
	ATextEdit* apTextEdit = NULL;

	if (cpCurPage != NULL)
	{
		apEditorTab = dynamic_cast<AEditorTab*>(cpCurPage);
		apConsolePage = dynamic_cast<AConsolePage*>(cpCurPage);

		if (apEditorTab != NULL)
			apTextEdit = apEditorTab->getTextEdit();
		else if (apConsolePage != NULL)
			apTextEdit = apConsolePage->getTextEdit();

		if (apTextEdit != NULL)
			emit findPatternResult(apTextEdit->editFind(irPattern, iAll, iMatchCase, iDown, iRegExp, iSelect, iMatchWord));
	}
}

/*!
 * \brief Displays the go to dialog.
 */
void ASessionForm::onGoTo()
{
	// Load the Go To Line Dialog
	if (cpGoDialog == NULL)
	{
		cpGoDialog = ATextEdit::createGoDialog(this, "Go Dialog");
		connect(cpGoDialog, SIGNAL(goToLine(long)),
				this, SLOT(onGoToLine(long)));
	}

	cpGoDialog->init();
	cpGoDialog->showOnTop();
}

/*!
 * \brief Handles the goToLine signal.
 */
void ASessionForm::onGoToLine(long iLineNum)
{
	AEditorTab* apEditorTab = NULL;
	AConsolePage* apConsolePage = NULL;
	ATextEdit* apTextEdit = NULL;

	if (cpCurPage != NULL)
	{
		apEditorTab = dynamic_cast<AEditorTab*>(cpCurPage);
		apConsolePage = dynamic_cast<AConsolePage*>(cpCurPage);

		if (apEditorTab != NULL)
			apTextEdit = apEditorTab->getTextEdit();
		else if (apConsolePage != NULL)
			apTextEdit = apConsolePage->getTextEdit();

		if (apTextEdit != NULL)
			apTextEdit->editGoToLine(iLineNum);
	}
}

void ASessionForm::onPaste()
{
	cpCurPage->paste();
}

/* onNext - Move to the next saved cursor position in the cursor stack
Args: none
Returns: void
SeeAlso: onPrev
Note:
 1. There is no next cursor in the cursor stack unless onPrev has been called at least once since the
 last long move.
 */
void ASessionForm::onNext()
{
	cpCurPage->move(true/*Prev*/);
}

/* onPrev - Move to the previously saved cursor position in the cursor stack
onPrev is called by selecting the Prev menu item or toolbar icon.  If the current page is a console page the call to move is
intercepted by AConsolePage::move. Else, if the current page is an edit page,  the call to move is intercepted by
AEditorTab::move; else, the call is caught by the APage::move default implementation (which just returns an error).
Args: none
Returns: void
*/
void ASessionForm::onPrev()
{
	cpCurPage->move(false/*Next*/);
}

void ASessionForm::onPrint()
{
	cpCurPage->print();
}

void ASessionForm::onRedo()
{
	cpCurPage->redo();
}

void ASessionForm::onReplace()
{
	AEditorTab* apEditorTab = NULL;
	AConsolePage* apConsolePage = NULL;
	ATextEdit* apTextEdit = NULL;

	apEditorTab = dynamic_cast<AEditorTab*>(cpCurPage);
	apConsolePage = dynamic_cast<AConsolePage*>(cpCurPage);

	// Find is only supported in the console page and editor tab
	if (apEditorTab == NULL && apConsolePage == NULL)
	{
		statusAlert("Replace operation not supported for this tab");
	}
	else
	{
		// Create replace dialog
		if (cpReplaceDialog == NULL)
		{
			cpReplaceDialog = ATextEdit::createReplaceDialog(this, "Replace Dialog");

			// Attach some signals/slots
			connect(cpReplaceDialog, SIGNAL(replaceFindPattern(const QString&, bool, bool, bool, bool, bool, bool)),
					this, SLOT(onReplaceFindPattern(const QString&, bool, bool, bool, bool, bool, bool)));

			connect(cpReplaceDialog, SIGNAL(replacePattern(const QString&, const QString&, bool, bool, bool, bool)),
					this, SLOT(onReplacePattern(const QString&, const QString&, bool, bool, bool, bool)));

			connect(cpReplaceDialog, SIGNAL(replaceAllPattern(const QString&, const QString&,bool, bool, bool, bool, bool)),
					this, SLOT(onReplaceAllPattern(const QString&, const QString&,bool, bool, bool, bool, bool)));

			connect(this, SIGNAL(replaceFindPatternResult(bool)),
					cpReplaceDialog, SLOT(onReplaceFindPatternResult(bool)));

			connect(this, SIGNAL(replacePatternResult(bool)),
					cpReplaceDialog, SLOT(onReplacePatternResult(bool)));
		}

		if (apEditorTab != NULL)
			apTextEdit = apEditorTab->getTextEdit();
		else if (apConsolePage != NULL)
			apTextEdit = apConsolePage->getTextEdit();

		cpReplaceDialog->init(NULL, apTextEdit);
		cpReplaceDialog->showOnTop();
	}
}

void ASessionForm::onReplaceFindPattern(const QString& irPattern, bool iAll, bool iMatchCase,
										bool iDown, bool iRegExp, bool iSelect, bool iMatchWord)
{
	AEditorTab* apEditorTab = NULL;
	AConsolePage* apConsolePage = NULL;
	ATextEdit* apTextEdit = NULL;

	if (cpCurPage != NULL)
	{
		apEditorTab = dynamic_cast<AEditorTab*>(cpCurPage);
		apConsolePage = dynamic_cast<AConsolePage*>(cpCurPage);

		if (apEditorTab != NULL)
			apTextEdit = apEditorTab->getTextEdit();
		else if (apConsolePage != NULL)
			apTextEdit = apConsolePage->getTextEdit();

		if (apTextEdit != NULL)
			emit replaceFindPatternResult(apTextEdit->editFind(irPattern, iAll, iMatchCase, iDown, iRegExp, iSelect, iMatchWord));
	}
}

void ASessionForm::onReplacePattern(const QString& irPattern, const QString& irText,
									bool iAllText, bool iMatchCase, bool iRegExp, bool iMatchWord)
{
	AEditorTab* apEditorTab = NULL;
	AConsolePage* apConsolePage = NULL;
	ATextEdit* apTextEdit = NULL;

	if (cpCurPage != NULL)
	{
		apEditorTab = dynamic_cast<AEditorTab*>(cpCurPage);
		apConsolePage = dynamic_cast<AConsolePage*>(cpCurPage);

		if (apEditorTab != NULL)
			apTextEdit = apEditorTab->getTextEdit();
		else if (apConsolePage != NULL)
			apTextEdit = apConsolePage->getTextEdit();

		if (apTextEdit != NULL)
			emit replacePatternResult(apTextEdit->editReplace(irPattern, irText, iAllText, iMatchCase, iRegExp, iMatchWord));
	}
}

void ASessionForm::onReplaceAllPattern(const QString& irPattern, const QString& irText,
										bool iAllText, bool iMatchCase, bool iRegExp, bool iSelect, bool iMatchWord)
{
	AEditorTab* apEditorTab = NULL;
	AConsolePage* apConsolePage = NULL;
	ATextEdit* apTextEdit = NULL;

	if (cpCurPage != NULL)
	{
		apEditorTab = dynamic_cast<AEditorTab*>(cpCurPage);
		apConsolePage = dynamic_cast<AConsolePage*>(cpCurPage);

		if (apEditorTab != NULL)
			apTextEdit = apEditorTab->getTextEdit();
		else if (apConsolePage != NULL)
			apTextEdit = apConsolePage->getTextEdit();

		if (apTextEdit != NULL)
			emit replacePatternResult(apTextEdit->editReplaceAll(irPattern, irText, iAllText, iMatchCase, iRegExp, iSelect, iMatchWord));
	}
}

/*!
 * \brief Slot function that handles logging requests.
 *
 * \param[in] iReqType Log request type.
 * \param[in] irOut Log data.
 */
void ASessionForm::onReturnMsg(AReqType iReqType, const QString &irOut)
{
    Q_UNUSED(iReqType);

	cpConsoleTab->append(irOut);
}

void ASessionForm::onUndo()
{
	cpCurPage->undo();
}

/*	---------------------------------------------------------------------------------------------------------------------------
onViewActivated - Makes call into OS to launch a browser pointed to the view page
Args:
	None
Returns:
	Nothing
Notes:
 1. On return of false, an explanatory message is returned in orErrMsg
	------------------------------------------------------------------------------------------------------------------------ */
void ASessionForm::onViewActivated()
{
	// PENDING ASessionForm::onViewActivated(). Show a dialog to adjust settings if process fails.
	bool aFoundUrl = false;
	QString aClientBrowser("C:/PROGRA~1/INTERN~1/IEXPLORE.EXE");
	QString aClientViewUrl;
	if (cContextMap.contains("clientviewurl"))
	{	QString aClientViewUrl(cContextMap["clientviewurl"]);
		if (!aClientViewUrl.isEmpty())
		{	QStringList aViewUrl(aClientViewUrl);
			cpViewProcess->start(aClientBrowser, aViewUrl);
			aFoundUrl = true;
		}
	}
	if (!aFoundUrl)
	{	QString aCaption("ClientViewUrl not found");
		QString aError = "Unable to locate the web page to view. The ClientViewUrl parameter is not set for this context. The "
		"ClientViewUrl is set in the context.ini file in the same folder as the startup file for this context on the AIS server.";
		QMessageBox::warning(this, aCaption, aError, QMessageBox::Ok, QMessageBox::NoButton);
	}
}

void ASessionForm::onViewError(QProcess::ProcessError iErrCode)
{
	QString aCaption("Browse Help Failed");
	QString aError;
	switch (iErrCode)
	{
	case QProcess::FailedToStart:
		aError = "Process failed to Start. Check the ClientBrowser setting.";
		break;
	case QProcess::Crashed:
		aError = "Process terminated after starting. Check the ClientHelpUrl setting.";
		break;
	case QProcess::Timedout:
		aError = "Process timed out before starting. Check the ClientBrowser setting. ";
		break;
	case QProcess::WriteError:
		aError = "Write to process failed. Process may have terminated. ";
		break;
	case QProcess::ReadError:
		aError = "Read from the process failed. Process may have terminated. ";
		break;
	default:
		aError = "Unknown Error. Check the ClientBrowser setting. ";
		break;
	}
	aError += "The ClientBrowser and ClientHelpUrl settings may be modified by selecting Settings dialog from the Help Menu.";
	QMessageBox::warning(this, aCaption, aError, QMessageBox::Ok, QMessageBox::NoButton);
}

/*!
\brief openPage - Add a new page to this tabbed-widget

\param irPageName - The name of the file or agent
\param irPageType - Contents of this new page (meDefault, meLocalFile, meRemoteFile, meAgent);
\return void
*/
void ASessionForm::openPage(const QString& irPageName, const QString& irPageType)
{
	APage* apEditorTab = cpPages[SESSION_EDITOR];
	const QString& arExtentName = cpAppClient->getCurrentExtentName();
	QStringList aServerActions = cpAppClient->getServerTypeActions(irPageType, arExtentName);
	QString aList = aServerActions.join(QString(","));
	AEditPage::APageType aPageType = aServerActions.contains(QString("checkin")) ? AEditPage::meAgent : AEditPage::meDefault;
	apEditorTab->openPage(arExtentName, irPageName, aPageType);
	setCurrentWidget(widget(SESSION_EDITOR));	// Triggers call to onCurrentTabChanged
}

void ASessionForm::processStartup(AReqType iReqType, long iRetValue, const QString& irOut, const QString& irError)
{
	QWidget* apFocus;
	switch (iReqType)
	{
	case geUnknown:
		enable(false/*IsEnabled*/);
		cpAppClient->onOpenConnection(this);
		break;

	case geOpenConnection:
		qDebug("Connected. Return:%s", irOut.toAscii().data());
		cpAppClient->onLogon(this, cTask.mUsrName, cTask.mPasswd);
		cConnected = true;
		break;

	case geLogon:
		qDebug("Logged on to context %s. Return:%s", cContextName.toAscii().data(), irOut.toAscii().data());
		cpAppClient->getContextParams(this, cContextName);
		break;

	case geGetContextParams:
		AUtil::stringToStringMap(irOut, cContextMap);
		if (cSessionId > 0)
			cpAppClient->connectSession(this, cSessionId);
		else
			cpAppClient->onOpenSession(this, cContextName);
		break;

	case geOpenSession:
	{
		if (iRetValue > 0)
		{
			qDebug("Opened session %ld. Return:%s", iRetValue, irOut.toAscii().data());
			setSessionId(iRetValue);

			cpAppClient->setLogLvl(this, geLogStatus, geInfo);
			qDebug("Setting LogStatus Level");
		}
		else
		{
			cpParent->statusAlert(irError);
			cpConsoleTab->append(irOut);
			qDebug("Open session failed");
		}
		break;
	}
	case geConnectSession:
	{
		if (iRetValue > 0)
		{
			qDebug("Reconnected session %ld.", iRetValue);
			setSessionId(iRetValue);

			if (!irOut.isEmpty() && !(irOut == "true" || irOut == "false"))
			{
				// split using VT (might cause problems if data to be displayed contains VT)
				QStringList aLogTkns = irOut.split(QChar('\013'), QString::KeepEmptyParts);
				long aLogTknSz = aLogTkns.size();
				for (long i = 0; i < aLogTknSz; i++)
				{
					QStringList aTkns = aLogTkns[i].split(QChar('|'), QString::KeepEmptyParts);
					long aTknSz = aTkns.size();
					if (aTknSz > 1)
						if (aTknSz > 4)
							cpConsoleTab->append(aTkns[4]);
				}
			}

			cpAppClient->setLogLvl(this, geLogStatus, geInfo);
			qDebug("Setting LogStatus Level");
		}
		else
		{
			cpParent->statusAlert(irError);
			cpConsoleTab->append(irOut);
			qDebug("Connect session failed");
		}
		break;
	}
	case geSetLogLvl:
	{	QString aExtentType(".Memory"), aOptions("#(1 1 1)"), aRules;
		cpAppClient->setExtentTypeOptions(aExtentType, aOptions);

		// Set rules for subscribing to console output sent to other sessions. E.g. view console output sent back to Http client.
		cpAppClient->getContextParam("clientsessionformrules", aRules);
		if (!aRules.isEmpty())
		{	cpAppClient->setRules(this, aRules);
			break;
		}
		// Fall thru.
	}
	case geSetRules:
		qDebug("Set Rules. Return:%s", irOut.toAscii().data());
		if (cTask.mRunScript)
		{	QString aPrefix;
			cpAppClient->getContextParam("startupprefix", aPrefix);
			cpAppClient->runRemoteScriptFile(this, ASMGR_STARTUPSCRIPT, aPrefix);
			break;
		}
		// Fall thru.
	case geRunScriptFile:
		qDebug("runRemoteScriptFile Startup Script:%s Return:%s", ASMGR_STARTUPSCRIPT, irOut.toAscii().data());
		cpAppClient->onSubmit(this, "(noop)", NULL/*Data*/, 0/*DataSize*/);
		break;
	case geEval:
		qDebug("Eval (noop). Return:%s", irOut.toAscii().data());
		cTask.mActive = false;
		enable(true);
		if ((apFocus = cpCurPage->getFocusWidget()) != NULL)
			apFocus->setFocus();
		// Pre-load cabinets
		cpAppClient->getExtentNames(cpCabinetTab);
		break;
	case geExecute:
		qDebug("Execute (noop). Return:%s", irOut.toAscii().data());
		cTask.mActive = false;
		enable(true);
		if ((apFocus = cpCurPage->getFocusWidget()) != NULL)
			apFocus->setFocus();
		break;
	default:
		break;
	}
}

/*!
\brief returnOutput - Update the task state and then process the returned information from the AIS server.

\param iDummyId - Placeholder (used in other parts of AIS for the connectID).
\param iXid - an incrementing integer assigned by appclient to each outgoing request to the server.
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
void ASessionForm::returnOutput(long iDummyId, long iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iDummyId);
    Q_UNUSED(iXid);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(iClientData);

	long n = irOut.length();
	cpParent->statusMsg("");
	switch (iReqType)
	{
	case geCheckCabinet:
		{
		QStringList aResultList = irOut.split('\t');
		QString aAction = "";
		QString aMessage = "";
		QString aImportSync = "";
		QString aExportSync = "";
		QString aName = "";
		QString aStorageScope = "";
		// Normal Results for a browseLib.checkStatus call
		if( aResultList.size() == 6)
		{
			aAction = aResultList[0];
			aMessage = aResultList[1];
			aImportSync = aResultList[2];
			aExportSync = aResultList[3];
			aName = aResultList[4];
			aStorageScope = aResultList[5];
		}

		if( aAction == "export" )
		{
			QString aLocation = "..Lambda location..";
			setCurrentWidget(cpEditorTab);
			cpEditorTab->exportCabinet(aExportSync, aMessage, aName, aLocation, aStorageScope );
			}
			else if( aAction == "import" )
			{
               	if( aImportSync == "ask" )
               	{
                   	if( QMessageBox::question( this,
                    	"Import",
                       	QString( aMessage + "\nWould you like to import the source file now?"),
                      	(QMessageBox::Yes | QMessageBox::No)) == QMessageBox::Yes)
                  	{
       					QString aLocation = "..Lambda location..";
						if( aStorageScope == "file" )
							cpAppClient->importCabinetRemote( this, aName, aLocation );
						else
							cpAppClient->importCabinetRemoteDir( this, aName, aLocation );
                       	return;
                   	}
					else
					{
                       	return;
                   	}
               	}
               	else
               	if( aImportSync == "auto" )
               	{
       				QString aLocation = "..Lambda location..";
					if( aStorageScope == "file" )
						cpAppClient->importCabinetRemote( this, aName, aLocation );
					else
						cpAppClient->importCabinetRemoteDir( this, aName, aLocation );
                   	return;
               	}
               	else
               	if( aImportSync == "notify" )
               	{
					statusAlert("This cabinet needs to be imported");
                   	return;
               	}
			}
			else if( aAction == "unknown" )
			{
				statusAlert("This cabinet needs to be merged");
				return;
			}
			else if( aAction == "nothing" )
			{
				statusAlert("This cabinet is up-to-date");
				return;
			}
			else if( aAction == "#void" )
			{
				statusAlert("This cabinet is not copied from an extentManagerTemplate");
				return;
			}
			else
			{
				return;
			}
		}
		break;
	case geImportCabinet:
	case geGetExtentNames:
	case geExportCabinet:
		break;
	// rca - This message is used for "file changed" event handling
	case geFcnSendToClient:
		{
		QStringList aTkns = irOut.split("\t");
		cpAppClient->setCurrentExtent(aTkns[1]/*This is the cabinet name*/);
		cpAppClient->checkCabinet(this, aTkns[1]);
		}
	break;
	// added by fchua to support correct display of disconnection notification
	case geCloseConnection:
		// set connected flag to false
		cConnected = false;
		if (iStatus == AERR_LOSTCONNECTION)
		{
			// cIsClosing flag is set in the first call to closeEvent() function
			if (cIsClosing)
			{
				// call to close() will trigger the closeEvent() function
				// which will finally close the form
				close();
			}
			else
			{
				cpParent->statusAlert(QString("Disconnected from the server."));
				cpConsoleTab->append(QString("Disconnected from the server.\n"));
				cpPages[SESSION_CABINET]->clear();
			}
		}
		else
		{
			cpParent->statusAlert(QString("Error: %1").arg(irError));
			cpConsoleTab->append(QString("Error: %1\n").arg(irError));
		}
		break;
	// added by fchua to support correct display of connection error notification
	case geConnectionError:
		cConnected = false;
		cpParent->statusAlert(QString("Connection Error: %1").arg(irError));
		cpConsoleTab->append(QString("Connection Error: %1\n").arg(irError));
		break;
	case geDisplay:
		cpConsoleTab->append(irDisplay + irOut);
		break;
	case geFcnEngineState:
		cpConsoleTab->setEngineState(-1/*Set*/);
		break;
	case geLogConsole:
		// Avoid double spaces on display from an HTML page
		if (n == 0)
		   cpConsoleTab->append(irOut + '\n');
		else if (irOut[(int)n-1] == '\n')
		   cpConsoleTab->append(irOut);
		else
		   cpConsoleTab->append(irOut + '\n');
		break;
	case geLogStatus:	// _icon=busy|status=off
		if (irOut.startsWith("_icon"))
		{	bool aIsEngineIdle = irOut.endsWith("off");
			cpParent->engineIdle(aIsEngineIdle);
		}
		else
			cpParent->statusMsg(irOut);
		break;
	case geProcSetDebuggerActive:
		if (!cDebuggerActive)
		{	cDebuggerActive = true;
			enable(true/*IsEnabled*/);
			cpConsoleTab->setEngineState(0/*Disable*/);
			setCurrentTab(SESSION_DEBUG, "");
			cpDebugTab->show();
		}
		break;
	case geProcDebugLineEdit:
		cpDebugTab->debugLineEdit(irOut);
		break;
	case geProcCodeLines:
		cpDebugTab->debugCodeLines(irOut, iRetValue);
		break;
	case geProcInfoLines:
		cpDebugTab->debugInfoLines(irOut);
		break;
	case geSetEscape:
		statusAlert("Execution Stopped!");
		break;
	case geEval:
		if (cTask.mUsrName.startsWith("Compile"))
		{	cpParent->statusAlert(cTask.mUsrName);
			cTask.mUsrName.clear();
			break;
		}
	case geExecute:
		if (cTask.mUsrName.startsWith("Compile"))
		{	cpParent->statusAlert(cTask.mUsrName);
			cTask.mUsrName.clear();
			break;
		}
	case geRunScriptFile:
		cpConsoleTab->append(irOut + '\n');
		// fall thru
	case geGetContextParams:
	case geLogon:
	case geOpenConnection:
	case geOpenSession:
	case geConnectSession:
	case geSetLogLvl:
	case geSetRules:
		if (!irDisplay.isEmpty())
			cpConsoleTab->append(irDisplay);
		if (cTask.mActive)
			processStartup(iReqType, iRetValue, irOut, irError);
		break;
	default:
		cpParent->statusAlert(QString("SessionForm: Unknown Response: %1").arg(gAis.mpRequestNames[iReqType]));
		cpConsoleTab->append(QString("Unknown Response:%1. ReqType:%2.\n").arg(irOut, gAis.mpRequestNames[iReqType]));
	}
}

/*!
\brief setContextName - Inherited from the AReturnRcvr abstract base class.

\param iConnectId  - The ID of the connection for this context.
\param irContextName - The new context name
\return void
*/
void ASessionForm::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);

	cContextName = irContextName;
}

/*!
\brief setCurrentTab - Write a message to the console and activate a selected tab.

\param iTabIx - Index into the tabs. The tab list is maintained by QTabWidget.
\param irMsg - Message to be appended to the console pane.
\return void
*/
void ASessionForm::setCurrentTab(long iTabIx, const QString& irMsg)
{
	if (!irMsg.isEmpty())
		cpConsoleTab->append(irMsg);
	setCurrentIndex(iTabIx);		// Note: generates a currentChanged signal
}

/*!
\brief setCursorLabel - Update the cursor row/column in the status bar to the cursor position in the
currently active text pane.

\param iCol Column position starting from column zero on the left.
\param iRow Row position starting from row zero at the top of the text.
\return void
*/
void ASessionForm::setCursorLabel(long iCol, long iRow)
{
	cpParent->setCursorLabel(iCol, iRow);
}

/*!
\brief setDebuggerActive - Change cDebuggerActive state

\param iActive - Active/inactive flag
\return void
*/
void ASessionForm::setDebuggerActive(bool iActive)
{
	cDebuggerActive = iActive;
}

/*!
\brief setEngineState - Forward engine state to the Console tab.

 Update the engine-state toolbar actions and the update the prefix.
\param iState -  -1 to update, 0 to disable, 1 to enable
\return void
*/
void ASessionForm::setEngineState(long iState)
{
	cpConsoleTab->setEngineState(iState);
}

/*!
 * \brief Update the look and feel of the console page and the editor page.
 *
 * \param[in] ipConsole Console page settings.
 * \param[in] ipEditor Editor page settings.
 * \param[in] ipLog Log page settings.
 * \param[in] ipCabinet Cabinet page settings.
 * \param[in] ipDebug Debug page settings.
 */
void ASessionForm::setParameters(AParameters* ipConsole, AParameters* ipEditor, AParameters* ipLog, AParameters* ipCabinet, AParameters* ipDebug)
{
    Q_UNUSED(ipLog);

	if (ipConsole != NULL)
		cpConsoleTab->setParameters(ipConsole);

	// update editor page settings if available
	if (ipEditor != NULL)
		cpEditorTab->setParameters(ipEditor);

	// update cabinet page settings if available
	if (ipCabinet != NULL)
		cpCabinetTab->setParameters(ipCabinet);

	// update debug page settings if available
	if (ipDebug != NULL)
		cpDebugTab->setParameters(ipDebug);
}

/*!
 * \brief Sets the name/title of the session form.
 *
 * \param[in] iSessionId Session Id.
 */
void ASessionForm::setSessionId(long iSessionId)
{
	QString aMemory;
	QString aServerName;
	cpAppClient->getContextParam("memory", aMemory);
	aServerName = cpAppClient->getServerName();
	QString aTitle;
	QTextStream aTitleStream(&aTitle, QIODevice::WriteOnly);
	aTitleStream << "Ver:" << AGBL_AISVERSION
				 << " Server:" << aServerName
				 << " " << cContextName << '(' << aMemory
				 << "MB) Session:" << iSessionId;
	setWindowTitle(aTitle);
	setObjectName(aServerName + " - " + cContextName + " - " + QString::number(iSessionId));
}

/*!
\brief startup - Start up an instance of the Session Form.

\param 	irStartupMsgs	Pending system messages waiting to be displayed.
\par Notes:
 -# Not currently used for session forms.
 -# Reimplemented method supplied by the AForm base class.
 */
void ASessionForm::startup(const QString& irStartupMsgs)
{
    Q_UNUSED(irStartupMsgs);
}

/*!
\brief startup - Start up an instance of the Session Form.

\param irContextName - Default context to be used for starting sessions.
\param iRunScript - If set, run the startup script saved in session manager for this session
\param irStartupMsgs - Pending system messages waiting to be displayed.
\param irUsrName - System wide logon which may also work for this session.
\param irPasswd - System wide password which may also work for this session.
\param iIsReconnect - Flag to indicate reconnection
\param iSessionId - Session Id for reconnection
\par Notes:
 -# Called from Connect Manager when this form is first instantiated.
 */
void ASessionForm::startup(const QString& irContextName, bool iRunScript, const QString& irStartupMsgs, const QString irUsrName,
const QString irPasswd, long iSessionId)
{
	if (cpAppClient != NULL)
	{	cContextName = irContextName;
		cTask.mActive = true;
		cTask.mRunScript = iRunScript;
		cTask.mStartupMsgs = irStartupMsgs;
		cTask.mUsrName = irUsrName;
		cTask.mPasswd = irPasswd;
		cSessionId = iSessionId;
		processStartup(geUnknown, 0, QString(), QString());
	}
}

/*!
\brief statusAlert - Show urgent message in the permanent section of the status bar.

\param irMsg - Message to be displayed in the status bar.
\return void
*/
void ASessionForm::statusAlert(const QString& irMsg)
{
	cpParent->statusAlert(irMsg);
}

/*!
\brief statusMsg - Show urgent message in the temoproary section of the status bar (on the left side).

\param irMsg - Message to be displayed in the status bar.
\return void
*/
void ASessionForm::statusMsg(const QString& irMsg)
{
	cpParent->statusMsg(irMsg);
}
// end
