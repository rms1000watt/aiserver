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
aisdev/aforms/amainwindow.cpp
														Main Window

CHANGE HISTORY
Version	Date		Who		Change
3.2009   5/08/2008	fchua	Update onEditPreferences, readSettings, writeSettings.
3.2009   5/08/2008	fchua	Added getCabinetParameters, getDebugParameters.
3.2008	4/08/2008	fchua	Added getConsoleParameters, getEditorParameters, getLogParameters, onEditPreferences.
3.2008	4/08/2008	fchua	Updated readSettings, writeSettings, openForm.
3.2002	2/04/2008	fchua	Updated Doxygen documentation.
3.2002	2/04/2008	fchua	Updated implementation of closeEvent to support proper cleanup of forms.
2.0001	1/03/2007	tlw		onHelpActivated. Use GblClientHelpUrl instead of hard-coded value.
1.0107	9/20/2006	tlw		keyPressEvent. Remove unused variable.
1.0070	10/3/2005	tlw		Add Doxygen documentation.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QSettings>
#include <QtCore/QSignalMapper>

#include <QtGui/QAction>
#include <QtGui/QApplication>		// qApp
#include <QtGui/QCloseEvent>
#include <QtGui/QHBoxLayout>
#include <QtGui/QLabel>
#include <QtGui/QMenuBar>
#include <QtGui/QMessageBox>
#include <QtGui/QStatusBar>
#include <QtGui/QToolBar>
#include <QtGui/QToolButton>
#include <QtGui/QWorkspace>

#include "aglobaldefs.h"
#include "aconnectmgr.h"
#include "aform.h"				// AMenuList, AToolList
#include "amainwindow.h"
#include "atextedit.h"
#include "aideprefdialog.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
 * \brief Constructor instantiates the main window when the system starts up.
 *
 * \param[in] irTitle Window title.
 *
 * \return void
 */
AMainWindow::AMainWindow(const QString& irTitle):
	cEngineBusyIcon(":/images/mainenginebusy.png"),
	cEngineIdleIcon(":/images/mainengineidle.png"),
	cSettings(QSettings::IniFormat, QSettings::UserScope, "ais", "ide", this),
	cpEditPrefDialog(0)
{
	// Workspace. Connect up workspace.
	cpWorkspace = new QWorkspace;
	setCentralWidget(cpWorkspace);
	connect(cpWorkspace, SIGNAL(windowActivated(QWidget *)), this, SLOT(onFormActivated(QWidget*)));
	cpWindowMapper = new QSignalMapper(this);
	connect(cpWindowMapper, SIGNAL(mapped(QWidget*)), cpWorkspace, SLOT(setActiveWindow(QWidget*)));

	// Settings. Initialize configuration parameters.
	readSettings();

	// Menus. Initialize menus, toolbar, 
	createActions();
	createMenus();
	createToolBars();
	createStatusBar();
	cpCurForm = NULL;
	setWindowTitle(irTitle);

	// Help. Connect up help process to MainWindow slots. Help Button connected in createActions
	cpHelpProcess = new QProcess(this);
	connect(cpHelpProcess, SIGNAL(error(QProcess::ProcessError)), this, SLOT(onHelpError(QProcess::ProcessError)));
	cInstallPath = QApplication::applicationFilePath();
	cIsClosing = false;
}

/*!
 * \brief Returns AForm pointer of the active window.
 */
AForm* AMainWindow::activeForm()
{
	return qobject_cast<AForm *>(cpWorkspace->activeWindow());
}

/*!
 * \brief This function is called when the close button is pressed.
 *
 * \param[in] ipEvent QCloseEvent object pointer to set the result.
 *
 * \note
 * - This function is designed to be called several times until it is safe to close.
 * - The child windows will be closed one at a time, each child window emits a formClosed signal,
 * which will trigger this function, that will again attempt to close the next active window.
 * - After all the session forms and server forms have been closed, the Connection Manager form is closed.
 * - The Connection Manager form emits a allConnectionsClosed signal, which will trigger this function,
 * and finally closes the main window.
 *
 * \sa onAllConnectionsClosed, onChildWindowClosed
 */
void AMainWindow::closeEvent(QCloseEvent* ipEvent)
{
	cIsClosing = true;
	
	// If there is an active window
	if (activeForm() != NULL)
	{
		// Close it
		cpWorkspace->closeActiveWindow();
		ipEvent->ignore();
	}
	else
	{
		// If there is no more active windows, close the Connection Manager dialog
		if(!cpConnectMgr->isReadyToClose())
		{
			// Not yet ready to close
			cpConnectMgr->closeAllConnections(); // sends a signal when completed
			ipEvent->ignore();
		}
		else
		{
			writeSettings();
			ipEvent->accept();
		}
	}

#ifdef AIS_DEBUG
	qDebug("AMainWindow::closeEvent()");
#endif
}

/*!
 * \brief Create actions.
 *
 * \note
 * See createStatusBar for other icons.
 */
void AMainWindow::createActions()
{
	// PENDING AMainWindow::createActions(). Sync up these shortcuts with those in aisedit.cpp.
	// Actions. Toolbar and menu actions.
	cpAboutAct = new QAction(tr("About AIS"), this);
	connect(cpAboutAct, SIGNAL(triggered()), this, SLOT(onAbout()));

	cpAboutQtAct = new QAction(tr("About Qt"), this);
	connect(cpAboutQtAct, SIGNAL(triggered()), this, SLOT(onAboutQt()));

	cpCascadeAct = new QAction(tr("Cascade"), this);
	connect(cpCascadeAct, SIGNAL(triggered()), cpWorkspace, SLOT(cascade()));

	cpCloseAct = new QAction(tr("Close"), this);
	cpCloseAct->setShortcut(tr("Ctrl+F4"));
	connect(cpCloseAct, SIGNAL(triggered()), cpWorkspace, SLOT(closeActiveWindow()));

	cpCloseAllAct = new QAction(tr("Close &All"), this);
	connect(cpCloseAllAct, SIGNAL(triggered()), cpWorkspace, SLOT(closeAllWindows()));

    cpConnectAct = new QAction(QIcon(":/images/connection-manager.png"), tr("Co&nnect"), this);
	cpConnectAct->setShortcut(tr("Ctrl+N"));
	cpConnectAct->setStatusTip("Launch dialog to show connections to AIS");
	cpConnectAct->setToolTip("Launch Connect Manager");
	connect(cpConnectAct, SIGNAL(triggered()), this, SLOT(onConnect()));

	cpExitAct = new QAction(tr("Exit"), this);
	cpExitAct->setShortcut(tr("Ctrl+Q"));
	connect(cpExitAct, SIGNAL(triggered()), qApp, SLOT(closeAllWindows()));

    cpHelpAct = new QAction(QIcon(":/images/help-faq.png"), tr("Help"), this);
	cpHelpAct->setStatusTip("Show help on using AIS");
	cpHelpAct->setToolTip("AIS Help ");
	connect(cpHelpAct, SIGNAL(triggered()), this, SLOT(onHelpActivated()));

	cpNextAct = new QAction(tr("Ne&xt"), this);
	cpNextAct->setShortcut(tr("Ctrl+F6"));
	connect(cpNextAct, SIGNAL(triggered()), cpWorkspace, SLOT(activateNextWindow()));

	cpPreviousAct = new QAction(tr("Pre&vious"), this);
	cpPreviousAct->setShortcut(tr("Ctrl+Shift+F6"));
	connect(cpPreviousAct, SIGNAL(triggered()), cpWorkspace, SLOT(activatePreviousWindow()));

	cpSeparatorAct = new QAction(this);
	cpSeparatorAct->setSeparator(true);

	cpTileAct = new QAction(tr("&Tile"), this);
	connect(cpTileAct, SIGNAL(triggered()), cpWorkspace, SLOT(tile()));
}

/*!
 * \brief Create menus.
 *
 * \note
 * -# Multiple menus with the same name can be added to menu bar.
 * -# Menu may be removed using removeAction(menuAction())
 */
void AMainWindow::createMenus()
{
	cpMenuBar = menuBar();
	cpWindowMenu = cpMenuBar->addMenu(tr("&Window"));
	connect(cpWindowMenu, SIGNAL(aboutToShow()), this, SLOT(onUpdateWindowMenu()));
	cpMenuBar->addSeparator();

	cpHelpMenu = cpMenuBar->addMenu(tr("&Help"));
	cpHelpMenu->addAction(cpHelpAct);
	cpHelpMenu->addAction(cpAboutAct);
	cpHelpMenu->addAction(cpAboutQtAct);
}

/*!
 * \brief Create status bar.
 */
void AMainWindow::createStatusBar()
{
	// Window Icon. Set the window icon too.
	cAisIcon.addFile(":/images/mainabase.png", QSize(22,22), QIcon::Normal, QIcon::Off);
	setWindowIcon(cAisIcon);

	// Status Message. Add a permanent and a normal status widget to hold alerts and messages.
	cpStatusBar = statusBar();
	// cpStatusBar->setFixedHeight(28);
	cpStatusAlertLabel = new QLabel(this);
	cpStatusAlertLabel->setObjectName("StatusAlertLabel");
	QFont aFont(cpStatusAlertLabel->font());
	aFont.setBold(true/*Enable*/);
	cpStatusAlertLabel->setFont(aFont);
	cpStatusBar->addPermanentWidget(cpStatusAlertLabel);
	cpStatusMsgLabel = new QLabel(this);
	cpStatusMsgLabel->setObjectName("StatusMsgLabel");
	cpStatusBar->addWidget(cpStatusMsgLabel);

	// Row/Column. Add a Row/Column display to the status bar.
	QFrame* apHbox = new QFrame;
	apHbox->setFrameStyle(QFrame::Panel | QFrame::Sunken);
	apHbox->setMaximumHeight(26);
	QLabel* apRowTitle = new QLabel("Row:", apHbox);
	cpRowLabel = new QLabel(apHbox);
	cpRowLabel->setMinimumWidth(20);
	cpRowLabel->setNum(0);		// Set initial value.
	QLabel* apColTitle = new QLabel("Col:", apHbox);
	cpColLabel = new QLabel(apHbox);
	cpColLabel->setMinimumWidth(20);
	cpColLabel->setNum(0);		// Set initial value.
	QHBoxLayout *apLayout = new QHBoxLayout;
	apLayout->setMargin(2);
	apLayout->addWidget(apRowTitle);
	apLayout->addWidget(cpRowLabel);
	apLayout->addWidget(apColTitle);
	apLayout->addWidget(cpColLabel);
	apHbox->setLayout(apLayout);
	cpStatusBar->addPermanentWidget(apHbox);

	// EngineBusy - Add engine busy icon on the right.
	cpEngineButton = new QToolButton(this);
	//cEngineBusyIcon.addFile(":/images/mainengineidle.png", QSize(22,22), QIcon::Normal, QIcon::Off);
	//cEngineBusyIcon.addFile(":/images/mainenginebusy.png", QSize(22,22), QIcon::Disabled, QIcon::Off);
	cpEngineButton->setIcon(cEngineIdleIcon);
	cpStatusBar->addPermanentWidget(cpEngineButton);
}

/*!
 * \brief Create toolbars.
 */
void AMainWindow::createToolBars()
{
	cpMainToolBar = addToolBar(tr("Main"));
	cpMainToolBar->addAction(cpConnectAct);
	cpMainToolBar->addAction(cpHelpAct);
	cpFormToolBar = addToolBar(tr("Form"));
	cpPageToolBar = addToolBar(tr("Page"));
}

/*!
 * \brief Set/reset engine busy flag and busy/arrow cursor.
 *
 * \param[in] iIsEnabled Enable/disable flag.
 *
 * \return void
 *
 * \note
 * -# Each form enables/disables its own widgets.
 */
void AMainWindow::enable(bool iIsEnabled)
{
	setCursor(iIsEnabled ? Qt::ArrowCursor : Qt::BusyCursor);
	engineIdle(iIsEnabled);

	// Main Actions. Enable/disable selected actions.
	cpCloseAct->setEnabled(iIsEnabled);
	cpCloseAllAct->setEnabled(iIsEnabled);
	cpConnectAct->setEnabled(iIsEnabled);
	cpExitAct->setEnabled(iIsEnabled);
}

/*!
 * \brief Set/reset engine busy flag$.
 *
 * \param[in] iIsEngineIdle Engine idle flag.
 *
 * \return void
 *
 * \note
 * -# The engine idle icon turns green when the engine is idle/ red when it is busy.
 */
void AMainWindow::engineIdle(bool iIsEngineIdle)
{
	if (iIsEngineIdle != true)
		cpEngineButton->setIcon(cEngineBusyIcon);
	else
		cpEngineButton->setIcon(cEngineIdleIcon);
}

/*!
 * \brief Returns a pointer to the active cabinet settings.
 */
AParameters* AMainWindow::getCabinetParameters()
{
	return &cCabParams;
}

/*!
 * \brief Returns a pointer to the active console settings.
 */
AParameters* AMainWindow::getConsoleParameters()
{
	return &cConsoleParams;
}

/*!
 * \brief Returns a pointer to the active debug settings.
 */
AParameters* AMainWindow::getDebugParameters()
{
	return &cDebugParams;
}

/*!
 * \brief Returns a pointer to the active editor settings.
 */
AParameters* AMainWindow::getEditorParameters()
{
	return &cEditParams;
}

/*!
 * \brief Returns a pointer to the active log settings.
 */
AParameters* AMainWindow::getLogParameters()
{
	return &cLogParams;
}

/*!
 * \brief Reimplement in order to catch Help key presses
 *
 * \param[in] ipEvent Ptr to the event emitted by main event loop.
 *
 * \return void
 *
 * \note
 * -# A Help key press will be caught in an edit window.  If not caught in an edit window, it is caught here.
 */
void AMainWindow::keyPressEvent(QKeyEvent* ipEvent)
{
	if (ipEvent->key()== Qt::Key_F1)
	{	ipEvent->accept();
		onHelpActivated();
	}
}

/*!
 * \brief Show the About dialog.
 */
void AMainWindow::onAbout()
{
	QString aAbout = QString("<b>AIS - Analytic Information Server&trade;</b><br>"
		"Copyright &copy; 2003-2009 All rights reserved. Investment Science Corporation<br>"
		"AIS Version " AGBL_AISVERSION "<br>"
		"Installed at %1<br><br>"
		"<b>Analytic Information Server</b><br>"
		"Each instance of the AIS engine attaches to a user defined TCP/IP port, making each instance of the AIS engine an "
		"network-aware analytic server. AIS lambdas can serve their analytic reports directly to a web browser or to any other "
		"program, which can open a TCP/IP connection. Pure HTML, XML, or a proprietary AMP protocol is supported and each "
		"of these protocols can be used to invoke individual lambdas residing on available AIS engines. Furthermore, lambdas in "
		"one instance of the AIS engine can invoke lambdas in any other available AIS engine connected by TCP/IP. Each instance "
		"of the AIS engine can support multiple lambda contexts, each running on its own machine thread.<br><br>"
		"<b>For More Information</b><ul>"
		"<li>Select Help in this menu.</li>"
		"<li>Visit us at <a href=https://sourceforge.net/projects/aiserver>https://sourceforge.net/projects/aiserver</a></li></ul>"
		).arg(cInstallPath);
	QMessageBox::about(this, tr("About AIS"), aAbout);
}

void AMainWindow::onAboutQt()
{
	QMessageBox::aboutQt(this, tr("About Qt"));
}

/*!
 * \brief Handles the allConnectionsClosed signal.
 *
 * \note
 * Calls close() which will trigger the closeEvent() function for final cleanup.
 */
void AMainWindow::onAllConnectionsClosed()
{
	if (cIsClosing)
		close();
}

/*!
 * \brief Handles the formClosed signal.
 *
 * \note
 * Calls close() which will trigger the closeEvent() function for the next cleanup.
 */
void AMainWindow::onChildWindowClosed(bool iClosed)
{
	// if iClosed == true
	if (iClosed && cIsClosing)
		close();
	else
		cIsClosing = false;
}

/*!
 * \brief Launches/shows the Connection Manager window.
 */
void AMainWindow::onConnect()
{
	cpConnectMgr->exec();
}

/*!
 * \brief Display the IDE preferences dialog and apply the changes.
 */
void AMainWindow::onEditPreferences()
{
	if (cpEditPrefDialog == NULL)
		cpEditPrefDialog = new AIdePrefDialog(this, "IDE Preferences");

	if (cpEditPrefDialog == NULL)
		return;

	// Temporary variables to be used for checking
	AParameters aCabParams;
	AParameters aConsoleParams;
	AParameters aDebugParams;
	AParameters aEditorParams;
	AParameters aLogParams;

	// Copy settings to preferences dialog
	cpEditPrefDialog->setCabinetParameters(&cCabParams);
	cpEditPrefDialog->setConsoleParameters(&cConsoleParams);
	cpEditPrefDialog->setDebugParameters(&cDebugParams);
	cpEditPrefDialog->setEditorParameters(&cEditParams);
	cpEditPrefDialog->setLogParameters(&cLogParams);

	// Display preferences dialog
	if (cpEditPrefDialog->exec() == QDialog::Accepted)
	{
		// Retrieve updated settings
		cpEditPrefDialog->getCabinetParameters(&aCabParams);
		cpEditPrefDialog->getConsoleParameters(&aConsoleParams);
		cpEditPrefDialog->getDebugParameters(&aDebugParams);
		cpEditPrefDialog->getEditorParameters(&aEditorParams);
		cpEditPrefDialog->getLogParameters(&aLogParams);

		// Get list of child windows
		QList<QWidget*> aChildForms = cpWorkspace->windowList();
		long aSz = aChildForms.size();
		AForm* apChildForm = 0;

		// Pass the new settings to each child window
		for (long i = 0; i < aSz; i++)
		{
			apChildForm = qobject_cast<AForm*>(aChildForms.at(i));
			if (apChildForm != NULL)
				apChildForm->setParameters(&aConsoleParams, &aEditorParams, &aLogParams, &aCabParams, &aDebugParams);
		}

		// Update stored settings
		cConsoleParams.mFont = aConsoleParams.mFont;
		cConsoleParams.mFontSize = aConsoleParams.mFontSize;
		cConsoleParams.mTabWidth = aConsoleParams.mTabWidth;
		cConsoleParams.mLineNumbers = aConsoleParams.mLineNumbers;
		cConsoleParams.mShowWhiteSpace = aConsoleParams.mShowWhiteSpace;
		cConsoleParams.mWrap = aConsoleParams.mWrap;

		cEditParams.mFont = aEditorParams.mFont;
		cEditParams.mFontSize = aEditorParams.mFontSize;
		cEditParams.mTabWidth = aEditorParams.mTabWidth;
		cEditParams.mLineNumbers = aEditorParams.mLineNumbers;
		cEditParams.mShowWhiteSpace = aEditorParams.mShowWhiteSpace;
		cEditParams.mWrap = aEditorParams.mWrap;

		cLogParams.mFont = aLogParams.mFont;
		cLogParams.mFontSize = aLogParams.mFontSize;
		cLogParams.mLineNumbers = aLogParams.mLineNumbers;
		cLogParams.mWrap = aLogParams.mWrap;

		cCabParams.mFont = aCabParams.mFont;
		cCabParams.mFontSize = aCabParams.mFontSize;

		cDebugParams.mFont = aDebugParams.mFont;
		cDebugParams.mFontSize = aDebugParams.mFontSize;
		cDebugParams.mTabWidth = aDebugParams.mTabWidth;
	}
}

/*!
 * \brief Called on windowActivated signal.
 *
 * \param[in] ipWidget Pointer to activated window/form.
 */
void AMainWindow::onFormActivated(QWidget* ipWidget)
{
	// Switch to menus for this form
	AForm* apForm = qobject_cast<AForm*>(ipWidget);
	if (cpCurForm != apForm)
	{	// Remove. Remove the current child's menus and toolbars if any.
		QAction* apAction;
		QMenu* apMenu;
		AMenuList aFormMenus, aTabMenus;
		AToolList aFormTools, aTabTools;
		if (cpCurForm != NULL)
		{	cpCurForm->getFormMenus(aFormMenus, aTabMenus, aFormTools, aTabTools);
			foreach (apMenu, aFormMenus)
			{	apAction = apMenu->menuAction();
				cpMenuBar->removeAction(apAction);
			}
			foreach (apMenu, aTabMenus)
			{	apAction = apMenu->menuAction();
				cpMenuBar->removeAction(apAction);
			}
			foreach (apAction, aFormTools)
			{	cpFormToolBar->removeAction(apAction);
			}
			foreach (apAction, aTabTools)
			{	cpPageToolBar->removeAction(apAction);
			}
		}
		// Add. Add the new child form's menus to the menu bar and tools to the tool bar.
		cpCurForm = apForm;
		if (apForm != NULL)
		{	apForm->getFormMenus(aFormMenus, aTabMenus, aFormTools, aTabTools);
			foreach (apMenu, aFormMenus)
			{	apAction = apMenu->menuAction();
				apAction->setVisible(true);
				cpMenuBar->addAction(apAction);
			}
			foreach (apMenu, aTabMenus)
			{	apAction = apMenu->menuAction();
				apAction->setVisible(false);		// Don't show tab menus yet.
				cpMenuBar->addAction(apAction);
			}
			// Tools. Add in tools but do not make child tabs' tools visible.
			foreach (apAction, aFormTools)
			{	cpFormToolBar->addAction(apAction);
			}
			foreach (apAction, aTabTools)
			{	apAction->setVisible(false);		// Don't show tab tools yet.
				cpPageToolBar->addAction(apAction);
			}
		}
	}
}

/*!
 * \brief Called when Help button or shortcut key is activated.
 */
void AMainWindow::onHelpActivated()
{
	QString aGblClientBrowser(gAis.mGblParams.value("gblclientbrowser", APPCLIENT_BROWSER));
	QStringList aGblClientHelpUrl(gAis.mGblParams.value("gblclienthelpurl", APPCLIENT_HELPURL));
	cpHelpProcess->start(aGblClientBrowser, aGblClientHelpUrl);
}

/*!
 * \brief Help error.
 *
 * \param[in] iErr Process error type.
 */
void AMainWindow::onHelpError(QProcess::ProcessError iErr)
{
	QString aCaption("Browse Help Failed");
	QString aError;
	switch (iErr)
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
 * \brief Called whenever the window menu is expanded.
 */
void AMainWindow::onUpdateWindowMenu()
{
	// Clear causes the actions owned by this menu to be destroyed.  In turn, the mappings to these actions in the window mapper
	// are automatically removed.
	cpWindowMenu->clear();
	cpWindowMenu->addAction(cpCloseAct);
	cpWindowMenu->addAction(cpCloseAllAct);
	cpWindowMenu->addSeparator();
	cpWindowMenu->addAction(cpTileAct);
	cpWindowMenu->addAction(cpCascadeAct);
	cpWindowMenu->addSeparator();
	cpWindowMenu->addAction(cpNextAct);
	cpWindowMenu->addAction(cpPreviousAct);
	cpWindowMenu->addAction(cpSeparatorAct);

	QList<QWidget *> aWindows = cpWorkspace->windowList();
	long aNWindows = aWindows.size();
	cpSeparatorAct->setVisible(!aWindows.isEmpty());
	QWidget* apForm;
	QString  aText;
	for (long i = 0; i < aNWindows; ++i)
	{	apForm = aWindows.at(i);
		if (i < 9)
			aText = tr("&%1. %2").arg(i + 1).arg(apForm->objectName());
		else
			aText = tr("%1. %2").arg(i + 1).arg(apForm->objectName());
		QAction *apAction = cpWindowMenu->addAction(aText);
		apAction->setCheckable(true);
		apAction->setChecked(apForm == activeForm());
		// If any one of the open files in the list of active files is selected from the menu, then a signal is sent to the
		// window mapper's map slot.  setMapping causes map() to generate a mapped signal with apForm as an argument. This
		// signal, in turn, was connected to the Workspace's setActiveWindow slot in this object's constructor  So a triggered
		// signal from ANY of these actions in the map generates a call to the Workspace's setActiveWindow slot.  We could have
		// generated a connection to each action separately but then we would have to disconnect each action in the action's
		// destructor.  This is better.
		connect(apAction, SIGNAL(triggered()), cpWindowMapper, SLOT(map()));
		cpWindowMapper->setMapping(apAction, apForm);
	}
}

/*!
 * \brief Called from the ConnectMgr dialog to open a new session form or server form.
 *
 * \param[in] ipForm -> Form that is to be opened.
 *
 * \return void
 *
 * \sa
 * AForm for more information on the AForm public methods that are available to AMainWindow.
 *
 * \note
 * # Both types of forms inherit from the abstract base class AForm methods that can be called by AMainWindow.
*/
void AMainWindow::openForm(AForm* ipForm)
{
	// Form.  Set up server form or a session form.
	cpWorkspace->addWindow(ipForm);		// Generates windowActivated signal
	connect(ipForm, SIGNAL(formClosed(bool)), this, SLOT(onChildWindowClosed(bool)), Qt::QueuedConnection);
	// Connect parameter changed signal to slot
	connect(ipForm, SIGNAL(editPreferences()), this, SLOT(onEditPreferences()));
	ipForm->showMaximized();
}

/*!
 * \brief Read/load project settings.
 */
void AMainWindow::readSettings()
{
	QFont aDefault = QApplication::font();

	cSettings.beginGroup("Main Window");
	QPoint aPos = cSettings.value("pos", QPoint(200, 200)).toPoint();
	QSize aSize = cSettings.value("size", QSize(400, 400)).toSize();
	int aState = cSettings.value("state", Qt::WindowNoState).toInt();
	cSettings.endGroup();

	move(aPos);
	resize(aSize);
	setWindowState(Qt::WindowStates(aState));

	// Load Console Settings
	cSettings.beginGroup("Console");
	cConsoleParams.mFont = cSettings.value("Font", QVariant(ATextEdit::scDefaultFont)).toString();
	cConsoleParams.mFontSize = cSettings.value("FontSize", QVariant((int)ATextEdit::scDefaultFontSize)).toInt();
	cConsoleParams.mTabWidth = cSettings.value("TabWidth", QVariant((int)ATextEdit::scDefaultTabWidth)).toInt();
	cConsoleParams.mLineNumbers = cSettings.value("LineNumbers", QVariant(false)).toBool();
	cConsoleParams.mShowWhiteSpace = cSettings.value("ShowWhiteSpace", QVariant(false)).toBool();
	cConsoleParams.mWrap = cSettings.value("WordWrap", QVariant(false)).toBool();
	cConsoleParams.mMaxRowWidth = cSettings.value("MaxRowLgth", QVariant((int)ATextEdit::scDefaultMaxRowWidth)).toInt();
	cConsoleParams.mMaxUndoDepth = cSettings.value("MaxUndoDepth", QVariant((int)ATextEdit::scDefaultMaxUndoDepth)).toInt();
	cSettings.endGroup();

	// Load Text Editor Settings
	cSettings.beginGroup("Editor");
	cEditParams.mFont = cSettings.value("Font", QVariant(ATextEdit::scDefaultFont)).toString();
	cEditParams.mFontSize = cSettings.value("FontSize", QVariant((int)ATextEdit::scDefaultFontSize)).toInt();
	cEditParams.mTabWidth = cSettings.value("TabWidth", QVariant((int)ATextEdit::scDefaultTabWidth)).toInt();
	cEditParams.mLineNumbers = cSettings.value("LineNumbers", QVariant(false)).toBool();
	cEditParams.mShowWhiteSpace = cSettings.value("ShowWhiteSpace", QVariant(false)).toBool();
	cEditParams.mWrap = cSettings.value("WordWrap", QVariant(false)).toBool();
	cEditParams.mMaxRowWidth = cSettings.value("MaxRowLgth", QVariant((int)ATextEdit::scDefaultMaxRowWidth)).toInt();
	cEditParams.mMaxUndoDepth = cSettings.value("MaxUndoDepth", QVariant((int)ATextEdit::scDefaultMaxUndoDepth)).toInt();
	cSettings.endGroup();

	// Load Log Page Settings
	cSettings.beginGroup("Log");
	cLogParams.mFont = cSettings.value("Font", QVariant(ATextEdit::scDefaultFont)).toString();
	cLogParams.mFontSize = cSettings.value("FontSize", QVariant((int)ATextEdit::scDefaultFontSize)).toInt();
	cLogParams.mLineNumbers = cSettings.value("LineNumbers", QVariant(false)).toBool();
	cLogParams.mWrap = cSettings.value("WordWrap", QVariant(false)).toBool();
	cSettings.endGroup();

	// Load Cabinet Page Settings
	cSettings.beginGroup("Cabinet");
	cCabParams.mFont = cSettings.value("Font", QVariant(aDefault.family())).toString();
	cCabParams.mFontSize = cSettings.value("FontSize", QVariant(aDefault.pointSize())).toInt();
	cSettings.endGroup();

	// Loag Debug Page Settings
	cSettings.beginGroup("Debug");
	cDebugParams.mFont = cSettings.value("Font", QVariant(aDefault.family())).toString();
	cDebugParams.mFontSize = cSettings.value("FontSize", QVariant(aDefault.pointSize())).toInt();
	cDebugParams.mTabWidth = cSettings.value("TabWidth", QVariant((int)ATextEdit::scDefaultTabWidth)).toInt();
	cSettings.endGroup();
}

/*!
 * \brief Update the cursor row/column in the status bar to the cursor position in the
 * currently active text pane.
 *
 * \param[in] iCol Column position starting from column zero on the left.
 * \param[in[ iRow Row position starting from row zero at the top of the text.
 *
 * \return void
 *
 * \note
 * -# The displayed row/column are both incremented by one so that they both start from one.
*/
void AMainWindow::setCursorLabel(long iCol, long iRow)
{
	cpRowLabel->setNum((int)iRow + 1);
	cpColLabel->setNum((int)iCol + 1);
}

/*!
 * \brief Initialize the initial set of forms for an in-process IDE.
 *
 * \param[in] irContextName The initial context for this form.
 * \param[in] irStartupMsgs Startup messages generated in Main that need a text-pane that can display them.
 *
 * \return void
 *
 * \sa
 * main in webide
 */
void AMainWindow::startup(const QString& irContextName, const QString& irStartupMsgs)
{
	// ConnectMgr. Start in-process server form, session form
	show();
	cpConnectMgr = new AConnectMgr(this, "ConnectMgr");
	connect(cpConnectMgr, SIGNAL(allConnectionsClosed()), this, SLOT(onAllConnectionsClosed()), Qt::QueuedConnection);
	cpConnectMgr->startup(irContextName, irStartupMsgs);
}

/*!
 * \brief Show urgent message in the permanent section of the status bar.
 *
 * \param[in] irMsg Message to be displayed in the status bar.
 *
 * \return void
 */
void AMainWindow::statusAlert(const QString& irMsg)
{
	if (!irMsg.isEmpty())
		QApplication::beep();
	cpStatusAlertLabel->setText(irMsg);
}

/*!
 * \brief Show information message in the left section of the status bar.
 *
 * \param[in] irMsg Message to be displayed in the status bar.
 *
 * \return void
 */
void AMainWindow::statusMsg(const QString& irMsg)
{
	if (!irMsg.isEmpty())
		cpStatusBar->clearMessage();
	cpStatusMsgLabel->setText(irMsg);
}

/*!
 * \brief Save application settings.
 */
void AMainWindow::writeSettings()
{
	cSettings.beginGroup("Main Window");
	cSettings.setValue("pos", pos());
	cSettings.setValue("size", size());
	cSettings.setValue("state", (int)windowState());
	cSettings.endGroup();

	// Write Console Settings
	cSettings.beginGroup("Console");
	cSettings.setValue("Font", QVariant(cConsoleParams.mFont));
	cSettings.setValue("FontSize", QVariant((int)cConsoleParams.mFontSize));
	cSettings.setValue("TabWidth", QVariant((int)cConsoleParams.mTabWidth));
	cSettings.setValue("LineNumbers", QVariant((int)cConsoleParams.mLineNumbers));
	cSettings.setValue("ShowWhiteSpace", QVariant((int)cConsoleParams.mShowWhiteSpace));
	cSettings.setValue("WordWrap", QVariant((int)cConsoleParams.mWrap));
	cSettings.setValue("MaxRowLgth", QVariant((int)cConsoleParams.mMaxRowWidth));
	cSettings.setValue("MaxUndoDepth", QVariant((int)cConsoleParams.mMaxUndoDepth));
	cSettings.endGroup();

	// Write Editor Settings
	cSettings.beginGroup("Editor");
	cSettings.setValue("Font", QVariant(cEditParams.mFont));
	cSettings.setValue("FontSize", QVariant((int)cEditParams.mFontSize));
	cSettings.setValue("TabWidth", QVariant((int)cEditParams.mTabWidth));
	cSettings.setValue("LineNumbers", QVariant((int)cEditParams.mLineNumbers));
	cSettings.setValue("ShowWhiteSpace", QVariant((int)cEditParams.mShowWhiteSpace));
	cSettings.setValue("WordWrap", QVariant((int)cEditParams.mWrap));
	cSettings.setValue("MaxRowLgth", QVariant((int)cEditParams.mMaxRowWidth));
	cSettings.setValue("MaxUndoDepth", QVariant((int)cEditParams.mMaxUndoDepth));
	cSettings.endGroup();

	// Write Log Settings
	cSettings.beginGroup("Log");
	cSettings.setValue("Font", QVariant(cLogParams.mFont));
	cSettings.setValue("FontSize", QVariant((int)cLogParams.mFontSize));
	cSettings.setValue("LineNumbers", QVariant((int)cLogParams.mLineNumbers));
	cSettings.setValue("WordWrap", QVariant((int)cLogParams.mWrap));
	cSettings.endGroup();

	// Write Cab Settings
	cSettings.beginGroup("Cabinet");
	cSettings.setValue("Font", QVariant(cCabParams.mFont));
	cSettings.setValue("FontSize", QVariant((int)cCabParams.mFontSize));
	cSettings.endGroup();

	// Write Debug Settings
	cSettings.beginGroup("Debug");
	cSettings.setValue("Font", QVariant(cDebugParams.mFont));
	cSettings.setValue("FontSize", QVariant((int)cDebugParams.mFontSize));
	cSettings.setValue("TabWidth", QVariant((int)cDebugParams.mTabWidth));
	cSettings.endGroup();
}

// end
