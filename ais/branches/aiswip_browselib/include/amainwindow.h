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

#ifndef AMAINWINDOW_H
#define AMAINWINDOW_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/amainwindow.h
															Main Window

CHANGE HISTORY
Version	Date		Who		Change
3.2009   5/08/2008	fchua	Added new members cDebugParams, cCabParams.
3.2009   5/08/2008	fchua	Added getCabinetParameters, getDebugParameters.
3.2008	4/08/2008	fchua	Added new members cConsoleParams, cEditParams, cLogParams, cSettings.
3.2008	4/08/2008	fchua	Added IDE preferences dialog cpEditPrefDialog.
3.2008	4/08/2008	fchua	Added getConsoleParameters, getEditorParameters, getLogParameters, onEditPreferences.
3.2002	2/04/2008	fchua	Added onAllConnectionsClosed, onChildWindowClosed.
1.0100	6/11/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QProcess>
#include <QtCore/QSettings>
#include <QtGui/QIcon>
#include <QtGui/QMainWindow>
#include "aparameters.h"

class QLabel;
class QSignalMapper;
class QToolButton;
class QWorkspace;
class AAppClient;
class AConnectMgr;
class AForm;
class AIdePrefDialog;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief AMainWindow - Implements the main application window.

This is an MDI (multiple-document interface) application that allows zero or more session (IDE) forms and server-monitor forms.
 */
class AMainWindow : public QMainWindow
{
	Q_OBJECT
public:
    AMainWindow(const QString& irTitle);
	void		openForm(AForm* ipForm);
	void		startup(const QString& irContextName, const QString& irStartupMsgs);
	// Call back methods
	void		enable(bool iIsEnabled);
	void		engineIdle(bool iIsEngineIdle);
	void		setCursorLabel(long iCol, long iRow);
	void		statusAlert(const QString& irMsg);
	void		statusMsg(const QString& irMsg);
	
	AParameters* getCabinetParameters();
	AParameters* getConsoleParameters();
	AParameters* getDebugParameters();
	AParameters* getEditorParameters();
	AParameters* getLogParameters();

protected:
	virtual	void keyPressEvent(QKeyEvent* ipEvent);
	virtual void closeEvent(QCloseEvent* ipEvent);

private slots:
	void		onAbout();
	void		onAboutQt();
	void		onAllConnectionsClosed();
	void		onChildWindowClosed(bool iClosed);
	void		onConnect();
	void		onEditPreferences();
	void		onFormActivated(QWidget* ipForm);
	void		onHelpActivated();
	void		onHelpError(QProcess::ProcessError iErr);
	void		onUpdateWindowMenu();

private:
	AForm*		activeForm();
	void		createActions();
	void		createMenus();
	void		createToolBars();
	void		createStatusBar();
	AForm*		findForm(const QString& irFileSpec);
	void		readSettings();
	void		writeSettings();

    QAction*		cpAboutAct;
    QAction*		cpAboutQtAct;
    QAction*		cpCascadeAct;
	QAction*		cpCloseAct;
	QAction*		cpCloseAllAct;
	QAction*		cpConnectAct;
    QAction*		cpExitAct;
	QAction*		cpHelpAct;
    QAction*		cpNextAct;
    QAction*		cpPreviousAct;
    QAction*		cpSeparatorAct;
    QAction*		cpTileAct;

	QIcon			cAisIcon;
	QLabel*			cpColLabel;
	AConnectMgr*	cpConnectMgr;
	AForm*			cpCurForm;
	QToolButton*	cpEngineButton;
	QIcon			cEngineBusyIcon;
	QIcon			cEngineIdleIcon;
    QToolBar*		cpFormToolBar;
    QMenu*			cpHelpMenu;
	QProcess*		cpHelpProcess;
	QString			cInstallPath;
	QToolBar*		cpMainToolBar;
	QMenuBar*		cpMenuBar;
	QToolBar*		cpPageToolBar;
	QLabel*			cpRowLabel;
	QStatusBar*		cpStatusBar;
	QLabel*			cpStatusMsgLabel;
	QLabel*			cpStatusAlertLabel;
	QSignalMapper*	cpWindowMapper;
    QMenu*			cpWindowMenu;
    QWorkspace*		cpWorkspace;

	bool			cIsClosing;
	
	AParameters		cCabParams;
	AParameters		cConsoleParams;
	AParameters		cDebugParams;
	AParameters		cEditParams;
	AParameters		cLogParams;
	
	QSettings		cSettings;
	AIdePrefDialog*	cpEditPrefDialog;	
};

#endif
