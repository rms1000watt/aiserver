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
#ifndef ACONSOLEPAGE_H
#define ACONSOLEPAGE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/aconsolepage.h
															Console Page

CHANGE HISTORY
Version	Date		Who		Change
3.2009	05/08/2007	fchua	Added setParameters().
3.2008	 4/08/2008	fchua	Added ipTextEditParams parameter in constructor.
3.2006	 3/10/2008	fchua	Removed find and replace functions. Added getTextEdit.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0110	10/14/2006	tlw		cEnable. Disable runCommand while request pending
1.0107	8/21/2006	tlw		Add move method to catch calls from onPrev and onNext for cursor stack.
1.0100	6/9/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "ais.h"
#include "apage.h"
#include "aremotefiledialog.h"
#include "ui_aconsolepage.h"

class QAction;
class QLineEdit;
class QMenu;
class QWidget;
class AAppClient;
class ASessionForm;
class AForm;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief AConsolePage provides a GUI to allow the user to send commands to an AIS server and to view the results.

AConsolePage is the first page of a set of tabbed pages in a SessionForm.  It allows the user to send commands and to control
the execution of these commands via a set of flags such as InstructionTrace, ErrorTrace, Jit, and SystemCheck.

\par Notes:
 -# Commands can be entered from the command line or by pressing Ctrl-Enter at the end of a command in the text pane.
 -# Commands can also be selected (highlighted) in the text pane and then run from the context menu.
 */
class AConsolePage : public QWidget, public APage, public AReturnRcvr
{
    Q_OBJECT
public:
    AConsolePage(AAppClient* ipAppClient, ASessionForm* ipParentForm,
				QWidget* ipParent, const char* ipName = NULL);
	enum AConsoleTask {meNoTask, meErrorTrace, meInstructionTrace, meJit, meSetEscape, meSysCheck};
	void			append(const QString& irMsg);
	void			enable(bool iEnable);
	ATextEdit*		getTextEdit();
	void			setEngineState(long iMode);
	void			setParameters(AParameters* ipParams);
	void			submit(QString& irCmd);
	void			updatePrefix();

	// AReturnRcvr Methods:
public:
	virtual bool	connectionClosed(long iConnectId);
	virtual void	returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
					, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
					= QString());
	virtual void	setContextName(long iConnectId, const QString& irContextName);

	// APage Methods:
	virtual void	copy();
	virtual void	cut();
	virtual QWidget*getFocusWidget();
	virtual void	getTabMenus(AMenuList &orTabMenus, AToolList &orTabTools, bool iSelected);
	virtual void	move(bool iNext);
	virtual void	paste();
	virtual void	print();
	virtual QString selectedText();
	virtual void	redo();
	virtual void	undo();

protected:
	virtual void	contextMenuEvent(QContextMenuEvent* ipContextEvent);

private slots:
	void			onClear();
	void			onCmdReturnPressed();
	void			onCursorPositionChanged(long iCol, long iRow);
	void			onDebugBreakpoint();
	void			onErrorTrace();
	void			onInstructionTrace();
	void			onJit();
	void			onRunCommand(const QByteArray& irCurLine);
	void			onRunRemoteScript();
	void			onRunSelection();
	void			onShowSelection();
	void			onStatusAlert(const QString& irMsg);
	void			onStop();
	void			onSysCheck();

private:
	void			createActions();
	void			createMenus();

	AAppClient*		cpAppClient;
	QLineEdit*		cpCmdLineEdit;
	QMenu*			cpConsoleContextMenu;
	QMenu*			cpConsoleMenu;
	AToolList		cConsoleTools;
	QAction*		cpDebugBreakpointAct;
	bool			cEnable;
	QAction*		cpErrorTraceAct;
	QWidget*		cpFocusWidget;
	QAction*		cpInstructionTraceAct;
	QAction*		cpJitAct;
	long			cMaxLines;
	ASessionForm*	cpParentForm;
  ARemoteFileDialog	cRemoteFileDialog;
	QAction*		cpRunRemoteScriptAct;
	QAction*		cpRunSelectionAct;
	QAction*		cpShowSelectionAct;
	QAction*		cpStopAct;
	QAction*		cpSysCheckAct;
	AConsoleTask	cTask;

	Ui::AConsolePageClass cUi;
};

#endif // ACONSOLEPAGE_H

