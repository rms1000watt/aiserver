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
#ifndef ASESSIONFORM_H
#define ASESSIONFORM_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/asessionform.h
															Session Form

CHANGE HISTORY
Version	Date		Who		Change
4.0003	9/28/2008	tlw	CR128. add subscribe to allow a connection to catch output from another session.
4.0003	 7/12/2008	rca  	[CR-125] Removed compile on/off actions.
3.2009	 5/08/2008	fchua	Updated setParameters interface.
3.2009	 5/08/2008	fchua	Added getCabinetParameters, getDebugParameters.
3.2008	 4/08/2008	fchua	Added cpEditorTab member variable.
3.2008	 4/08/2008	fchua	Added setParameters, getConsoleParameters, getEditorParameters, onEditPreferences.
3.2006	 3/10/2008	fchua	Added new signals, slots and functions to fix the dialog focus issue.
3.1007	 1/04/2008	fchua	processStartup. Added new irError argument.
3.1006	12/21/2007	fchua	Added iSessionId parameter in startup.
3.1005	12/14/2007	fchua	Added closeEvent. Added cConnected and cIsClosing member variables.
3.1003	10/23/2007	fchua	Added ~ASessionForm.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/9/2003	tlw		onCursorStackChanged. Enable/disable cursor stack operations.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0107	9/21/2006	tlw		Add cursor stack operations.
1.0070	10/14/2005	tlw		Convert to Qt4
1.0039	4/29/2004	tlw		Revise specification
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QProcess>

#include "ais.h"		// AReqType
#include "aform.h"		// AForm
#include "autilities.h" // AStringMap
#include "aparameters.h"

class QSignalMapper;
class AAppClient;
class AConsolePage;
class ADebugPage;
class AMainWindow;
class APage;
class AOperationsDialog;
class AEditorTab;
class ACabinetPage;
class ADataPage;
class ADataPreviewPage;
class AModelPage;
class ARunPage;
class AResultsPage;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define SESSION_CONSOLE         0   // Tab index to Console page
#define SESSION_CABINET         1   // Tab index to Cabinets page
#define SESSION_EDITOR          2   // Tab index to Editor tab
#define SESSION_DEBUG           3   // Tab index to Debugger page
#define SESSION_DATA            4   // Tab index to Data page
#define SESSION_PREVIEW         5   // Tab index to Preview page
#define SESSION_MODEL           6   // Tab index to Model page
#define SESSION_RUN             7   // Tab index to Run page
#define SESSION_RESULTS         8   // Tab index to Results page
#define SESSION_PAGES           4   // Number of child pages (change this to 9 to enable extra tabs)
//#define SESSION_NODE		3	// Node generated context menu event (see onContextEvent)

typedef struct
{	bool		mActive;
	bool		mRunScript;
	QString		mStartupMsgs;
	QString		mUsrName;
	QString		mPasswd;
} AFormTask;

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ASessionForm - Session Form is instantiated in the AMainForm workspace to allow user to create, modify and debug AIS code.

 */
class ASessionForm : public AForm, public AReturnRcvr
{
    Q_OBJECT
public:
	ASessionForm(AAppClient* ipAppClient, AMainWindow* ipParent, const char* ipSessionName);
	~ASessionForm();

	// ReturnRcvr Methods:
	virtual void returnOutput(long iDummyId, long iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
				 , char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
				 = QString());
	virtual void setContextName(long iConnectId, const QString& irContextName);

	// AForm Methods:
	virtual void getFormMenus(AMenuList& orFormMenus, AMenuList& orTabMenus, AToolList& orFormTools, AToolList& orTabTools);
	virtual void setParameters(AParameters* ipConsole, AParameters* ipEditor, AParameters* ipLog, AParameters* ipCabinet, AParameters* ipDebug);
	virtual void startup(const QString& irStartupMsgs);

	// Public Methods:
	void		consoleOutput(const QString& irMsg);
	void		cursorStackChanged(bool iForward, bool iPrevious);
	bool		debuggerActive();
	void		enable(bool iIsEnabled);
	void		engineIdle(bool iIsEngineIdle);

	AParameters* getCabinetParameters();
	AParameters* getConsoleParameters();
	AParameters* getDebugParameters();
	AParameters* getEditorParameters();

	void		openPage(const QString& irPageName, const QString& irPageType);
	void		setCurrentTab(long iTabIx, const QString& irMsg);
	void		setCursorLabel(long iCol, long iRow);
	void		setDebuggerActive(bool iActive);
	void		setEngineState(long iState);
	void		setSessionId(long iSessionId);
	void		statusAlert(const QString& irMsg);
    void		startup(const QString& irContextName, bool iRunScript, const QString& irStartupMsgs,
						const QString irUsrName, const QString irPasswd, long iSessionId = 0);
	void		statusMsg(const QString& irMsg);

    /*!
      \brief Return data tab pointer.
      */
    ADataPage* getDataPage();

    /*!
      \brief Return model tab pointer.
      */
    AModelPage* getModelPage();

    /*!
      \brief Return run tab pointer.
      */
    ARunPage* getRunPage();

    void setDemoRunMode(bool runmode);

	QString getCommandPrefix();

public slots:
	void		onReturnMsg(AReqType iReqType, const QString& irOut);

signals:
	void		findPatternResult(bool iResult);
	void		replaceFindPatternResult(bool iResult);
	void		replacePatternResult(bool iResult);

protected:
	void		closeEvent(QCloseEvent*);

private slots:
	//void		onCompile();
	void		onCopy();
	void		onCurrentTabChanged(int iTabIdx);
	void		onCut();
	void		onEditPreferences();
	void		onFind();
	void		onFindPattern(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord);
	void		onGoTo();
	void		onGoToLine(long iLineNum);
	void		onNext();
	void		onPaste();
	void		onPrev();
	void		onPrint();
	void		onRedo();
	void		onReplace();
	void		onReplaceFindPattern(const QString& irPattern, bool iAll, bool iMatchCase,
								bool iDown, bool iRegExp, bool iSelect, bool iMatchWord);
	void		onReplacePattern(const QString& irPattern, const QString& irText,
								bool iAllText, bool iMatchCase, bool iRegExp, bool iMatchWord);
	void		onReplaceAllPattern(const QString& irPattern, const QString& irText,
								bool iAllText, bool iMatchCase, bool iRegExp, bool iSelect, bool iMatchWord);
	void		onUndo();
	void		onViewActivated();
	void		onViewError(QProcess::ProcessError iErrCode);

private:

    void		createActions();
    void		createMenus();
    void		processStartup(AReqType iReqType, long iRetValue, const QString& irOut, const QString& irError);

    bool            cDemoRunMode;

    AAppClient*             cpAppClient;
    ACabinetPage*           cpCabinetTab;
    AConsolePage*           cpConsoleTab;
    ADebugPage*             cpDebugTab;
    ADataPage*              cpDataTab;
    ADataPreviewPage*       cpDataPreviewTab;
    AModelPage*             cpModelTab;
    ARunPage*               cpRunTab;
    AResultsPage*           cpResultsTab;

    QAction*		cpCompileAct;
    bool			cConnected;
    AStringMap		cContextMap;
    QString			cContextName;
    QAction*		cpCopyAct;
    APage*			cpCurPage;
    QAction*		cpCutAct;
    bool			cDebuggerActive;
    QMenu*			cpEditorContextMenu;
    QAction*		cpFindAct;
    AOperationsDialog*	cpFindDialog;
    AOperationsDialog*	cpGoDialog;
    QMenu*			cpEditMenu;				// Form-level menus.
    AEditorTab*		cpEditorTab;
    AToolList		cEditTools;
    bool			cIsClosing;
    QAction*		cpNextAct;
    QMenu*			cpNodeContextMenu;
    APage*			cpPages[SESSION_PAGES];
    AMainWindow*	cpParent;
    QAction*		cpPasteAct;
    QAction*		cpPrevAct;
    QAction*		cpPrintAct;
    QAction*		cpReplaceAct;
    QAction*		cpEditPrefAct;
    AOperationsDialog*	cpReplaceDialog;
    QAction*		cpRedoAct;
    QAction*		cpSeparatorAct;
    long			cSessionId;
    AToolList		cSessionTools;
    AFormTask		cTask;
    QAction*		cpUndoAct;
    QAction*		cpViewAct;
    QProcess*		cpViewProcess;
};

#endif // ASESSIONFORM_H
