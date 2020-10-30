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
#ifndef AEDITORTAB_H
#define AEDITORTAB_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/aeditortab.h
															Editor Tab

CHANGE HISTORY
Version	Date		Who		Change
4.0003	 7/12/2008	rca  	[CR-125] Removed compile on/off actions.
3.2008	 4/08/2008	fchua	Added setParameters function.
3.2006	 3/10/2008	fchua	Added cpFcnListDialog member.
3.2006	 3/10/2008	fchua	Removed find and replace functions. Added getTextEdit.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/7/2006	tlw		Omit unused destructor. onCursorStackChanged.  Add slot to save changes to stack enables.
1.0107	9/21/2006	tlw		Add move for cursor stack. Remove superflous slots onCopy,onCut,onFind,onPaste,onRedo,onReplace,onUndo
1.0106	9/14/2006	tlw		Added cStaleMsg, cStaleAlert
1.0070	10/14/2005	tlw		Revise specification
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QTabWidget>
#include "aform.h"				// AForm, AToolList
#include "ais.h"				// AReqType, AReturnRcvr
#include "aeditpage.h"
#include "apage.h"				// APage
#include "asaveasagentdialog.h"

class AAppClient;
class ASessionForm;
class ATextEdit;
class AOperationsDialog;

//	------------------------------------------------------ TYPEDEFS -----------------------------------------------------------
typedef QHash<QString, AEditPage*> APageMap;		// Key: FullPageName, Value: AEditPage for this page.

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief AEditorTab provides a GUI to allow the user to edit files and agents.

AEditorTab is itself a tabbed widget that is one of the pages in a set of tabbed pages in a SessionForm.  It allows the user to
files and AIS Lisp source code.
*/
class AEditorTab : public QTabWidget, public APage, public AReturnRcvr
{
	Q_OBJECT

public:
	AEditorTab(AAppClient* ipAppClient, ASessionForm* ipParentForm, 
				QWidget* ipParent, const char* ipPageName = NULL);
	// ReturnRcvr methods:
	virtual bool connectionClosed(long iConnectId);
	virtual	ATextEdit* getTextEdit();
	virtual void returnOutput(long iDummyId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
				 , char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
				 = QString());
	virtual void setContextName(long iConnectId, const QString& irContextName);

	// Page methods:
	virtual void	copy();
	virtual void	cut();
	virtual QWidget*getFocusWidget();
	virtual void	getTabMenus(AMenuList &orTabMenus, AToolList &orTabTools, bool iSelected);
	virtual void	move(bool iNext);
	virtual void	openPage(const QString& irExtentName, const QString& irPageName, AEditPage::APageType iPageType);
	virtual void	paste();
	virtual void	print();
	virtual void	redo();
	virtual void	undo();
	virtual void	exportCabinet(const QString& aExportSync, 
		const QString& aMessage, 
		const QString& aName, 
		const QString& aLocation,
		const QString& aStorageScope);

	void			setParameters(AParameters* ipEditorParams);

protected:
	virtual void	contextMenuEvent(QContextMenuEvent* ipContextEvent);
	
private slots:
	void		onClose();
	void		onCloseAll();
	void		onComment();
	void		onSaveCompileDebug();
	void		onCurrentTabChanged(int iTabIdx);
	void		onCursorPositionChanged(long iCol, long iRow);
	void		onCursorStackChanged(bool iForward, bool iPrevious);
	void		onIndent();
	void		onNew();
	void		onOpen();
	void		onOutdent();
	void		onSave();
	void		onSaveAll();
	void		onSaveAsAgent();
	void		onSaveAsExport();
	void		onSaveAsLocalFile();
	void		onSaveAsRemoteFile();
	void		onSaveCompile();
	void		onSaveCompileAll();
	void		onShowAgents();
	void		onStatusAlert(const QString& irMsg);
	void		onTextModified(bool iModified);
	void		onUncomment();

private:
	enum ATask	{ ceNoTask, ceSaveAsAgent, ceSaveAll, ceSaveCompile, ceSaveCompileDebug, ceSaveCompileAll};
	bool		addPage(QString& irFileSpec, AEditPage::APageType iPageType);
	void		compile(AEditPage* ipPage);
	void		compileDebug(AEditPage* ipPage);
	void		createActions();
	void		createMenus();
	bool		launchNextSave(long iTabIx, ATask iTask);
	bool		loadFile(const QString& irFileSpec);
	void		save(AEditPage* ipPage);
	void		saveAsAgent(AEditPage* ipPage);
	void		saveAsLocal(AEditPage* ipPage);
	void		saveAsRemote(AEditPage* ipPage);
	void		saveAgent(AEditPage* ipPage);
	void		saveLocal(AEditPage* ipPage);
	void		saveRemote(AEditPage* ipPage);
	void		setTabModified(bool iModified, long iTabIx);

	AAppClient*			cpAppClient;
	QAction*			cpCloseAct;
	QAction*			cpCloseAllAct;
	QAction*			cpCommentAct;
	QAction*			cpCompileDebug;
	QMenu*				cpCodeMenu;
	QString				cCurFileSpec;	// File path + file name
	QString				cCurFileName;	// File name w/ extension
	AEditPage*			cpCurPage;
	QMenu*				cpEditorContextMenu;
	AOperationsDialog*	cpFcnListDialog;
	QMenu*				cpFileMenu;
	QAction*			cpIndentAct;
	bool				cIsUntitled;
	QAction*			cpNewAct;
	QAction*			cpOpenAct;
	QAction*			cpOutdentAct;
	ASessionForm*		cpParentForm;
	QAction*			cpSaveAct;
	QAction*			cpSaveAllAct;
	QAction*			cpSaveAsLocalFileAct;
	QMenu*				cpSaveAsMenu;
	QAction*			cpSaveAsRemoteFileAct;
	QAction*			cpSaveAsAgentAct;
	ASaveAsAgentDialog	cSaveAsAgentDialog;
	QAction*			cpSaveAsExportAct;
	QAction*			cpSaveCompileAct;
	QAction*			cpSaveCompileAllAct;
	QAction*			cpSeparatorAct;
	QAction*			cpShowAgentsAct;
	bool				cStaleAlert;
	bool				cStaleMsg;
	APageMap			cTabList;
	ATask				cTask;
	long				cTaskIx;
	AToolList			cToolList;
	QAction*			cpUncommentAct;
};

#endif	// AEDITORTAB_H
