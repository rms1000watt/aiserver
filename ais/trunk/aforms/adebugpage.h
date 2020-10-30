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
#ifndef ADEBUGGPAGE_H
#define ADEBUGGPAGE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/adebugpage.h
															Debug Page

CHANGE HISTORY
Version	Date		Who		Change
3.2009	05/08/2007	fchua	Added setParameters().
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0100	6/9/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "ais.h"		// AReqType
#include "apage.h"				// APage
#include "awatchdialog.h"		// AWatchDialog
#include "ui_adebugpage.h"
#include <QtGui/QMenu>

class QStandardItemModel;
class QWidget;
class AAppClient;
class ASessionForm;
class AReturnRcvr;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief ADebugPage provides a page in the ASessionForm tabbed widget to allow the user to debug their AIS Lisp code.

It allows the user to view source/virtual machine code step thru the execution and view variables at any point in the execution.

\par Notes:
 -# Commands can be entered from the command line or by selecting the toolbar icons.
 -# A list of debugging commands is available by selecting the help icon in the toolbar.
 -# To view source the agent must first be compiled with the compile flag turned on.
 -# The APage edit commands (copy, cut, find, paste, print, replace, selectedText, redo, undo) are not reimplemented by
 this class (the base class methods prevail).
 */
class ADebugPage : public QWidget, public APage
{
    Q_OBJECT
public:
    ADebugPage(AAppClient* ipAppClient, ASessionForm* ipParentForm, AReturnRcvr* ipReturnRcvr, const char* ipName = NULL);
	// APage Methods:
	virtual QWidget*getFocusWidget();
	virtual void	getTabMenus(AMenuList &orTabMenus, AToolList &orTabTools, bool iSelected);
	virtual void	show();
	// Other:
	void		debugLineEdit(const QString& irText);
	void		debugCodeLines(const QString& irCode, long iCurrentLine);
	void		debugInfoLines(const QString& irVariables);
	void		setParameters(AParameters* ipDebugParams);

signals:
	void		statusAlert(const QString& irMsg);

private slots:
	void		onCmdReturnPressed();
	void		onWatchPressed();
	void		onCodeListDoubleClicked(const QModelIndex&);
	void		onGoClicked();
	void		onHelpClicked();
	void		onQuitClicked();
	void		onRunToClicked();
	void		onSourceClicked();
	void		onStepIntoClicked();
	void		onStepOutClicked();
	void		onStepOverClicked();
	void		onVarListDoubleClicked(const QModelIndex& irIdx);
	void		onVarListCollapsed();
	void		onVarListExpanded();
	void		onWatchVariable();
	void		showTreeContextMenu(const QPoint& point);

private:
	void		sendShowVarList(bool aRetrieveVarsFlag);
	void		submit(const QString& irCmd);
	void		cleanVarTree(const QStringList iList);
	bool 		eventFilter(QObject* o, QEvent* e);


	AAppClient*			cpAppClient;
	QIcon				cBlankIcon;
	QStandardItemModel*	cpCodeListModel;
	long				cCodeLinesCount;
	QIcon				cCurrentIcon;
	long				cCurrentLine;
	ASessionForm*		cpParentForm;
	AReturnRcvr*		cpReturnRcvr;
	bool				cShowSource;
	QIcon				cSourceOff;
	QIcon				cSourceOn;
	Ui::ADebugPageClass	cUi;
	QStandardItemModel*	cpVarListModel;
	QModelIndex			cIdx;
	long				cTabWidth;
	AWatchDialog*		cpWatchList;
    QStandardItem*		cpRegisterTree;
    QStandardItem*		cpArgumentTree;
    QStandardItem*		cpPersistentTree;
    QStandardItem*		cpConstantTree;
    QStandardItem*		cpTemporaryTree;
    QStandardItem*		cpClassTree;
    QStandardItem*		cpInterfaceTree;
    QWidget*			cpLineNumberArea; // This is the gutter area inside the QPlainTextEdit Source Code viewer
    QAction*			cpWatchVariableAct;
    QMenu*				cpVarListTreeMenu;
	QString				cVarList;
};

#endif // ADEBUGPAGE_H
