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
#ifndef ACABINETPAGE_H
#define ACABINETPAGE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/acabinetpage.h
															Cabinet Page

CHANGE HISTORY
Version	Date		Who		Change
4.0003	7/12/2008	rca  	[CR-125] Removed compile on/off actions.
3.2009	05/08/2007	fchua	Added setParameters().
3.1003	10/29/2007	fchua	Added member function clear().
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0114	11/15/2006	tlw		onHeaderClicked. Use new QSortFilterProxy capability.
1.0113	11/7/2006	tlw		Remove unused destructor
1.0100	4/20/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_aaboutdialog.h"


#include "ui_acabinetpage.h"
#include "ais.h"			// AReqType
#include "apage.h"			// APage, AActionList
#include "anodevaluedialog.h"
#include "aremotefiledialog.h"
#include "acabsettingsdialog.h"
#include "amanagecabinetsdialog.h"
#include "aregisterdirdialog.h"

class AAppClient;
class ASessionForm;
class QAction;
class QMenu;
class QWidget;
class QSortFilterProxyModel;
class QStandardItemModel;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
 * \brief Implements the cabinet page used in each session form.
 *
 */
class ACabinetPage : public QWidget, public APage, public AReturnRcvr
{
    Q_OBJECT
public:
    ACabinetPage(AAppClient* ipAppClient, ASessionForm* ipParentForm, QWidget* ipParent, const char* ipName = NULL);
	QModelIndex findExtent(const QString& irExtentName);
	void		setExtentFocus(const QModelIndex& irIx);

	// APage Methods.
	virtual QWidget*getFocusWidget();
	virtual void	getTabMenus(AMenuList &orTabMenus, AToolList &orTabTools, bool iSelected);
	virtual void	show();
	virtual void	clear();

	// AReturnRcvr Methods.
	virtual bool connectionClosed(long iConnectId);
	virtual void returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue,const QString& irOut
				 , char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
				 = QString());
	virtual void setContextName(long iConnectId, const QString& irContextName);

	void setParameters(AParameters* ipCabinetParams);
	void checkCabinet(const QString& irCabinetName);
	// Handler for different tasks in returnOutput function.
	void importCabinetRemote(const QString& irOut);
	void importCabinetRemoteDir(const QString& irOut);
	void exportCabinetRemote(const QString& irOut);
	void exportCabinetRemoteDir(const QString& irOut);
public slots:
	void		onCabinetContextMenu(const QPoint& irPos);
	void		onCheckCabinet();
	void		onCloseCabinet();
	void		onCompileAgent();
	void		onCompileAgentDebug();
	void		onCompileCabinet();
	void		onCompileCabinetDebug();
	void		onCompileAllCabinets();
	void		onCompileAllCabinetsDebug();
	void		onDeleteNode();
	void		onDescendNode();
	void		onExportCabinetLocal();
	void		onExportCabinetRemote();
	void		onExportCabinetRemoteDir();
	void		onExportNodeLocal();
	void		onExportNodeRemote();
	void		onExtentDoubleClicked(const QModelIndex& irIdx);
	void		onImportCabinetLocal();
	void		onImportCabinetRemote();
	void		onImportCabinetRemoteDir();
	void		onManageCabinets();
	void		onNewCabinet();
	void		onNodeContextMenu(const QPoint& irPos);
	void		onNodeDoubleClicked(const QModelIndex& irIdx);
	void		onOpenCabinet();
	void		onOpenNode();
	void		onRefreshClicked();
	void		onRefreshCabinets();
	void		onRegisterDirectory();
	void		onRootClicked();
	void		onShowLockedVariables();
	void		onShowNodeContextMenu();
	void		onShowNodeValue();
	void		onShowSystemVariables();
	void		onUpClicked();
	void		onUpdateCabinetSettings();
	void		onWorkspaceStatistics();

private:
	enum		ACabinetTask {ecNone, ecExtentFocus, ecCheckCabinet, ecRefreshCabinet, 
					ecUpdateStatus, ecUpdateMetadata, ecImportCabinetRemote, ecExportCabinetRemote,
					ecImportCabinetRemoteDir, ecExportCabinetRemoteDir};
	void		createActions();
	void		createMenus();
	void		openSelectedNodes(long iRequest);
	void		showExtentNodes();

	AAppClient*			cpAppClient;
	QMenu*				cpCabinetContextMenu;
	QMenu*				cpCabinetMenu;
	AToolList			cCabinetTools;
	QIcon				cCheckOff;
	QIcon				cCheckOn;
	QIcon				cImport;
	QIcon				cExport;
	QStringList			cExtentStatusList;
	QIcon				cMerge;
	QIcon				cUnknown;
	QIcon				cUptodate;
	QAction*			cpCheckCabinetAct;
	QAction*			cpCloseCabinetAct;
	QAction*			cpCompileAgentAct;
	QAction*			cpCompileAgentDebugAct;
	QAction*			cpCompileCabinetAct;
	QAction*			cpCompileCabinetDebugAct;
	QAction*			cpCompileAllCabinetsAct;
	QAction*			cpCompileAllCabinetsDebugAct;
	long				cSortColumn;
	QAction*			cpDeleteNodeAct;
	QAction*			cpDescendNodeAct;
	QStandardItemModel*	cpExtentModel;
	QMenu*				cpExportCabinetMenu;
	QAction*			cpExportCabinetLocalAct;
	QAction*			cpExportCabinetRemoteAct;
	QAction*			cpExportCabinetRemoteDirAct;
	QMenu*				cpExportNodeMenu;
	QAction*			cpExportNodeLocalAct;
	QAction*			cpExportNodeRemoteAct;
	QMenu*				cpImportCabinetMenu;
	QAction*			cpImportCabinetLocalAct;
	QAction*			cpImportCabinetRemoteAct;
	QAction*			cpImportCabinetRemoteDirAct;
	bool				cManageCabinets;
	QAction*			cpManageCabinetsAct;
	QAction*			cpNewCabinetAct;
	QMenu*				cpNodeContextMenu;
	QStandardItemModel*	cpNodeModel;
 QSortFilterProxyModel*	cpNodeProxyModel;
	ANodeValueDialog	cNodeValueDialog;
	QAction*			cpOpenCabinetAct;
	QAction*			cpOpenNodeAct;
	ASessionForm*		cpParentForm;
	QAction*			cpRefreshCabinetsAct;
	QAction*			cpRegisterDirAct;
	ACabSettingsDialog	cCabSettingsDialog;
	ARemoteFileDialog	cRemoteFileDialog;
 AManageCabinetsDialog	cManageCabinetsDialog;
    ARegisterDirDialog	cRegisterDirDialog;
	QModelIndex			cSelectedIdx;
	QAction*			cpShowLockedVariablesAct;
	QAction*			cpShowNodeValueAct;
	QAction*			cpShowSystemVariablesAct;
	enum ACabinetTask	cNextTask; // Marker for the next task to be processed.
	enum ACabinetTask	cTask;
Ui::ACabinetPageClass	cUi;
	QAction*			cpUpdateSettingsAct;
	QAction*			cpWorkspaceStatisticsAct;
	QStringList			cUpdateList;
};

#endif // ACABINETPAGE_H
