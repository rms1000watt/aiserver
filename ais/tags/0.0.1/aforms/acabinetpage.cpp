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
aisdev/aforms/acabinetpage.cpp
														Cabinet Page Widget

CHANGE HISTORY
Version	Date		Who		Change
4.0003	7/12/2008	rca  	[CR-125] Removed compile on/off actions.
3.2009	05/08/2007	fchua	Added setParameters().
3.1005	12/14/2007	fchua	returnOutput. Enabled the form when processing GetWorkspaceStatistics.
3.1005	12/14/2007	fchua	Constructor. Removed setting of cRemoteFileDialog parent.
3.1003	10/29/2007	fchua	Added member function clear().
3.1002	10/22/2007	fchua	Added handling of disconnection in returnOutput. Added Doxyen documentation.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0116	11/29/2006	tlw		Constructor. Set column width after setting model.
1.0114	11/15/2006	tlw		onDeleteNode,onExportNodeRemote,openSelectedNodes. Revise to use ID in col 6 as mNodes index.
1.0105	8/13/2006	tlw		returnOutput. In case of getNextLevel showExtentNodes unless iStatus > 0.
1.0100	4/20/2006	tlw		Add Doxygen documentation
1.0070	10/16/2005	tlw		Create cabinet.ui
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QFileInfo>
#include <QtGui/QContextMenuEvent>
#include <QtGui/QHeaderView>
#include <QtGui/QInputDialog>
#include <QtGui/QMenu>
#include <QtGui/QMessageBox>
#include <QtGui/QSortFilterProxyModel>
#include <QtGui/QStandardItemModel>
#include "appclient.h"
#include "acabinetpage.h"
#include "asessionform.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
 * \brief Constructor instantiates the GUI elements generated in ui_cabinetpage.h.
 *
 * \param[in] ipAppClient -> Application client that forwards commands to the AIS server.
 * \param[in] ipParentForm -> Parent form that created this instance of a cabinet page.
 * \param[in] ipParent -> Parent widget that hosts this page.
 * \param[in] ipName -> Name assigned to this instance of the cabinet page.
 */
ACabinetPage::ACabinetPage(AAppClient* ipAppClient, ASessionForm* ipParentForm, QWidget* ipParent, const char* ipName):
    QWidget(ipParent), cpAppClient(ipAppClient),
    cCheckOff(":images/checkoff.png"), cCheckOn(":images/checkon.png"), cImport(":images/statusimport.png"),
    cExport(":images/statusexport.png"), cMerge(":images/statusmerge.png"), cUnknown(":images/statusunknown.png"),
    cUptodate(":images/statusuptodate.png"), cSortColumn(0), cNodeValueDialog(ipParent,"NodeValueDialog"), cpParentForm(ipParentForm), 
    cCabSettingsDialog(ipAppClient,ipParent,"",true,0), cRemoteFileDialog(ipAppClient,"&path",this,"Cabinet File Name"),
    cManageCabinetsDialog(ipAppClient,ipParent,"", true, 0), cRegisterDirDialog(ipParent), cTask(ecNone)
{
    Q_UNUSED(ipName);

	// Gui. Configure widgets
	cUi.setupUi(this);

	// Extent View. Configure the Extent list view
	cUi.upExtentListView->setEditTriggers(QAbstractItemView::NoEditTriggers);
	cUi.upExtentListView->setSelectionBehavior(QAbstractItemView::SelectRows);
	cUi.upExtentListView->setSelectionMode(QAbstractItemView::SingleSelection);
	cUi.upExtentListView->setContextMenuPolicy(Qt::CustomContextMenu);	// Enable customContextMenuRequested signal
	cUi.upExtentListView->setShowGrid(false);	// Enable customContextMenuRequested signal
	QHeaderView* headerView = cUi.upExtentListView->horizontalHeader();	// Enable customContextMenuRequested signal
	headerView->setVisible(false);
	cUi.upExtentListView->setHorizontalHeader(headerView);	// Enable customContextMenuRequested signal
	headerView = cUi.upExtentListView->verticalHeader();	// Enable customContextMenuRequested signal
	headerView->setVisible(false);
	cUi.upExtentListView->setVerticalHeader(headerView);	// Enable customContextMenuRequested signal
	cUi.upExtentListView->setShowGrid(false);	// Enable customContextMenuRequested signal
	// The next line was removed since it causes a weird behavior in the remote file dialog
	//cRemoteFileDialog.setParent(cUi.upExtentListView, Qt::Popup);		// Return focus to ExtentListView
	connect(cUi.upExtentListView, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(onCabinetContextMenu(const QPoint&)));
	connect(cUi.upExtentListView, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(onExtentDoubleClicked(const QModelIndex&)));
	connect(cUi.upExtentListView, SIGNAL(clicked(const QModelIndex&)), this, SLOT(onExtentDoubleClicked(const QModelIndex&)));

	// Extent Model. Configure the extent model to feed the list view
	cpExtentModel = new QStandardItemModel(0/*Rows*/, 2/*Cols*/, this);
	cpExtentModel->setObjectName("ExtentModel");
	cUi.upExtentListView->setModel(cpExtentModel);

	// Node View. Configure the node table view.
	cUi.upNodeTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);
	cUi.upNodeTableView->setSelectionBehavior(QAbstractItemView::SelectItems);
	cUi.upNodeTableView->setSelectionMode(QAbstractItemView::ExtendedSelection);
	cUi.upNodeTableView->setShowGrid(false/*Show*/);
	cUi.upNodeTableView->setContextMenuPolicy(Qt::CustomContextMenu);		// Enable customContextMenuRequested signal
	connect(cUi.upNodeTableView, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(onNodeContextMenu(const QPoint&)));
	QHeaderView* apVerticalHeader = cUi.upNodeTableView->verticalHeader();
	apVerticalHeader->hide();
	connect(cUi.upNodeTableView, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(onNodeDoubleClicked(const QModelIndex&)));

	// Node Model. Configure the node model to feed the table view
	cpNodeModel = new QStandardItemModel(1/*Rows*/, 7/*Columns*/, this);
	cpNodeModel->setObjectName("NodeModel");
	cpNodeModel->setHeaderData(0/*Column*/, Qt::Horizontal, QVariant("Name/Value"), Qt::EditRole);
	cpNodeModel->setHeaderData(1/*Column*/, Qt::Horizontal, QVariant("Type"), Qt::EditRole);
	cpNodeModel->setHeaderData(2/*Column*/, Qt::Horizontal, QVariant("Size"), Qt::EditRole);
	cpNodeModel->setHeaderData(3/*Column*/, Qt::Horizontal, QVariant("Date"), Qt::EditRole);
	cpNodeModel->setHeaderData(4/*Column*/, Qt::Horizontal, QVariant("Time"), Qt::EditRole);
	cpNodeModel->setHeaderData(5/*Column*/, Qt::Horizontal, QVariant("Version"), Qt::EditRole);
	cpNodeModel->setHeaderData(6/*Column*/, Qt::Horizontal, QVariant("Id"), Qt::EditRole);

	// Sorting. Configure Proxy Model to sort columns. Configure Table View to show sort indicator in header. For now, no filtering.
	cpNodeProxyModel = new QSortFilterProxyModel(this);
	cpNodeProxyModel->setDynamicSortFilter(true);
	cpNodeProxyModel->setSourceModel(cpNodeModel);
	cpNodeProxyModel->setSortCaseSensitivity(Qt::CaseSensitive);
	cUi.upNodeTableView->sortByColumn(0, Qt::AscendingOrder);
	cUi.upNodeTableView->setModel(cpNodeProxyModel);
	cUi.upNodeTableView->setSortingEnabled(true);
	cUi.upNodeTableView->setColumnWidth(0, 200);

	// Menus. Create actions, menus. Start out with invisible tools and menus
	createActions();
	createMenus();

	// Buttons.  Set Icons
	cUi.upRefreshButton->setIcon(QIcon(":images/folderrefresh.png"));
	connect(cUi.upRefreshButton, SIGNAL(clicked()), this, SLOT(onRefreshClicked()));
	cUi.upRootButton->setIcon(QIcon(":images/folderroot.png"));
	connect(cUi.upRootButton, SIGNAL(clicked()), this, SLOT(onRootClicked()));
	cUi.upUpButton->setIcon(QIcon(":images/folderup.png"));
	connect(cUi.upUpButton, SIGNAL(clicked()), this, SLOT(onUpClicked()));

	setParameters(ipParentForm->getCabinetParameters());
	cManageCabinets = false;
}
/*!
 * \brief Check the status of a single cabinet.
 */
void ACabinetPage::checkCabinet(const QString& irCabinetName)
{
	cTask = ecCheckCabinet;
	cpParentForm->enable(false);
	cpAppClient->setCurrentExtent(irCabinetName);
	cpAppClient->checkCabinet(this, irCabinetName);
	#if 0
	QModelIndex aIdx;
	// Search for the cabinet and mark it with an unknown status
	for (long aExtent = 0; aExtent < aCount; ++aExtent)
	{
		aIdx = cpExtentModel->index(aExtent, 0/*Col*/);
		const QString& arExtentName = cpExtentModel->data(aIdx, Qt::DisplayRole).toString();
		if( arExtentName == irCabinetName )
		{
			QIcon arStatusIcon;
			arStatusIcon = cUnknown;
			cpExtentModel->setData( aIdx.sibling( aIdx.row(), 1 ), qVariantFromValue(arStatusIcon), Qt::DecorationRole);
		}
	}
	#endif
}


/*!
 * \brief Inherited from the AReturnRcvr abstract base class.
 *
 * \param[in] iConnectId The ID of the connection that was closed.
 * \return false
 */
bool ACabinetPage::connectionClosed(long iConnectId)
{
    Q_UNUSED(iConnectId);

	return false;
}

/*!
 * \brief Create action objects for menus.
 */
void ACabinetPage::createActions()
{
	// Actions.
	cpManageCabinetsAct = new QAction(tr("Manage Cabinets"), this);
	connect(cpManageCabinetsAct, SIGNAL(triggered()), this, SLOT(onManageCabinets()));

	cpCheckCabinetAct = new QAction(tr("Check Cabinet"), this);
	connect(cpCheckCabinetAct, SIGNAL(triggered()), this, SLOT(onCheckCabinet()));

	cpCloseCabinetAct = new QAction(tr("Close Cabinet"), this);
	connect(cpCloseCabinetAct, SIGNAL(triggered()), this, SLOT(onCloseCabinet()));

	cpCompileCabinetDebugAct = new QAction(tr("Compile Cabinet w/ Dbg"), this);
	connect(cpCompileCabinetDebugAct, SIGNAL(triggered()), this, SLOT(onCompileCabinetDebug()));

	cpCompileCabinetAct = new QAction(tr("Compile Cabinet"), this);
	connect(cpCompileCabinetAct, SIGNAL(triggered()), this, SLOT(onCompileCabinet()));

	cpCompileAgentAct = new QAction(tr("Compile Lambda"), this);
	connect(cpCompileAgentAct, SIGNAL(triggered()), this, SLOT(onCompileAgent()));

	cpCompileAgentDebugAct = new QAction(tr("Compile Lambda w/ Dbg"), this);
	connect(cpCompileAgentDebugAct, SIGNAL(triggered()), this, SLOT(onCompileAgentDebug()));

	cpCompileAllCabinetsAct = new QAction(tr("Compile All Cabinets"), this);
	connect(cpCompileAllCabinetsAct, SIGNAL(triggered()), this, SLOT(onCompileAllCabinets()));

	cpCompileAllCabinetsDebugAct = new QAction(tr("Compile All Cabinets w/ Dbg"), this);
	connect(cpCompileAllCabinetsDebugAct, SIGNAL(triggered()), this, SLOT(onCompileAllCabinetsDebug()));

	cpDeleteNodeAct = new QAction(tr("Delete Node"), this);
	connect(cpDeleteNodeAct, SIGNAL(triggered()), this, SLOT(onDeleteNode()));

	cpDescendNodeAct = new QAction(tr("Descend into Node"), this);
	connect(cpDescendNodeAct, SIGNAL(triggered()), this, SLOT(onDescendNode()));

	cpExportCabinetLocalAct = new QAction(tr("To Local File..."), this);
	connect(cpExportCabinetLocalAct, SIGNAL(triggered()), this, SLOT(onExportCabinetLocal()));

	cpExportCabinetRemoteAct = new QAction(tr("To Remote File..."), this);
	cpExportCabinetRemoteAct->setStatusTip(tr("Export contents of cabinet to file on server"));
	connect(cpExportCabinetRemoteAct, SIGNAL(triggered()), this, SLOT(onExportCabinetRemote()));

	cpExportCabinetRemoteDirAct = new QAction(tr("To Remote Directory..."), this);
	cpExportCabinetRemoteDirAct->setStatusTip(tr("Export contents of cabinet to directory on server"));
	connect(cpExportCabinetRemoteDirAct, SIGNAL(triggered()), this, SLOT(onExportCabinetRemoteDir()));

	cpExportNodeLocalAct = new QAction(tr("To Local File"), this);
	cpExportNodeLocalAct->setStatusTip(tr("Export contents of node to local file"));
	connect(cpExportNodeLocalAct, SIGNAL(triggered()), this, SLOT(onExportNodeLocal()));

	cpExportNodeRemoteAct = new QAction(tr("To Remote File..."), this);
	connect(cpExportNodeRemoteAct, SIGNAL(triggered()), this, SLOT(onExportNodeRemote()));

	cpImportCabinetLocalAct = new QAction(tr("From Local File..."), this);
	connect(cpImportCabinetLocalAct, SIGNAL(triggered()), this, SLOT(onImportCabinetLocal()));

	cpImportCabinetRemoteAct = new QAction(tr("From Remote File..."), this);
	cpImportCabinetRemoteAct->setStatusTip(tr("Replace contents of cabinet with file on server"));
	connect(cpImportCabinetRemoteAct, SIGNAL(triggered()), this, SLOT(onImportCabinetRemote()));

	cpImportCabinetRemoteDirAct = new QAction(tr("From Remote Directory..."), this);
	cpImportCabinetRemoteDirAct->setStatusTip(tr("Replace contents of cabinet with folder on server"));
	connect(cpImportCabinetRemoteDirAct, SIGNAL(triggered()), this, SLOT(onImportCabinetRemoteDir()));

	cpNewCabinetAct = new QAction(QIcon(":/images/filenew.png"), tr("New Cabinet..."), this);
	cpNewCabinetAct->setToolTip("New Cabinet");
	cpNewCabinetAct->setStatusTip("Create a new file cabinet");
	connect(cpNewCabinetAct, SIGNAL(triggered()), this, SLOT(onNewCabinet()));

	cpRefreshCabinetsAct = new QAction(QIcon(":/images/refreshcabinet.png"), tr("Refresh Cabinets"), this);
	cpRefreshCabinetsAct->setToolTip("Refresh Cabinet List");
	cpRefreshCabinetsAct->setStatusTip("Retrieve current list of cabinets from BrowseAgent");
	connect(cpRefreshCabinetsAct, SIGNAL(triggered()), this, SLOT(onRefreshCabinets()));

	cpOpenCabinetAct = new QAction(QIcon(":/images/fileopen.png"), tr("Open Cabinet..."), this);
	cpOpenCabinetAct->setToolTip("Open Cabinet");
	cpOpenCabinetAct->setStatusTip("Open an existing cabinet");
	connect(cpOpenCabinetAct, SIGNAL(triggered()), this, SLOT(onOpenCabinet()));

	cpOpenNodeAct = new QAction(tr("Open Node"), this);
	connect(cpOpenNodeAct, SIGNAL(triggered()), this, SLOT(onOpenNode()));

	cpShowLockedVariablesAct = new QAction(tr("Show Locked Variables"), this);
	connect(cpShowLockedVariablesAct, SIGNAL(triggered()), this, SLOT(onShowLockedVariables()));

	cpShowNodeValueAct = new QAction(tr("Show Node Value..."), this);
	connect(cpShowNodeValueAct, SIGNAL(triggered()), this, SLOT(onShowNodeValue()));

	cpShowSystemVariablesAct = new QAction(tr("Show System Variables"), this);
	cpShowSystemVariablesAct->setCheckable(true);
	connect(cpShowSystemVariablesAct, SIGNAL(triggered()), this, SLOT(onShowSystemVariables()));

	cpRegisterDirAct = new QAction(tr("Register Directory"), this);
	connect(cpRegisterDirAct, SIGNAL(triggered()), this, SLOT(onRegisterDirectory()));

	cpUpdateSettingsAct = new QAction(tr("Update Cabinet Settings"), this);
	connect(cpUpdateSettingsAct, SIGNAL(triggered()), this, SLOT(onUpdateCabinetSettings()));

	cpWorkspaceStatisticsAct = new QAction(tr("Display Workspace Statistics"), this);
	connect(cpWorkspaceStatisticsAct, SIGNAL(triggered()), this, SLOT(onWorkspaceStatistics()));

	// Tools. Initialize cabinet-level tool bar
	cCabinetTools << cpNewCabinetAct << cpOpenCabinetAct << cpRefreshCabinetsAct;
}

/*!
 * \brief Create menus and sub-menus.
 */
void ACabinetPage::createMenus()
{
	// Cabinet Submenus.
	cpExportCabinetMenu = new QMenu("Export Cabinet", this);
	cpExportNodeMenu = new QMenu("Export Node", this);
	cpImportCabinetMenu = new QMenu("Import Cabinet", this);
	cpExportCabinetMenu->addAction(cpExportCabinetRemoteAct);
	cpExportCabinetMenu->addAction(cpExportCabinetRemoteDirAct);
	cpExportCabinetMenu->addAction(cpExportCabinetLocalAct);
	cpExportNodeMenu->addAction(cpExportNodeRemoteAct);
	cpExportNodeMenu->addAction(cpExportNodeLocalAct);
	cpImportCabinetMenu->addAction(cpImportCabinetRemoteAct);
	cpImportCabinetMenu->addAction(cpImportCabinetRemoteDirAct);
	cpImportCabinetMenu->addAction(cpImportCabinetLocalAct);

	// Cabinet Menu.
	cpCabinetMenu = new QMenu("Cabinets", this);
	cpCabinetMenu->addAction(cpNewCabinetAct);
	cpCabinetMenu->addAction(cpOpenCabinetAct);
	cpCabinetMenu->addAction(cpCloseCabinetAct);
	cpCabinetMenu->addAction(cpCheckCabinetAct);
	cpCabinetMenu->addAction(cpCompileCabinetAct);
	cpCabinetMenu->addAction(cpCompileCabinetDebugAct);
	cpCabinetMenu->addAction(cpCompileAllCabinetsAct);
	cpCabinetMenu->addAction(cpCompileAllCabinetsDebugAct);
	cpCabinetMenu->addAction(cpManageCabinetsAct);
	cpCabinetMenu->addAction(cpRefreshCabinetsAct);
	cpCabinetMenu->addAction(cpRegisterDirAct);
	cpCabinetMenu->addMenu(cpImportCabinetMenu);
	cpCabinetMenu->addMenu(cpExportCabinetMenu);
	cpCabinetMenu->addSeparator();
	cpCabinetMenu->addAction(cpDeleteNodeAct);
	cpCabinetMenu->addAction(cpOpenNodeAct);
	cpCabinetMenu->addAction(cpCompileAgentAct);
	cpCabinetMenu->addAction(cpCompileAgentDebugAct);
	cpCabinetMenu->addMenu(cpExportNodeMenu);
	cpCabinetMenu->addSeparator();
	cpCabinetMenu->addAction(cpWorkspaceStatisticsAct);
	cpCabinetMenu->addAction(cpShowLockedVariablesAct);
	cpCabinetMenu->addAction(cpShowSystemVariablesAct);

	// Context Menus.
	cpCabinetContextMenu = new QMenu("Cabinet Context Menu", this);
	cpCabinetContextMenu->addAction(cpNewCabinetAct);
	cpCabinetContextMenu->addAction(cpOpenCabinetAct);
	cpCabinetContextMenu->addAction(cpCloseCabinetAct);
	cpCabinetContextMenu->addAction(cpCheckCabinetAct);
	cpCabinetContextMenu->addAction(cpCompileCabinetAct);
	cpCabinetContextMenu->addAction(cpCompileCabinetDebugAct);
	cpCabinetContextMenu->addAction(cpCompileAllCabinetsAct);
	cpCabinetContextMenu->addAction(cpCompileAllCabinetsDebugAct);
	cpCabinetContextMenu->addAction(cpManageCabinetsAct);
	cpCabinetContextMenu->addAction(cpRefreshCabinetsAct);
	//cpCabinetContextMenu->addAction(cpRegisterDirAct);
	cpCabinetContextMenu->addAction(cpUpdateSettingsAct);
	cpCabinetContextMenu->addMenu(cpImportCabinetMenu);
	cpCabinetContextMenu->addMenu(cpExportCabinetMenu);

	cpNodeContextMenu = new QMenu("Cabinet Context Menu", this);
	cpNodeContextMenu->addAction(cpShowNodeValueAct);
	cpNodeContextMenu->addAction(cpDescendNodeAct);
	cpNodeContextMenu->addAction(cpOpenNodeAct);
	cpNodeContextMenu->addAction(cpDeleteNodeAct);
	cpNodeContextMenu->addAction(cpCompileAgentAct);
	cpNodeContextMenu->addAction(cpCompileAgentDebugAct);
	cpNodeContextMenu->addMenu(cpExportNodeMenu);
	connect(cpNodeContextMenu, SIGNAL(aboutToShow()), this, SLOT(onShowNodeContextMenu()));

	// Disable Node Actions. Disable selected node-context-menu actions. See onShowNodeContextMenu for details.
	cpShowNodeValueAct->setEnabled(false);
	cpDescendNodeAct->setEnabled(false);
	cpExportNodeMenu->setEnabled(false);
}

/*!
 * \brief Look up extent name in the extent model.
 *
 * \param[in] irExtentName Name of extent.
 *
 * \return aIdx - the index into the data model for the extent.
 */
QModelIndex ACabinetPage::findExtent(const QString& irExtentName)
{
	QModelIndex aIdx;
	long aIx, aSize = cpExtentModel->rowCount();
	for (aIx = 0; aIx < aSize; ++aIx)
	{	aIdx = cpExtentModel->index(aIx, 0);
		QString aExtentName = cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
		if (aExtentName == irExtentName)
			break;
	}
	return aIx >= aSize ? QModelIndex() : aIdx;
}

/*!
 * \brief Returns a pointer to the focused widget.
 *
 * \return Pointer to QWidget.
 */
QWidget* ACabinetPage::getFocusWidget()
{
	QWidget* apFocus = focusWidget();
	if (apFocus == NULL)
		apFocus = cUi.upExtentListView;
	return apFocus;
}

/*!
 * \brief Look up extent name in the extent model.
 *
 * \param[out] orTabMenus List of menu items returned by this routine.
 * \param[out] orTabTools List of toolbar icons returned by this routine.
 * \param[in] iSelected True iff this tab is selected.
 *
 * \note
 * -# Implements pure virtual function required by APageMethods
 * -# This page does not have any toolbar icons.
 */
void ACabinetPage::getTabMenus(AMenuList &orTabMenus, AToolList &orTabTools, bool iSelected)
{
    Q_UNUSED(iSelected);

	// Menus
	orTabMenus.clear();
	orTabMenus << cpCabinetMenu;
	orTabTools.clear();
	orTabTools = cCabinetTools;

	// Status. Initialize status messages when this tab is selected/deselected.
	cpParentForm->statusAlert("");			// Erase stale messages.
}

/*!
 * \brief Display context menu of cabinet.
 *
 * \param[in] irPos Position.
 */
void ACabinetPage::onCabinetContextMenu(const QPoint& irPos)
{
	cpCabinetContextMenu->exec(mapToGlobal(irPos));
}

/*!
 * \brief Register all the cabinets in a directory
 */
void ACabinetPage::onRegisterDirectory()
{
	cRegisterDirDialog.setDefault();
	if( cRegisterDirDialog.exec() == QDialog::Accepted )
	{
		cpParentForm->enable(false);
		QString aDatabaseDirectory = cRegisterDirDialog.getDatabaseDirectory();
		QString aSourceDirectory = cRegisterDirDialog.getSourceDirectory();
		cpAppClient->registerDirectory( this, aDatabaseDirectory, aSourceDirectory );
	}
}

/*!
 * \brief Register all the cabinets in a directory
 */
void ACabinetPage::onUpdateCabinetSettings()
{
	QString aCurName = cpAppClient->getCurrentExtentName();
	cpAppClient->getMetaData( this, aCurName );
	cTask = ecUpdateMetadata;
}

/*!
 * \brief Check the status of all cabinets and launch the update cabinet settings.
 */
void ACabinetPage::onManageCabinets()
{
	QModelIndex aIdx;
	QStringList aExtentList;
	//QItemSelectionModel* apSelectionModel = cUi.upExtentListView->selectionModel();
	long aSize = cpExtentModel->rowCount();

	for (long aRow = 0; aRow < aSize; ++aRow)
	{	aIdx = cpExtentModel->index(aRow, 0/*Column*/);
		aExtentList += cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
	}

	if( aExtentList.size() > 0 )
	{
		cTask = ecUpdateStatus;
		cUpdateList = aExtentList;
		cExtentStatusList.clear();
		// Set true flag for managing cabinets
		cManageCabinets = true;
		cpParentForm->enable(false);
		cpAppClient->checkCabinet(this, aExtentList.takeFirst());
	}
	else
	{
		cTask = ecNone;
		QString aMsg = "onManageCabinets:No Cabinets";
		cpParentForm->statusAlert(aMsg);
	}
}
/*!
 * \brief Check the status of a single cabinet.
 */
void ACabinetPage::onCheckCabinet()
{
	QModelIndex aIdx;
	QStringList aExtentList;
	QItemSelectionModel* apSelectionModel = cUi.upExtentListView->selectionModel();
	QModelIndexList aIndices = apSelectionModel->selectedIndexes();

	foreach(aIdx, aIndices)
		aExtentList += cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();

	if( aExtentList.size() > 0 )
	{
		cTask = ecCheckCabinet;
		cpParentForm->enable(false);
		cpAppClient->checkCabinet(this, aExtentList[0]);
	}
	else
	{
		cTask = ecNone;
		QString aMsg = "onCheckCabinet:No Cabinet selected";
		cpParentForm->statusAlert(aMsg);
	}
}

/*!
 * \brief Close cabinet.
 */
void ACabinetPage::onCloseCabinet()
{
	bool aOk;
	QStringList aList = cpAppClient->getCurrentExtentNames();
	QString aCurName = cpAppClient->getCurrentExtentName();
	QString aCabinet;
	long aCurItem = aList.indexOf(aCurName);
	if (aCurItem < 0) aCurItem = 0;
	aCabinet = QInputDialog::getItem(this, "Close Cabinet", "Select a cabinet to close", aList, aCurItem, false/*Editable*/, &aOk);
	if (aOk)
	{	cpParentForm->enable(false);
		cpAppClient->closeCabinet(this, aCabinet);
	}
}

/*!
 * \brief Compile agent.
 */
void ACabinetPage::onCompileAgent()
{
    QString apCmdOff;
    QString aAisOut("");
	QModelIndex aIdx;
	QStringList aAgentList;
	QString aExtent(cpAppClient->getCurrentExtentName());
	QItemSelectionModel* apSelectionModel = cUi.upNodeTableView->selectionModel();
	QModelIndexList aIndices = apSelectionModel->selectedIndexes();

    apCmdOff = "_ais\177eval\177exp\177(debug compileoff:)";
    cpAppClient->submit(this, apCmdOff, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);

	foreach(aIdx, aIndices)
	{	aAgentList += aExtent;
		aAgentList += cpNodeProxyModel->data(aIdx, Qt::DisplayRole).toString();
	}
	if (!aAgentList.isEmpty())
	{	cpParentForm->enable(false);
		cpAppClient->compileLambda(this, aAgentList);
	}
}

/*!
 * \brief Compile agent with debug compile on.
 */
void ACabinetPage::onCompileAgentDebug()
{
	QString apCmdOff;
	QString apCmdOn;
	QString aAisOut("");
	QModelIndex aIdx;
	QStringList aAgentList;
	QString aExtent(cpAppClient->getCurrentExtentName());
	QItemSelectionModel* apSelectionModel = cUi.upNodeTableView->selectionModel();
	QModelIndexList aIndices = apSelectionModel->selectedIndexes();

    apCmdOn = "_ais\177eval\177exp\177(debug compileon:)";
    apCmdOff = "_ais\177eval\177exp\177(debug compileoff:)";
    cpAppClient->submit(this, apCmdOn, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);

	foreach(aIdx, aIndices)
	{	aAgentList += aExtent;
		aAgentList += cpNodeProxyModel->data(aIdx, Qt::DisplayRole).toString();
	}
	if (!aAgentList.isEmpty())
	{	cpParentForm->enable(false);
		cpAppClient->compileLambda(this, aAgentList);
	}
    cpAppClient->submit(this, apCmdOff, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);
}

/*!
 * \brief Compile all cabinets.
 */
void ACabinetPage::onCompileAllCabinets()
{
	QString apCmdOff;
	QString aAisOut("");
	QModelIndex aIdx;
	QStringList aExtentList;
	long aSize = cpExtentModel->rowCount();

	apCmdOff = "_ais\177eval\177exp\177(debug compileoff:)";

	cpAppClient->submit(this, apCmdOff, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);

	for (long aRow = 0; aRow < aSize; ++aRow)
	{	aIdx = cpExtentModel->index(aRow, 0/*Column*/);	
		aExtentList += cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
	}
	cpAppClient->compileCabinet(this, aExtentList);
}


/*!
 * \brief Compile all cabinets with debug compile on.
 */
void ACabinetPage::onCompileAllCabinetsDebug()
{
	QString apCmdOn;
	QString apCmdOff;
	QString aAisOut("");
	QModelIndex aIdx;
	QStringList aExtentList;
	long aSize = cpExtentModel->rowCount();

	apCmdOn = "_ais\177eval\177exp\177(debug compileon:)";
	apCmdOff = "_ais\177eval\177exp\177(debug compileoff:)";

	cpAppClient->submit(this, apCmdOn, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);

	for (long aRow = 0; aRow < aSize; ++aRow)
	{	aIdx = cpExtentModel->index(aRow, 0/*Column*/);	
		aExtentList += cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
	}
	cpAppClient->compileCabinet(this, aExtentList);
	cpAppClient->submit(this, apCmdOff, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);
}

/*!
 * \brief Compile cabinet.
 */
void ACabinetPage::onCompileCabinet()
{
	QString apCmdOff;
	QString aAisOut("");
	QModelIndex aIdx;
	QStringList aExtentList;
	QItemSelectionModel* apSelectionModel = cUi.upExtentListView->selectionModel();
	QModelIndexList aIndices = apSelectionModel->selectedIndexes();

	apCmdOff = "_ais\177eval\177exp\177(debug compileoff:)";

	cpAppClient->submit(this, apCmdOff, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);

	foreach(aIdx, aIndices)
		aExtentList += cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
	if (!aExtentList.isEmpty())
	{	cpParentForm->enable(false);
		cpAppClient->compileCabinet(this, aExtentList);
	}
}

/*!
 * \brief Compile cabinet with debug compile on.
 */
void ACabinetPage::onCompileCabinetDebug()
{
	QString apCmdOn;
	QString apCmdOff;
	QString aAisOut("");
	QModelIndex aIdx;
	QStringList aExtentList;
	QItemSelectionModel* apSelectionModel = cUi.upExtentListView->selectionModel();
	QModelIndexList aIndices = apSelectionModel->selectedIndexes();

	apCmdOn = "_ais\177eval\177exp\177(debug compileon:)";
	apCmdOff = "_ais\177eval\177exp\177(debug compileoff:)";

	cpAppClient->submit(this, apCmdOn, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);

	foreach(aIdx, aIndices)
		aExtentList += cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
	if (!aExtentList.isEmpty())
	{	cpParentForm->enable(false);
		cpAppClient->compileCabinet(this, aExtentList);
	}
	cpAppClient->submit(this, apCmdOff, aAisOut, 0, NULL/*Data*/, 0/*DataSize*/);
}

/*!
 * \brief Delete node.
 */
void ACabinetPage::onDeleteNode()
{
	// Delete. Delete all the selected nodes.
	long aId;
	QModelIndex aIdx;
	QStringList aNodeList;
	QItemSelectionModel* apSelectionModel = cUi.upNodeTableView->selectionModel();
	QModelIndexList aIndices = apSelectionModel->selectedIndexes();
	foreach(aIdx, aIndices)
	{	aIdx = cpNodeProxyModel->index(aIdx.row(), 6/*Column*/);
		aId = cpNodeProxyModel->data(aIdx, Qt::DisplayRole).toInt();
		aNodeList += cpAppClient->getFullNodeName(aId);
	}
	if (!aNodeList.isEmpty() && QMessageBox::information(this, "Delete Nodes", "Are you sure you want to remove "
		"the selected nodes from this cabinet?", QMessageBox::Yes, QMessageBox::No) == QMessageBox::Yes)
	{	cpParentForm->enable(false);
		cpAppClient->eraseNode(this, aNodeList);
	}
}

/*!
 * \brief Descend node.
 */
void ACabinetPage::onDescendNode()
{
	openSelectedNodes(3/*Descend*/);
}

/*!
 * \brief Export cabinet locally.
 *
 * \note Not yet implemented.
 * \todo Add implementation.
 */
void ACabinetPage::onExportCabinetLocal()
{
	// PENDING...
}

/*!
 * \brief Export cabinet remotely.
 */
void ACabinetPage::onExportCabinetRemote()
{
	QString aCurName = cpAppClient->getCurrentExtentName();
	cpAppClient->getMetaData( this, aCurName );
	cTask = ecExportCabinetRemote;
}

/*!
 * \brief Export cabinet remotely.
 */
void ACabinetPage::onExportCabinetRemoteDir()
{
	QString aCurName = cpAppClient->getCurrentExtentName();
	cpAppClient->getMetaData( this, aCurName );
	cTask = ecExportCabinetRemoteDir;
}

/*!
 * \brief Export node locally.
 *
 * \note Not yet implemented.
 * \todo Add implementation.
 */
void ACabinetPage::onExportNodeLocal()
{
	// PENDING
}

/*!
 * \brief Export node remotely.
 */
void ACabinetPage::onExportNodeRemote()
{
	// Agent. Get the currently selected agent
	QString aCurExtent;
	QItemSelectionModel* apCurSelection = cUi.upNodeTableView->selectionModel();
	QModelIndex aIdx = cpNodeProxyModel->index(apCurSelection->currentIndex().row(), 6/*Column*/);
	long aIx = cpNodeProxyModel->data(aIdx, Qt::DisplayRole).toInt();
	QString aAgentName = cpAppClient->getFullNodeName(aIx);
	QString aFileName = cpAppClient->getNodeSymbolName(aIx);
	// File Path. Get the selected file path
	if (!aAgentName.isEmpty() && !((aCurExtent = cpAppClient->getCurrentExtentName()).isEmpty()))
	{	cRemoteFileDialog.setFilters("AIS (*.sl *.db);;Any (* *.*)");
		cRemoteFileDialog.setWindowTitle("Export Lambda - Specify destination file");
		cRemoteFileDialog.setFileName(aFileName + ".sl");
		cRemoteFileDialog.setDirectoriesOnly(false);
		if (cRemoteFileDialog.exec() == QDialog::Accepted)
		{	QString aFilePath = cRemoteFileDialog.filePath();
			if (!aFilePath.endsWith(QChar('/')))
			{	cpParentForm->enable(false);
				cpAppClient->exportNodeRemote(this, aCurExtent, aAgentName, aFilePath);
			}
		}
	}
}

/*!
 * \brief Called when extent item is double-clicked.
 *
 * \param[in] irIdx Model item index.
 */
void ACabinetPage::onExtentDoubleClicked(const QModelIndex& irIdx)
{
	setExtentFocus(irIdx.sibling(irIdx.row(), 0));
}

/*!
 * \brief Import cabinet locally.
 */
void ACabinetPage::onImportCabinetLocal()
{
	// PENDING
}

/*!
 * \brief Import cabinet remotely.
 */
void ACabinetPage::onImportCabinetRemote()
{
	QString aCurName = cpAppClient->getCurrentExtentName();
	cpAppClient->getMetaData( this, aCurName );
	cTask = ecImportCabinetRemote;
}

/*!
 * \brief Import cabinet remotely.
 */
void ACabinetPage::onImportCabinetRemoteDir()
{	
	QString aCurName = cpAppClient->getCurrentExtentName();
	cpAppClient->getMetaData( this, aCurName );
	cTask = ecImportCabinetRemoteDir;
}

/*!
 * \brief Create a new cabinet.
 */
void ACabinetPage::onNewCabinet()
{
	cCabSettingsDialog.setDefault();
	if (cCabSettingsDialog.exec() == QDialog::Accepted)
	{
		QString aName = cCabSettingsDialog.getDatabaseName();
		QString aFilePath = cCabSettingsDialog.getDatabaseLocation();
		QString aFileLocation = cCabSettingsDialog.getSourceLocation();
		QString aStorageScope = cCabSettingsDialog.getStorageScope();
		QString aImportSync = cCabSettingsDialog.getImportSync();
		QString aExportSync = cCabSettingsDialog.getExportSync();
		QString aAutoCompile = cCabSettingsDialog.getAutoCompile();
		QString aSearchSwitch = cCabSettingsDialog.getSearchSwitch();
		QString aDependencies = cCabSettingsDialog.getDependencies();
		cpParentForm->enable(false);
		cpAppClient->newCabinet(this, aName, aFilePath, aFileLocation, aStorageScope, aImportSync, aExportSync, aAutoCompile, aSearchSwitch, aDependencies);
	}
}

/*!
 * \brief Display context menu of node.
 *
 * \param[in] irPos Position.
 */
void ACabinetPage::onNodeContextMenu(const QPoint& irPos)
{
	QPoint aPos(irPos.x() + cUi.upExtentListView->width(), irPos.y() + 50);
	cpNodeContextMenu->exec(mapToGlobal(aPos));
}

/*!
 * \brief Called when node is double-clicked.
 *
 * \param[in] irIdx Model item index.
 */
void ACabinetPage::onNodeDoubleClicked(const QModelIndex& irIdx)
{
    Q_UNUSED(irIdx);

	openSelectedNodes(0/*Double-clicked*/);
}

/*!
 * \brief Open a cabinet.
 */
void ACabinetPage::onOpenCabinet()
{
	cCabSettingsDialog.setDefault();
	if (cCabSettingsDialog.exec() == QDialog::Accepted)
	{
		QString aName = cCabSettingsDialog.getDatabaseName();
		QString aFilePath = cCabSettingsDialog.getDatabaseLocation();
		QString aFileLocation = cCabSettingsDialog.getSourceLocation();
		QString aStorageScope = cCabSettingsDialog.getStorageScope();
		QString aImportSync = cCabSettingsDialog.getImportSync();
		QString aExportSync = cCabSettingsDialog.getExportSync();
		QString aAutoCompile = cCabSettingsDialog.getAutoCompile();
		QString aSearchSwitch = cCabSettingsDialog.getSearchSwitch();
		QString aDependencies = cCabSettingsDialog.getDependencies();
		cpParentForm->enable(false);
		cpAppClient->openCabinet(this, aName, aFilePath, aFileLocation, aStorageScope, aImportSync, aExportSync, aAutoCompile, aSearchSwitch, aDependencies);
	}
}

/*!
 * \brief Open a node.
 */
void ACabinetPage::onOpenNode()
{
	openSelectedNodes(1/*Open*/);
}

/*!
 * \brief Refresh list of nodes.
 */
void ACabinetPage::onRefreshClicked()
{
	cpNodeModel->removeRows(0/*StartRow*/, cpNodeModel->rowCount());
	cpParentForm->enable(false);
	cpAppClient->refreshLevel(this);
}

/*!
 * \brief Refresh list of cabinets.
 */
void ACabinetPage::onRefreshCabinets()
{
	cpAppClient->getExtentStatus(this);
	cTask = ecRefreshCabinet; // Set the current task to refresh cabinets
	cpParentForm->enable(false);
}

/*!
 * \brief Called when root node is clicked.
 */
void ACabinetPage::onRootClicked()
{
	cpNodeModel->removeRows(0/*StartRow*/, cpNodeModel->rowCount());
	cpParentForm->enable(false);
	cpAppClient->getRootLevel(this);
}

/*!
 * \brief Display locked variables.
 */
void ACabinetPage::onShowLockedVariables()
{
	QString aExtentType(".Memory"), aOptions("#1 ");
	aOptions += cpShowLockedVariablesAct->isChecked() ? "1 " : "0 ";
	aOptions += cpShowSystemVariablesAct->isChecked() ? "1)" : "0)";
	cpAppClient->setExtentTypeOptions(aExtentType, aOptions);
}

/*!
 * \brief Called just before showing the node context menu.
 */
void ACabinetPage::onShowNodeContextMenu()
{
	// Disable.
	cpShowNodeValueAct->setEnabled(false);
	cpDescendNodeAct->setEnabled(false);
	cpOpenNodeAct->setEnabled(false);
	cpDeleteNodeAct->setEnabled(false);
	cpCompileAgentAct->setEnabled(false);
	cpExportNodeMenu->setEnabled(false);

	// Selected. Determine number of selected nodes
	QItemSelectionModel* apSelectionModel = cUi.upNodeTableView->selectionModel();
	QModelIndexList aIndices = apSelectionModel->selectedIndexes();
	long aCount = aIndices.count();
	// Debug.
	QModelIndex aIdx;

	// Single Selection. Enable actions in the action list for this node.
	if (aCount == 1)
	{	QModelIndex aIdx = aIndices.at(0);
		QStringList aActions = cpAppClient->getNodeActions(aIdx.row());
		cpShowNodeValueAct->setEnabled(aActions.contains("popup", Qt::CaseInsensitive));
		cpDescendNodeAct->setEnabled(aActions.contains("descend", Qt::CaseInsensitive));
		cpOpenNodeAct->setEnabled(aActions.contains("edit", Qt::CaseInsensitive));
		cpDeleteNodeAct->setEnabled(aActions.contains("erase", Qt::CaseInsensitive));
		cpCompileAgentAct->setEnabled(aActions.contains("compile", Qt::CaseInsensitive));
		cpExportNodeMenu->setEnabled(aActions.contains("export", Qt::CaseInsensitive));
	}
	// Multi Selection. Allow open, erase, compile, export but not show or descend on multiple nodes.
	// Note. If a selected node does not allow an enabled action, the action is skipped for that node in openSelectedNodes.
	else if (aCount > 1)
	{	cpOpenNodeAct->setEnabled(true);
		cpDeleteNodeAct->setEnabled(true);
		cpCompileAgentAct->setEnabled(true);
		cpExportNodeMenu->setEnabled(true);
	}
}

/*!
 * \brief Show node value.
 */
void ACabinetPage::onShowNodeValue()
{
	openSelectedNodes(2/*Show*/);
}

/*!
 * \brief Show system variables.
 */
void ACabinetPage::onShowSystemVariables()
{
	QString aExtentType(".Memory"), aOptions("#1 ");
	aOptions += cpShowLockedVariablesAct->isChecked() ? "1 " : "0 ";
	aOptions += cpShowSystemVariablesAct->isChecked() ? "1)" : "0)";
	cpAppClient->setExtentTypeOptions(aExtentType, aOptions);
}

/*!
 * \brief Called when up is clicked.
 */
void ACabinetPage::onUpClicked()
{
	cpNodeModel->removeRows(0/*StartRow*/, cpNodeModel->rowCount());
	cpParentForm->enable(false);
	cpAppClient->getPreviousLevel(this);
}

/*!
 * \brief Retrieve workspace statistics.
 */
void ACabinetPage::onWorkspaceStatistics()
{
	cpParentForm->enable(false);
	cpAppClient->getWorkspaceStatistics(this);
}

/*!
 * \brief Perform a requested action on the selected node.
 *
 * \param[in] iRequest Type of request.
 *
 * \note
 * Allows the actions in the order listed, depending upon the type of request.
 * 0 - Double-Click. Edit, Popup, Descend
 * 1 - Open. Edit
 * 2 - Show-Value. Popup
 * 3 - Descend. Descend.
 * If none of the actions listed above is allowed for a selected node, no action is taken.
 */
void ACabinetPage::openSelectedNodes(long iRequest)
{
	// Selected. Get list of selected items in node table view.
	long aIx;
	QModelIndex aIdx;
	QString aFullNodeName, aNodeType;
	QStringList aActions;
	QItemSelectionModel* apSelectionModel = cUi.upNodeTableView->selectionModel();
	QModelIndexList aIndices = apSelectionModel->selectedIndexes();

	// Actions. Implement the first allowed action for each selected node. Quit on first show-value or descend action.
	foreach(aIdx, aIndices)
	{	// Index. Get the original index stored in the row.  Do not use cpNodeModel, use cpNodeProxyModel here.
		aIx = aIdx.row();
		if ((aIdx=cpNodeProxyModel->index(aIx,6)).row() >= 0 && (aIx=cpNodeProxyModel->data(aIdx,Qt::DisplayRole).toInt()) >= 0)
		{	aFullNodeName = cpAppClient->getFullNodeName(aIx);
			if (!(aActions = cpAppClient->getNodeActions(aIx)).isEmpty())
			{	// Edit. If allowed, open edit window for double-click or open requests
				if ((iRequest == 0 && aActions[0] == "edit") || (iRequest == 1 && aActions.contains("edit", Qt::CaseInsensitive)))
				{	if (!aFullNodeName.isEmpty())
					{	aNodeType = cpAppClient->getNodeType(aIx);
						cpParentForm->openPage(aFullNodeName, aNodeType);
					}
				}
				// Show-value. If allowed, show the node value in popup dialog for double-click or show-value requests
				else if ((iRequest == 0 && aActions[0] == "popup") || (iRequest == 2 && aActions.contains("popup", Qt::CaseInsensitive)))
				{	cpParentForm->enable(false);
					cpAppClient->openNode(this, cpAppClient->getCurrentExtentName(), aFullNodeName);
					break;
				}
				// Descend. If allowed, expand node for double-click or descend requests
				else if ((iRequest == 0 && aActions[0] == "descend") || (iRequest == 3 && aActions.contains("descend", Qt::CaseInsensitive)))
				{	cpParentForm->enable(false);
					cpAppClient->getNextLevel(this, aFullNodeName);
					break;
				}
			}
		}
	}
}

/*!
 * \brief Receives payload returned from the server.
 *
 * \param[in] iConnectId A dummy placeholder (not used by AIS clients)
 * \param[in] iRqId Request ID assigned to requests from this client
 * \param[in] iStatus A positive error code (zero iff no error)
 * \param[in] iReqType Indicates the type of response being returned
 * \param[in] iRetValue Integer value returned by some types of responses, else zero
 * \param[in] irOut Message returned by this response
 * \param[in] ipData Binary buffer containing returned object closure (not used here)
 * \param[in] iDataSize Binary buffer size (not used here)
 * \param[in] irDisplay Console output returned by this response
 * \param[in] irError Error message if any
 * \param[in] iClientData Arbitrary string attached to the request and returned in the response
 *
 * \note
 * -# iRqId is zero for pushed output.
 * -# All of the expected types of returned output is listed in the method
 */
void ACabinetPage::returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(iRqId);
    Q_UNUSED(iRetValue);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(irDisplay);

	cpParentForm->statusMsg("");
	switch (iReqType)
	{
	case geUpdateMetadata:
		cpParentForm->enable(true);
		cTask = ecNone;
	break;
	case geGetMetaData:
		{
			switch(cTask)
			{
			case ecImportCabinetRemote:
				{
					if(irOut != "#void" )
					{
						importCabinetRemote(irOut);
					}
					else
					{
						cpParentForm->enable(true);
						cTask = ecNone;
					}
				}
				break;
			case ecImportCabinetRemoteDir:
				{
					if(irOut != "#void" )
					{
						importCabinetRemoteDir(irOut);
					}
					else
					{
						cpParentForm->enable(true);
						cTask = ecNone;
					}
				}
				break;
			case ecExportCabinetRemote:
				{
					if(irOut != "#void" )
					{
						exportCabinetRemote(irOut);
					}
					else
					{
						cpParentForm->enable(true);
						cTask = ecNone;
					}
				}
				break;
			case ecExportCabinetRemoteDir:
				{
					if(irOut != "#void" )
					{
						exportCabinetRemoteDir(irOut);
					}
					else
					{
						cpParentForm->enable(true);
						cTask = ecNone;
					}
				}
				break;
			case ecUpdateMetadata:
				{
					if(irOut == "#void" )
					{
					cpParentForm->enable(true);
					cTask = ecNone;
					}
				else
					{
					QStringList aResultList = irOut.split('\t');
					if( aResultList.size() >= 7 )
					{
						cCabSettingsDialog.setData(aResultList[0], aResultList[1], aResultList[2], aResultList[3], aResultList[4], aResultList[5], aResultList[6], aResultList[7], aResultList[8]);
						if (cCabSettingsDialog.exec() == QDialog::Accepted)
						{
							QString aName = cCabSettingsDialog.getDatabaseName();
							QString aFilePath = cCabSettingsDialog.getDatabaseLocation();
							QString aFileLocation = cCabSettingsDialog.getSourceLocation();
							QString aStorageScope = cCabSettingsDialog.getStorageScope();
							QString aImportSync = cCabSettingsDialog.getImportSync();
							QString aExportSync = cCabSettingsDialog.getExportSync();
							QString aAutoCompile = cCabSettingsDialog.getAutoCompile();
							QString aSearchSwitch = cCabSettingsDialog.getSearchSwitch();
							QString aDependencies = cCabSettingsDialog.getDependencies();
							cpParentForm->enable(false);
							cpAppClient->updateMetadata(this, aName, aFilePath, aFileLocation, aStorageScope, aImportSync, aExportSync, aAutoCompile, aSearchSwitch, aDependencies);
						}
					}
					cpParentForm->enable(true);
					cTask = ecNone;
					}
				}
				break;
            default:
                // FC - Add handling?
                break;
			}
		}
		break;
	case geCheckCabinet:
		{
		QStringList aResultList = irOut.split('\t');
		QString aAction = "";
		QString aMessage = "";
		QString aImportSync = "";
		QString aExportSync = "";
		QString aName = "";
		QString aStorageScope = "";
		QModelIndex aIdx;
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

		switch( cTask )
		{
			case ecRefreshCabinet:
			{
				long aCount = cpExtentModel->rowCount();
				for (long aExtent = 0; aExtent < aCount; ++aExtent)
				{
					aIdx = cpExtentModel->index(aExtent, 0/*Col*/);
					const QString& arExtentName = cpExtentModel->data(aIdx, Qt::DisplayRole).toString();
					if( arExtentName == aName )
					{
						QIcon arStatusIcon;
						if( aAction == "nothing" )
						{
							arStatusIcon = cUptodate;
						}
						else if( aAction == "import" )
						{
							arStatusIcon = cImport;
						}
						else if( aAction == "export" )
						{
							arStatusIcon = cExport;
						}
						else if( aAction == "merge" )
						{
							arStatusIcon = cMerge;
						}
						else if( aAction == "#void" ) // Special case for non-extentManagerTemplates, this is already set as #void in (browseLib.getExtentStatus // RAJAH Modify this documetation, #void also if importSync and exportSync is none
						{
							arStatusIcon = cCheckOff;
						}
						else
						{
							arStatusIcon = cUnknown;
						}
						cpExtentModel->setData( aIdx.sibling( aIdx.row(), 1 ), qVariantFromValue(arStatusIcon), Qt::DecorationRole);
					}
				}
				cTask = ecNone;
				cpParentForm->enable(true);
				return;
			}
			break;
			case ecCheckCabinet:
			{
				cpParentForm->enable(true);
				long aCount = cpExtentModel->rowCount();
				for (long aExtent = 0; aExtent < aCount; ++aExtent)
				{
					aIdx = cpExtentModel->index(aExtent, 0/*Col*/);
					const QString& arExtentName = cpExtentModel->data(aIdx, Qt::DisplayRole).toString();
					if( arExtentName == aName )
					{
						QIcon arStatusIcon;
						if( aAction == "nothing" )
						{
							arStatusIcon = cUptodate;
						}
						else if( aAction == "import" )
						{
							arStatusIcon = cImport;
						}
						else if( aAction == "export" )
						{
							arStatusIcon = cExport;
						}
						else if( aAction == "merge" )
						{
							arStatusIcon = cMerge;
						}
						else if( aAction == "#void" ) // Special case for non-extentManagerTemplates, this is already set as #void in (browseLib.getExtentStatus
						{
							arStatusIcon = cCheckOff;
						}
						else
						{
							arStatusIcon = cUnknown;
						}
						cpExtentModel->setData( aIdx.sibling( aIdx.row(), 1 ), qVariantFromValue(arStatusIcon), Qt::DecorationRole);
					}
				}

                // Current Task is save, we should check if we need to export saved agent to its source file location
				if( aAction == "export" )
				{
                	if( aExportSync == "ask" )
                	{
                    	if( QMessageBox::question( this,
                        	"Export",
                        	QString( aMessage + "\nWould you like to export the source file now?"),
                        	(QMessageBox::Yes | QMessageBox::No)) == QMessageBox::Yes)
                    	{
        					QString aLocation = "..Lambda location..";
							cpParentForm->enable(false);
                            if( aStorageScope == "file" )
        					    cpAppClient->exportCabinetRemote( this, aName, aLocation );
        					else
        					    cpAppClient->exportCabinetRemoteDir( this, aName, aLocation );
                        	return;
                    	}
                	}
                	else
                	if( aExportSync == "auto" )
                	{
        				QString aLocation = "..Lambda location..";
						cpParentForm->statusAlert("Automatically exporting source code.");
						cpParentForm->enable(false);
						if( aStorageScope == "file" )
							cpAppClient->exportCabinetRemote( this, aName, aLocation );
						else
							cpAppClient->exportCabinetRemoteDir( this, aName, aLocation );
                    	return;
                	}
                	else
                	if( aExportSync == "notify" )
                	{
						cpParentForm->statusAlert("Cabinet needs to be exported");
						cpParentForm->enable(true);
						cTask = ecNone;
                    	return;
                	}
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
							cpParentForm->enable(false);
							if( aStorageScope == "file" )
								cpAppClient->importCabinetRemote( this, aName, aLocation );
							else
								cpAppClient->importCabinetRemoteDir( this, aName, aLocation );
                        	return;
                    	}
						else
						{
							cpParentForm->enable(true);
							cTask = ecNone;
                        	return;
                    	}
                	}
                	else
                	if( aImportSync == "auto" )
                	{
        				QString aLocation = "..Lambda location..";
						cpParentForm->statusAlert("Automatically importing source code.");
						cpParentForm->enable(false);
						if( aStorageScope == "file" )
							cpAppClient->importCabinetRemote( this, aName, aLocation );
						else
							cpAppClient->importCabinetRemoteDir( this, aName, aLocation );
                    	return;
                	}
                	else
                	if( aImportSync == "notify" )
                	{
						cpParentForm->statusAlert("This cabinet needs to be imported");
						cpParentForm->enable(true);
						cTask = ecNone;
                    	return;
                	}
				}
				else if( aAction == "unknown" )
				{
						cpParentForm->statusAlert("This cabinet needs to be merged");
						cpParentForm->enable(true);
						cTask = ecNone;
						return;
				}
				else if( aAction == "nothing" )
				{
						cpParentForm->statusAlert("This cabinet is up-to-date");
						cpParentForm->enable(true);
						cTask = ecNone;
						return;
				}
				else if( aAction == "#void" )
				{
						cpParentForm->statusAlert("This cabinet is not copied from an extentManagerTemplate");
						cpParentForm->enable(true);
						cTask = ecNone;
						return;
				}
				else
				{
						cpParentForm->enable(true);
						cTask = ecNone;
						return;
				}
			}
			break;
			case ecUpdateStatus:
			{
				long aCount = cpExtentModel->rowCount();
				for (long aExtent = 0; aExtent < aCount; ++aExtent)
				{
					aIdx = cpExtentModel->index(aExtent, 0/*Col*/);
					const QString& arExtentName = cpExtentModel->data(aIdx, Qt::DisplayRole).toString();
					if( arExtentName == aName )
					{
						QIcon arStatusIcon;
						if( aAction == "nothing" )
						{
							arStatusIcon = cUptodate;
						}
						else if( aAction == "import" )
						{
							arStatusIcon = cImport;
						}
						else if( aAction == "export" )
						{
							arStatusIcon = cExport;
						}
						else if( aAction == "merge" )
						{
							arStatusIcon = cMerge;
						}
						else if( aAction == "#void" ) // Special case for non-extentManagerTemplates, this is already set as #void in (browseLib.getExtentStatus
						{
							arStatusIcon = cCheckOff;
						}
						else
						{
							arStatusIcon = cUnknown;
						}
						cpExtentModel->setData( aIdx.sibling( aIdx.row(), 1 ), qVariantFromValue(arStatusIcon), Qt::DecorationRole);
						//cpExtentModel->setData( aIdx.sibling( aIdx.row(), 1 ), QVariant(aAction), Qt::EditRole );
						cExtentStatusList << irOut;
					}
				}
				
				if( cUpdateList.size() > 0 )
				{
					cpParentForm->enable(false);
					cpAppClient->checkCabinet( this, cUpdateList.takeFirst());
					return;
				}
				else
				{
					if( cManageCabinets == true )
					{
						cManageCabinetsDialog.setCabinetList( cExtentStatusList );
						cManageCabinetsDialog.exec();
						cManageCabinets = false;
						// Refresh status for all registered cabinets
						cpParentForm->enable(false);
						cTask = ecRefreshCabinet;
						//cpAppClient->getExtentNames(this); // RAJAH - Changed to refresh cabinets call
						cpAppClient->getExtentStatus(this);
						return;
					}
					cpParentForm->enable(true);
					cTask = ecNone;
					return;
				}
			}
			break;
			default:
			break;
		}

		cpParentForm->enable(true);
		cTask = ecNone;
		}
		break;
	case geCloseCabinet:
		cpAppClient->getExtentNames(this);
		break;
	case geCompileLambda:
	case geCompileCabinet:
		cpParentForm->enable(true);
		if (irOut != "true")
			cpParentForm->setCurrentTab(0, irOut + '\n');	// Console Tab
		if (irDisplay != "")
		{
			cpParentForm->consoleOutput(irDisplay);
			cpParentForm->statusAlert("Engine error! See Console page for details");
		}
		break;
	// Added geEval case for the debug flag setting
	case geEval:
		cpParentForm->enable(true);
		if (irOut != "true")
			cpParentForm->statusAlert("ACabinetPage:getEval:Debug flag setting returned an error");
		break;
	case geGetExtentNames:
		cpParentForm->enable(true);
		if (irOut == "false")
			cpParentForm->statusAlert("ACabinetPage:getExtentNames, No cabinets found");
		else
		{	QString aCurExtentName = cpAppClient->getCurrentExtentName();
			QStringList aExtents = irOut.split('\t', QString::SkipEmptyParts);
			QModelIndex aIdx;
			long aCount = cpExtentModel->rowCount();
			cpExtentModel->removeRows(0/*StartRow*/, aCount);
			aCount = aExtents.size();
			cpExtentModel->insertRows(0, aCount);
			for (long aExtent = 0; aExtent < aCount; ++aExtent)
			{	aIdx = cpExtentModel->index(aExtent, 0/*Col*/);
				QIcon& arCheckIcon = (aIdx == cSelectedIdx) ? cCheckOn : cCheckOff;
				cpExtentModel->setData(aIdx, qVariantFromValue(arCheckIcon), Qt::DecorationRole);
				cpExtentModel->setData(aIdx, QVariant(aExtents[aExtent]), Qt::EditRole);
				aIdx = cpExtentModel->index(aExtent, 1/*Col*/);
				cpExtentModel->setData( aIdx.sibling( aIdx.row(), 1 ),qVariantFromValue(cCheckOff), Qt::DecorationRole);
				//cpExtentModel->setData( aIdx.sibling( aIdx.row(), 1), QVariant(""), Qt::EditRole);
				cUi.upExtentListView->resizeColumnsToContents();
			}
			if( aCount > 0 )
			{
				cUpdateList = aExtents;
				cTask = ecUpdateStatus;
				cpAppClient->checkCabinet( this, cUpdateList.takeFirst());
			}
		}
		break;
	case geGetExtentStatus:
		{
			switch(cTask)
			{
			case ecRefreshCabinet:
				{
				QString aName;
				QString aAction;
				QStringList aExtentStatus;
				QModelIndex aIdx;
				QStringList aExtentStatusList = irOut.split('\n', QString::SkipEmptyParts);
				long aCount = cpExtentModel->rowCount();
				cpExtentModel->removeRows(0/*StartRow*/, aCount);
				aCount = aExtentStatusList.size();
				cpExtentModel->insertRows(0, aCount);
				for (long aExtent = 0; aExtent < aCount; ++aExtent)
				{
					aIdx = cpExtentModel->index(aExtent, 0/*Col*/);
					aExtentStatus = aExtentStatusList[aExtent].split('\t', QString::SkipEmptyParts);
	
                	if( aExtentStatus.size() == 6 )
					{ 
			    		aAction = aExtentStatus[0];
			    		aName = aExtentStatus[4];
	
						QIcon arStatusIcon;
						if( aAction == "nothing" )
						{
							arStatusIcon = cUptodate;
						}
						else if( aAction == "import" )
						{
							arStatusIcon = cImport;
						}
						else if( aAction == "export" )
						{
							arStatusIcon = cExport;
						}
						else if( aAction == "merge" )
						{
							arStatusIcon = cMerge;
						}
						else if( aAction == "#void" ) // Special case for non-extentManagerTemplates, this is already set as #void in (browseLib.getExtentStatus
						{
							arStatusIcon = cCheckOff;
						}
						else
						{
							arStatusIcon = cUnknown;
						}
						QIcon& arCheckIcon = (aIdx == cSelectedIdx) ? cCheckOn : cCheckOff;
						cpExtentModel->setData( aIdx, qVariantFromValue(arCheckIcon), Qt::DecorationRole);
						cpExtentModel->setData( aIdx, QVariant(aName), Qt::EditRole);
						cpExtentModel->setData( aIdx.sibling( aIdx.row(), 1 ), qVariantFromValue(arStatusIcon), Qt::DecorationRole);
					} 
				}

				cUi.upExtentListView->resizeColumnsToContents();
				cpParentForm->enable(true);
				cTask = ecNone;
				return;
				}
			break;
			default:
				QMessageBox::warning( this, "Unknown Task", "Unknown Task for geGetExtentStatus response");
				cpParentForm->enable(true);
				cTask = ecNone;
			break;
			}
		}
		break;
	case geGetExtentTypes:
		if (cTask == ecExtentFocus)
			cpAppClient->getNextLevel(this, QString());
		break;
	case geGetNextLevel:
		cTask = ecNone;
		cpParentForm->enable(true);
		if (iStatus == 0)
			showExtentNodes();
		else
			cpParentForm->setCurrentTab(0/*SESSION_CONSOLE*/, irOut + '\n');
		cTask = ecRefreshCabinet;
		cpParentForm->enable(false);
		cpAppClient->checkCabinet(this, cpAppClient->getCurrentExtentName());
		break;
	case geEraseNode:
		cpAppClient->getNextLevel(this);
		break;
	case geExportCabinet:
		cpParentForm->enable(true);
		cTask = ecRefreshCabinet;
		cpParentForm->enable(false);
		cpAppClient->checkCabinet(this, cpAppClient->getCurrentExtentName());
		break;
	case geExportNode:
		cpAppClient->getNextLevel(this);
		break;
	case geImportCabinet:
		cpAppClient->getNextLevel(this);
		break;
	case geNewCabinet:
		if( irOut == "existing" )
		{
			QMessageBox::information( this, "New Cabinet", "The cabinet already exists");
		}
		cpAppClient->getExtentNames(this);
		break;
	case geOpenCabinet:
		if( irOut == "notfound" )
		{
			QMessageBox::information( this, "Open Cabinet", "The cabinet does not exist");
		}
		cpAppClient->getExtentNames(this);
		break;
	case geOpenNode:
		cpParentForm->enable(true);
		cNodeValueDialog.setup(irOut, iClientData);
		cNodeValueDialog.exec();
		break;
	case geRegisterDirectory:
		cpParentForm->enable(true);
		cpAppClient->getExtentNames(this);
		break;
	case geGetWorkspaceStatistics:
		cpParentForm->setCurrentTab(0/*SESSION_CONSOLE*/, irOut + '\n');
		cpParentForm->enable(true);
		break;
	// added by fchua to display disconnection notification in status bar.
	case geAmpMsg:
		if (iStatus == AERR_DISCONNECTED)
		{
			// display error in status bar
			cpParentForm->statusAlert(irError);
		}
		break;
	default:
		QString aMsg = QString("CabinetPage::returnOutput, Unexpected response %1. ReqType %2").arg(irOut, gAis.mpRequestNames[iReqType]);
		cpParentForm->statusAlert(aMsg);
		break;
	}
}

/*!
 * \brief Inherited from the AReturnRcvr abstract base class.
 *
 * \param[in] iConnectId - The ID of the connection for this context.
 * \param[in] irContextName - The new context name.
 */
void ACabinetPage::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irContextName);
}

/*!
 * \brief Select a new extent from the extent list and get its extent types.
 *
 * \param[in] irIdx Index into the extent model for the selected extent.
 */
void ACabinetPage::setExtentFocus(const QModelIndex& irIdx)
{
	const QString& arExtentName = cpExtentModel->data(irIdx, Qt::DisplayRole).toString();
	cpAppClient->setCurrentExtent(arExtentName);
	if (cSelectedIdx.isValid())
		cpExtentModel->setData(cSelectedIdx, qVariantFromValue(cCheckOff), Qt::DecorationRole);
	cpExtentModel->setData(irIdx, qVariantFromValue(cCheckOn), Qt::DecorationRole);
	cSelectedIdx = irIdx;
	QItemSelectionModel* apSelection = cUi.upExtentListView->selectionModel();
	apSelection->select(irIdx, QItemSelectionModel::Rows | QItemSelectionModel::SelectCurrent);
	cUi.upNodeTableView->clearSelection();
	cTask = ecExtentFocus;
	cpParentForm->enable(false);
	cpAppClient->getExtentTypes(this);
}

/*!
 * \brief Update widget properties.
 *
 * \param[in] ipCabinetParams New widget properties.
 */
void ACabinetPage::setParameters(AParameters* ipCabinetParams)
{
	if (ipCabinetParams != NULL)
	{
		QFont aNewFont(ipCabinetParams->mFont,ipCabinetParams->mFontSize);
		cUi.upExtentListView->setFont(aNewFont);
		cUi.upNodeTableView->setFont(aNewFont);
		cUi.upPathCombo->setFont(aNewFont);
	}
}

/*!
 * \brief Select a new extent from the extent list and get its extent types.
 *
 * \notes
 * -# Called by SessionForm when page is activated.
 */
void ACabinetPage::show()
{
	if (cpExtentModel->rowCount() <= 0)
		cpAppClient->getExtentNames(this);
}

/*!
 * \brief Show extent nodes.
 */
void ACabinetPage::showExtentNodes()
{
	// Path Combo Box. Populate combo box with new list of nodes.
	QStringList aNodePaths = cpAppClient->getNodePathTree();
	cUi.upPathCombo->clear();
	cUi.upPathCombo->insertItems(0, aNodePaths);

	// Node List. Populate NodeTableView with new list of nodes.
	QModelIndex aIdx;
	int aNodes = cpNodeModel->rowCount();
	cpNodeModel->removeRows(0/*StartRow*/, aNodes);
	aNodes = cpAppClient->getNumNodes();
	cpNodeModel->insertRows(0, aNodes);
	for (int aY = 0; aY < aNodes; ++aY)
	{	ANodeInfo aNode = cpAppClient->getNode(aY);
		if (!aNode.mValue.isEmpty())
		{	aIdx = cpNodeModel->index(aY, 0);
			cpNodeModel->setData(aIdx, QVariant(aNode.mValue));
			aIdx = cpNodeModel->index(aY, 1);
			cpNodeModel->setData(aIdx, QVariant(aNode.mType));
			aIdx = cpNodeModel->index(aY, 2);
			cpNodeModel->setData(aIdx, QVariant(aNode.mSize));
			aIdx = cpNodeModel->index(aY, 3);
			cpNodeModel->setData(aIdx, QVariant(aNode.mDate));
			aIdx = cpNodeModel->index(aY, 4);
			cpNodeModel->setData(aIdx, QVariant(aNode.mTime));
			aIdx = cpNodeModel->index(aY, 5);
			cpNodeModel->setData(aIdx, QVariant(aNode.mVersion));
			aIdx = cpNodeModel->index(aY, 6);
			cpNodeModel->setData(aIdx, QVariant(aY));
		}
	}
	// Resize. Eliminate the double-spacing of rows
	for (long aY = 0; aY < aNodes; ++aY)
		cUi.upNodeTableView->resizeRowToContents(aY);
}

/*!
 * \brief Clears the list of extents and nodes.
 */
void ACabinetPage::clear()
{
	// clear list of cabinets/extents
	long aCount = cpExtentModel->rowCount();
	cpExtentModel->removeRows(0/*StartRow*/, aCount);
	// clear list of nodes
	aCount = cpNodeModel->rowCount();
	cpNodeModel->removeRows(0, aCount);
}

/*!
 * \brief Process importing of source to a cabinet
 */
void ACabinetPage::importCabinetRemote(const QString& irOut)
{
	QStringList aResultList = irOut.split('\t');
	if( aResultList.size() == 9 )
	{
		QString iCabName = aResultList[0];
		QString iDatabaseLocation = aResultList[1];
		QString iSourceLocation = aResultList[2];
		QString iStorageScope = aResultList[3];
		QString iImportSync = aResultList[4];
		QString iExportSync = aResultList[5];
		QString iAutoCompile = aResultList[6];
		QStringList aList = cpAppClient->getCurrentExtentNames();
		QString aCurExtentName = cpAppClient->getCurrentExtentName();
		if (aCurExtentName.isEmpty())
		{	QModelIndex aIdx = cUi.upExtentListView->currentIndex();
			aCurExtentName = cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
		}

		// Cabinet File.
		cRemoteFileDialog.setFilters("AIS (*.sl *.db);;Any (* *.*)");
		cRemoteFileDialog.setWindowTitle("Import Cabinet[" + iCabName + "] - Select file");
		// Set the directory path to the remote file dialog and then remove the directory path from 
		// the source location string.  Leave the file path in the source string.
		cRemoteFileDialog.setInfoText( "Current source location: " + iSourceLocation + "\tStorage Type: " + iStorageScope);
		cRemoteFileDialog.setDirectoryPath(iSourceLocation);
		iSourceLocation = iSourceLocation.mid(iSourceLocation.lastIndexOf( "/" ) + 1);
		cRemoteFileDialog.setFileName(iSourceLocation);
		cRemoteFileDialog.setDirectoriesOnly(false);
		if (cRemoteFileDialog.exec() == QDialog::Accepted)
		{	QString aFilePath = cRemoteFileDialog.filePath();
			if (!aFilePath.endsWith(QChar('/')))
			{
				bool aSetLocation = false;
				cpParentForm->enable(false);
				if( iStorageScope == "folder" )
				{
					if( QMessageBox::question( this,
						"Cabinet Source Location",
						QString( "Save this file location in the cabinet? Storage scope will become file type."),
						(QMessageBox::Yes | QMessageBox::No)) == QMessageBox::Yes)
							aSetLocation = true;
				}
				cpAppClient->importCabinetRemote(this, iCabName, aFilePath, aSetLocation);
			}
		}
		// Clear the extra information previously set.
		cRemoteFileDialog.setInfoText( "" );
	}
}


/*!
 * \brief Import cabinet remotely.
 */
void ACabinetPage::importCabinetRemoteDir(const QString& irOut)
{
	QStringList aResultList = irOut.split('\t');
	if( aResultList.size() == 9 )
	{
		QString iCabName = aResultList[0];
		QString iDatabaseLocation = aResultList[1];
		QString iSourceLocation = aResultList[2];
		QString iStorageScope = aResultList[3];
		QString iImportSync = aResultList[4];
		QString iExportSync = aResultList[5];
		QString iAutoCompile = aResultList[6];
		QStringList aList = cpAppClient->getCurrentExtentNames();
		QString aCurExtentName = cpAppClient->getCurrentExtentName();
		if (aCurExtentName.isEmpty())
		{	QModelIndex aIdx = cUi.upExtentListView->currentIndex();
			aCurExtentName = cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
		}

		// Cabinet File.
		cRemoteFileDialog.setFilters("AIS Directory (*);;Any (*)");
		cRemoteFileDialog.setWindowTitle("Import Cabinet[" + iCabName + "] - Select directory");
		// Set the directory path to the remote file dialog and then remove the directory path from 
		// the source location string.  Leave the file path in the source string.
		cRemoteFileDialog.setInfoText( "Current source location: " + iSourceLocation + "\tStorage Type: " + iStorageScope);
		cRemoteFileDialog.setDirectoryPath(iSourceLocation);
		iSourceLocation = iSourceLocation.mid(iSourceLocation.lastIndexOf( "/" ) + 1);
		cRemoteFileDialog.setFileName(iSourceLocation);
		cRemoteFileDialog.setDirectoriesOnly(true);
		if (cRemoteFileDialog.exec() == QDialog::Accepted)
		{	QString aFilePath = cRemoteFileDialog.filePath();
			if (!aFilePath.endsWith(QChar('/')))
			{
				bool aSetLocation = false;
				cpParentForm->enable(false);
				if( iStorageScope == "file" )
				{
					if( QMessageBox::question( this,
						"Cabinet Source Location",
						QString( "Save this file location in the cabinet? Storage scope will become folder type."),
						(QMessageBox::Yes | QMessageBox::No)) == QMessageBox::Yes)
							aSetLocation = true;
				}
				cpAppClient->importCabinetRemoteDir(this, aCurExtentName, aFilePath, aSetLocation);
			}
		}
		// Clear the extra information previously set.
		cRemoteFileDialog.setInfoText( "" );
	}
}

/*!
 * \brief Export cabinet remotely.
 */
void ACabinetPage::exportCabinetRemote(const QString& irOut)
{
	QStringList aResultList = irOut.split('\t');
	if( aResultList.size() == 9 )
	{
		QString iCabName = aResultList[0];
		QString iDatabaseLocation = aResultList[1];
		QString iSourceLocation = aResultList[2];
		QString iStorageScope = aResultList[3];
		QString iImportSync = aResultList[4];
		QString iExportSync = aResultList[5];
		QString iAutoCompile = aResultList[6];
		QStringList aList = cpAppClient->getCurrentExtentNames();
		QString aCurExtentName = cpAppClient->getCurrentExtentName();
		QFileInfo aCurrentFileInfo( iSourceLocation );
		if (aCurExtentName.isEmpty())
		{	QModelIndex aIdx = cUi.upExtentListView->currentIndex();
			aCurExtentName = cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
		}

		// Cabinet File.
		cRemoteFileDialog.setFilters("AIS (*.sl *.db);;Any (* *.*)");
		cRemoteFileDialog.setWindowTitle("Export Cabinet[" + iCabName + "] - Select file");
		// Set the directory path to the remote file dialog and then remove the directory path from 
		// the source location string.  Leave the file path in the source string.
		cRemoteFileDialog.setInfoText( "Current source location: " + iSourceLocation + "\tStorage Type: " + iStorageScope);
		cRemoteFileDialog.setDirectoryPath(iSourceLocation);
		iSourceLocation = iSourceLocation.mid(iSourceLocation.lastIndexOf( "/" ) + 1);
		cRemoteFileDialog.setFileName(iSourceLocation);
		cRemoteFileDialog.setDirectoriesOnly(false);
		if (cRemoteFileDialog.exec() == QDialog::Accepted)
		{	QString aFilePath = cRemoteFileDialog.filePath();
			QFileInfo aTargetFileInfo(aFilePath);
			if (!aFilePath.endsWith(QChar('/')))
			{
				bool aSetLocation = false;
				cpParentForm->enable(false);
				if( iStorageScope == "folder" )
				{
					if( QMessageBox::question( this,
						"Cabinet Source Location",
						QString( "Save this file location in the cabinet? Storage scope will become file type."),
						(QMessageBox::Yes | QMessageBox::No)) == QMessageBox::Yes)
							aSetLocation = true;
				}
				cpAppClient->exportCabinetRemote(this, aCurExtentName, aFilePath, aSetLocation);
			}
		}
		// Clear the extra information previously set.
		cRemoteFileDialog.setInfoText( "" );
	}
}


/*!
 * \brief Export cabinet remotely.
 */
void ACabinetPage::exportCabinetRemoteDir(const QString& irOut)
{
	QStringList aResultList = irOut.split('\t');
	if( aResultList.size() == 9 )
	{
		QString iCabName = aResultList[0];
		QString iDatabaseLocation = aResultList[1];
		QString iSourceLocation = aResultList[2];
		QString iStorageScope = aResultList[3];
		QString iImportSync = aResultList[4];
		QString iExportSync = aResultList[5];
		QString iAutoCompile = aResultList[6];
		QStringList aList = cpAppClient->getCurrentExtentNames();
		QString aCurExtentName = cpAppClient->getCurrentExtentName();

		if (aCurExtentName.isEmpty())
		{	QModelIndex aIdx = cUi.upExtentListView->currentIndex();
			aCurExtentName = cpExtentModel->data(aIdx.sibling(aIdx.row(),0), Qt::DisplayRole).toString();
		}

		// Cabinet File.
		cRemoteFileDialog.setFilters("AIS (*.sl *.db);;Any (* *.*)");
		cRemoteFileDialog.setWindowTitle("Export Cabinet[" + iCabName + "] - Select directory");
		// Set the directory path to the remote file dialog and then remove the directory path from 
		// the source location string.  Leave the file path in the source string.
		cRemoteFileDialog.setInfoText( "Current source location: " + iSourceLocation + "\tStorage Type: " + iStorageScope);
		cRemoteFileDialog.setDirectoryPath(iSourceLocation);
		iSourceLocation = iSourceLocation.mid(iSourceLocation.lastIndexOf( "/" ) + 1);
		cRemoteFileDialog.setFileName(iSourceLocation);
		cRemoteFileDialog.setDirectoriesOnly(false);
		if (cRemoteFileDialog.exec() == QDialog::Accepted)
		{	QString aFilePath = cRemoteFileDialog.filePath();
			if (!aFilePath.endsWith(QChar('/')))
			{
				bool aSetLocation = false;
				cpParentForm->enable(false);
				if( iStorageScope == "file" )
				{
					if( QMessageBox::question( this,
						"Cabinet Source Location",
						QString( "Save this file location in the cabinet? Storage scope will become folder type."),
						(QMessageBox::Yes | QMessageBox::No)) == QMessageBox::Yes)
							aSetLocation = true;
				}
				cpAppClient->exportCabinetRemoteDir(this, aCurExtentName, aFilePath, aSetLocation);
			}
		}
		// Clear the extra information previously set.
		cRemoteFileDialog.setInfoText( "" );
	}
}

// end
