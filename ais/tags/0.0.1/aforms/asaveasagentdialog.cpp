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
aisdev/aforms/asaveasagentdialog.cpp
														  Save As Lambda Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0070	1/21/2006	tlw		Convert from Qt3 to Qt4
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include <QtGui/QStandardItemModel>

#include "appclient.h"
#include "asaveasagentdialog.h"
#include "asessionform.h"
class QWidget;

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief ASaveAsAgentDialog - constructor instantiates the GUI elements generated in ui_saveasagentdialog.h
 
\par Args:
\param ipAppClient -> Application client that forwards commands to the AIS server
\param ipParent -> Parent widget that created this instance of ASaveAsAgentDialog
\param ipName -> Name assigned to this instance of the dialog.
\return void
 */
ASaveAsAgentDialog::ASaveAsAgentDialog(AAppClient* ipAppClient, QWidget* ipParent, const char* ipName)
 : QDialog(ipParent), cpAppClient(ipAppClient), cCheckOff(":images/checkoff.png"),cCheckOn(":images/checkon.png"),cTask(ecNone)
{
	// Gui. Configure widgets
	cUi.setupUi(this);
	setObjectName(ipName);

	// ExtentsListView.
	cUi.upExtentsListView->setEditTriggers(QAbstractItemView::NoEditTriggers);
	cUi.upExtentsListView->setObjectName("ExtentsListView");
	cUi.upExtentsListView->setSelectionBehavior(QAbstractItemView::SelectItems);
	cUi.upExtentsListView->setSelectionMode(QAbstractItemView::SingleSelection);
    connect(cUi.upExtentsListView, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(onExtentsList(const QModelIndex&)));

	// ExtentsListModel. Configure the extent model to feed the extents list view
	cpExtentsListModel = new QStandardItemModel(0/*Rows*/, 1/*Cols*/, this);
	cpExtentsListModel->setObjectName("ExtentsListModel");
	cUi.upExtentsListView->setModel(cpExtentsListModel);

	// AgentsListView.
	cUi.upAgentsListView->setEditTriggers(QAbstractItemView::NoEditTriggers);
	cUi.upAgentsListView->setObjectName("AgentsListView");
	cUi.upAgentsListView->setSelectionBehavior(QAbstractItemView::SelectItems);
	cUi.upAgentsListView->setSelectionMode(QAbstractItemView::SingleSelection);
    connect(cUi.upAgentsListView, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(onAgentsList(const QModelIndex&)));

	// AgentsListModel. Configure the agent model to feed the agents list view
	cpAgentsListModel = new QStandardItemModel(0/*Rows*/, 1/*Cols*/, this);
	cpAgentsListModel->setObjectName("AgentsListModel");
	cUi.upAgentsListView->setModel(cpAgentsListModel);

    // Buttons. Hook up buttons.
    connect(cUi.upCancelButton, SIGNAL(clicked()), this, SLOT(reject()));
    connect(cUi.upOkButton, SIGNAL(clicked()), this, SLOT(onOkButtonClicked()));
}

/*!
\brief connectionClosed - Inherited from the AReturnRcvr abstract base class.

\param iConnectId  - The ID of the connection that was closed.
\return false
*/
bool ASaveAsAgentDialog::connectionClosed(long iConnectId)
{
    Q_UNUSED(iConnectId);

	return false;
}

/*	---------------------------------------------------------------------------------------------------------------------------

onExtentsListBox - Upon double-click of item in ExtentsListView, get new list of nodes and set extent name.
	------------------------------------------------------------------------------------------------------------------------ */
void ASaveAsAgentDialog::onExtentsList(const QModelIndex& irIdx)
{
	QString aExtentName(cpExtentsListModel->data(irIdx, Qt::DisplayRole).toString());
    if (!aExtentName.isEmpty() && cpAppClient->setCurrentExtent(aExtentName) && irIdx != cSelectedExtent)
	{	// Extent List. Deselect current selection. Select extent that was clicked.
		if (cSelectedExtent.isValid())
			cpExtentsListModel->setData(cSelectedExtent, qVariantFromValue(cCheckOff), Qt::DecorationRole);
		cpExtentsListModel->setData(irIdx, qVariantFromValue(cCheckOn), Qt::DecorationRole);
		cSelectedExtent = irIdx;
		cExtentName = aExtentName;

		// Node List. Refresh node list (see returnOutput).
		cTask = ecShowAgents;
		setEnabled(false);
		cpAppClient->getExtentTypes(this);
    }
}

// onAgentsList - Upon double-click of item in AgentsListView, put the selected item in the AgentLineEdit
void ASaveAsAgentDialog::onAgentsList(const QModelIndex& irIdx)
{
	QString aAgentName(cpAgentsListModel->data(irIdx, Qt::DisplayRole).toString());
	cUi.upAgentLineEdit->setText(aAgentName);
}

// onOkButton - Ok push-button clicked
void ASaveAsAgentDialog::onOkButtonClicked()
{
    cAgentName = cUi.upAgentLineEdit->text();
    accept();
}

void ASaveAsAgentDialog::refreshTargets(const QString& irAgentName)
{
	// Lambda Name. Set default agent name
	cUi.upAgentLineEdit->setText(irAgentName);

	// Extents. Fill extents list on the left.
	QStringList aExtents = cpAppClient->getCurrentExtentNames();
	long aCount = aExtents.count();
	QString aCurrentExtent(cpAppClient->getCurrentExtentName());
	QString aExtentName;
	QModelIndex aIdx;
	cpExtentsListModel->removeRows(0/*StartRow*/, cpExtentsListModel->rowCount());
	cpExtentsListModel->insertRows(0, aCount);
	for (long aExtent = 0; aExtent < aCount; ++aExtent)
	{	aExtentName = aExtents[aExtent];
		aIdx = cpExtentsListModel->index(aExtent, 0/*Col*/);
		if (aExtentName == aCurrentExtent)
		{	cpExtentsListModel->setData(aIdx, qVariantFromValue(cCheckOn), Qt::DecorationRole);
			cSelectedExtent = aIdx;
			cExtentName = aExtentName;
		}
		else
			cpExtentsListModel->setData(aIdx, qVariantFromValue(cCheckOff), Qt::DecorationRole);
		cpExtentsListModel->setData(aIdx, QVariant(aExtentName), Qt::EditRole);
	}
	// Agents. Fill the agents list on the right
	// NOTE: Probably should just show nodes of type agent!
	QStringList aAgents = cpAppClient->getCurrentNodeNames();
	aCount = aAgents.count();
	cpAgentsListModel->removeRows(0/*StartRow*/, cpAgentsListModel->rowCount());
	cpAgentsListModel->insertRows(0, aCount);
	for (long aAgent = 0; aAgent < aCount; ++aAgent)
	{	aIdx = cpAgentsListModel->index(aAgent, 0/*Col*/);
		cpAgentsListModel->setData(aIdx, QVariant(aAgents[aAgent]), Qt::EditRole);
	}
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
void ASaveAsAgentDialog::returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(iRqId);
    Q_UNUSED(iStatus);
    Q_UNUSED(iRetValue);
    Q_UNUSED(irOut);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(irDisplay);
    Q_UNUSED(irError);
    Q_UNUSED(iClientData);

	switch (iReqType)
	{
	case geGetExtentTypes:
		if (cTask == ecShowAgents)
			cpAppClient->getNextLevel(this, QString());
		break;
	case geGetNextLevel:
		cTask = ecNone;
		setEnabled(true);
		showAgents();
		break;
    default:
        // FC - Add handling?
        break;
	}
}

/*!
\brief setContextName - Inherited from the AReturnRcvr abstract base class.

\param iConnectId  - The ID of the connection for this context.
\param irContextName - The new context name
\return void
*/
void ASaveAsAgentDialog::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irContextName);
}

void ASaveAsAgentDialog::showAgents()
{
	// Agent List. Populate NodeTableView with new list of nodes.
	QModelIndex aIdx;
	long aNodes = cpAppClient->getNumNodes();
	cpAgentsListModel->removeRows(0/*StartRow*/, cpAgentsListModel->rowCount());
	cpAgentsListModel->insertRows(0, aNodes);
	for (long aY = 0; aY < aNodes; ++aY)
	{	ANodeInfo aNode = cpAppClient->getNode(aY);
		if (!aNode.mValue.isEmpty())
		{	aIdx = cpAgentsListModel->index(aY, 0);
			cpAgentsListModel->setData(aIdx, QVariant(aNode.mValue));
		}
	}
	// Resize. Eliminate the double-spacing of rows
	//for (long aY = 0; aY < aNodes; ++aY)
	//	cUi.upAgentsListView->resizeRowToContents(aY);
}
// end
