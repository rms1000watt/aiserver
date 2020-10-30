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
#ifndef ASAVEASAGENTDIALOG_H
#define ASAVEASAGENTDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/asaveasagentdialog.h
															Save As Agent Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0070	1/21/2006	tlw		Convert to Qt4
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>

#include "ais.h"			// AReqType, AReturnRcvr
#include "ui_asaveasagentdialog.h"

class QListView;
class QStandardItemModel;
class QWidget;
class AAppClient;

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ASaveAsAgentDialog - Dialog to allow user to select an extent and an agent name for saving an agent in the specified
repository.

 */
class ASaveAsAgentDialog : public QDialog, public AReturnRcvr
{
    Q_OBJECT

public:
    ASaveAsAgentDialog(AAppClient* ipAppClient, QWidget* ipParent, const char* ipName);
	QString			agentName() { return cAgentName;};
	QString			extent() {return cExtentName;};
	void			refreshTargets(const QString& irAgentName);

	// AReturnRcvr Methods.
	virtual bool	connectionClosed(long iConnectId);
	virtual void	returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
					, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
					= QString());
	virtual void	setContextName(long iConnectId, const QString& irContextName);

public slots:
    void			onAgentsList(const QModelIndex& irIdx);
    void			onExtentsList(const QModelIndex& irIdx);
	void			onOkButtonClicked();

private:
	enum			ASaveAsAgentTask {ecNone, ecShowAgents};
	void			showAgents();

	QStandardItemModel*	cpAgentsListModel;
    QString				cAgentName;
	AAppClient*			cpAppClient;
	QIcon				cCheckOff;
	QIcon				cCheckOn;
	QString				cExtentName;
	QStandardItemModel*	cpExtentsListModel;
	QModelIndex			cSelectedExtent;
	ASaveAsAgentTask	cTask;
	Ui::ASaveAsAgentDialogClass	cUi;
};

#endif // ASAVEASAGENTDIALOG_H
