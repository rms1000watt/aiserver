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
#ifndef AWATCHDIALOG_H
#define AWATCHDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/awatchdialog.h
															Watch Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0000	04/13/2009	rca		New Dialog class for watching specific variables
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include <QtGui/QStandardItemModel>
#include "ais.h"
#include "appclient.h"
#include "ui_awatchdialog.h"

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief AWatchDialog - Show a brief description of the AIS project.
 */
class AWatchDialog : public QDialog
{
    Q_OBJECT
public:
    AWatchDialog(QWidget* ipParent, AAppClient* ipAppClient, AReturnRcvr* ipReturnRcvr, const char* ipName, bool iModal = false, Qt::WFlags iFlgs = 0);
	QStringList getWatchList();
	void updateValue(const QString& irVarValue);
	void clearValues();
	void watchVariable( const QString& iVarName );


private slots:
	void onVarListDoubleClicked(const QModelIndex& irIdx);
	void onAddVarPressed();
	void onRemoveVarPressed();
	void onRemoveAllVarPressed();

private:
	void debugInfoLines(const QString& irVariables);

	AAppClient*				cpAppClient;
	AReturnRcvr*			cpReturnRcvr;
	Ui::AWatchDialogClass	cUi;
	QStandardItemModel*		cpVarListModel;
	QModelIndex				cIdx;
};

#endif // AABOUTDIALOG_H
