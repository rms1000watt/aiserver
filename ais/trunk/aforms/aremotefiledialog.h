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
#ifndef REMOTEFILEDIALOG_H
#define REMOTEFILEDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/webide/aremotefiledialog.h
													Remote File Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0039	4/29/2004	tlw		Revise specification
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>

#include "aglobals.h"			// ACHECK, LOGSYSMSG
#include "appclient.h"			// AAppClient
#include "ui_aremotefiledialog.h"
class QStandardItemModel;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief ARemoteFileDialog - Dialog that allows user to select an existing file on the Server

 */
class ARemoteFileDialog : public QDialog, public AReturnRcvr
{ 
    Q_OBJECT
public:
    ARemoteFileDialog(AAppClient* ipAppClient, const QString& irCurDir, QWidget* ipParent, const char* ipName = 0);
	// ReturnRcvr methods.
	virtual void	returnOutput(long iDummyId, long iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
					, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
					= QString());
	virtual void	setContextName(long iConnectId, const QString& irContextName);

	// Public methods.
	QString			filePath();		// File spec of selected file
	void			setDirectoriesOnly(bool iFlag);
	void			setFileName(const QString& irFileName);
    void			setFilters(const QString& irFilters);
	void			setDirectoryPath(const QString& irDirectory);
	void			setInfoText(const QString& irInfo);

private slots:
	void			onDirComboBoxActivated(int i);
    void			onTableViewDoubleClicked(const QModelIndex& irIx);
    void			onTableViewClicked(const QModelIndex& irIx);
    void			onTypeComboBoxActivated(const QString& irNewFilter);
	void			onUpDir();

private:
	void			expandCurDir(long iX);
	bool			match(const QStringList&, const QString&);
    void			processGetDirInfo(const QString& irDirInfo);
	void			setFilter(const QString& irFilter);

    AAppClient*			cpAppClient;
    QString				cCurDir;
    QStringList			cDirInfo;
	QStandardItemModel* cpTableModel;
	Ui::ARemoteFileDialogClass	cUi;
	bool			cDirectoriesOnlyFlag;
};

#endif // REMOTEFILEDIALOG_H
