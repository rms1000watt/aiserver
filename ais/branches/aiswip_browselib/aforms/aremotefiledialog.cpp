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
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/webide/aremotefiledialog.cpp
											Remote File Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	----------------------------------------- IMPORTS ---------------------------------------------

#include <QtCore/QEvent>
#include <QtCore/QRegExp>
#include <QtGui/QHeaderView>
#include <QtGui/QMessageBox>
#include <QtGui/QStandardItemModel>
#include "aremotefiledialog.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief ARemoteFileDialog - constructor instantiates the GUI elements generated in ui_remotefiledialog.h
 
\par Args:
\param ipAppClient -> Application client that forwards commands to the AIS server
\param irCurDir - Current directory to be opened..
\param ipParent -> Parent widget that created this instance of a cabinet page
\param ipName -> Name assigned to this instance of the cabinet page
\return void
 */
ARemoteFileDialog::ARemoteFileDialog(AAppClient* ipAppClient, const QString& irCurDir, QWidget* ipParent, const char* ipName)
    :	 QDialog(ipParent), cpAppClient(ipAppClient), cCurDir(irCurDir), cDirectoriesOnlyFlag(false)
{
	// Gui. Configure widgets.
	cUi.setupUi(this);
	cUi.upUpButton->setIcon(QIcon(":/images/folderup.png"));
	setObjectName(ipName);

	// TableView. Configure table view
	cUi.upTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);
	cUi.upTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	cUi.upTableView->setSelectionMode(QAbstractItemView::SingleSelection);
	cUi.upTableView->setShowGrid(false/*Show*/);

	QHeaderView* apVertHeader = cUi.upTableView->verticalHeader();
	apVertHeader->hide();

	// TableView Model. Configure the TableView model to feed the table view
	cpTableModel = new QStandardItemModel(1/*Rows*/, 4/*Columns*/, this);
	cpTableModel->setObjectName("TableModel");
	cpTableModel->setHeaderData(0/*Column*/, Qt::Horizontal, QVariant("Name"), Qt::EditRole);
	cpTableModel->setHeaderData(1/*Column*/, Qt::Horizontal, QVariant("Size"), Qt::EditRole);
	cpTableModel->setHeaderData(2/*Column*/, Qt::Horizontal, QVariant("Type"), Qt::EditRole);
	cpTableModel->setHeaderData(3/*Column*/, Qt::Horizontal, QVariant("Modified"), Qt::EditRole);
	QModelIndex aIdx;
	aIdx = cpTableModel->index(0/*Row*/, 0/*Col*/);
	QIcon aDocument(":images/folderdocument.png");
	cpTableModel->setData(aIdx, qVariantFromValue(aDocument), Qt::DecorationRole);
	cpTableModel->setData(aIdx, QVariant("FileName"));
	aIdx = cpTableModel->index(0/*Row*/, 1/*Col*/);
	cpTableModel->setData(aIdx, QVariant("1234"));
	aIdx = cpTableModel->index(0/*Row*/, 2/*Col*/);
	cpTableModel->setData(aIdx, QVariant("File"));
	aIdx = cpTableModel->index(0/*Row*/, 3/*Col*/);
	cpTableModel->setData(aIdx, QVariant("Mon January 23 06 6:23 pm"));
	cUi.upTableView->setModel(cpTableModel);

	// Connections.
    connect(cUi.upOkButton, SIGNAL(clicked()), this, SLOT(accept()));
    connect(cUi.upCancelButton, SIGNAL(clicked()), this, SLOT(reject()));
    connect(cUi.upTableView, SIGNAL(clicked(const QModelIndex&)),this, SLOT(onTableViewClicked(const QModelIndex&)));
    connect(cUi.upTableView, SIGNAL(doubleClicked(const QModelIndex&)),this, SLOT(onTableViewDoubleClicked(const QModelIndex&)));
	connect(cUi.upDirComboBox, SIGNAL(activated(int)), this, SLOT(onDirComboBoxActivated(int)));
	connect(cUi.upUpButton, SIGNAL(clicked()), this, SLOT(onUpDir()));
    connect(cUi.upTypeComboBox, SIGNAL(activated(const QString&)),this,SLOT(onTypeComboBoxActivated(const QString&)));
}

/*!
\brief connectionClosed - Inherited from the AReturnRcvr abstract base class.

\param iConnectId  - The ID of the connection that was closed.
\return false
*/
bool ARemoteFileDialog::connectionClosed(long iConnectId)
{
    Q_UNUSED(iConnectId);

	return false;
}

void ARemoteFileDialog::expandCurDir(long iIx)
{
	// Current Path Folders. Add individual directories in current path to ComboBox drop-down list
	bool aIsCurrent, aIsDrive;
	int aEntryType;				// 0-drive, 1-folder, 2-file
	QStringList aCurDirs;
	QString aIndent;		// Indentation in spaces
	aCurDirs = cCurDir.split("/", QString::SkipEmptyParts);
	long aSize = aCurDirs.count();
	QIcon aClosedIcon(":images/folderclosed.png");
	QIcon aDriveIcon(":images/folderdrive.png");
	QIcon aOpenIcon(":images/folderopen.png");
	for (long aIx = 0; aIx < aSize; )
	{	QString& arDir = aCurDirs[aIx];
		aIsCurrent = (++aIx == aSize);
		aIsDrive = arDir.at(1).toAscii() == ':';
		aEntryType = aIsDrive ? 0 : aIsCurrent ? 2 : 1;
		cUi.upDirComboBox->addItem(aIsDrive ? aDriveIcon : aIsCurrent ? aOpenIcon : aClosedIcon, aIndent + arDir, QVariant(aEntryType));
		aIndent += "   ";			// * 10
	}
	cUi.upDirComboBox->setCurrentIndex(iIx + aSize - 1);
}

/*!
\brief filePath - The complete path to the file selected by the user.

\return aFilePath - the path + file name selected by the user. 
*/
QString ARemoteFileDialog::filePath()
{
	QString aFilePath;
	if (!cUi.upFileNameLineEdit->text().isNull())
	{	if (cUi.upFileNameLineEdit->text().startsWith("/") || cUi.upFileNameLineEdit->text().mid(1,1) == ":")
			aFilePath = cUi.upFileNameLineEdit->text();
		else
		{	aFilePath = cCurDir;
			if (!aFilePath.endsWith("/"))
				aFilePath += "/";
			aFilePath += cUi.upFileNameLineEdit->text();
		}
	}
	return aFilePath;
}

bool ARemoteFileDialog::match(const QStringList& irFilters, const QString& irFileName)
{
	bool aFoundIt = false;
	long aSize = irFilters.count();
    for (long aIx = 0;  aIx < aSize; ++aIx)
	{	QRegExp aRx(irFilters.value(aIx), Qt::CaseInsensitive, QRegExp::Wildcard); 
		if (aRx.exactMatch(irFileName))
		{	aFoundIt = true;
			break;
		}
	}
    return aFoundIt;
}

void ARemoteFileDialog::onDirComboBoxActivated(int iX)
{
	// Drive. Use drive path if a drive selected.
	long aIx;
	long aType = cUi.upDirComboBox->itemData(iX).toInt();
	QString aEntry = cUi.upDirComboBox->itemText(iX);
	QString aPath;
	switch (aType)
	{
	case 0:		// Drive
		aPath = aEntry + '/';
		break;
	case 1:		// Folder
		if ((aIx = cCurDir.indexOf(aEntry, 0)) >= 0)
		{	aPath = cCurDir.left(aIx + aEntry.length());
			break;
		}
		// Fall thru
	case 2:		// File
		if ((aIx = cCurDir.lastIndexOf('/')) >= 0)
			aPath = cCurDir.left(aIx + 1);
		else
			aPath = cCurDir;
		break;
	}
	if (!aPath.isEmpty())
		cpAppClient->getDirInfo(this, aPath);
}

void ARemoteFileDialog::onTableViewClicked(const QModelIndex& irIdx)
{
	long aRow = irIdx.row();
	QModelIndex aIdx = cpTableModel->index(aRow, 2/*Type*/);
	QString aName = "";
	aIdx = cpTableModel->index(aRow, 0/*Name*/);
	aName = cpTableModel->data(aIdx).toString();
	if( aName == ".." || aName == "." )
		aName = "";
	//cUi.upFileNameLineEdit->setText(aName);
}

void ARemoteFileDialog::onTableViewDoubleClicked(const QModelIndex& irIdx)
{
	long aRow = irIdx.row();
	QModelIndex aIdx = cpTableModel->index(aRow, 2/*Type*/);
	//Directory. Tack the selected directory onto the end of the current path and get new dir info.
	if (cpTableModel->data(aIdx).toString() == "Directory")
	{	QString aNewDir(cCurDir);
		if (!aNewDir.endsWith("/"))
			aNewDir += "/";
		aIdx = cpTableModel->index(aRow, 0/*Name*/);
		aNewDir += cpTableModel->data(aIdx).toString();
		setEnabled(false);	
		cpAppClient->getDirInfo(this, aNewDir);
	}
	else	// File. Put selected file in file name box
	{	aIdx = cpTableModel->index(aRow, 0/*Name*/);
		cUi.upFileNameLineEdit->setText(cpTableModel->data(aIdx).toString());
	}
}

void ARemoteFileDialog::onTypeComboBoxActivated(const QString& irNewFilter)
{
	setFilter(irNewFilter);
}

void ARemoteFileDialog::onUpDir()
{
	if (!cCurDir.endsWith(".."))
		cCurDir += cCurDir.endsWith("/") ? ".." : "/..";
	setEnabled(false);
	cpAppClient->getDirInfo(this, cCurDir);
}

void ARemoteFileDialog::processGetDirInfo(const QString& irDirInfo)
{
	// Clear. Get a fresh start.
	cpTableModel->removeRows(0/*StartRow*/, cpTableModel->rowCount());
	cUi.upDirComboBox->clear();
	if (irDirInfo.isEmpty())
		return;

	// Filter. Get current filter expression
	QString aFilter = cUi.upTypeComboBox->currentText();	
	QRegExp aFilterExp(QString("([a-zA-Z0-9 ]*)\\(([a-zA-Z0-9_.*? +;#\\[\\]]*)\\)$"));
	long aIx = aFilterExp.indexIn(aFilter);
	if (aIx >= 0)
		aFilter = aFilterExp.cap(2);
	QStringList aFilters = aFilter.split(" ", QString::SkipEmptyParts);
	QStringList aDirInfo = irDirInfo.split('\n', QString::SkipEmptyParts);

	// CurDir. Capture current path in cCurDir
	// Drives. Capture drive info in aDrives
	// Table. Put Files and Directories in table model
	long aEntryCh;			// First char of entry denotes type of entry.
	long aCount, aSize = aDirInfo.count();
	long aRows = aSize;		// Number of directory and file rows in table view
	long aRow = 0;			// Row in table model
	QModelIndex aIdx;		// Index into table model
	QStringList aDrives, aFields;// List of tab-delimited fields in one entry
	QString aField;	
	QIcon aClosedIcon(":images/folderclosed.png");
	QIcon aDocumentIcon(":images/folderdocument.png");
	QIcon aDriveIcon(":images/folderdrive.png");
	for (aIx = 0; aIx < aSize; ++aIx)			
	{	aFields = aDirInfo.value(aIx).split('\t', QString::KeepEmptyParts);
		aCount = aFields.size();
		if (aCount >= 2)
		{	aEntryCh = aFields.at(0).toAscii().at(0);
			aField = aFields.at(1);
			if (aEntryCh == 'R')
			{	if (aField.length() >= 2 && aField.at(1) == ':')
					aField = aField.toUpper();
				aDrives += aField;
				--aRows;
			} 
			else if (aEntryCh == 'C')
			{	cCurDir = aField;
				--aRows;
			}
			// TableModel.  Fill table model with returned directories and files.
			else if (aEntryCh == 'D')
			{	if (aRows > 0)
				{	cpTableModel->insertRows(0, aRows);
					aRows = 0;
				}
				aIdx = cpTableModel->index(aRow, 0/*Col*/);			// Name
				cpTableModel->setData(aIdx, qVariantFromValue(aClosedIcon), Qt::DecorationRole);
				cpTableModel->setData(aIdx, QVariant(aField));
				aIdx = cpTableModel->index(aRow, 1/*Col*/);			// Size
				cpTableModel->setData(aIdx, QVariant(""));
				aIdx = cpTableModel->index(aRow++, 2/*Col*/);		// Type
				cpTableModel->setData(aIdx, QVariant("Directory"));
			}
			else if ( !cDirectoriesOnlyFlag && aEntryCh == 'F' && aCount >= 4 && match(aFilters, aField))
			{	aIdx = cpTableModel->index(aRow, 0/*Col*/);			// Name
				cpTableModel->setData(aIdx, qVariantFromValue(aDocumentIcon), Qt::DecorationRole);
				cpTableModel->setData(aIdx, QVariant(aField));
				aIdx = cpTableModel->index(aRow, 1/*Col*/);			// Size
				cpTableModel->setData(aIdx, QVariant(aFields.at(2)));
				aIdx = cpTableModel->index(aRow, 2/*Col*/);			// Type
				cpTableModel->setData(aIdx, QVariant("File"));
				aIdx = cpTableModel->index(aRow++, 3/*Col*/);		// Date Modified
				cpTableModel->setData(aIdx, QVariant(aFields.at(3)));
			}
			else
			{
				--aRows;
			}

		}
	}
	for (aIx = 0; aIx < aRows; ++aIx)
		cUi.upTableView->resizeRowToContents(aIx);

	// Drive Letter. Note drive letter of cCurDir, if any.
	long aCurDriveCh = '/';
	if (cCurDir.length() > 1 && cCurDir[1] == ':')
		aCurDriveCh = cCurDir.at(0).toAscii();

	// Drives. Put drives and expanded CurDir in the ComboBox drop-down list
	aSize = aDrives.count();
	if (aSize > 0)
	{	for (aIx = 0; aIx < aSize; ++aIx)			
		{	QString& arDrive = aDrives[aIx];
			// CurDir. If at matching drive, include each dir in ComboBox list
			if (arDrive.at(0).toAscii() == aCurDriveCh)
				expandCurDir(aIx);
			else
				cUi.upDirComboBox->addItem(aDriveIcon, arDrive);
		}
	}
	else
		expandCurDir(0);
				/// cUi.upDirComboBox->setCurDir();
}

/*!
\brief returnOutput - Update the task state and then process the returned information from the AIS server.

\param iDummyId - Placeholder (used in other parts of AIS for the connectID).
\param iXid - an incrementing integer assigned by appclient to each outgoing request to the server.
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
void ARemoteFileDialog::returnOutput(long iDummyId, long iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iDummyId);
    Q_UNUSED(iXid);
    Q_UNUSED(iRetValue);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(irDisplay);
    Q_UNUSED(irError);
    Q_UNUSED(iClientData);

    // Process events returned by AAppClient
  	AReqType aReqType = iReqType;
	QString aOut(irOut);

    // Process events returned by the AAppClient object
	setEnabled(true);
    switch(aReqType)
	{
	case geGetDirInfo:
		if (iStatus == 0)
			processGetDirInfo(aOut);
		break;

	case geFcnError:
		QMessageBox::warning(this,"AIS Request Error", aOut);
		break;

	default:
		break;
	}
}

/*!
\brief setContextName - Inherited from the AReturnRcvr abstract base class.

\param iConnectId  - The ID of the connection for this context.
\param irContextName - The new context name
\return void
*/
void ARemoteFileDialog::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irContextName);
}

/*!
\brief setFileName - Set the default file name in the dialog.

\return void
*/
void ARemoteFileDialog::setFileName(const QString& irFileName)
{
	cUi.upFileNameLineEdit->setText(irFileName);
}

void ARemoteFileDialog::setFilter(const QString& irFilter)
{
    if (!irFilter.isEmpty())
	{	long aIx;
		if (cUi.upTypeComboBox->count() == 1)
		{	cUi.upTypeComboBox->clear();
			cUi.upTypeComboBox->addItem(irFilter);
		}
		else if ((aIx = cUi.upTypeComboBox->findText(irFilter, Qt::MatchStartsWith)) >= 0)
			cUi.upTypeComboBox->setCurrentIndex(aIx);
		cpAppClient->getDirInfo(this, cCurDir);		// Filter changed, so clear out table and drop-down list and recalc new entries.
	}
}

/*!
\brief setFilters - Set the type of file to be opened.

\param irFilters - One or more filters to select the type of file to be opened.
\return void
*/
void ARemoteFileDialog::setFilters(const QString& irFilters)
{
	QString aFilters(irFilters);
	if (aFilters.isEmpty())
		aFilters = "Any (* *.*)";
	QString aSep(aFilters.contains(";;") ? ";;" : aFilters.contains("\n") ? "\n" : "");
	QStringList aTypes(aSep.isEmpty() ? QStringList(aFilters) : aFilters.split(aSep, QString::KeepEmptyParts));
	cUi.upTypeComboBox->clear();
	cUi.upTypeComboBox->addItems(aTypes);
	cUi.upTypeComboBox->setCurrentIndex(0);
	setFilter(cUi.upTypeComboBox->currentText());
}

/*!
\brief setDirectoriesOnly - Set to true to display directories only

\param irFlag - Boolean if set true will only display directories
\return void
*/
void ARemoteFileDialog::setDirectoriesOnly(bool iFlag)
{
	cDirectoriesOnlyFlag = iFlag;
}

/*!
\brief setDirectoriesOnly - Set to true to display directories only

\param irFlag - Boolean if set true will only display directories
\return void
*/
void ARemoteFileDialog::setDirectoryPath(const QString& irDirectory)
{
	QString aDirectory = irDirectory;
	int aIndex = aDirectory.lastIndexOf( "/" );
	if( aIndex != -1 )
		aDirectory = aDirectory.mid( 0, aIndex );
	else
		aDirectory = cCurDir;
	cpAppClient->getDirInfo(this, aDirectory);
}

/*!
\brief setDirectoriesOnly - Set to true to display directories only

\param irFlag - Boolean if set true will only display directories
\return void
*/
void ARemoteFileDialog::setInfoText(const QString& irInfo)
{
	cUi.upInfoLabel->setText( irInfo );
}
// end
