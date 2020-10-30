/**********************************************************************************
    Copyright (C) 2011 AIS Foundation, Inc.

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

#include "adatapage.h"
#include "appclient.h"
#include "asessionform.h"
#include "ageneratedatadialog.h"

#include <QtGui/QClipboard>
#include <QtGui/QAction>
#include <QtGui/QFileDialog>
#include <QtGui/QKeyEvent>
#include <QtGui/QMenu>
#include <QtGui/QProgressDialog>

ADataPage::ADataPage(AAppClient* ipAppClient,
                     ASessionForm* ipParentForm,
                     QWidget* ipParent,
                     const char* ipName)
                         : QWidget(ipParent),
                         cpAppClient(ipAppClient),
                         cpSessionForm(ipParentForm),
                         cpGenerateDataDialog(NULL)
{
    Q_UNUSED(ipName);
    cUi.setupUi(this);

    cUi.upDataTabWidget->setTabText(0, "Training");
    cUi.upDataTabWidget->setTabText(1, "Testing");
    cUi.upDataTabWidget->setTabText(2, "Estimates");

    cUi.upTrainingTableView->setModel(&cTrainingDataModel);
    cUi.upTestingTableView->setModel(&cTestingDataModel);
    cUi.upEstimatesTableView->setModel(&cEstimatesDataModel);

    cUi.upTrainingTableView->resizeRowsToContents();
    cUi.upTestingTableView->resizeRowsToContents();
    cUi.upEstimatesTableView->resizeRowsToContents();

    cUi.upTrainingTableView->setSelectionMode(QAbstractItemView::ContiguousSelection);
    cUi.upTestingTableView->setSelectionMode(QAbstractItemView::ContiguousSelection);
    cUi.upEstimatesTableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

    cUi.upTrainingTableView->installEventFilter(new ATableViewKeyPressHandler());
    cUi.upTestingTableView->installEventFilter(new ATableViewKeyPressHandler());
    cUi.upEstimatesTableView->installEventFilter(new ATableViewKeyPressHandler());

    cpCmdLineEdit = cUi.upFunctionComboBox->lineEdit();
    connect((QObject*)cpCmdLineEdit, SIGNAL(returnPressed()), (QObject*)this, SLOT(onReturnPressed()));

    cMode = 0;

    // pre-load dialog
    // dialog also contains the default parameters
    cpGenerateDataDialog = new AGenerateDataDialog(this);
    cpGenerateDataDialog->setRandomSeedChecked(true);
    cpGenerateDataDialog->setRandomSeed(2407987);
    cpGenerateDataDialog->setGenerateTrainingDataChecked(true);
    cpGenerateDataDialog->setGenerateTestingDataChecked(true);

    createActions();
    createMenus();
}

void ADataPage::getTabMenus(AMenuList& orTabMenus,
                            AToolList& orTabTools,
                            bool iSelected)
{
    Q_UNUSED(iSelected);
    orTabMenus.clear();

    orTabMenus << cpFileMenu << cpEditMenu;

    orTabTools = cToolList;
}

void ADataPage::returnOutput(long iConnectId,
                             long iRqId,
                             long iStatus,
                             AReqType iReqType,
                             long iRetValue,
                             const QString& irOut,
                             char* ipData,
                             long iDataSize,
                             const QString& irDisplay,
                             const QString& irError,
                             const QString iClientData)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(iRqId);
    Q_UNUSED(iStatus);
    Q_UNUSED(iReqType);
    Q_UNUSED(iRetValue);
    Q_UNUSED(irOut);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(irDisplay);
    Q_UNUSED(irError);
    Q_UNUSED(iClientData);

    if (!irOut.isEmpty() && irError.isEmpty())
    {
        QStringList aRows;
        QStringList aCols;
        int nTrainingRows = 0;
        int nTestingRows = 0;
        int nNumCols = 0;

        aRows = irOut.split('\n', QString::SkipEmptyParts);
        if (aRows.count() == 0)
            return;

        // get number of columns by looking at the first row
        aCols = aRows.first().split('\t');
        nNumCols = aCols.count();

        nTrainingRows = cpGenerateDataDialog->getNumTrainingDataRows();
        nTestingRows = cpGenerateDataDialog->getNumTestingDataRows();

        if ((cMode == 1) || (cMode == 3))
        {
            // only training data will be populated
            // disconnect from view to speed up update
            cUi.upTrainingTableView->setModel(NULL);

            if (nTrainingRows > cTrainingDataModel.dataRowCount())
                cTrainingDataModel.setRowCount(nTrainingRows);

            if (nNumCols > cTrainingDataModel.columnCount())
                cTrainingDataModel.setColumnCount(nNumCols);

            cTrainingDataModel.clear();

            // set independent variable names
            int i = 0;
            int i2 = 0;
            for (i = 0; i < (nNumCols - 1); ++i)
                cTrainingDataModel.setData(cTrainingDataModel.index(0,i), QString("x%1").arg(i));

            // set dependent variable name
            if (nNumCols > 1)
                cTrainingDataModel.setData(cTrainingDataModel.index(0,i), QString("y"));

            //QProgressDialog progressDlg("Populating training data...", QString(), 0, nTrainingRows, this);
            //progressDlg.setModal(true);
            //progressDlg.show();

            for (i = 0; i < nTrainingRows; ++i)
            {
                // extract columns
                //progressDlg.setValue(i);
                aCols = aRows.at(i).split('\t');
                i2 = i + 1;
                for (int j = 0; j < nNumCols; ++j)
                {
                    cTrainingDataModel.setData(cTrainingDataModel.index(i2,j), aCols.at(j));
                }
            }

            //progressDlg.setValue(nTrainingRows);

            // reconnect to view
            cUi.upTrainingTableView->setModel(&cTrainingDataModel);
            // resize rows
            cUi.upTrainingTableView->resizeRowsToContents();
        }

        if (cMode == 2)
            nTrainingRows = 0;

        if ((cMode == 2) || (cMode == 3))
        {
            // only testing data will be populated
            // disconnect from view to speed up update
            cUi.upTestingTableView->setModel(NULL);

            if (nTestingRows > cTestingDataModel.dataRowCount())
            {
                cTestingDataModel.setRowCount(nTestingRows);
                cEstimatesDataModel.setRowCount(nTestingRows);
            }

            if (nNumCols > cTestingDataModel.columnCount())
            {
                cTestingDataModel.setColumnCount(nNumCols);
                cEstimatesDataModel.setColumnCount(nNumCols);
            }

            cTestingDataModel.clear();

            // set independent variable names
            int i = 0;
            int i2 = 0;
            for (i = 0; i < (nNumCols - 1); ++i)
                cTestingDataModel.setData(cTestingDataModel.index(0,i), QString("x%1").arg(i));

            // set dependent variable name
            if (nNumCols > 1)
                cTestingDataModel.setData(cTestingDataModel.index(0,i), QString("y"));

            for (i = 0; i < nTestingRows; ++i)
            {
                // extract columns
                // continue at the end of training data
                aCols = aRows.at(nTrainingRows + i).split('\t');
                i2 = i + 1;
                for (int j = 0; j < nNumCols; ++j)
                {
                    cTestingDataModel.setData(cTestingDataModel.index(i2,j), aCols.at(j));
                }
            }

            // reconnect to view
            cUi.upTestingTableView->setModel(&cTestingDataModel);
            // resize rows
            cUi.upTestingTableView->resizeRowsToContents();
        }
    }
    else
    {
        cpSessionForm->statusAlert(irError);
    }

    cpSessionForm->enable(true);
    unsetCursor();
}

ADataPageModel& ADataPage::getCurrentModel()
{
    int nIndex = cUi.upDataTabWidget->currentIndex();
    return getModel(nIndex);
}

ADataPageModel& ADataPage::getModel(int index)
{
    if (index == INDEX_TRAINING_DATA)
        return cTrainingDataModel;

    if (index == INDEX_TESTING_DATA)
        return cTestingDataModel;

    if (index == INDEX_ESTIMATES_DATA)
        return cEstimatesDataModel;

    // return training model by default
    return cTrainingDataModel;
}

QTableView* ADataPage::getCurrentView()
{
    int nIndex = cUi.upDataTabWidget->currentIndex();
    return getTableView(nIndex);
}

QTableView* ADataPage::getTableView(int index)
{
    if (index == INDEX_TRAINING_DATA)
        return cUi.upTrainingTableView;

    if (index == INDEX_TESTING_DATA)
        return cUi.upTestingTableView;

    if (index == INDEX_ESTIMATES_DATA)
        return cUi.upEstimatesTableView;

    // return training table view by default
    return cUi.upTrainingTableView;
}

void ADataPage::on_upClearPushButton_clicked(bool)
{
    // clear all or just clear active tab?
    cTrainingDataModel.clear();
    cTestingDataModel.clear();
    cEstimatesDataModel.clear();
}

void ADataPage::generateData()
{
    QString model("0.0");

    if (cUi.upFunctionComboBox->currentText().trimmed().isEmpty() == false)
    {
		model = cUi.upFunctionComboBox->currentText().trimmed();
    }

    int nNumColumns = 0;
    int nNumRows = 0;
    int nIndex = 0;
    QRegExp aRegExp("x([0-9]+)");
    while ((nIndex = model.indexOf(aRegExp, nIndex)) != -1)
    {
        if (aRegExp.cap(1).toInt() > nNumColumns)
        {
            nNumColumns = aRegExp.cap(1).toInt();
        }
        nIndex += aRegExp.matchedLength();
    }

    nNumColumns++;
    if (cpGenerateDataDialog->getNumColumns() > nNumColumns)
        nNumColumns = cpGenerateDataDialog->getNumColumns();

    // determine data generation mode
    bool bGenTraining = cpGenerateDataDialog->isGenerateTrainingDataChecked();
    bool bGenTesting = cpGenerateDataDialog->isGenerateTestingDataChecked();

    if (!bGenTraining && !bGenTesting)
    {
        // nothing will be generated
        return;
    }

    if (bGenTraining && bGenTesting)
    {
        // generate both
        cMode = 3;
        nNumRows = cpGenerateDataDialog->getNumTrainingDataRows() + cpGenerateDataDialog->getNumTestingDataRows();
    }
    else if (bGenTesting)
    {
        // generate testing only
        cMode = 2;
        nNumRows = cpGenerateDataDialog->getNumTestingDataRows();
    }
    else
    {
        cMode = 1; // generate training only
        nNumRows = cpGenerateDataDialog->getNumTrainingDataRows();
    }

    QString commandStr("_ais\177eval\177exp\177");
    commandStr.append("(arcDemo.generateData \"model(");
    commandStr.append(model);
    commandStr.append(");\" ");
    commandStr.append(QString("%1").arg(cpGenerateDataDialog->getRandomError()));
    commandStr.append(" ");
    commandStr.append(QString::number(nNumRows));
    commandStr.append(" ");
    commandStr.append(QString::number(nNumColumns));
    commandStr.append(" ");
    commandStr.append(QString("%1").arg(cpGenerateDataDialog->getRange()));
    commandStr.append(" ");
    commandStr.append(QString("%1").arg(cpGenerateDataDialog->getSkew()));

    if (cpGenerateDataDialog->isRandomSeedChecked())
    {
        commandStr.append(" ");
        commandStr.append(QString("%1").arg(cpGenerateDataDialog->getRandomSeed()));
    }

    commandStr.append(")");

    if (cpAppClient->submit(this, commandStr, "", 0, NULL, 0) != 0)
    {
        cpSessionForm->enable(false);
        setCursor(Qt::BusyCursor);
    }
}

void ADataPage::on_upGeneratePushButton_clicked(bool)
{
    generateData();
}

void ADataPage::on_upOptionsPushButton_clicked(bool)
{
    if (cpGenerateDataDialog != NULL)
    {
        cpGenerateDataDialog->exec();
    }
}

void ADataPage::onReturnPressed()
{
    generateData();
}

void ADataPage::createActions()
{
    cpFileNewAct = new QAction(QIcon(":/images/document-new.png"), tr("&New"), this);
    cpFileNewAct->setShortcut(tr("Alt+N"));
    cpFileNewAct->setToolTip(tr("New data"));
    connect(cpFileNewAct, SIGNAL(triggered()), this, SLOT(onFileNew()));

    cpFileOpenAct = new QAction(QIcon(":/images/document-open.png"), tr("&Import"), this);
    cpFileOpenAct->setShortcut(tr("Alt+I"));
    cpFileOpenAct->setStatusTip("Import data from an existing file.");
    cpFileOpenAct->setToolTip(tr("Import data"));
    connect(cpFileOpenAct, SIGNAL(triggered()), this, SLOT(onFileOpen()));

    cpFileSaveAct = new QAction(QIcon(":/images/document-save.png"), tr("&Export"), this);
    cpFileSaveAct->setShortcut(tr("Alt+E"));
    cpFileSaveAct->setStatusTip("Export data to file.");
    cpFileSaveAct->setToolTip(tr("Export data"));
    connect(cpFileSaveAct, SIGNAL(triggered()), this, SLOT(onFileSave()));

    cpEditCutAct = new QAction(QIcon(":/images/edit-cut.png"), tr("Cut"), this);
    cpEditCutAct->setShortcut(tr("Ctrl+X"));
    cpEditCutAct->setStatusTip("Copy selection to clipboard then delete it.");
    cpEditCutAct->setToolTip("Cut Selection");
    connect(cpEditCutAct, SIGNAL(triggered()), this, SLOT(onEditCut()));

    cpEditCopyAct = new QAction(QIcon(":/images/edit-copy.png"), tr("Copy"), this);
    cpEditCopyAct->setShortcut(tr("Ctrl+C"));
    cpEditCopyAct->setStatusTip("Copy selection to the clipboard.");
    cpEditCopyAct->setToolTip("Copy Selection");
    connect(cpEditCopyAct, SIGNAL(triggered()), this, SLOT(onEditCopy()));

    cpEditPasteAct = new QAction(QIcon(":/images/edit-paste.png"), tr("Paste"), this);
    cpEditPasteAct->setShortcut(tr("Ctrl+V"));
    cpEditPasteAct->setStatusTip("Paste the clipboard into the current document");
    cpEditPasteAct->setToolTip("Paste");
    connect(cpEditPasteAct, SIGNAL(triggered()), this, SLOT(onEditPaste()));
}

void ADataPage::createMenus()
{
    cpFileMenu = new QMenu(QString("&File"), this);
    cpFileMenu->addAction(cpFileNewAct);
    cpFileMenu->addAction(cpFileOpenAct);
    cpFileMenu->addAction(cpFileSaveAct);

    cpEditMenu = new QMenu(QString("&Edit"), this);
    cpEditMenu->addAction(cpEditCutAct);
    cpEditMenu->addAction(cpEditCopyAct);
    cpEditMenu->addAction(cpEditPasteAct);

    cToolList << cpFileNewAct << cpFileOpenAct << cpFileSaveAct
            << cpEditCutAct << cpEditCopyAct << cpEditPasteAct;
}

void ADataPage::onFileNew()
{
    int index = cUi.upDataTabWidget->currentIndex();

    if (index == INDEX_TRAINING_DATA)
        cTrainingDataModel.clear();
    else if (index == INDEX_TESTING_DATA)
        cTestingDataModel.clear();
    else if (index == INDEX_ESTIMATES_DATA)
        cEstimatesDataModel.clear();
}

void ADataPage::onFileOpen()
{
    QString filter = "Tab Delimited Values (*.txt);;Comma Separated Values (*.csv)";
    QString fileName;
    QFileDialog openDialog(this, tr("Import data"));

    openDialog.setAcceptMode(QFileDialog::AcceptOpen);
    openDialog.setFileMode(QFileDialog::ExistingFile);
    openDialog.setFilter(filter);

    if (openDialog.exec())
    {
        fileName = openDialog.selectedFiles().first();
        getCurrentModel().loadFromFile(fileName);
        getCurrentView()->resizeRowsToContents();
    }
}

void ADataPage::onFileSave()
{
    //QString filter = "Tab Delimited Values (*.txt);;Comma Separated Values (*.csv)";
    QString fileName;
    QFileDialog saveDialog(this, tr("Export data"));

    saveDialog.setDefaultSuffix(tr("csv"));
    saveDialog.setAcceptMode(QFileDialog::AcceptSave);
    saveDialog.setFileMode(QFileDialog::AnyFile);
    saveDialog.selectFile(tr(".csv"));

    if (saveDialog.exec())
    {
        fileName = saveDialog.selectedFiles().first();
        getCurrentModel().saveToFile(fileName);
    }
}

void ADataPage::onEditCut()
{
    doCut();
}

void ADataPage::onEditCopy()
{
    doCopy();
}

void ADataPage::onEditPaste()
{
    doPaste();
}

void ADataPage::doCopy()
{
    QModelIndexList indexList = getCurrentView()->selectionModel()->selectedIndexes();
    QModelIndex first = indexList.first();
    QModelIndex last = indexList.last();
    QString clipboardText;
    ADataPageModel &model = getCurrentModel();
    int firstRow = first.row();
    int lastRow = last.row();
    int firstCol = first.column();
    int lastCol = last.column();

    for (int i = firstRow; i <= lastRow; ++i)
    {
        if (i > firstRow)
            clipboardText.append("\n");

        for (int j = firstCol; j <= lastCol; ++j)
        {
            if (j > firstCol)
                clipboardText.append("\t");

            clipboardText.append(model.data(model.index(i, j), Qt::EditRole).toString());
        }
    }

    QApplication::clipboard()->setText(clipboardText);
}

void ADataPage::doPaste()
{
    ADataPageModel &model = getCurrentModel();
    QTableView *pTableView = getCurrentView();
    QModelIndexList indexList = pTableView->selectionModel()->selectedIndexes();

    if (indexList.count() == 0)
        return;

    QString clipboardText = QApplication::clipboard()->text();
    QStringList rows = clipboardText.split('\n');

    // for compatibility with Microsoft Excel
    if (rows.last().isEmpty())
        rows.removeLast();

    int numRows = rows.count();

    if (numRows == 0)
        return;

    int numCols = rows.first().count('\t') + 1;
    int firstRow = indexList.first().row();
    int firstCol = indexList.first().column();

    if (model.rowCount() < (firstRow + numRows))
        model.setRowCount(firstRow + numRows - 1);

    if (model.columnCount() < (firstCol + numCols))
        model.setColumnCount(firstCol + numCols);

    for (int i = 0; i < numRows; ++i)
    {
        QStringList cols = rows.at(i).split('\t');
        for (int j = 0; j < numCols; ++j)
        {
            model.setData(model.index(firstRow + i, firstCol + j), cols.at(j));
        }
    }

    pTableView->resizeRowsToContents();
}

void ADataPage::doCut()
{
    doCopy();
    doDelete();
}

void ADataPage::doDelete()
{
    QModelIndexList indexList = getCurrentView()->selectionModel()->selectedIndexes();
    ADataPageModel &model = getCurrentModel();
    model.deleteItems(indexList);
}

bool ATableViewKeyPressHandler::eventFilter(QObject *obj, QEvent *event)
{
    if (event->type() == QEvent::KeyPress)
    {
        QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
        if (keyEvent->key() == Qt::Key_Return)
        {
            // handle input if there's any
            QObject::eventFilter(obj, event);
            QTableView *tableView = static_cast<QTableView *>(obj);

            // move to the next row
            QModelIndex index = tableView->currentIndex();
            tableView->setCurrentIndex(tableView->model()->index(index.row() + 1, index.column()));
            return true;
        }
        else if (keyEvent->key() == Qt::Key_Delete)
        {
            QTableView *pTableView = static_cast<QTableView *>(obj);
            QModelIndexList indexList = pTableView->selectionModel()->selectedIndexes();
            ADataPageModel *pModel = dynamic_cast<ADataPageModel *>(pTableView->model());
            pModel->deleteItems(indexList);
        }
    }

    return QObject::eventFilter(obj, event);
}
