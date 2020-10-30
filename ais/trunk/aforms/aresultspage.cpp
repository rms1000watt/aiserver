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

#include "aresultspage.h"
#include "appclient.h"
#include "asessionform.h"
#include "adatapage.h"
#include "amodelpage.h"
#include "arunpage.h"

AResultsPage::AResultsPage(AAppClient* ipAppClient,
                           ASessionForm* ipParentForm,
                           QWidget* ipParent,
                           const char* ipName)
                               : QWidget(ipParent),
                               cpAppClient(ipAppClient),
                               cpSessionForm(ipParentForm)
{
    Q_UNUSED(ipName);
    cUi.setupUi(this);

    QStringList solutionListLabels;
    solutionListLabels.append("Fitness");
    solutionListLabels.append("Testing Score");
    solutionListLabels.append("Solution");

    cUi.upSolutionsTableWidget->setColumnCount(3);
    cUi.upSolutionsTableWidget->setHorizontalHeaderLabels(solutionListLabels);
    cUi.upSolutionsTableWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    cUi.upSolutionsTableWidget->setSelectionMode(QAbstractItemView::SingleSelection);
    cUi.upSolutionsTableWidget->setColumnWidth(0, 100);
    cUi.upSolutionsTableWidget->setColumnWidth(1, 100);
    cUi.upSolutionsTableWidget->setColumnWidth(2, 240);
    cUi.upSolutionsTableWidget->resizeRowsToContents();

    QFont font = cUi.upSolutionTexEdit->font();
    font.setPointSize(font.pointSize() + 4);
    font.setStyleHint(QFont::TypeWriter);
    font.setItalic(true);

    QFontMetrics fontMetrics(font);
    cUi.upSolutionTexEdit->setFixedHeight(fontMetrics.height() * 4);
    cUi.upSolutionTexEdit->setFont(font);
    cUi.upSolutionTexEdit->setReadOnly(true);

    QStringList aColLabels;
    aColLabels.append("Statistic");
    aColLabels.append("Value");

    QStringList aRowLabels;
    aRowLabels.append("Normalized mean absolute error");
    aRowLabels.append("Normalized least squared error");
    aRowLabels.append("Tail classification error");
    aRowLabels.append("R-square statistic");

    cUi.upSolutionStatisticsTableWidget->setColumnCount(aColLabels.count());
    cUi.upSolutionStatisticsTableWidget->setRowCount(aRowLabels.count());

    cUi.upSolutionStatisticsTableWidget->setHorizontalHeaderLabels(aColLabels);

    QTableWidgetItem *pItem = NULL;

    for (int i = 0; i < aRowLabels.count(); ++i)
    {
        pItem = new QTableWidgetItem(aRowLabels.at(i));
        cUi.upSolutionStatisticsTableWidget->setItem(i, 0, pItem);

        pItem = new QTableWidgetItem("");
        cUi.upSolutionStatisticsTableWidget->setItem(i, 1, pItem);
    }

    cUi.upSolutionStatisticsTableWidget->resizeColumnsToContents();
    cUi.upSolutionStatisticsTableWidget->resizeRowsToContents();
}

void AResultsPage::getTabMenus(AMenuList& orTabMenus,
                               AToolList& orTabTools,
                               bool iSelected)
{
    Q_UNUSED(iSelected);
    orTabMenus.clear();
    orTabTools.clear();
}

void AResultsPage::returnOutput(long iConnectId,
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
        QTableWidgetItem *pItem = NULL;

        aRows = irOut.split('\n', QString::SkipEmptyParts);
        if (aRows.count() == 0)
            return;

        if (aRows.first() == "solutions")
        {
            // remove the first entry
            aRows.removeFirst();

            cUi.upSolutionsTableWidget->clearContents();
            cResultsList.clear();

            cUi.upSolutionsTableWidget->setRowCount(aRows.count());

            for (int i = 0; i < aRows.count(); ++i)
            {
                aCols = aRows.at(i).split('\t');

                if (aCols.count() < 5)
                    continue;

                // fields
                // 1 - solution
                // 2 - nmae
                // 3 - nlse
                // 4 - tce
                // 5 - rsq

                cResultsList.append(aCols);

                pItem = new QTableWidgetItem(aCols.at(2));
                pItem->setTextAlignment(Qt::AlignCenter);
                pItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
                cUi.upSolutionsTableWidget->setItem(i, INDEX_FITNESS_COLUMN, pItem);

                pItem = new QTableWidgetItem(aCols.at(0));
                pItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
                cUi.upSolutionsTableWidget->setItem(i, INDEX_SOLUTION_COLUMN, pItem);

                //cUi.upSolutionsTableWidget->setRowHeight(i, 20);
                cUi.upSolutionsTableWidget->resizeRowsToContents();
            }

            cpSessionForm->statusMsg("Solutions list updated.");
        }
        else if (aRows.first() == "score")
        {
            // remove the first entry
            aRows.removeFirst();

            if (aRows.count() == 0)
                return;

            QStringList aResult = aRows.first().split('\t');

            if (aResult.count() < 2)
                return;

            int index = aResult.first().toInt();

            pItem = new QTableWidgetItem(aResult.at(1));
            pItem->setTextAlignment(Qt::AlignCenter);
            pItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
            cUi.upSolutionsTableWidget->setItem(index, INDEX_TESTING_COLUMN, pItem);

            cpSessionForm->statusMsg("Testing score updated.");
        }
        else if (aRows.first() == "estimates")
        {
            // remove the first entry - label
            aRows.removeFirst();

            if (aRows.count() == 0)
                return;

            // remove the second entry - formula index
            int index = aRows.first().toInt();
            aRows.removeFirst();

            QStringList aEstimates = aRows.first().split('\t');

            ADataPageModel &model = cpSessionForm->getDataPage()->getModel(INDEX_ESTIMATES_DATA);
            model.setData(model.index(0, 1), QString("Est. y[%1]").arg(index + 1));
            for (int i = 0; i < aEstimates.count(); ++i)
            {
                model.setData(model.index(i + 1, 1), aEstimates.at(i), Qt::EditRole);
            }

            cpSessionForm->getDataPage()->getTableView(INDEX_ESTIMATES_DATA)->resizeRowsToContents();
            cpSessionForm->statusMsg("Estimates tab updated.");
        }
    }
    else
    {
        cpSessionForm->statusAlert(irError);
    }

    cpSessionForm->enable(true);
    unsetCursor();
}

void AResultsPage::getSolutions()
{
    QString commandStr("_ais\177eval\177exp\177");
    commandStr.append("(arcDemo.getResults)");

    if (cpAppClient->submit(this, commandStr, "", 0, NULL, 0) != 0)
    {
        cpSessionForm->enable(false);
        setCursor(Qt::BusyCursor);
    }
}

void AResultsPage::getTestingScore()
{
    // do we send the testing data every time?
    // can we just do it once for everything?
    // what do we pass?
    // how do we distinguish the return of this call from the return of getSolutions

    QString commandStr("_ais\177eval\177exp\177");
    commandStr.append("(begin ");

    int index = cUi.upSolutionsTableWidget->currentIndex().row();

    // if there are no solutions, quit
    if (cResultsList.isEmpty())
        return;

    // if nothing was selected, quit
    if (index < 0)
        return;

    // get selected solution
    QString modelStr = cResultsList.at(index).at(0);

    if (modelStr.isEmpty())
        return;

    ADataPage *pDataPage = cpSessionForm->getDataPage();

    if (pDataPage->getModel(INDEX_TESTING_DATA).isDirty())
    {
        // testing data was changed, we need to send this to the engine
        AModelPage *pModelPage = cpSessionForm->getModelPage();
        QList<int> independentVars = pModelPage->getIndependentVariableIndexes();
        int dependentVar = pModelPage->getDependentVariableIndex();

        // if no dependent variable was specified, quit
        if ((dependentVar < 0) || independentVars.isEmpty())
            return;

        QString objVector = cpSessionForm->getDataPage()->getModel(INDEX_TESTING_DATA).toStringConstantObjVector(independentVars);
        QString numVector = cpSessionForm->getDataPage()->getModel(INDEX_TESTING_DATA).toStringConstantNumVector(dependentVar);

        commandStr.append("(setq arcDemo.cTestX ");
        commandStr.append(objVector);
        commandStr.append(")");
        commandStr.append("(setq arcDemo.cTestY ");
        commandStr.append(numVector);
        commandStr.append(")");

        // set dirty flag to false to prevent resending of testing data
        pDataPage->getModel(INDEX_TESTING_DATA).setDirty(false);
    }

    commandStr.append("(arcDemo.getScore ");
    commandStr.append(QString("\"%1\" %2").arg(modelStr).arg(index));
    commandStr.append("))");

    if (cpAppClient->submit(this, commandStr, "", 0, NULL, 0) != 0)
    {
        cpSessionForm->enable(false);
        setCursor(Qt::BusyCursor);
    }
}

void AResultsPage::getEstimates()
{
    QString commandStr("_ais\177eval\177exp\177");
    commandStr.append("(begin ");

    int index = cUi.upSolutionsTableWidget->currentIndex().row();

    // if there are no solutions, quit
    if (cResultsList.isEmpty())
        return;

    // if nothing was selected, quit
    if (index < 0)
        return;

    // get selected solution
    QString modelStr = cResultsList.at(index).at(0);

    if (modelStr.isEmpty())
        return;

    ADataPage *pDataPage = cpSessionForm->getDataPage();
    AModelPage *pModelPage = cpSessionForm->getModelPage();
    int dependentVar = pModelPage->getDependentVariableIndex();

    ADataPageModel &rTestingModel =  pDataPage->getModel(INDEX_TESTING_DATA);
    ADataPageModel &rEstimatesModel = pDataPage->getModel(INDEX_ESTIMATES_DATA);

    // copy actual y values to estimates tab
    int nRows = rTestingModel.rowCount();
    for (int i = 0; i < nRows; ++i)
    {
        rEstimatesModel.setData(rEstimatesModel.index(i, 0),
                                rTestingModel.data(rTestingModel.index(i, dependentVar), Qt::EditRole));
    }

    if (rTestingModel.isDirty())
    {
        // testing data was changed, we need to send this to the engine
        QList<int> independentVars = pModelPage->getIndependentVariableIndexes();

        // if no dependent variable was specified, quit
        if ((dependentVar < 0) || independentVars.isEmpty())
            return;

        QString objVector = rTestingModel.toStringConstantObjVector(independentVars);
        QString numVector = rTestingModel.toStringConstantNumVector(dependentVar);

        commandStr.append("(setq arcDemo.cTestX ");
        commandStr.append(objVector);
        commandStr.append(")");
        commandStr.append("(setq arcDemo.cTestY ");
        commandStr.append(numVector);
        commandStr.append(")");

        // set dirty flag to false to prevent resending of testing data
        rTestingModel.setDirty(false);
    }

    commandStr.append("(arcDemo.getEstimates ");
    commandStr.append(QString("\"%1\" %2").arg(modelStr).arg(index));
    commandStr.append("))");

    if (cpAppClient->submit(this, commandStr, "", 0, NULL, 0) != 0)
    {
        cpSessionForm->enable(false);
        setCursor(Qt::BusyCursor);
    }
}

void AResultsPage::on_upRefreshPushButton_clicked(bool)
{
    getSolutions();
}

void AResultsPage::on_upScorePushButton_clicked(bool)
{
    getTestingScore();
}

void AResultsPage::on_upEstimatePushButton_clicked(bool)
{
    getEstimates();
}

void AResultsPage::on_upSolutionsTableWidget_currentCellChanged(int curRow, int /*curCol*/, int /*prevRow*/, int /*prevCol*/)
{
    displaySolutionDetails(curRow);
}

void AResultsPage::on_upSolutionsTableWidget_cellActivated(int curRow, int /*curCol*/)
{
    displaySolutionDetails(curRow);
}

void AResultsPage::displaySolutionDetails(int row)
{
    cUi.upSolutionTexEdit->clear();
    if (row < 0)
        return;

    cUi.upSolutionTexEdit->setText(cResultsList.at(row).at(0));

    for (int i = 1; i < cResultsList.at(row).count(); ++i)
    {
        cUi.upSolutionStatisticsTableWidget->item(i - 1, 1)->setText(cResultsList.at(row).at(i));
    }

    cUi.upSolutionStatisticsTableWidget->resizeColumnsToContents();
}

void AResultsPage::show()
{
    ARunPage *pRunPage = cpSessionForm->getRunPage();

    if (pRunPage->isDirty())
    {
        getSolutions();
        pRunPage->setDirty(false);
    }
}
