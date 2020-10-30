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

#include "arunpage.h"
#include "appclient.h"
#include "asessionform.h"
#include "adatapage.h"
#include "amodelpage.h"

ARunPage::ARunPage(AAppClient* ipAppClient,
                   ASessionForm* ipParentForm,
                   QWidget* ipParent,
                   const char* ipName)
                       : QWidget(ipParent),
                       cpAppClient(ipAppClient),
                       cpSessionForm(ipParentForm)
{
    Q_UNUSED(ipName);
    cUi.setupUi(this);
    cUi.upStopPushButton->setEnabled(false);

	cLabelRowMap["G"] = 0;
	cLabelRowMap["WFFs"] = 1;
    // 2 is separator
    cLabelRowMap["SCORE"] = 3;
    cLabelRowMap["NLSE"] = 4;
    cLabelRowMap["RSQ"] = 5;
    cLabelRowMap["HSRC"] = 6;

    cIsDirty = false;

    // set up column headers
    QStringList aColLabels;
    aColLabels.append("Statistic");
    aColLabels.append("Value");

    QStringList aRowLabels;
    aRowLabels.append("No. of generations");
    aRowLabels.append("No. of WFFs");
    aRowLabels.append("...");
    aRowLabels.append("Best fitness");
    aRowLabels.append("Best normalized least squared error");
    aRowLabels.append("Best r-squared");
    aRowLabels.append("Best homeomorphic source");

    cUi.upStatisticsTableWidget->setColumnCount(aColLabels.count());
    cUi.upStatisticsTableWidget->setHorizontalHeaderLabels(aColLabels);
    cUi.upStatisticsTableWidget->setRowCount(aRowLabels.count());

	QTableWidgetItem *pItem = NULL;

	for (int i = 0; i < aRowLabels.count(); ++i)
	{
		pItem = new QTableWidgetItem(aRowLabels.at(i));
		cUi.upStatisticsTableWidget->setItem(i, 0, pItem);

		pItem = new QTableWidgetItem("");
		cUi.upStatisticsTableWidget->setItem(i, 1, pItem);
	}

    cUi.upStatisticsTableWidget->resizeColumnsToContents();
    cUi.upStatisticsTableWidget->resizeRowsToContents();
}

void ARunPage::getTabMenus(AMenuList& orTabMenus,
                           AToolList& orTabTools,
                           bool iSelected)
{
    Q_UNUSED(iSelected);
    orTabMenus.clear();
    orTabTools.clear();
}

void ARunPage::returnOutput(long iConnectId,
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

    if (irError.isEmpty())
    {
        cpSessionForm->statusMsg("Training complete.");
    }
    else
    {
        cpSessionForm->statusAlert("Training failed.");
    }

    cIsDirty = true;

    cpSessionForm->setDemoRunMode(false);
    cpSessionForm->enable(true);

    cUi.upStartPushButton->setEnabled(true);
    cUi.upStopPushButton->setEnabled(false);

    cUi.upLogsTextEdit->appendPlainText(irDisplay);
}

void ARunPage::onStatusUpdateEvent(const QString& irMsg)
{
	QStringList aList = irMsg.split(',');
	QRegExp aRegExp("(.+)=\\[(.+)\\]");

	for (int i = 0; i < aList.count(); ++i)
	{
		if (aRegExp.indexIn(aList.at(i)) != -1)
		{
			QString aCode = aRegExp.cap(1);
			QString aValue = aRegExp.cap(2);
			int nRowIndex = cLabelRowMap.value(aCode, -1);

			if (nRowIndex < 0)
				continue;

			QTableWidgetItem *pItem = cUi.upStatisticsTableWidget->item(nRowIndex, 1);
			if (pItem != NULL)
				pItem->setText(aValue);
		}
	}

	cUi.upStatisticsTableWidget->resizeColumnsToContents();
}

void ARunPage::on_upStartPushButton_clicked(bool)
{
    AModelPage *pModelPage = cpSessionForm->getModelPage();
    ADataPage *pDataPage = cpSessionForm->getDataPage();

    //qDebug("%s", qPrintable(numVector));
    //qDebug("%s", qPrintable(objVector));

    int nMaxGens = pModelPage->getMaxGenerations();
    double dHaltingFitness = pModelPage->getHaltingFitness();
    QString aGoal = pModelPage->getGoalExpression();
	QString aType = pModelPage->getRegressionType();

    QString commandStr("_ais\177eval\177exp\177");
	commandStr.append(cpSessionForm->getCommandPrefix());
    commandStr.append("(begin ");

    // if the data was modified, we need to send it to the engine
    if (pDataPage->getModel(INDEX_TRAINING_DATA).isDirty())
    {
        QList<int> independentVars = pModelPage->getIndependentVariableIndexes();
        int dependentVar = pModelPage->getDependentVariableIndex();

        if ((dependentVar < 0) || independentVars.isEmpty())
            return;

        QString objVector = pDataPage->getModel(INDEX_TRAINING_DATA).toStringConstantObjVector(independentVars);
        QString numVector = pDataPage->getModel(INDEX_TRAINING_DATA).toStringConstantNumVector(dependentVar);

        commandStr.append("(setq arcDemo.cTrainX ");
        commandStr.append(objVector);
        commandStr.append(")");
        commandStr.append("(setq arcDemo.cTrainY ");
        commandStr.append(numVector);
        commandStr.append(")");

        pDataPage->getModel(INDEX_TRAINING_DATA).setDirty(false);
    }

    commandStr.append(QString("(arcDemo.start \"%1\" %2 %3 %4)").arg(aGoal).arg(nMaxGens).arg(dHaltingFitness).arg(aType));
    commandStr.append(")");

    if (cpAppClient->submit(this, commandStr, "", 0, NULL, 0) != 0)
    {
        cUi.upStopPushButton->setEnabled(true);
        cUi.upStartPushButton->setEnabled(false);

        // change mode of session form to redirect all output to this tab
        cpSessionForm->setDemoRunMode(true);
        cpSessionForm->enable(false);

        clearResults();
    }
}

void ARunPage::display(const QString& irDisplay, const QString& irOut)
{
    cUi.upLogsTextEdit->appendPlainText(irDisplay + irOut);
}

void ARunPage::on_upStopPushButton_clicked(bool)
{
    cpAppClient->setEscape(this, 0);
}

void ARunPage::clearResults()
{
    cUi.upLogsTextEdit->clear();
    int nRows = cUi.upStatisticsTableWidget->rowCount();
    for (int i = 0; i < nRows; ++i)
    {
        cUi.upStatisticsTableWidget->item(i, 1)->setText("");
    }
}

bool ARunPage::isDirty()
{
    return cIsDirty;
}

void ARunPage::setDirty(bool dirty)
{
    cIsDirty = dirty;
}
