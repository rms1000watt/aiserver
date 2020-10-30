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
#include "adatapreviewpage.h"
#include "appclient.h"
#include "asessionform.h"
#include "aplotter.h"

#include <QtGui/QVBoxLayout>

ADataPreviewPage::ADataPreviewPage(AAppClient* ipAppClient,
                                   ASessionForm* ipParentForm,
                                   QWidget* ipParent,
                                   const char* ipName)
                                       : QWidget(ipParent),
                                       cpAppClient(ipAppClient),
                                       cpSessionForm(ipParentForm),
                                       cDisplayEnabled(false)
{
    Q_UNUSED(ipName);
    cUi.setupUi(this);

    //QVBoxLayout *pLayout = new QVBoxLayout(this);
    cpPlotter = new APlotter(this);
    cUi.verticalLayout->addWidget(cpPlotter);
    //pLayout->addWidget(cpPlotter);
}

void ADataPreviewPage::getTabMenus(AMenuList& orTabMenus,
                                   AToolList& orTabTools,
                                   bool iSelected)
{
    Q_UNUSED(iSelected);
    orTabMenus.clear();
    orTabTools.clear();
}

void ADataPreviewPage::returnOutput(long iConnectId,
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
}

void ADataPreviewPage::show()
{
    int nIndexX = cUi.upXaxisComboBox->currentIndex();
    int nIndexY = cUi.upYaxisComboBox->currentIndex();

    cUi.upXaxisComboBox->clear();
    cUi.upYaxisComboBox->clear();

    ADataPage *pDataPage = cpSessionForm->getDataPage();
    const ADataPageHeader &columns = pDataPage->getModel(INDEX_TRAINING_DATA).getActiveColumns();
    ADataPageHeader::const_iterator column_iter;
    column_iter = columns.constBegin();
    while (column_iter != columns.constEnd())
    {
        if (!column_iter.value().isEmpty())
        {
            cUi.upXaxisComboBox->addItem(column_iter.value(), column_iter.key());
            cUi.upYaxisComboBox->addItem(column_iter.value(), column_iter.key());
        }
        column_iter++;
    }

    if ((nIndexX == -1) || (nIndexX >= cUi.upXaxisComboBox->count()))
    {
        nIndexX = 0;
    }

    if ((nIndexY == -1) || (nIndexY >= cUi.upYaxisComboBox->count()))
    {
        nIndexY = 0;
    }

    setDisplayEnabled(false);
    cUi.upXaxisComboBox->setCurrentIndex(nIndexX);
    cUi.upYaxisComboBox->setCurrentIndex(nIndexY);
    setDisplayEnabled(true);

    displayData();
}

void ADataPreviewPage::on_upXaxisComboBox_currentIndexChanged(int index)
{
    Q_UNUSED(index);
    if (cDisplayEnabled)
        displayData();
}

void ADataPreviewPage::on_upYaxisComboBox_currentIndexChanged(int index)
{
    Q_UNUSED(index);
    if (cDisplayEnabled)
        displayData();
}

void ADataPreviewPage::displayData()
{
    double dMinX = 0.0;
    double dMaxX = 0.0;
    double dMinY = 0.0;
    double dMaxY = 0.0;
    double dX = 0.0;
    double dY = 0.0;
    QVector<QPointF> trainingData; // training data
    QVector<QPointF> testingData; // testing data

    int nIndexX = cUi.upXaxisComboBox->currentIndex();
    int nIndexY = cUi.upYaxisComboBox->currentIndex();
    nIndexX = cUi.upXaxisComboBox->itemData(nIndexX).toInt();
    nIndexY = cUi.upYaxisComboBox->itemData(nIndexY).toInt();

    if (cUi.upXaxisComboBox->count() == 0 || cUi.upYaxisComboBox->count() == 0)
    {
        cpPlotter->clearCurve(0);
        cpPlotter->clearCurve(1);
        return;
    }

    const ADataPageTable &trainingRows = cpSessionForm->getDataPage()->getModel(INDEX_TRAINING_DATA).getActiveRows();
    if (trainingRows.isEmpty())
    {
        cpPlotter->clearCurve(0);
    }
    else
    {
        dMinX = dMaxX = trainingRows[1][nIndexX];
        dMinY = dMaxY = trainingRows[1][nIndexY];

        QMapIterator<int, ADataPageRow> row(trainingRows);
        while (row.hasNext())
        {
            row.next();
            dX = row.value()[nIndexX];
            dY = row.value()[nIndexY];

            // get 1st and 2nd columns
            trainingData.append(QPointF(dX, dY));
            if (dX < dMinX)
                dMinX = dX;
            if (dY < dMinY)
                dMinY = dY;

            if (dX > dMaxX)
                dMaxX = dX;
            if (dY > dMaxY)
                dMaxY = dY;
        }
    }

    const ADataPageTable &testingRows = cpSessionForm->getDataPage()->getModel(INDEX_TESTING_DATA).getActiveRows();
    if (testingRows.isEmpty())
    {
        cpPlotter->clearCurve(1);
    }
    else
    {
        if (trainingRows.isEmpty())
        {
            dMinX = dMaxX = testingRows[1][nIndexX];
            dMinY = dMaxY = testingRows[1][nIndexY];
        }

        QMapIterator<int, ADataPageRow> row(testingRows);
        while (row.hasNext())
        {
            row.next();
            dX = row.value()[nIndexX];
            dY = row.value()[nIndexY];

            // get 1st and 2nd columns
            testingData.append(QPointF(dX, dY));
            if (dX < dMinX)
                dMinX = dX;
            if (dY < dMinY)
                dMinY = dY;

            if (dX > dMaxX)
                dMaxX = dX;
            if (dY > dMaxY)
                dMaxY = dY;
        }
    }

    APlotSettings aPlotSettings;
    aPlotSettings.minX = dMinX;
    aPlotSettings.minY = dMinY;
    aPlotSettings.maxX = dMaxX;
    aPlotSettings.maxY = dMaxY;
    aPlotSettings.numXTicks = 5.0;
    aPlotSettings.numYTicks = 5.0;
    cpPlotter->setPlotSettings(aPlotSettings);

    if (!trainingData.isEmpty())
    {
        cpPlotter->setCurveData(0, trainingData);
        cpPlotter->setCurveLabel(0, "Training data");
    }

    if (!testingData.isEmpty())
    {
        cpPlotter->setCurveData(1, testingData);
        cpPlotter->setCurveLabel(1, "Testing data");
    }
}

void ADataPreviewPage::setDisplayEnabled(bool enabled)
{
    cDisplayEnabled = enabled;
}
