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

#include "amodelpage.h"
#include "adatapage.h"
#include "appclient.h"
#include "asessionform.h"
#include "agenerategoaldialog.h"
#include "ageneratecartltabstractdialog.h"

#include <QtGui/QListWidgetItem>

AModelPage::AModelPage(AAppClient* ipAppClient,
                       ASessionForm* ipParentForm,
                       QWidget* ipParent,
                       const char* ipName)
                           : QWidget(ipParent),
                           cpAppClient(ipAppClient),
                           cpSessionForm(ipParentForm)
{
    Q_UNUSED(ipName);
    cUi.setupUi(this);

    cpAbstractDialog = new AGenerateGoalDialog(this);
    cpAbstractDialog->setMaxDepth(3);
    cpAbstractDialog->setNumBasisFunctions(1);
    cpAbstractDialog->setUseTerms(true);

    cpCartLtAbstractDialog = new AGenerateCartLtAbstractDialog(this);
    cpCartLtAbstractDialog->setMaxTreeDepth(2);
    cpCartLtAbstractDialog->setMaxLeafNodeDepth(3);
    cpCartLtAbstractDialog->setUseTerms(true);

    setupWidgetValues();
}

void AModelPage::getTabMenus(AMenuList& orTabMenus,
                            AToolList& orTabTools,
                            bool iSelected)
{
    Q_UNUSED(iSelected);
    orTabMenus.clear();
    orTabTools.clear();
}

void AModelPage::returnOutput(long iConnectId,
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

    cpSessionForm->enable(true);
    cUi.upGoalExprTextEdit->clear();
    cUi.upGoalExprTextEdit->setText(irOut);
}

void AModelPage::setupWidgetValues()
{
    //int index = cUi.upAlgorithmComboBox->currentIndex();
    //cUi.upAlgorithmComboBox->clear();
    //cUi.upAlgorithmComboBox->addItem("ARC", QVariant());
    //if (index == -1)
    //    index = 0;
    //cUi.upAlgorithmComboBox->setCurrentIndex(index);

    //index = cUi.upScoringComboBox->currentIndex();
    //cUi.upScoringComboBox->clear();
    //cUi.upScoringComboBox->addItem("Normalized Least Squared Error");
    //cUi.upScoringComboBox->addItem("Normalized Mean Absolute Error");
    //cUi.upScoringComboBox->addItem("User Defined - ?");
    //if (index == -1)
    //    index = 0;
    //cUi.upScoringComboBox->setCurrentIndex(index);

	int index = 0;

    ADataPage *pDataPage = cpSessionForm->getDataPage();
    const ADataPageHeader &columns = pDataPage->getModel(INDEX_TRAINING_DATA).getActiveColumns();
    ADataPageHeader::const_iterator column_iter;

    // add only columns with a variable name
    QString text = cUi.upDependentVarComboBox->currentText();
    cUi.upDependentVarComboBox->clear();
    column_iter = columns.constBegin();
    while (column_iter != columns.constEnd())
    {
        if (!column_iter.value().isEmpty())
            cUi.upDependentVarComboBox->addItem(column_iter.value(), column_iter.key());

        column_iter++;
    }

    // by default, the last column is the dependent variable
    if ((index = cUi.upDependentVarComboBox->findText(text)) != -1)
        cUi.upDependentVarComboBox->setCurrentIndex(index);
    else
        cUi.upDependentVarComboBox->setCurrentIndex(cUi.upDependentVarComboBox->count() - 1);

    if (getGoalExpression().isEmpty())
    {
        generateLinearModel();
    }

    if ((index = cUi.upRegressionTypeComboBox->currentIndex()) == -1)
        index = 0;

    cUi.upRegressionTypeComboBox->clear();
    cUi.upRegressionTypeComboBox->addItem("Fast Regression", "fastREGRESSION:");
    cUi.upRegressionTypeComboBox->addItem("Sampled Regression", "sampledREGRESSION:");
    cUi.upRegressionTypeComboBox->addItem("Full Regression", "fullREGRESSION:");
    cUi.upRegressionTypeComboBox->setCurrentIndex(index);

//    // add only columns with values
//    QList<QListWidgetItem*> items = cUi.upIndependentVarList->selectedItems();
//    QStringList items_str;
//    for (int i = 0; i < items.count(); ++i)
//        items_str.append(items[i]->text());

//    cUi.upIndependentVarList->clear();
//    column_iter = columns.constBegin();
//    while (column_iter != columns.constEnd())
//    {
//        if (!column_iter.value().isEmpty())
//        {
//            QListWidgetItem *pListItem = new QListWidgetItem(column_iter.value());
//            pListItem->setData(Qt::UserRole, column_iter.key());
//            cUi.upIndependentVarList->addItem(pListItem);
//        }

//        column_iter++;
//    }

//    for (int i = 0; i < items_str.count(); ++i)
//    {
//        items = cUi.upIndependentVarList->findItems(items_str.at(i), Qt::MatchExactly | Qt::MatchCaseSensitive);
//        for (int j = 0; j < items.count(); ++j)
//            items.at(j)->setSelected(true);
//    }
}

void AModelPage::on_upAbstractModelPushButton_clicked(bool)
{
    generateAbstractModel();
}

void AModelPage::on_upCartLtAbstractModelPushButton_clicked(bool)
{
    generateCartLtAbstractModel();
}

void AModelPage::on_upLinearModelPushButton_clicked(bool)
{
    generateLinearModel();
}

void AModelPage::on_upCubicModelPushButton_clicked(bool)
{
    generateCubicModel();
}

void AModelPage::generateLinearModel()
{
    // get total number of independent variables
    int nCnt = cUi.upDependentVarComboBox->count() - 1;

    cUi.upGoalExprTextEdit->clear();

   QString aGoal("regress(");
   if (nCnt > 0)
    {
        // the default goal expression is
        // regress(x0,x1,...x?);
        for (int i = 0; i < (nCnt - 1); ++i)
        {
            aGoal.append(QString("x%1,").arg(i));
        }
        aGoal.append(QString("x%1").arg(nCnt - 1));
    }
   aGoal.append(");");
   cUi.upGoalExprTextEdit->append(aGoal);
}

void AModelPage::generateCubicModel()
{
    // get total number of independent variables
    int nCnt = cUi.upDependentVarComboBox->count() - 1;

    cUi.upGoalExprTextEdit->clear();

   QString aGoal("regress(");
   if (nCnt > 0)
    {
        // the default goal expression is
        // regress(x0,x1,...x?);
        for (int i = 0; i < (nCnt - 1); ++i)
        {
            aGoal.append(QString("cubic(x%1),").arg(i));
        }
        aGoal.append(QString("cubic(x%1)").arg(nCnt - 1));
    }
   aGoal.append(");");
   cUi.upGoalExprTextEdit->append(aGoal);
}

void AModelPage::generateAbstractModel()
{
    if (cpAbstractDialog->exec() == QDialog::Accepted)
    {
        int nMaxDepth = cpAbstractDialog->getMaxDepth();
        int nNumBasis = cpAbstractDialog->getNumBasisFunctions();
        QString useTerms = "false";
        if (cpAbstractDialog->isUseTerms())
            useTerms = "true";

        QString commandStr("_ais\177eval\177exp\177");
        commandStr.append(QString("(arcDemo.getAbstractModel %1 %2 %3)").arg(nMaxDepth).arg(nNumBasis).arg(useTerms));

        if (cpAppClient->submit(this, commandStr, "", 0, NULL, 0) != 0)
        {
            cpSessionForm->enable(false);
        }
    }
}

void AModelPage::generateCartLtAbstractModel()
{
    if (cpCartLtAbstractDialog->exec() == QDialog::Accepted)
    {
        int nMaxTreeDepth = cpCartLtAbstractDialog->getMaxTreeDepth();
        int nMaxLeafNodeDepth = cpCartLtAbstractDialog->getMaxLeafNodeDepth();
        QString useTerms = "false";
        if (cpCartLtAbstractDialog->isUseTerms())
            useTerms = "true";

        QString commandStr("_ais\177eval\177exp\177");
        commandStr.append(QString("(arcDemo.getCartLtAbstractModel %1 %2 %3)").arg(nMaxTreeDepth).arg(nMaxLeafNodeDepth).arg(useTerms));

        if (cpAppClient->submit(this, commandStr, "", 0, NULL, 0) != 0)
        {
            cpSessionForm->enable(false);
        }
    }
}

void AModelPage::show()
{
    setupWidgetValues();
}

QList<int> AModelPage::getIndependentVariableIndexes()
{
    QList<int> independentVars;
//    QList<QListWidgetItem *> selectedItems = cUi.upIndependentVarList->selectedItems();
//    QListIterator<QListWidgetItem *> selectedItem(selectedItems);
//    while (selectedItem.hasNext())
//    {
//        QListWidgetItem *pListItem = selectedItem.next();
//        independentVars.append(pListItem->data(Qt::UserRole).toInt());
//    }

    int nDepIdx = getDependentVariableIndex();
    int nCount = cUi.upDependentVarComboBox->count();
    for (int i = 0; i < nCount; i++)
    {
        int nColIdx = cUi.upDependentVarComboBox->itemData(i, Qt::UserRole).toInt();
        if (nDepIdx != nColIdx)
        {
            independentVars.append(nColIdx);
        }
    }

    return independentVars;
}

int AModelPage::getDependentVariableIndex()
{
    return cUi.upDependentVarComboBox->itemData(cUi.upDependentVarComboBox->currentIndex()).toInt();
}

int AModelPage::getMaxGenerations()
{
    return cUi.upMaxGenSpinBox->value();
}

double AModelPage::getHaltingFitness()
{
    return cUi.upHaltingFitnessSpinBox->value();
}

QString AModelPage::getGoalExpression()
{
    return cUi.upGoalExprTextEdit->toPlainText().trimmed();
}

QString AModelPage::getRegressionType()
{
	return cUi.upRegressionTypeComboBox->itemData(cUi.upRegressionTypeComboBox->currentIndex()).toString();
}
