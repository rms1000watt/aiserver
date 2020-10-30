#include "agenerategoaldialog.h"
#include "ui_agenerategoaldialog.h"

AGenerateGoalDialog::AGenerateGoalDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AGenerateGoalDialog)
{
    ui->setupUi(this);
}

AGenerateGoalDialog::~AGenerateGoalDialog()
{
    delete ui;
}

int AGenerateGoalDialog::getMaxDepth()
{
    return ui->upMaxDepthSpinBox->value();
}

int AGenerateGoalDialog::getNumBasisFunctions()
{
    return ui->upBasisFunctionsSpinBox->value();
}

bool AGenerateGoalDialog::isUseTerms()
{
    return ui->upUseTermsCheckBox->isChecked();
}

void AGenerateGoalDialog::setMaxDepth(int depth)
{
    ui->upMaxDepthSpinBox->setValue(depth);
}

void AGenerateGoalDialog::setNumBasisFunctions(int num)
{
    ui->upBasisFunctionsSpinBox->setValue(num);
}

void AGenerateGoalDialog::setUseTerms(bool use)
{
    ui->upUseTermsCheckBox->setChecked(use);
}
