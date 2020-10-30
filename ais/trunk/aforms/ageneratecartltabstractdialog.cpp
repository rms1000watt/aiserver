#include "ageneratecartltabstractdialog.h"
#include "ui_ageneratecartltabstractdialog.h"

AGenerateCartLtAbstractDialog::AGenerateCartLtAbstractDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AGenerateCartLtAbstractDialog)
{
    ui->setupUi(this);
}

AGenerateCartLtAbstractDialog::~AGenerateCartLtAbstractDialog()
{
    delete ui;
}

int AGenerateCartLtAbstractDialog::getMaxTreeDepth()
{
    return ui->upMaxTreeDepthSpinBox->value();
}

int AGenerateCartLtAbstractDialog::getMaxLeafNodeDepth()
{
    return ui->upMaxLeafNodeDepthSpinBox->value();
}

bool AGenerateCartLtAbstractDialog::isUseTerms()
{
    return ui->upUseTermsCheckBox->isChecked();
}

void AGenerateCartLtAbstractDialog::setMaxTreeDepth(int depth)
{
    ui->upMaxTreeDepthSpinBox->setValue(depth);
}

void AGenerateCartLtAbstractDialog::setMaxLeafNodeDepth(int depth)
{
    ui->upMaxLeafNodeDepthSpinBox->setValue(depth);
}

void AGenerateCartLtAbstractDialog::setUseTerms(bool use)
{
    ui->upUseTermsCheckBox->setChecked(use);
}
