#include "ageneratedatadialog.h"
#include "ui_ageneratedatadialog.h"

AGenerateDataDialog::AGenerateDataDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AGenerateDataDialog)
{
    ui->setupUi(this);
    ui->upRandomSeedSpinBox->setEnabled(false);
}

AGenerateDataDialog::~AGenerateDataDialog()
{
    delete ui;
}

bool AGenerateDataDialog::isGenerateTrainingDataChecked()
{
    return ui->upGenTrainingDataCheckBox->isChecked();
}

bool AGenerateDataDialog::isGenerateTestingDataChecked()
{
    return ui->upGenTestingDataCheckBox->isChecked();
}

bool AGenerateDataDialog::isRandomSeedChecked()
{
    return ui->upRandomSeedCheckBox->isChecked();
}

int AGenerateDataDialog::getNumTrainingDataRows()
{
    return ui->upNumTrainingRowsSpinBox->value();
}

int AGenerateDataDialog::getNumTestingDataRows()
{
    return ui->upNumTestingRowsSpinBox->value();
}

int AGenerateDataDialog::getNumColumns()
{
    return ui->upNumColsSpinBox->value();
}

int AGenerateDataDialog::getRandomSeed()
{
    return ui->upRandomSeedSpinBox->value();
}

double AGenerateDataDialog::getRange()
{
    return ui->upRangeSpinBox->value();
}

double AGenerateDataDialog::getRandomError()
{
    return ui->upRandomErrorSpinBox->value();
}

double AGenerateDataDialog::getSkew()
{
    return ui->upSkewSpinBox->value();
}

void AGenerateDataDialog::setGenerateTrainingDataChecked(bool checked)
{
    ui->upGenTrainingDataCheckBox->setChecked(checked);
}

void AGenerateDataDialog::setGenerateTestingDataChecked(bool checked)
{
    ui->upGenTestingDataCheckBox->setChecked(checked);
}

void AGenerateDataDialog::setRandomSeedChecked(bool checked)
{
    ui->upRandomSeedCheckBox->setChecked(checked);
}

void AGenerateDataDialog::setNumTrainingDataRows(int numRows)
{
    ui->upNumTrainingRowsSpinBox->setValue(numRows);
}

void AGenerateDataDialog::setNumTestingDataRows(int numRows)
{
    ui->upNumTestingRowsSpinBox->setValue(numRows);
}

void AGenerateDataDialog::setNumColumns(int numColumns)
{
    ui->upNumColsSpinBox->setValue(numColumns);
}

void AGenerateDataDialog::setRandomSeed(int seed)
{
    ui->upRandomSeedSpinBox->setValue(seed);
}

void AGenerateDataDialog::setRandomError(double error)
{
    ui->upRandomErrorSpinBox->setValue(error);
}

void AGenerateDataDialog::setSkew(double skew)
{
    ui->upSkewSpinBox->setValue(skew);
}

void AGenerateDataDialog::on_upRandomSeedCheckBox_stateChanged(int state)
{
    ui->upRandomSeedSpinBox->setEnabled((state == Qt::Checked));
}
