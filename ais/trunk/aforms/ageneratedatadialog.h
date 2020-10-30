#ifndef AGENERATEDATADIALOG_H
#define AGENERATEDATADIALOG_H

#include <QtGui/QDialog>

namespace Ui {
    class AGenerateDataDialog;
}

class AGenerateDataDialog : public QDialog
{
    Q_OBJECT

public:

    explicit AGenerateDataDialog(QWidget *parent = 0);
    ~AGenerateDataDialog();

    bool isGenerateTrainingDataChecked();
    bool isGenerateTestingDataChecked();
    bool isRandomSeedChecked();

    int getNumTrainingDataRows();
    int getNumTestingDataRows();
    int getNumColumns();
    int getRandomSeed();
    double getRange();
    double getRandomError();
    double getSkew();

    void setGenerateTrainingDataChecked(bool checked);
    void setGenerateTestingDataChecked(bool checked);
    void setRandomSeedChecked(bool checked);

    void setNumTrainingDataRows(int numRows);
    void setNumTestingDataRows(int numRows);
    void setNumColumns(int numColumns);
    void setRandomSeed(int seed);

    void setRange(double range);
    void setRandomError(double error);
    void setSkew(double skew);

private slots:

    void on_upRandomSeedCheckBox_stateChanged(int state);

private:

    Ui::AGenerateDataDialog *ui;
};

#endif // AGENERATEDATADIALOG_H
