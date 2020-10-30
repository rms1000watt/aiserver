#ifndef AGENERATEGOALDIALOG_H
#define AGENERATEGOALDIALOG_H

#include <QtGui/QDialog>

namespace Ui {
    class AGenerateGoalDialog;
}

class AGenerateGoalDialog : public QDialog
{
    Q_OBJECT

public:
    explicit AGenerateGoalDialog(QWidget *parent = 0);
    ~AGenerateGoalDialog();

    int getMaxDepth();
    int getNumBasisFunctions();
    bool isUseTerms();

    void setMaxDepth(int depth);
    void setNumBasisFunctions(int num);
    void setUseTerms(bool use);

private:
    Ui::AGenerateGoalDialog *ui;
};

#endif // AGENERATEGOALDIALOG_H
