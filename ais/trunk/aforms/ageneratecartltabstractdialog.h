#ifndef AGENERATECARTLTABSTRACTDIALOG_H
#define AGENERATECARTLTABSTRACTDIALOG_H

#include <QtGui/QDialog>

namespace Ui {
    class AGenerateCartLtAbstractDialog;
}

class AGenerateCartLtAbstractDialog : public QDialog
{
    Q_OBJECT

public:
    explicit AGenerateCartLtAbstractDialog(QWidget *parent = 0);
    ~AGenerateCartLtAbstractDialog();

    int getMaxTreeDepth();
    int getMaxLeafNodeDepth();
    bool isUseTerms();

    void setMaxTreeDepth(int depth);
    void setMaxLeafNodeDepth(int depth);
    void setUseTerms(bool use);

private:
    Ui::AGenerateCartLtAbstractDialog *ui;
};

#endif // AGENERATECARTLTABSTRACTDIALOG_H
