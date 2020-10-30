#ifndef AMODELPAGE_H
#define AMODELPAGE_H

#include "ais.h"
#include "apage.h"
#include "ui_amodelpage.h"

class QWidget;
class AAppClient;
class ASessionForm;
class AGenerateGoalDialog;
class AGenerateCartLtAbstractDialog;

/*!
\brief AModelPage provides a GUI to allow the user specify search parameters.

AModelPage provides a facility to configure search parameters.

\par Notes:
-# Algorithm: GSM, ARC, etc.
-# Goal Expression: Default button loads the default goal expression.
-# Advanced Search: Allows user to type in where clauses.
-# Scoring Function: Allows user to choose scoring function or define their own.
-# Dependent Variable: Allows user to select which column is the dependent variable.
-# Independent Variables: Allows user to choose which columns are the independent variables.
-# Maximum Generations: Allows user to specify maximum no. of generations.
-# Halting Score: Allow user to specify halting score.
  */
class AModelPage : public QWidget, public APage, public AReturnRcvr
{
    Q_OBJECT

public:

    /*!
      \brief Class constructor.
      */
    AModelPage(AAppClient* ipAppClient,
               ASessionForm* ipParentForm,
               QWidget* ipParent,
               const char* ipName = NULL);

    /*!
      \brief Returns the menu and toolbar items associated with this tab.
      */
    virtual void getTabMenus(AMenuList& orTabMenus,
                             AToolList& orTabTools,
                             bool iSelected);

    /*!
      \brief Handler for AppClient output.
      */
    virtual void returnOutput(long iConnectId,
                              long iRqId,
                              long iStatus,
                              AReqType iReqType,
                              long iRetValue,
                              const QString& irOut,
                              char* ipData,
                              long iDataSize,
                              const QString& irDisplay,
                              const QString& irError,
                              const QString iClientData = QString());

    /*!
      \brief Called when the page receives focus.
      */
    virtual void show();

    QList<int> getIndependentVariableIndexes();

    int getDependentVariableIndex();

    int getMaxGenerations();

    double getHaltingFitness();

    QString getGoalExpression();

	QString getRegressionType();

    // TODO: When do we update the widget values?

protected:

    void setupWidgetValues();
    void generateAbstractModel();
    void generateCartLtAbstractModel();
    void generateLinearModel();
    void generateCubicModel();

private slots:

    void on_upAbstractModelPushButton_clicked(bool checked = false);
    void on_upCartLtAbstractModelPushButton_clicked(bool checked = false);
    void on_upLinearModelPushButton_clicked(bool checked = false);
    void on_upCubicModelPushButton_clicked(bool checked = false);

private:

    Ui::AModelPageClass cUi;

    AAppClient *cpAppClient;
    ASessionForm *cpSessionForm;
    AGenerateGoalDialog *cpAbstractDialog;
    AGenerateCartLtAbstractDialog *cpCartLtAbstractDialog;
};

#endif // AMODELPAGE_H
