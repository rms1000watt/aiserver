#ifndef ARESULTSPAGE_H
#define ARESULTSPAGE_H

#include "ais.h"
#include "apage.h"
#include "ui_aresultspage.h"

#include <QtCore/QList>
#include <QtCore/QStringList>

class QWidget;
class AAppClient;
class ASessionForm;

typedef QList<QStringList> AResultsList;

#define INDEX_FITNESS_COLUMN    0
#define INDEX_TESTING_COLUMN    1
#define INDEX_SOLUTION_COLUMN   2

/*!
\brief AResults provides a GUI to show run results to user.

\par Notes:
  */
class AResultsPage : public QWidget, public APage, public AReturnRcvr
{
    Q_OBJECT

public:

    /*!
      \brief Class constructor.
      */
    AResultsPage(AAppClient* ipAppClient,
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

private slots:

    void on_upRefreshPushButton_clicked(bool checked = false);
    void on_upScorePushButton_clicked(bool checked = false);
    void on_upEstimatePushButton_clicked(bool checked = false);

    void on_upSolutionsTableWidget_currentCellChanged(int curRow, int curCol, int prevRow, int prevCol);
    void on_upSolutionsTableWidget_cellActivated(int curRow, int curCol);

protected:

    void displaySolutionDetails(int row);
    void getSolutions();
    void getTestingScore();
    void getEstimates();

private:

    Ui::AResultsPageClass cUi;

    AAppClient *cpAppClient;
    ASessionForm *cpSessionForm;
    AResultsList cResultsList;
};

#endif // ARESULTSPAGE_H
