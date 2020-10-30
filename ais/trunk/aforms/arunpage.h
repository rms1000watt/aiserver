#ifndef ARUNPAGE_H
#define ARUNPAGE_H

#include "ais.h"
#include "apage.h"
#include "ui_arunpage.h"

#include <QtCore/QMap>

class QWidget;
class AAppClient;
class ASessionForm;

/*!
\brief ARunPage provides a GUI to allow the user to start/stop runs.

\par Notes:
-# Regular cells can only contain numerical values.
-# The 1nd row is reserved for variable name.
  */
class ARunPage : public QWidget, public APage, public AReturnRcvr
{
    Q_OBJECT

public:

    /*!
      \brief Class constructor.
      */
    ARunPage(AAppClient* ipAppClient,
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

	void onStatusUpdateEvent(const QString& irMsg);

    void display(const QString& irDisplay, const QString& irOut = QString());

    bool isDirty();

    void setDirty(bool dirty = true);

protected:

    void clearResults();

private slots:

    void on_upStartPushButton_clicked(bool checked = false);
    void on_upStopPushButton_clicked(bool checked = false);

private:

    Ui::ARunPageClass cUi;

    AAppClient *cpAppClient;
    ASessionForm *cpSessionForm;

	QMap<QString, int> cLabelRowMap;

    bool cIsDirty;
};

#endif // ARUNPAGE_H
