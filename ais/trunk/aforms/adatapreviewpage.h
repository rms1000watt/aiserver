#ifndef ADATAPREVIEWPAGE_H
#define ADATAPREVIEWPAGE_H

#include "ais.h"
#include "apage.h"
#include "ui_adatapreviewpage.h"

class QWidget;
class AAppClient;
class ASessionForm;
class APlotter;

/*!
\brief ADataPreviewPage provides a GUI to visualize data.

\par Notes:
-# User may choose which column to display.
  */
class ADataPreviewPage : public QWidget, public APage, public AReturnRcvr
{
    Q_OBJECT

public:

    /*!
      \brief Class constructor.
      */
    ADataPreviewPage(AAppClient* ipAppClient,
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

    virtual void show();

    virtual void displayData();

    virtual void setDisplayEnabled(bool enabled);

private slots:

    void on_upXaxisComboBox_currentIndexChanged(int index);
    void on_upYaxisComboBox_currentIndexChanged(int index);

private:

    Ui::ADataPreviewPageClass cUi;

    AAppClient *cpAppClient;
    ASessionForm *cpSessionForm;
    APlotter *cpPlotter;
    bool cDisplayEnabled;
};

#endif // ADATAPREVIEWPAGE_H
