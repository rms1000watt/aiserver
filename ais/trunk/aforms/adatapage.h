/**********************************************************************************
    Copyright (C) 2011 AIS Foundation, Inc.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/
#ifndef ADATAPAGE_H
#define ADATAPAGE_H

#include "ais.h"
#include "apage.h"
#include "ui_adatapage.h"
#include "adatapagemodel.h"

class QAction;
class QLineEdit;
class QMenu;
class QWidget;

class AAppClient;
class ASessionForm;
class AGenerateDataDialog;

#define INDEX_TRAINING_DATA     0
#define INDEX_TESTING_DATA      1
#define INDEX_ESTIMATES_DATA    2

/*!
\brief ADataPage provides a GUI to allow the user to enter training and/or testing data.

ADataPage provides a spreadsheet-like interface for training and/or testing data.
Source of data could be any of the following: Input, clipboard, or file.

\par Notes:
-# Regular cells can only contain numerical values.
-# The 1nd row is reserved for variable name.
  */
class ADataPage : public QWidget, public APage, public AReturnRcvr
{
    Q_OBJECT

public:

    /*!
      \brief Class constructor.
      */
    ADataPage(AAppClient* ipAppClient,
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
      \brief Returns the current data model.
      */
    ADataPageModel& getCurrentModel();

    /*!
      \brief Returns the current table view.
      */
    QTableView* getCurrentView();

    /*!
      \brief Returns the table model in the specified index.
      */
    ADataPageModel& getModel(int index);

    /*!
      \brief Returns the table view in the specified index.
      */
    QTableView* getTableView(int index);

protected:

    void generateData();
    void doCut();
    void doCopy();
    void doPaste();
    void doDelete();

private slots:

    void on_upClearPushButton_clicked(bool);
    void on_upGeneratePushButton_clicked(bool);
    void on_upOptionsPushButton_clicked(bool);

    void onReturnPressed();
    void onFileNew();
    void onFileOpen();
    void onFileSave();

    void onEditCut();
    void onEditCopy();
    void onEditPaste();

private:

    void createActions();
    void createMenus();

    Ui::ADataPageClass cUi;

    ADataPageModel cTrainingDataModel;
    ADataPageModel cTestingDataModel;
    ADataPageModel cEstimatesDataModel;

    int cMode;
    // 0 - none
    // 1 - training data requested
    // 2 - testing data requested
    // 3 - training and testing data requested

    AAppClient *cpAppClient;
    ASessionForm *cpSessionForm;
    AGenerateDataDialog *cpGenerateDataDialog;
    QLineEdit *cpCmdLineEdit;
    AToolList cToolList;

    QMenu *cpFileMenu;
    QMenu *cpEditMenu;

    QAction *cpFileNewAct;
    QAction *cpFileOpenAct;
    QAction *cpFileSaveAct;
    QAction *cpFileSaveAsAct;

    QAction *cpEditCutAct;
    QAction *cpEditCopyAct;
    QAction *cpEditPasteAct;
};

class ATableViewKeyPressHandler : public QObject
{
    Q_OBJECT

protected:

    bool eventFilter(QObject *obj, QEvent *event);
};

#endif // ADATAPAGE_H
