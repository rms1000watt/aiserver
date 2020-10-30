/**********************************************************************************
    Copyright (C) 2008 Investment Science Corp.

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
#ifndef AFORM_H
#define AFORM_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/aform.h
															Form

CHANGE HISTORY
Version	Date		Who		Change
3.2009	05/08/2007	fchua	Added new parameters in setParameters().
3.2008	4/8/2008	fchua	Added editPreferences signal. Added setParameters function.
3.2002	2/4/2008	fchua	Added formClosed signal.
1.0100	6/9/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QTabWidget>
#include "aparameters.h"

class QMenu;

//	------------------------------------------------------- TYPEDEFS ----------------------------------------------------------
typedef QList<QMenu*> AMenuList;
typedef QList<QAction*> AToolList;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief AForm - An abstract base class that is inherited by all forms instantiated by AMainWindow.

This interface describes the common set of public methods provided by every form (currently AServerForm and ASessionForm).
\par Notes:
 -# AMainWindow can call common methods on any of its child forms that it instantiates in its workspace. 

\par New Form:
- ConnectMgr creates new child form (ServerForm, or SessionForm)
- MainWindow removes the current child menus, if any, from the menu bar.
- MainWindow gets the menu names from the child form, and adds these new menus to the menu bar.
- MainWindow passes the list of new menu pointers back to the child form. Child form fills in these menus with its actions.
- ConnectMgr calls child form's startup routine

\par Change Forms:
- MainWindow removes the menus for the current child form from the menu bar
- MainWindow gets the menu ptrs from the new child form and then adds these menus to the menu bar.
 */
class AForm : public QTabWidget
{
	Q_OBJECT
public:
	AForm(QWidget* ipParent, const char* ipFormName);

	virtual void getFormMenus(AMenuList& orFormMenus, AMenuList& orTabMenus, AToolList& orFormTools, AToolList& orTabTools) = 0;
	virtual void setParameters(AParameters* ipConsole = 0, AParameters* ipEditor = 0, AParameters* ipLog = 0, AParameters* ipCabinet = 0, AParameters* ipDebug = 0) = 0;
	virtual void startup(const QString& irStartupMsgs) = 0;
	
	void setCloseEnabled(bool iEnabled) { cEnabled = iEnabled; }
	bool isCloseEnabled() { return cEnabled; }

signals:
	/*!
	 * \brief This signal can be emitted when a closing of a window is accepted or cancelled.
	 *
	 * \param[in] iClosed True if accepted, False if cancelled.
	 */
	void formClosed(bool iClosed);
	
	/*!
	 * \brief This signal can be emitted when there's request to change the application settings.
	 */
	void editPreferences();

private:
	bool cEnabled;
};

#endif		// AFORM_H
