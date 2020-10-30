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
#ifndef ASYSRESMONPAGE_H
#define ASYSRESMONPAGE_H

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/asysresmonpage.h
												System Resource Monitor Page

CHANGE HISTORY
Version	Date		Who		Change
3.2003	02/15/2008	fchua	Initial version.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "apage.h"
#include "ui_asysresmonpage.h"

//  ------------------------------------------------- FORWARD DECLARATIONS ----------------------------------------------------
class QWidget;
class AServerForm;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
 * \brief Provides a page for system resource monitoring.
 *
 * The initial implementation only includes monitoring of the following:
 * -# The number of connections per connection type (HTTP, XML, APP)
 * -# The number of active logons per user
 * -# The number of active sessions per user (including disconnected sessions)
 * -# The number of pending requests per session
 */
class ASysResMonPage : public QWidget, public APage
{
	Q_OBJECT

public:
	ASysResMonPage(QWidget *ipParent = 0, AServerForm *ipSvrForm = 0);
	~ASysResMonPage();

	virtual void getTabMenus(AMenuList& orTabMenus, AToolList& orTabTools, bool iSelected);

	int getMonitorInterval();
	bool isResourceMonitorEnabled(int iResType);

public slots:
	void onMonitorResult(int iResType, const QString& irOut);

signals:
	/*!
	 * \brief This signal is emitted when then monitor interval value has changed.
	 */
	void monitorIntervalChanged();

private:
	Ui::ASysResMonPageClass cUi;
	AServerForm *cpSvrForm;

	void setTableHeaders();

private slots:
	void on_upIntervalSpinBox_valueChanged(int);
};

#endif // ASYSRESMONPAGE_H
